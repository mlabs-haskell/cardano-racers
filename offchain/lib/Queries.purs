module Lib.CardanoRacers.Queries where

import Contract.Prelude

import Aeson (Aeson, encodeAeson)
import Cardano.AsCbor (encodeCbor)
import Cardano.Plutus.Types.Address as PlutusAddress
import Cardano.Serialization.Lib
  ( address_toBech32
  , assetName_name
  , baseAddress_toAddress
  , byronAddress_toAddress
  , enterpriseAddress_toAddress
  , pointerAddress_toAddress
  , rewardAddress_toAddress
  )
import Cardano.Types (Asset(Asset), AssetName, Bech32String)
import Cardano.Types.Address
  ( Address
      ( BaseAddress
      , ByronAddress
      , EnterpriseAddress
      , RewardAddress
      , PointerAddress
      )
  )
import Cardano.Types.BaseAddress (toCsl) as BA
import Cardano.Types.EnterpriseAddress as EA
import Cardano.Types.Internal.Helpers (decodeUtf8)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.RewardAddress as RA
import Cardano.Types.Value (getMultiAsset)
import CardanoRacers.Common.Types (RacersParams, nitroToken)
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types (GameAssetType(CarType, DriverType))
import CardanoRacers.Helpers (fromBigNumToBI, fromJSBIToBI)
import CardanoRacers.Nitro.Contract (mkNitroPolicy)
import CardanoRacers.RaceRegistry.Contract (queryRegistryUtxos)
import CardanoRacers.RaceRegistry.Types
  ( RegistryEntry(PendingSelection, AssetSelection)
  )
import CardanoRacers.RacersState.Contract (queryRacersState)
import CardanoRacers.RacersState.Types (AssetPrices(AssetPrices))
import Contract.Address (getNetworkId)
import Contract.Config (ContractParams, WalletSpec)
import Contract.Monad (liftContractM, liftedM, runContract)
import Contract.Value (valueOf)
import Contract.Wallet
  ( getWalletAddresses
  , getWalletBalance
  , ownPaymentPubKeyHashes
  )
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, fromAff)
import Data.Array (concat, fromFoldable, head) as Array
import Data.Array (head)
import Data.ByteArray (byteArrayToHex)
import Data.Map (empty, keys, lookup, toUnfoldable) as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Effect.Aff.Compat (EffectFn1, mkEffectFn1)
import JS.BigInt (BigInt) as JSBigInt
import Lib.CardanoRacers.Common
  ( AssetPricesFFI
  , Lovelace
  , Nitro
  , Race
  , createRegistryParams
  , mintingPolicyHash
  , toJsBigInt
  , tokenNameToString
  )
import Literals.Undefined (undefined)
import Racers (Racers, runRacers, withContract)
import Unsafe.Coerce (unsafeCoerce)

type NFT =
  { name :: String
  , assetType :: String
  }

type Queries r =
  ( getNitroPrice :: EffectFn1 Unit (Promise Lovelace)
  , getAssetPrices ::
      EffectFn1 Unit (Promise AssetPricesFFI) -- Asset prices as object
  , getTreasuryAddress :: EffectFn1 Unit (Promise String) -- Address as bech32
  , getOperatingAddress :: EffectFn1 Unit (Promise String) -- Address as bech32
  , queryRaceRegistry :: EffectFn1 Race (Promise (Array Aeson))
  , getWalletNitroBalance :: EffectFn1 Unit (Promise Nitro)
  , getWalletNFTs :: EffectFn1 Unit (Promise (Array NFT))
  , getWalletAddress :: EffectFn1 Unit (Promise String)
  , getWalletPubKeyHash :: EffectFn1 Unit (Promise String)
  | r
  )

mkQueries :: ContractParams -> WalletSpec -> RacersParams -> Record (Queries ())
mkQueries cp walletSpec rp =
  let
    cfg = cp { walletSpec = Just walletSpec }

    runQ :: Racers ~> Aff
    runQ = runContract cfg <<< runRacers rp
  in
    { getNitroPrice: mkEffectFn1 $ const $ fromAff $ runQ getNitroPrice
    , getAssetPrices: mkEffectFn1 $ const $ fromAff $ runQ getAssetPrices
    , getTreasuryAddress: mkEffectFn1 $ const $ fromAff $ runQ
        getTreasuryAddress
    , getOperatingAddress: mkEffectFn1 $ const $ fromAff $ runQ
        getOperatingAddress
    , getWalletNitroBalance: mkEffectFn1 $ const $ fromAff $ runQ $
        getWalletNitroBalance
    , queryRaceRegistry: mkEffectFn1 $ fromAff <<< runQ <<< queryRaceRegistry
    , getWalletNFTs: mkEffectFn1 $ const $ fromAff $ runQ $ getWalletNFTs
    , getWalletAddress: mkEffectFn1 $ const $ fromAff $ runQ $ getWalletAddress
    , getWalletPubKeyHash: mkEffectFn1 $ const $ fromAff $ runQ $
        getWalletPubKeyHash
    }

getNitroPrice :: Racers Lovelace
getNitroPrice = do
  (st /\ _ /\ _) <- queryRacersState
  pure $ toJsBigInt $ fromJSBIToBI $ (unwrap st).nitroPrice

getAssetPrices :: Racers AssetPricesFFI
getAssetPrices = queryRacersState <#> fst >>> unwrap >>> _.assetPrices >>>
  assetPricesToObject
  where
  assetPricesToObject (AssetPrices x) =
    { "common": toJSBI x.common
    , "rare": toJSBI x.rare
    , "epic": toJSBI x.epic
    }

toJSBI :: JSBigInt.BigInt -> Lovelace
toJSBI = toJsBigInt <<< fromJSBIToBI

getTreasuryAddress :: Racers String
getTreasuryAddress = do
  networkId <- lift $ getNetworkId
  plutusAddr <- queryRacersState <#>
    (fst >>> unwrap >>> _.treasuryAddress)
  addrStr <- lift $ liftContractM "Could not convert Plutus address to Cardano"
    $ toBech32
    <$> PlutusAddress.toCardano networkId plutusAddr
  pure addrStr

getOperatingAddress :: Racers String
getOperatingAddress = do
  networkId <- lift $ getNetworkId
  plutusAddr <- queryRacersState <#>
    (fst >>> unwrap >>> _.operatingAddress)
  addrStr <- lift $ liftContractM "Could not convert Plutus address to Cardano"
    $ toBech32
    <$> PlutusAddress.toCardano networkId plutusAddr
  pure addrStr

registryEntryToAeson :: RegistryEntry -> Racers Aeson
registryEntryToAeson (PendingSelection pkh) = pure $ encodeAeson
  { "registered": encodeAeson pkh }
registryEntryToAeson (AssetSelection par) = do
  networkId <- lift $ getNetworkId
  addrStr <- lift $ liftContractM "Could not convert Plutus address to Cardano"
    $ toBech32
    <$>
      PlutusAddress.toCardano networkId (unwrap par).payoutAddress

  pure $ encodeAeson
    { "assetSelection":
        { "car": tokenNameToString (unwrap par).car
        , "driver": tokenNameToString (unwrap par).driver
        , "address": addrStr
        }
    }

-- TODO: check this
toBech32 :: Address -> Bech32String
toBech32 = toCsl >>> flip address_toBech32 (unsafeCoerce undefined)
  where
  toCsl = case _ of
    BaseAddress ba ->
      baseAddress_toAddress $ BA.toCsl ba
    ByronAddress ba ->
      byronAddress_toAddress $ unwrap ba
    EnterpriseAddress ea ->
      enterpriseAddress_toAddress $ EA.toCsl ea
    RewardAddress ra ->
      rewardAddress_toAddress $ RA.toCsl ra
    PointerAddress pc ->
      pointerAddress_toAddress $ unwrap pc

queryRaceRegistry :: Race -> Racers (Array Aeson)
queryRaceRegistry race = do
  rgp <- createRegistryParams race
  us <- queryRegistryUtxos rgp <#> Map.toUnfoldable >>> map (snd >>> snd) >>>
    Array.concat
  traverse registryEntryToAeson us

getWalletAddress :: Racers String
getWalletAddress = do
  ownAddr <- lift $ liftedM "could not get first wallet address"
    (Array.head <$> getWalletAddresses)
  pure $ toBech32 ownAddr

getWalletPubKeyHash :: Racers String
getWalletPubKeyHash = do
  pkh <- lift $ liftedM "could not get first wallet pubkeyhash"
    (Array.head <$> ownPaymentPubKeyHashes)
  pure $ byteArrayToHex $ unwrap $ encodeCbor $ unwrap pkh

getWalletNitroBalance :: Racers Nitro
getWalletNitroBalance = do
  bal <- lift $ liftedM "Could not get wallet balance" getWalletBalance
  nitroSymbol <- withContract (liftedM "Could not get Nitro symbol")
    $ mintingPolicyHash
    <$> mkNitroPolicy
  let
    v = valueOf (Asset (unwrap nitroSymbol) (unwrap nitroToken)) bal
  pure $ toJsBigInt $ fromBigNumToBI v

getWalletNFTs :: Racers (Array NFT)
getWalletNFTs = do
  bal <- lift $ liftedM "Could not get wallet balance" getWalletBalance
  carPolicy <- mkGameAssetPolicy CarType
  carSymbol <- lift $ liftContractM "Could not get game asset symbol (car)"
    $ head
    $ map PlutusScript.hash
    $ (unwrap carPolicy).plutusMintingPolicies

  driverPolicy <- mkGameAssetPolicy DriverType
  driverSymbol <- lift
    $ liftContractM "Could not get game asset symbol (driver)"
    $ head
    $ map PlutusScript.hash
    $ (unwrap driverPolicy).plutusMintingPolicies
  let
    (cars :: Array AssetName) = Array.fromFoldable
      $ Map.keys
      $ fromMaybe Map.empty
      $ Map.lookup carSymbol (unwrap $ getMultiAsset bal)
    (drivers :: Array AssetName) = Array.fromFoldable
      $ Map.keys
      $ fromMaybe Map.empty
      $ Map.lookup driverSymbol (unwrap $ getMultiAsset bal)
  carArray <- for cars \car -> do
    name <- lift $ liftContractM "Could not make required token names"
      $ hush
      $ decodeUtf8
      $ assetName_name
      $ unwrap
      $ car
    pure { assetType: "car", name }
  driverArray <- for drivers \driver -> do
    name <- lift $ liftContractM "Could not make required token names"
      $ hush
      $ decodeUtf8
      $ assetName_name
      $ unwrap
      $ driver
    pure { assetType: "driver", name }
  pure $ carArray <> driverArray
