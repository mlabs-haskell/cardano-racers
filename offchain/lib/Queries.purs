module Lib.CardanoRacers.Queries where

import Contract.Prelude

import Aeson (Aeson, encodeAeson)
import CardanoRacers.Common.Types (RacersParams, nitroToken)
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types (GameAssetType(CarType, DriverType))
import CardanoRacers.Nitro.Contract (mkNitroPolicy)
import CardanoRacers.RaceRegistry.Contract (queryRegistryUtxos)
import CardanoRacers.RaceRegistry.Types
  ( RegistryEntry(PendingSelection, AssetSelection)
  )
import CardanoRacers.RacersState.Contract (queryRacersState)
import CardanoRacers.RacersState.Types (AssetPrices(AssetPrices))
import Contract.Address (addressToBech32)
import Contract.AssocMap as PlutusMap
import Contract.Config (ContractParams, WalletSpec)
import Contract.Monad (liftContractM, liftedM, runContract)
import Contract.Prim.ByteArray (rawBytesToHex)
import Contract.Scripts (mintingPolicyHash)
import Contract.Value (getValue, mpsSymbol, scriptCurrencySymbol, valueOf)
import Contract.Value as Value
import Contract.Wallet (getWalletBalance)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, fromAff)
import Ctl.Internal.Serialization.Hash (ed25519KeyHashToBytes)
import Data.Array (concat) as Array
import Data.Map (toUnfoldable) as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.TextDecoder (decodeUtf8)
import Effect.Aff.Compat (EffectFn1, mkEffectFn1)
import Lib.CardanoRacers.Common
  ( AssetPricesFFI
  , Lovelace
  , Nitro
  , Race
  , createRegistryParams
  , toJsBigInt
  , tokenNameToString
  )
import Racers (Racers, runRacers, withContract)

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
    }

getNitroPrice :: Racers Lovelace
getNitroPrice = queryRacersState <#> fst >>> unwrap >>> _.nitroPrice >>>
  toJsBigInt

getAssetPrices :: Racers AssetPricesFFI
getAssetPrices = queryRacersState <#> fst >>> unwrap >>> _.assetPrices >>>
  assetPricesToObject
  where
  assetPricesToObject (AssetPrices x) =
    { "common": toJsBigInt x.common
    , "rare": toJsBigInt x.rare
    , "epic": toJsBigInt x.epic
    }

getTreasuryAddress :: Racers String
getTreasuryAddress = queryRacersState <#> (fst >>> unwrap >>> _.treasuryAddress)
  >>= lift
  <<< addressToBech32

getOperatingAddress :: Racers String
getOperatingAddress = queryRacersState
  <#> (fst >>> unwrap >>> _.operatingAddress)
  >>= lift
  <<< addressToBech32

queryRaceRegistry :: Race -> Racers (Array Aeson)
queryRaceRegistry race = do
  rgp <- createRegistryParams race
  us <- queryRegistryUtxos rgp <#> Map.toUnfoldable >>> map (snd >>> snd) >>>
    Array.concat
  traverse entryToAeson us
  where
  entryToAeson (PendingSelection pkh) = pure $ encodeAeson
    { "registered": rawBytesToHex $ ed25519KeyHashToBytes (unwrap pkh) }
  entryToAeson (AssetSelection par) =
    lift (addressToBech32 (unwrap par).payoutAddress) <#> \addrStr ->
      encodeAeson
        { "assetSelection":
            { "car": tokenNameToString (unwrap par).car
            , "driver": tokenNameToString (unwrap par).driver
            , "address": addrStr
            }
        }

getWalletNitroBalance :: Racers Nitro
getWalletNitroBalance = do
  bal <- lift $ liftedM "Could not get wallet balance" getWalletBalance
  nitroSymbol <- withContract (liftedM "Could not get Nitro symbol") $ mpsSymbol
    <<< mintingPolicyHash
    <$> mkNitroPolicy
  pure $ toJsBigInt $ valueOf bal nitroSymbol nitroToken

getWalletNFTs :: Racers (Array NFT)
getWalletNFTs = do
  bal <- lift $ liftedM "Could not get wallet balance" getWalletBalance
  carPolicy <- mkGameAssetPolicy CarType
  carSymbol <- lift $ liftContractM "Could not get game asset symbol (car)" $
    scriptCurrencySymbol carPolicy
  driverPolicy <- mkGameAssetPolicy DriverType
  driverSymbol <- lift
    $ liftContractM "Could not get game asset symbol (driver)"
    $ scriptCurrencySymbol driverPolicy
  let
    cars =
      PlutusMap.keys $ fromMaybe PlutusMap.empty $
        PlutusMap.lookup carSymbol (getValue bal)
    drivers =
      PlutusMap.keys $ fromMaybe PlutusMap.empty $
        PlutusMap.lookup driverSymbol (getValue bal)
  carArray <- for cars \car -> do
    name <- lift $ liftContractM "Could not make required token names"
      $ hush
      $ decodeUtf8
      $ unwrap
      $ Value.getTokenName car
    pure { assetType: "car", name }
  driverArray <- for drivers \driver -> do
    name <- lift $ liftContractM "Could not make required token names"
      $ hush
      $ decodeUtf8
      $ unwrap
      $ Value.getTokenName driver
    pure { assetType: "driver", name }
  pure $ carArray <> driverArray
