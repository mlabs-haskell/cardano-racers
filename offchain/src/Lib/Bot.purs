module Lib.CardanoRacers.Bot where

import Contract.Prelude

import Aeson (Aeson)
import Cardano.AsCbor (encodeCbor)
import Cardano.Plutus.Types.Address (scriptHashAddress)
import Cardano.Plutus.Types.Address as PlutusAddress
import Cardano.Types.Address (toBech32)
import Cardano.Types.Asset (Asset(Asset, AdaAsset))
import Cardano.Types.BigNum as BigNum
import Cardano.Types.PlutusScript (hash)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Deposit.Contract
  ( PendingAssetRequest
  , consumeAndRedeemRequests
  , queryRequestsWithAirdropAddress
  )
import CardanoRacers.GameAsset.Types
  ( AssetOption
  , CarAttributes(CarAttributes)
  , DriverAttributes(DriverAttributes)
  , GameAssetAttributes(CarAttrs, DriverAttrs)
  , GameAssetObject
  , Rarity(Common, Rare, Epic)
  )
import CardanoRacers.Helpers
  ( fromBIToBigNum
  , fromBIToJSBI
  , fromBigNumToBI
  , fromJSBIToInt
  , paysToAddrConstraint
  )
import CardanoRacers.Nitro.Contract (mintNitroContract)
import CardanoRacers.Race.Contract (distributeRewards, startRace)
import CardanoRacers.RaceRegistry.Contract
  ( collectRegistryScriptLeftovers
  , initRace
  , mkRaceRegistryScript
  , queryRegistryUtxos
  , supplyRegistrySlots
  )
import CardanoRacers.RaceRegistry.Types (RegistryParams)
import CardanoRacers.RaceSlot.Types (RaceHash)
import CardanoRacers.Services.HydraDelegate (hostRaceRequest)
import CardanoRacers.Utils.HasJson (fromJs, toJs)
import Common.ContractHelpers (collectDustByThreshold)
import Contract.Address (addressFromBech32, getNetworkId)
import Contract.CborBytes (cborBytesToHex)
import Contract.Config (ContractParams, WalletSpec)
import Contract.Monad (liftContractM, liftedM, runContract)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionInput
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (lovelaceValueOf, valueOf)
import Contract.Wallet (getWalletBalance)
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, fromAff, toAffE)
import Data.Array (concat, cons, replicate) as Array
import Data.Bifunctor (rmap)
import Data.BigInt (BigInt)
import Data.BigInt (toInt, toString) as BigInt
import Data.Bitraversable (ltraverse, rtraverse)
import Data.Map (Map)
import Data.Map (fromFoldable, toUnfoldable) as Map
import Data.Newtype (modify)
import Data.String (toLower)
import Data.UInt (toInt) as UInt
import Effect.Aff.Compat
  ( EffectFn1
  , EffectFn2
  , EffectFn3
  , mkEffectFn1
  , mkEffectFn2
  , mkEffectFn3
  , runEffectFn1
  )
import Effect.Exception (error)
import Effect.Uncurried (EffectFn4, mkEffectFn4)
import Foreign.Object (Object)
import Foreign.Object (fromFoldable, toUnfoldable) as Object
import Lib.CardanoRacers.Common
  ( AssetPricesFFI
  , Lovelace
  , Nitro
  , Race
  , TransactionHashFFI
  , assetTypeFromString
  , assetTypeToString
  , createRegistryParams
  , fromJsBigInt
  , setAssetPrices
  , toJsBigInt
  , tokenNameToString
  )
import Lib.CardanoRacers.Queries (Queries, mkQueries, registryEntryToAeson)
import Racers (Racers, runRacers)
import Racers.Metadata.Cip25.Cip25String (mkCip25String, unCip25String)
import Record (merge)
import Type.Row (type (+))

type AssetRequestFFI = { rarity :: String, address :: String }
type AssetOptionFFI =
  { name :: String
  , assetType :: String -- "driver" | "car"
  , imageUrl :: String
  , description :: String
  , nitroAmount :: Nitro
  }

type AvailableAssetsFFI =
  { common :: AssetOptionFFI
  , rare :: AssetOptionFFI
  , epic :: AssetOptionFFI
  }

type GameAssetFFI =
  { assetType :: String -- "driver" | "car"
  , attributes :: Object Int
  , imageUrl :: String
  , rarity :: String
  , name :: String
  , tokenName :: String
  , description :: String
  }

type SlotUtxoFFI =
  { slotTxIn :: String
  , slotCount :: String
  , registrations :: Array Aeson
  }

type RewardDistributionFFI = Object Lovelace -- bech321 address -> lovelace

type Bot r =
  ( queryAssetRequests ::
      EffectFn1 Unit (Promise (Object (Array AssetRequestFFI)))
  , queryRaceSlotUtxos :: EffectFn1 Race (Promise (Array SlotUtxoFFI))
  , setAssetPrices :: EffectFn1 AssetPricesFFI (Promise TransactionHashFFI)
  , getWalletLovelaceBalance :: EffectFn1 Unit (Promise Lovelace)
  , mintNitro :: EffectFn1 Nitro (Promise TransactionHashFFI)
  , tryRedeemingPendingRequests ::
      EffectFn4 AvailableAssetsFFI Int Int
        (EffectFn1 AssetOptionFFI (Promise BigInt))
        (Promise (Array GameAssetFFI)) -- TODO: This should also return the transaction input to match the api
  , resupplySlots :: EffectFn3 Race Int Int (Promise Unit)
  , closeRace ::
      EffectFn2 Race RewardDistributionFFI (Promise (Array TransactionHashFFI))
  , createRace :: EffectFn3 Race Int Int (Promise Unit)
  , collectDust :: EffectFn1 Lovelace (Promise TransactionHashFFI)
  , startRace :: EffectFn1 Aeson (Promise Aeson)
  , hostRace :: EffectFn2 String Aeson (Promise TransactionHashFFI)
  , distributeRewards :: EffectFn1 Aeson (Promise Aeson)
  | r
  )

mkBot
  :: ContractParams -> WalletSpec -> RacersParams -> Record (Bot + Queries + ())
mkBot cp walletSpec rp =
  let
    queries = mkQueries cp walletSpec rp
    cfg = cp { walletSpec = Just walletSpec }

    runC :: Racers ~> Aff
    runC = runContract cfg <<< runRacers rp
  in
    { queryAssetRequests: mkEffectFn1 $ const $ fromAff $ runC $
        queryAssetRequests
    , queryRaceSlotUtxos: mkEffectFn1 $ fromAff <<< runC <<< queryRaceSlotUtxos
    , getWalletLovelaceBalance: mkEffectFn1 $ const $ fromAff $ runC $
        getWalletLovelaceBalance
    , mintNitro: mkEffectFn1 $ fromAff <<< runC <<< mintNitro
    , setAssetPrices: mkEffectFn1 $ fromAff <<< runC <<< setAssetPrices
    , tryRedeemingPendingRequests: mkEffectFn4 $
        \assets maxRequests chunkBy generateUniquenessNonce ->
          fromAff $ runC $ tryRedeemingPendingRequests assets maxRequests
            chunkBy
            generateUniquenessNonce
    , resupplySlots: mkEffectFn3 $ \race slots utxoCount -> fromAff $ runC $
        resupplySlots
          race
          slots
          utxoCount
    , closeRace: mkEffectFn2 $ \race rewardDistribution -> fromAff $ runC $
        closeRace race rewardDistribution
    , createRace: mkEffectFn3 $ \race slots utxoCount -> fromAff $ runC $
        createRace race
          slots
          utxoCount
    , collectDust: mkEffectFn1 $ fromAff <<< runC <<< collectDust
    , startRace: mkEffectFn1 $ \startParams -> fromAff $ runC do
        network <- lift getNetworkId
        toJs network <$> startRace (fromJs network startParams)
    , hostRace: mkEffectFn2 $ \httpServer hostParams -> fromAff do
        network <- runC $ lift getNetworkId
        resp <- hostRaceRequest httpServer network $ modify
          (_ { racersParams = Just rp })
          (fromJs network hostParams)
        case resp of
          Right (Right txHash) ->
            pure $ cborBytesToHex $ encodeCbor txHash
          Right (Left err) ->
            throwError $ error $ "hostRace endpoint returned error: "
              <> show err
          Left httpError ->
            throwError $ error $ "hostRace request failed with error: "
              <> show httpError
    , distributeRewards: mkEffectFn1 $ \raceParams -> fromAff $ runC do
        network <- lift getNetworkId
        toJs unit <$> distributeRewards (fromJs network raceParams)
    } `merge` queries

mintNitro :: Nitro -> Racers TransactionHashFFI
mintNitro = map (cborBytesToHex <<< encodeCbor) <<< mintNitroContract <<<
  fromJsBigInt

queryAssetRequests :: Racers (Object (Array AssetRequestFFI))
queryAssetRequests = do
  (at :: Array (Tuple TransactionInput PendingAssetRequest)) <- Map.toUnfoldable
    <$> queryRequestsWithAirdropAddress
  x <- traverse toAssetRequestFFI at
  pure $ Object.fromFoldable x
  where
  toAssetRequestFFI
    :: Tuple TransactionInput PendingAssetRequest
    -> Racers (Tuple String (Array AssetRequestFFI))
  toAssetRequestFFI (txi /\ { airdropAddress, requestedAssets }) = lift do
    let
      addrStr = toBech32 airdropAddress
    as <- Array.concat <$> traverse
      ( \(r /\ bi) -> do
          i <- liftContractM "Could not convert BigInt to Int" $ BigInt.toInt bi
          pure $ Array.replicate i
            { address: addrStr, rarity: toLower $ show r }
      )
      requestedAssets
    let
      txiHash = cborBytesToHex $ encodeCbor $ (unwrap txi).transactionId
      txiIdx = show $ UInt.toInt (unwrap txi).index
    pure $ (txiHash <> "#" <> txiIdx) /\ as

queryRaceSlotUtxos :: Race -> Racers (Array SlotUtxoFFI)
queryRaceSlotUtxos race = do
  rgp <- createRegistryParams race
  regUtxos <- queryRegistryUtxos rgp <#> Map.toUnfoldable
  let
    assetName = Asset (fst (unwrap rgp).slotAssetClass)
      (snd (unwrap rgp).slotAssetClass)
  traverse
    ( \(txi /\ (txo /\ rges)) -> do

        reAeson <- traverse registryEntryToAeson rges
        pure $
          { slotTxIn: (cborBytesToHex $ encodeCbor $ (unwrap txi).transactionId)
              <> "#"
              <> show (UInt.toInt (unwrap txi).index)
          , slotCount: BigNum.toString $
              (valueOf assetName (unwrap txo).amount)

          , registrations: reAeson
          }
    )
    regUtxos

getWalletLovelaceBalance :: Racers Lovelace
getWalletLovelaceBalance = lift do
  bal <- liftedM "Could not get wallet balance" getWalletBalance
  pure $ toJsBigInt $ fromBigNumToBI $ valueOf AdaAsset bal

tryRedeemingPendingRequests
  :: AvailableAssetsFFI
  -> Int
  -> Int
  -> (EffectFn1 AssetOptionFFI (Promise BigInt))
  -> Racers (Array GameAssetFFI)
tryRedeemingPendingRequests
  assetsFFI
  chunkBy
  maxRequests
  generateUniquenessNonce = do
  assets <- toAssetOptionMap assetsFFI
  map toGameAssetFFI <$> consumeAndRedeemRequests chunkBy (Just maxRequests)
    assets
    ( map BigInt.toString <<< toAffE <<< runEffectFn1 generateUniquenessNonce
        <<< toAssetOptionFFI
    )
  where
  toAssetOptionMap :: AvailableAssetsFFI -> Racers (Map Rarity AssetOption)
  toAssetOptionMap av = Map.fromFoldable <$>
    ( traverse (rtraverse toAssetOption)
        [ Common /\ av.common
        , Rare /\ av.rare
        , Epic /\ av.epic
        ]
    )

  toAssetOption :: AssetOptionFFI -> Racers AssetOption
  toAssetOption affi = lift $ do
    name <- liftContractM "Could not create token name from given name" $
      mkCip25String affi.name
    assetType <- liftContractM "Could not create asset type from given type" $
      assetTypeFromString affi.assetType
    pure
      { name
      , assetType
      , imageUrl: affi.imageUrl
      , description: affi.description
      , nitroAmount: fromJsBigInt affi.nitroAmount
      }

  toAssetOptionFFI :: AssetOption -> AssetOptionFFI
  toAssetOptionFFI ao =
    { name: unCip25String ao.name
    , assetType: assetTypeToString ao.assetType
    , imageUrl: ao.imageUrl
    , description: ao.description
    , nitroAmount: toJsBigInt ao.nitroAmount
    }

  toGameAssetFFI :: GameAssetObject -> GameAssetFFI
  toGameAssetFFI gao =
    { name: unCip25String gao.name
    , assetType: assetTypeToString gao.assetType
    , imageUrl: gao.imageUrl
    , rarity: toLower $ show gao.rarity
    , description: gao.description
    , tokenName: tokenNameToString $ unwrap gao.tokenName
    , attributes: attributesToObject gao.attributes
    }

  attributesToObject :: GameAssetAttributes -> Object Int
  attributesToObject (DriverAttrs (DriverAttributes da)) = Object.fromFoldable
    [ "experience" /\ fromJSBIToInt da.experience
    , "aggression" /\ fromJSBIToInt da.aggression
    , "reflexes" /\ fromJSBIToInt da.reflexes
    , "luck" /\ fromJSBIToInt da.luck
    ]
  attributesToObject (CarAttrs (CarAttributes ca)) = Object.fromFoldable
    [ "topSpeed" /\ fromJSBIToInt ca.topSpeed
    , "acceleration" /\ fromJSBIToInt ca.acceleration
    , "cornering" /\ fromJSBIToInt ca.cornering
    , "aerodynamics" /\ fromJSBIToInt ca.aerodynamics
    ]

createRace :: Race -> Int -> Int -> Racers Unit
createRace race slotCount utxoCount = void
  $ initRace
      (wrap race.raceId)
      (fromJsBigInt race.nitroFee)
      slotCount
      utxoCount

resupplySlots :: Race -> Int -> Int -> Racers Unit
resupplySlots race slotCount utxoCount = do
  rgp <- createRegistryParams race
  void $ supplyRegistrySlots (wrap race.raceId) rgp slotCount utxoCount

closeRace :: Race -> RewardDistributionFFI -> Racers (Array TransactionHashFFI)
closeRace race rewardsFFI = do
  rgp <- createRegistryParams race

  collectTxIds <- collectRegistryRetryOnFailure (wrap race.raceId) rgp

  rewards <-
    traverse
      ( ltraverse $ lift <<<
          ( addressFromBech32
              >>>
                ( \ca ->
                    do
                      addr <- ca
                      paddr <-
                        liftContractM
                          "Could not convert address from Plutus to Cardano"
                          $ PlutusAddress.fromCardano addr
                      pure paddr
                )
          )
      )
      $ rmap (fromBIToBigNum <<< fromJsBigInt)
      <$> (Object.toUnfoldable rewardsFFI :: Array _)

  let
    constraints :: Constraints.TxConstraints
    constraints = foldMap
      (\(addr /\ amount) -> paysToAddrConstraint addr $ lovelaceValueOf amount)
      rewards

  if null rewards then pure collectTxIds
  else do
    txId <- lift $ submitTxFromConstraints (mempty :: Lookups.ScriptLookups)
      constraints
    lift $ awaitTxConfirmed txId
    pure $ Array.cons (cborBytesToHex $ encodeCbor txId) collectTxIds

-- TODO: Collecting is very inefficient, at time of writing, testing on preview
-- scripts can only handle 2 registry utxos in one tx.
collectRegistryRetryOnFailure
  :: RaceHash -> RegistryParams -> Racers (Array TransactionHashFFI)
collectRegistryRetryOnFailure raceHash rgp = do
  networkId <- lift $ getNetworkId
  let
    go 1 txhs = collectRegistryScriptLeftovers raceHash rgp 1 <#>
      (flip Array.cons txhs)
    go n txhs = do
      registryScript <- mkRaceRegistryScript rgp
      let
        addrPlutus = scriptHashAddress (wrap $ hash $ unwrap registryScript)
          Nothing
      addr <- lift
        $ liftContractM "Could not convert address from Plutus to Cardano"
        $ PlutusAddress.toCardano networkId addrPlutus

      utxosAtRegistry <- lift $ utxosAt addr
      if null utxosAtRegistry then pure txhs
      else try (collectRegistryScriptLeftovers raceHash rgp n)
        >>= either
          (const $ go (n - 1) txhs)
          ( \txId -> lift (awaitTxConfirmed txId) *> go (n + 1)
              (Array.cons txId txhs)
          )
  go 5 [] <#> map (encodeCbor >>> cborBytesToHex)

collectDust :: Lovelace -> Racers TransactionHashFFI
collectDust = lift <<< map (cborBytesToHex <<< encodeCbor)
  <<< collectDustByThreshold
  <<< fromBIToJSBI
  <<< fromJsBigInt
