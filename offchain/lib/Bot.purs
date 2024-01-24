module Lib.CardanoRacers.Bot where

import Contract.Prelude

import Aeson (Aeson, encodeAeson, stringifyAeson)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Deposit.Contract (PendingAssetRequest, consumeAndRedeemRequests, queryRequestsWithAirdropAddress)
import CardanoRacers.GameAsset.Types (AssetOption, CarAttributes(CarAttributes), DriverAttributes(DriverAttributes), GameAssetAttributes(CarAttrs, DriverAttrs), GameAssetObject, Rarity(Common, Rare, Epic))
import CardanoRacers.Helpers (paysToAddrConstraint)
import CardanoRacers.Nitro.Contract (mintNitroContract)
import CardanoRacers.RaceRegistry.Contract (collectRegistryScriptLeftovers, initRace, mkRaceRegistryScript, queryRegistryUtxos, supplyRegistrySlots)
import CardanoRacers.RaceRegistry.Types (RegistryParams)
import CardanoRacers.RaceSlot.Types (RaceHash)
import Common.ContractHelpers (collectDustByThreshold)
import Contract.Address (addressFromBech32, addressToBech32, scriptHashAddress)
import Contract.Config (ContractParams, WalletSpec)
import Contract.Metadata (mkCip25String, unCip25String)
import Contract.Monad (liftContractM, liftedM, runContract)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionInput, awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (adaSymbol, adaToken, lovelaceValueOf, valueOf)
import Contract.Wallet (getWalletBalance)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, fromAff, toAffE)
import Data.Array (concat, replicate, cons) as Array
import Data.Bifunctor (rmap)
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, toInt, toString) as BigInt
import Data.Bitraversable (ltraverse, rtraverse)
import Data.Map (Map)
import Data.Map (fromFoldable, toUnfoldable) as Map
import Data.String (toLower)
import Data.UInt (toInt) as UInt
import Effect.Aff.Compat (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn1)
import Effect.Uncurried (EffectFn4, mkEffectFn4)
import Foreign.Object (Object)
import Foreign.Object (fromFoldable, toUnfoldable) as Object
import Lib.CardanoRacers.Common (Lovelace, Nitro, Race, TransactionHashFFI, assetTypeFromString, assetTypeToString, createRegistryParams, fromJsBigInt, toJsBigInt, tokenNameToString)
import Lib.CardanoRacers.Queries (Queries, mkQueries, registryEntryToAeson)
import Partial.Unsafe (unsafePartial)
import Racers (Racers, runRacers)
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
  , getWalletLovelaceBalance :: EffectFn1 Unit (Promise Lovelace)
  , mintNitro :: EffectFn1 Nitro (Promise TransactionHashFFI)
  , tryRedeemingPendingRequests ::
      EffectFn4 AvailableAssetsFFI Int Int
        (EffectFn1 AssetOptionFFI (Promise BigInt))
        (Promise (Array GameAssetFFI)) -- TODO: This should also return the transaction input to match the api
  , resupplySlots :: EffectFn3 Race Int Int (Promise Unit)
  , closeRace ::
      EffectFn2 Race RewardDistributionFFI (Promise (Array TransactionHashFFI))
  -- TODO: parameterise the slot distribution number (no of slots per utxo)
  , createRace :: EffectFn3 Race Int Int (Promise Unit)
  , collectDust :: EffectFn1 Lovelace (Promise TransactionHashFFI)
  -- TODO: query the list of utxos that contain slots given a race
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
    } `merge` queries

mintNitro :: Nitro -> Racers TransactionHashFFI
mintNitro = map (byteArrayToHex <<< unwrap) <<< mintNitroContract <<<
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
    addrStr <- addressToBech32 airdropAddress
    as <- Array.concat <$> traverse
      ( \(r /\ bi) -> do
          i <- liftContractM "Could not convert BigInt to Int" $ BigInt.toInt bi
          pure $ Array.replicate i
            { address: addrStr, rarity: toLower $ show r }
      )
      requestedAssets
    let
      txiHash = byteArrayToHex (unwrap (unwrap txi).transactionId)
      txiIdx = show $ UInt.toInt (unwrap txi).index
    pure $ (txiHash <> "#" <> txiIdx) /\ as

queryRaceSlotUtxos :: Race -> Racers (Array SlotUtxoFFI)
queryRaceSlotUtxos race = do
  rgp <- createRegistryParams race
  regUtxos <- queryRegistryUtxos rgp <#> Map.toUnfoldable
  traverse 
      ( \(txi /\ (txo /\ rges)) -> do
          reAeson <- traverse registryEntryToAeson rges
          pure $
            { slotTxIn: byteArrayToHex (unwrap (unwrap txi).transactionId) <> "#"
                <> show (UInt.toInt (unwrap txi).index)
            , slotCount: BigInt.toString $ uncurry
                (valueOf (unwrap (unwrap txo).output).amount)
                (unwrap rgp).slotAssetClass
            , registrations: reAeson
            }
      )
      regUtxos

getWalletLovelaceBalance :: Racers Lovelace
getWalletLovelaceBalance = lift do
  bal <- liftedM "Could not get wallet balance" getWalletBalance
  pure $ toJsBigInt $ valueOf bal adaSymbol adaToken

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
    , tokenName: tokenNameToString gao.tokenName
    , attributes: attributesToObject gao.attributes
    }

  attributesToObject :: GameAssetAttributes -> Object Int
  attributesToObject (DriverAttrs (DriverAttributes da)) = Object.fromFoldable
    [ "experience" /\ unsafePartial (fromJust $ BigInt.toInt da.experience)
    , "aggression" /\ unsafePartial (fromJust $ BigInt.toInt da.aggression)
    , "reflexes" /\ unsafePartial (fromJust $ BigInt.toInt da.reflexes)
    , "luck" /\ unsafePartial (fromJust $ BigInt.toInt da.luck)
    ]
  attributesToObject (CarAttrs (CarAttributes ca)) = Object.fromFoldable
    [ "topSpeed" /\ unsafePartial (fromJust $ BigInt.toInt ca.topSpeed)
    , "acceleration" /\ unsafePartial (fromJust $ BigInt.toInt ca.acceleration)
    , "cornering" /\ unsafePartial (fromJust $ BigInt.toInt ca.cornering)
    , "aerodynamics" /\ unsafePartial (fromJust $ BigInt.toInt ca.aerodynamics)
    ]

createRace :: Race -> Int -> Int -> Racers Unit
createRace race slotCount utxoCount = void
  $ initRace
      (wrap race.raceId)
      (fromJsBigInt race.nitroFee)
      (BigInt.fromInt slotCount)
      (BigInt.fromInt utxoCount)

resupplySlots :: Race -> Int -> Int -> Racers Unit
resupplySlots race slotCount utxoCount = do
  rgp <- createRegistryParams race
  void $ supplyRegistrySlots (wrap race.raceId) rgp (BigInt.fromInt slotCount)
    (BigInt.fromInt utxoCount)

closeRace :: Race -> RewardDistributionFFI -> Racers (Array TransactionHashFFI)
closeRace race rewardsFFI = do
  rgp <- createRegistryParams race

  collectTxIds <- collectRegistryRetryOnFailure (wrap race.raceId) rgp

  rewards <- traverse (ltraverse $ lift <<< addressFromBech32)
    $ rmap fromJsBigInt
    <$> (Object.toUnfoldable rewardsFFI :: Array _)

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = foldMap
      (\(addr /\ amount) -> paysToAddrConstraint addr $ lovelaceValueOf amount)
      rewards

  if null rewards
    then pure collectTxIds
    else do
      txId <- lift $ submitTxFromConstraints (mempty :: Lookups.ScriptLookups Void)
        constraints
      lift $ awaitTxConfirmed txId
      pure $ Array.cons (byteArrayToHex (unwrap txId)) collectTxIds


-- TODO: Collecting is very inefficient, at time of writing, testing on preview
-- scripts can only handle 2 registry utxos in one tx.
collectRegistryRetryOnFailure :: RaceHash -> RegistryParams -> Racers (Array TransactionHashFFI)
collectRegistryRetryOnFailure raceHash rgp = do
  let go 1 txhs = collectRegistryScriptLeftovers raceHash rgp 1 <#> (flip Array.cons txhs)
      go n txhs = do
        registryScript <- mkRaceRegistryScript rgp
        utxosAtRegistry <- lift $ utxosAt
          (scriptHashAddress (validatorHash registryScript) Nothing)
        if null utxosAtRegistry
          then pure txhs
          else try (collectRegistryScriptLeftovers raceHash rgp n)
               >>= either 
                (const $ go (n - 1) txhs) 
                (\txId -> lift (awaitTxConfirmed txId) *> go (n + 1) (Array.cons txId txhs))
  go 5 [] <#> map (unwrap >>> byteArrayToHex)


collectDust :: Lovelace -> Racers TransactionHashFFI
collectDust = lift <<< map (byteArrayToHex <<< unwrap)
  <<< collectDustByThreshold
  <<< fromJsBigInt
