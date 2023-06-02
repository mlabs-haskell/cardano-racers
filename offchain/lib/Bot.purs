module Lib.CardanoRacers.Bot where

import Contract.Prelude

import CardanoRacers.Common.Types (RacersParams, nitroToken)
import CardanoRacers.Deposit.Contract (PendingAssetRequest, consumeAndRedeemRequests, queryRequestsWithAirdropAddress)
import CardanoRacers.GameAsset.Types (AssetOption, CarAttributes(..), DriverAttributes(..), GameAssetAttributes(..), GameAssetObject, Rarity(..))
import CardanoRacers.Helpers (paysToAddrConstraint)
import CardanoRacers.Nitro.Contract (mintNitroContract, mkNitroPolicy)
import CardanoRacers.RaceRegistry.Contract (collectRegistryScriptLeftovers, initRace, supplyRegistrySlots)
import Contract.Address (addressFromBech32, addressToBech32)
import Contract.Config (testnetConfig)
import Contract.Metadata (mkCip25String, unCip25String)
import Contract.Monad (liftContractM, liftedM, runContract)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (mintingPolicyHash)
import Contract.Transaction (TransactionHash, TransactionInput, awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (adaSymbol, adaToken, getLovelace, lovelaceValueOf, mpsSymbol, valueOf, valueToCoin)
import Contract.Wallet (getWalletBalance)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, fromAff, toAffE)
import Data.Array (concat, replicate) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, toInt, toString) as BigInt
import Data.Bitraversable (ltraverse, rtraverse)
import Data.Map (Map)
import Data.Map (fromFoldable, toUnfoldable) as Map
import Effect.Aff.Compat (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2, runEffectFn1)
import Effect.Uncurried (EffectFn4, mkEffectFn4)
import Foreign.Object (Object)
import Foreign.Object (fromFoldable, toUnfoldable) as Object
import Lib.CardanoRacers.Common (CredentialProvider, Lovelace, Nitro, Race, assetTypeFromString, assetTypeToString, createRegistryParams, toWalletSpec, tokenNameToString)
import Lib.CardanoRacers.Queries (Queries, mkQueries)
import Partial.Unsafe (unsafePartial)
import Racers (Racers, runRacers, withContract)
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
  , name :: String
  , tokenName :: String
  , description :: String
  }

type RewardDistributionFFI = Object Lovelace

type Bot r =
  ( queryAssetRequests ::
      EffectFn1 Unit (Promise (Object (Array AssetRequestFFI)))
  , getWalletLovelaceBalance :: EffectFn1 Unit (Promise Lovelace)
  , getWalletNitroBalance :: EffectFn1 Unit (Promise Nitro)
  , mintNitro :: EffectFn1 Nitro (Promise TransactionHash)
  , tryRedeemingPendingRequests ::
      EffectFn4 AvailableAssetsFFI Int Int
        (EffectFn1 AssetOptionFFI (Promise BigInt))
        (Promise (Array GameAssetFFI)) -- TODO: This should also return the transaction input to match the api
  , resupplySlots :: EffectFn2 Race Int (Promise Unit)
  , closeRace ::
      EffectFn2 Race RewardDistributionFFI (Promise (Array TransactionHash))
  , createRace :: EffectFn2 Race Int (Promise Unit)
  | r
  )

mkBot
  :: CredentialProvider -> RacersParams -> Record (Bot + Queries + ())
mkBot cp rp =
  let
    queries = mkQueries cp rp
    walletSpec = toWalletSpec cp
    cfg = testnetConfig { walletSpec = Just walletSpec }

    runC :: Racers ~> Aff
    runC = runContract cfg <<< runRacers rp
  in
    { queryAssetRequests: mkEffectFn1 $ const $ fromAff $ runC $
        queryAssetRequests
    , getWalletLovelaceBalance: mkEffectFn1 $ const $ fromAff $ runC $
        getWalletLovelaceBalance
    , getWalletNitroBalance: mkEffectFn1 $ const $ fromAff $ runC $
        getWalletNitroBalance
    , mintNitro: mkEffectFn1 $ fromAff <<< runC <<< mintNitro
    , tryRedeemingPendingRequests: mkEffectFn4 $
        \assets maxRequests chunkBy generateUniquenessNonce ->
          fromAff $ runC $ tryRedeemingPendingRequests assets maxRequests
            chunkBy
            generateUniquenessNonce
    , resupplySlots: mkEffectFn2 $ \race slots -> fromAff $ runC $ resupplySlots
        race
        slots
    , closeRace: mkEffectFn2 $ \race rewardDistribution -> fromAff $ runC $
        closeRace race rewardDistribution
    , createRace: mkEffectFn2 $ \race slots -> fromAff $ runC $ createRace race
        slots
    } `merge` queries

mintNitro :: Nitro -> Racers TransactionHash
mintNitro nitroAmount = mintNitroContract nitroAmount

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
          pure $ Array.replicate i { address: addrStr, rarity: show r }
      )
      requestedAssets
    pure $ show txi /\ as

getWalletLovelaceBalance :: Racers Lovelace
getWalletLovelaceBalance = lift do
  bal <- liftedM "Could not get wallet balance" getWalletBalance
  pure $ valueOf bal adaSymbol adaToken

getWalletNitroBalance :: Racers Nitro
getWalletNitroBalance = do
  bal <- lift $ liftedM "Could not get wallet balance" getWalletBalance
  nitroSymbol <- withContract (liftedM "Could not get Nitro symbol") $ mpsSymbol
    <<< mintingPolicyHash
    <$> mkNitroPolicy
  pure $ valueOf bal nitroSymbol nitroToken

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
      , nitroAmount: affi.nitroAmount
      }

  toAssetOptionFFI :: AssetOption -> AssetOptionFFI
  toAssetOptionFFI ao =
    { name: unCip25String ao.name
    , assetType: assetTypeToString ao.assetType
    , imageUrl: ao.imageUrl
    , description: ao.description
    , nitroAmount: ao.nitroAmount
    }

  toGameAssetFFI :: GameAssetObject -> GameAssetFFI
  toGameAssetFFI gao =
    { name: unCip25String gao.name
    , assetType: assetTypeToString gao.assetType
    , imageUrl: gao.imageUrl
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

createRace :: Race -> Int -> Racers Unit
createRace race slotCount = void $ initRace (wrap race.raceId) race.nitroFee
  $ BigInt.fromInt slotCount

resupplySlots :: Race -> Int -> Racers Unit
resupplySlots race slotCount = do
  rgp <- createRegistryParams race
  void $ supplyRegistrySlots (wrap race.raceId) rgp $ BigInt.fromInt slotCount

closeRace :: Race -> RewardDistributionFFI -> Racers (Array TransactionHash)
closeRace race rewardsFFI = do
  rgp <- createRegistryParams race
  collectTxId <- collectRegistryScriptLeftovers (wrap race.raceId) rgp
  rewards <- traverse (ltraverse $ lift <<< addressFromBech32) $
    (Object.toUnfoldable :: _ -> Array _) rewardsFFI

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = foldMap
      (\(addr /\ amount) -> paysToAddrConstraint addr $ lovelaceValueOf amount)
      rewards

  txId <- lift $ submitTxFromConstraints (mempty :: Lookups.ScriptLookups Void)
    constraints
  lift $ awaitTxConfirmed txId
  pure [ collectTxId, txId ]
