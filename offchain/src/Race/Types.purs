module CardanoRacers.Race.Types
  ( DistributeRewardsContractError(..)
  , RaceDatum(ValueEscrow, RaceState, TokenBin)
  , RaceParams(RaceParams)
  , RaceRedeemer(MoveL2, DistributeRewards, ClaimTTL, CleanupUsedTokens)
  , RewardDistribution
  , StartRaceParams(StartRaceParams)
  , StartRaceResult(StartRaceResult)
  , raceParamsCodec
  , raceStateTokenName
  , startRaceParamsCodec
  , startRaceResultCodec
  , valueEscrowTokenName
  ) where

import Prelude

import Cardano.Plutus.DataSchema (S, Z)
import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Map (Map) as Plutus
import Cardano.Plutus.Types.Value (Value) as Plutus
import Cardano.Types
  ( AssetName
  , Ed25519KeyHash
  , NetworkId
  , ScriptHash
  , TransactionHash
  , Value
  )
import CardanoRacers.Helpers (assetNameFromAsciiUnsafe)
import CardanoRacers.RaceRegistry.Types (RaceParticipant, raceParticipantCodec)
import CardanoRacers.RaceSlot.Types (RaceHash)
import CardanoRacers.Types.FixedDecimal (FixedDecimal, N5, fixedDecimalCodec)
import CardanoRacers.Utils.Codec
  ( plutusAddressBech32Codec
  , plutusValueCodec
  , posixTimeCodec
  , valueCodec
  )
import CardanoRacers.Utils.HasJson (class HasJson)
import Contract.PlutusData
  ( class FromData
  , class HasPlutusSchema
  , class ToData
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
  , genericFromData
  , genericToData
  )
import Contract.Time (POSIXTime)
import Data.Codec.Argonaut (JsonCodec, array, object) as CA
import Data.Codec.Argonaut.Compat (maybe) as CACompat
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import HydraSdk.Lib
  ( byteArrayCodec
  , ed25519KeyHashCodec
  , scriptHashCodec
  , txHashCodec
  )

newtype StartRaceParams = StartRaceParams
  { raceId :: RaceHash
  , totalRewardValue :: Value
  , rewardWeights :: Array (FixedDecimal N5)
  , participants :: Array RaceParticipant
  , delegates :: Array Ed25519KeyHash
  , feePerDelegate :: Maybe Value
  }

derive instance Generic StartRaceParams _
derive instance Newtype StartRaceParams _
derive instance Eq StartRaceParams

instance Show StartRaceParams where
  show = genericShow

instance HasJson StartRaceParams NetworkId where
  jsonCodec network = const (startRaceParamsCodec network)

startRaceParamsCodec :: NetworkId -> CA.JsonCodec StartRaceParams
startRaceParamsCodec network =
  wrapIso StartRaceParams $ CA.object "StartRaceParams" $ CAR.record
    { raceId: byteArrayCodec
    , totalRewardValue: valueCodec
    , rewardWeights: CA.array fixedDecimalCodec
    , participants: CA.array $ raceParticipantCodec network
    , delegates: CA.array ed25519KeyHashCodec
    , feePerDelegate: CACompat.maybe valueCodec
    }

newtype StartRaceResult = StartRaceResult
  { txHash :: TransactionHash
  , raceParams :: RaceParams
  }

derive instance Generic StartRaceResult _
derive instance Newtype StartRaceResult _
derive instance Eq StartRaceResult

instance Show StartRaceResult where
  show = genericShow

instance HasJson StartRaceResult NetworkId where
  jsonCodec network = const (startRaceResultCodec network)

startRaceResultCodec :: NetworkId -> CA.JsonCodec StartRaceResult
startRaceResultCodec network =
  wrapIso StartRaceResult $ CA.object "StartRaceResult" $ CAR.record
    { txHash: txHashCodec
    , raceParams: raceParamsCodec network
    }

-- RaceParams

newtype RaceParams = RaceParams
  { stateCurrencySymbol :: ScriptHash
  , totalRewardValue :: Plutus.Value
  , participants :: Array Plutus.Address
  , delegates :: Array Ed25519KeyHash
  , escrowTtl :: POSIXTime
  , feePerDelegate :: Maybe Plutus.Value
  -- TODO: update validator to ensure the final distribution is consistent
  -- with reward weights
  , rewardWeights :: Array (FixedDecimal N5)
  }

derive instance Generic RaceParams _
derive instance Newtype RaceParams _
derive instance Eq RaceParams

instance Show RaceParams where
  show = genericShow

instance
  HasPlutusSchema RaceParams
    ( "RaceParams"
        :=
          ( "stateCurrencySymbol"
              := I ScriptHash
              :+ "totalRewardValue"
              := I Plutus.Value
              :+ "participants"
              := I (Array Plutus.Address)
              :+ "delegates"
              := I (Array Ed25519KeyHash)
              :+ "escrowTtl"
              := I POSIXTime
              :+ "feePerDelegate"
              := I (Maybe Plutus.Value)
              :+ "rewardWeights"
              := I (Array (FixedDecimal N5))
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData RaceParams where
  toData = genericToData

instance FromData RaceParams where
  fromData = genericFromData

instance HasJson RaceParams NetworkId where
  jsonCodec network = const (raceParamsCodec network)

raceParamsCodec :: NetworkId -> CA.JsonCodec RaceParams
raceParamsCodec network =
  wrapIso RaceParams $ CA.object "RaceParams" $ CAR.record
    { stateCurrencySymbol: scriptHashCodec
    , totalRewardValue: plutusValueCodec
    , participants: CA.array $ plutusAddressBech32Codec network
    , delegates: CA.array ed25519KeyHashCodec
    , escrowTtl: posixTimeCodec
    , feePerDelegate: CACompat.maybe plutusValueCodec
    , rewardWeights: CA.array fixedDecimalCodec
    }

-- RaceDatum

type RewardDistribution = Plutus.Map Plutus.Address Plutus.Value

data RaceDatum
  = ValueEscrow
  | RaceState
      { distribution :: Maybe RewardDistribution
      }
  | TokenBin

derive instance Generic RaceDatum _
derive instance Eq RaceDatum

instance Show RaceDatum where
  show = genericShow

instance
  HasPlutusSchema RaceDatum
    ( "ValueEscrow"
        := PNil
        @@ Z
        :+ "RaceState"
        :=
          ( "distribution"
              := I (Maybe (Plutus.Map Plutus.Address Plutus.Value))
              :+ PNil
          )
        @@ (S Z)
        :+ "TokenBin"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

instance ToData RaceDatum where
  toData = genericToData

instance FromData RaceDatum where
  fromData = genericFromData

-- RaceRedeemer

data RaceRedeemer = MoveL2 | DistributeRewards | ClaimTTL | CleanupUsedTokens

derive instance Generic RaceRedeemer _
derive instance Eq RaceRedeemer

instance Show RaceRedeemer where
  show = genericShow

instance
  HasPlutusSchema RaceRedeemer
    ( "MoveL2"
        := PNil
        @@ Z
        :+ "DistributeRewards"
        := PNil
        @@ (S Z)
        :+ "ClaimTTL"
        := PNil
        @@ (S (S Z))
        :+ "CleanupUsedTokens"
        := PNil
        @@ (S (S (S Z)))
        :+ PNil
    )

instance ToData RaceRedeemer where
  toData = genericToData

instance FromData RaceRedeemer where
  fromData = genericFromData

raceStateTokenName :: AssetName
raceStateTokenName = assetNameFromAsciiUnsafe "RACE_STATE"

valueEscrowTokenName :: AssetName
valueEscrowTokenName = assetNameFromAsciiUnsafe "VALUE_ESCROW"

-- DistributeRewardsContractError

data DistributeRewardsContractError
  = CouldNotFindRaceStateUtxo
  | CouldNotFindValueEscrowUtxo
  | CouldNotDecodeRaceDatum
  | CouldNotConvertDistribution
  | RewardDistributionNotAnnounced
  | UnexpectedRaceDatumVariant
  | CouldNotConvertRewardValue
  | CouldNotConvertFeePerDelegateValue

derive instance Generic DistributeRewardsContractError _
derive instance Eq DistributeRewardsContractError

instance Show DistributeRewardsContractError where
  show = genericShow
