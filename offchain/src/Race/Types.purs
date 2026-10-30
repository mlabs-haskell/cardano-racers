module CardanoRacers.Race.Types
  ( RaceDatum(ValueEscrow, RaceState, TokenBin)
  , RaceParams(RaceParams)
  , RaceRedeemer(MoveL2, DistributeRewards, ClaimTTL, CleanupUsedTokens)
  , RewardDistribution
  , raceStateTokenName
  , valueEscrowTokenName
  ) where

import Prelude

import Cardano.Plutus.DataSchema (S, Z)
import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Map (Map) as Plutus
import Cardano.Plutus.Types.Value (Value) as Plutus
import Cardano.Types (AssetName, Ed25519KeyHash, ScriptHash)
import CardanoRacers.Helpers (assetNameFromAsciiUnsafe)
import CardanoRacers.Types.FixedDecimal (FixedDecimal, N5)
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
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

-- RaceParams

newtype RaceParams = RaceParams
  { stateCurrencySymbol :: ScriptHash
  , totalRewardValue :: Plutus.Value
  , participants :: Array Plutus.Address
  , delegates :: Array Ed25519KeyHash
  , escrowTtl :: POSIXTime
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
