module CardanoRacers.Race.Types
  ( RaceDatum(ValueEscrow, RaceState)
  , RaceParams(RaceParams)
  , RaceRedeemer(MoveL2, AnnounceDistribution, Distribute, ClaimTTL)
  ) where

import Prelude

import Cardano.Plutus.DataSchema (S, Z)
import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Map (Map) as Plutus
import Cardano.Plutus.Types.Value (Value) as Plutus
import Cardano.Types (AssetName, Ed25519KeyHash, ScriptHash)
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
import Data.Tuple.Nested (type (/\))

-- RaceParams

newtype RaceParams = RaceParams
  { stateAssetClass :: ScriptHash /\ AssetName
  , totalRewardValue :: Plutus.Value
  , delegates :: Array Ed25519KeyHash
  , escrowTtl :: POSIXTime
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
          ( "stateAssetClass"
              := I (ScriptHash /\ AssetName)
              :+ "totalRewardValue"
              := I Plutus.Value
              :+ "delegates"
              := I (Array Ed25519KeyHash)
              :+ "escrowTtl"
              := I POSIXTime
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

data RaceDatum
  = ValueEscrow
  | RaceState
      { distribution :: Maybe (Plutus.Map Plutus.Address Plutus.Value)
      }

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
        :+ PNil
    )

instance ToData RaceDatum where
  toData = genericToData

instance FromData RaceDatum where
  fromData = genericFromData

-- RaceRedeemer

data RaceRedeemer = MoveL2 | AnnounceDistribution | Distribute | ClaimTTL

derive instance Generic RaceRedeemer _
derive instance Eq RaceRedeemer

instance Show RaceRedeemer where
  show = genericShow

instance
  HasPlutusSchema RaceRedeemer
    ( "MoveL2"
        := PNil
        @@ Z
        :+ "AnnounceDistribution"
        := PNil
        @@ (S Z)
        :+ "Distribute"
        := PNil
        @@ (S (S Z))
        :+ "ClaimTTL"
        := PNil
        @@ (S (S (S Z)))
        :+ PNil
    )

instance ToData RaceRedeemer where
  toData = genericToData

instance FromData RaceRedeemer where
  fromData = genericFromData
