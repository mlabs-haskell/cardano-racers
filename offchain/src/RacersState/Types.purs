module CardanoRacers.RacersState.Types where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, (.:))
import CardanoRacers.GameAsset.Types (Rarity)
import CardanoRacers.Helpers (decodeWrappedAeson, wrapEncodeAeson)
import Contract.Address (Address)
import Contract.AssocMap (Map)
import Contract.PlutusData
  ( class FromData
  , class HasPlutusSchema
  , class ToData
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
  , Z
  , genericFromData
  , genericToData
  )
import Contract.Scripts (ValidatorHash)
import Data.BigInt (BigInt)

newtype RacersState = RacersState
  { nitroPrice :: BigInt -- Nitro price in Lovelace
  , treasuryAddress :: Address
  , operatingAddress :: Address
  , assetPrices :: Map Rarity BigInt
  , depositScript :: ValidatorHash
  }

derive instance Generic RacersState _
derive instance Newtype RacersState _
derive instance Eq RacersState

instance
  HasPlutusSchema RacersState
    ( "RacersState"
        :=
          ( "nitroPrice" := I BigInt
              :+ "treasuryAddress"
              := I Address
              :+ "operatingAddress"
              := I Address
              :+ "assetPrices"
              := I (Map Rarity BigInt)
              :+ "depositScript"
              := I ValidatorHash
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData RacersState where
  toData = genericToData

instance FromData RacersState where
  fromData = genericFromData

instance Show RacersState where
  show = genericShow

instance EncodeAeson RacersState where
  encodeAeson = wrapEncodeAeson "RacersState" <<< unwrap

instance DecodeAeson RacersState where
  decodeAeson = decodeWrappedAeson "RacersState" \obj -> do
    nitroPrice <- obj .: "nitroPrice"
    treasuryAddress <- obj .: "treasuryAddress"
    operatingAddress <- obj .: "operatingAddress"
    assetPrices <- obj .: "assetPrices"
    depositScript <- obj .: "depositScript"
    pure $ RacersState
      { nitroPrice
      , treasuryAddress
      , operatingAddress
      , assetPrices
      , depositScript
      }

newtype RacersStateRedeemer = SetRacersState RacersState

derive instance Generic RacersStateRedeemer _
derive instance Newtype RacersStateRedeemer _
derive instance Eq RacersStateRedeemer
instance
  HasPlutusSchema RacersStateRedeemer
    ("SetRacersState" := PNil @@ Z :+ PNil)

instance ToData RacersStateRedeemer where
  toData = genericToData

instance FromData RacersStateRedeemer where
  fromData = genericFromData

instance Show RacersStateRedeemer where
  show = genericShow

instance EncodeAeson RacersStateRedeemer where
  encodeAeson = wrapEncodeAeson "SetRacersState" <<< unwrap

instance DecodeAeson RacersStateRedeemer where
  decodeAeson = decodeWrappedAeson "SetRacersState" (pure <<< SetRacersState)
