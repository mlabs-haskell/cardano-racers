module CardanoRacers.Common.Types where

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
import Contract.Value (CurrencySymbol, TokenName)
import Data.BigInt (BigInt)

newtype RacersParams = RacersParams
  { adminToken :: (CurrencySymbol /\ TokenName)
  , botToken :: (CurrencySymbol /\ TokenName)
  , stateToken :: (CurrencySymbol /\ TokenName)
  , nitroToken :: TokenName
  }

derive instance Generic RacersParams _
derive instance Newtype RacersParams _
derive instance Eq RacersParams

instance
  HasPlutusSchema RacersParams
    ( "RacersParams"
        :=
          ( "adminToken" := I (CurrencySymbol /\ TokenName)
              :+ "botToken"
              := I (CurrencySymbol /\ TokenName)
              :+ "stateToken"
              := I (CurrencySymbol /\ TokenName)
              :+ "nitroToken"
              := I TokenName
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData RacersParams where
  toData = genericToData

instance FromData RacersParams where
  fromData = genericFromData

instance Show RacersParams where
  show = genericShow

instance EncodeAeson RacersParams where
  encodeAeson = wrapEncodeAeson "RacersParams" <<< unwrap

instance DecodeAeson RacersParams where
  decodeAeson = decodeWrappedAeson "RacersParams" \obj -> do
    adminToken <- obj .: "adminToken"
    botToken <- obj .: "botToken"
    stateToken <- obj .: "stateToken"
    nitroToken <- obj .: "nitroToken"
    pure $ RacersParams { adminToken, botToken, stateToken, nitroToken }

newtype RacersState = RacersState
  { nitroPrice :: BigInt -- Nitro price in Lovelace
  , treasuryAddress :: Address
  , operatingAddress :: Address
  , driverPrices :: Map Rarity BigInt
  , carPrices :: Map Rarity BigInt
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
              :+ "driverPrices"
              := I (Map Rarity BigInt)
              :+ "carPrices"
              := I (Map Rarity BigInt)
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
    driverPrices <- obj .: "driverPrices"
    carPrices <- obj .: "carPrices"
    pure $ RacersState
      { nitroPrice, treasuryAddress, operatingAddress, driverPrices, carPrices }

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
