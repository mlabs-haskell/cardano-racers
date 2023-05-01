module CardanoRacers.Common.Types (RacersParams(RacersParams)) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, (.:))
import CardanoRacers.Helpers (decodeWrappedAeson, wrapEncodeAeson)
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

-- | Game parameters that uniquely identify an instance of the game.
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
