module CardanoRacers.Common.Types (RacersParams(RacersParams), nitroToken) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, (.:))
import Cardano.Plutus.DataSchema (Z)
import Cardano.Plutus.Types.CurrencySymbol (CurrencySymbol)
import Cardano.Plutus.Types.TokenName (TokenName, mkTokenName)
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
  , genericFromData
  , genericToData
  )
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Partial.Unsafe (unsafePartial)

-- | Game parameters that uniquely identify an instance of the game.
newtype RacersParams = RacersParams
  { adminToken :: (CurrencySymbol /\ TokenName)
  , botToken :: (CurrencySymbol /\ TokenName)
  , stateToken :: (CurrencySymbol /\ TokenName)
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
    pure $ RacersParams { adminToken, botToken, stateToken }

nitroToken :: TokenName
nitroToken = unsafePartial $ fromJust $ mkTokenName <=< byteArrayFromAscii $
  "NITRO"
