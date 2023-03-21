module CardanoRacers.AssetRequest.Types
  ( AirdropAddressDatum(AirdropAddressDatum)
  ) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, (.:))
import CardanoRacers.Helpers (decodeWrappedAeson, wrapEncodeAeson)
import Contract.Address (Address)
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

newtype AirdropAddressDatum = AirdropAddressDatum
  { airdropAddress :: Address }

derive instance Generic AirdropAddressDatum _
derive instance Newtype AirdropAddressDatum _
derive instance Eq AirdropAddressDatum

instance Show AirdropAddressDatum where
  show = genericShow

instance
  HasPlutusSchema AirdropAddressDatum
    ( "AirdropAddressDatum"
        :=
          ( "airdropAddress"
              := I Address
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData AirdropAddressDatum where
  toData = genericToData

instance FromData AirdropAddressDatum where
  fromData = genericFromData

instance EncodeAeson AirdropAddressDatum where
  encodeAeson = wrapEncodeAeson "AirdropAddressDatum" <<< unwrap

instance DecodeAeson AirdropAddressDatum where
  decodeAeson = decodeWrappedAeson "AirdropAddressDatum" \obj -> do
    airdropAddress <- obj .: "airdropAddress"
    pure $ AirdropAddressDatum { airdropAddress }
