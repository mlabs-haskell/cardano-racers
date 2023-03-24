module CardanoRacers.AssetRequest.Types
  ( AirdropAddressDatum(AirdropAddressDatum)
  , AssetRequestRedeemer(MintRequestToken, BurnRequestToken)
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
  , S
  , Z
  , genericFromData
  , genericToData
  )
import Control.Alt ((<|>))
import Foreign.Object (Object)

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

data AssetRequestRedeemer = MintRequestToken | BurnRequestToken

derive instance Generic AssetRequestRedeemer _
derive instance Eq AssetRequestRedeemer

instance Show AssetRequestRedeemer where
  show = genericShow

instance
  HasPlutusSchema AssetRequestRedeemer
    ( "MintRequestToken"
        := PNil
        @@ Z
        :+ "BurnRequestToken"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance ToData AssetRequestRedeemer where
  toData = genericToData

instance FromData AssetRequestRedeemer where
  fromData = genericFromData

instance EncodeAeson AssetRequestRedeemer where
  encodeAeson MintRequestToken = wrapEncodeAeson "MintRequestToken" {}
  encodeAeson BurnRequestToken = wrapEncodeAeson "BurnRequestToken" {}

instance DecodeAeson AssetRequestRedeemer where
  decodeAeson aes =
    decodeWrappedAeson "MintRequestToken" (constMono $ pure MintRequestToken)
      aes
      <|> decodeWrappedAeson "BurnRequestToken"
        (constMono $ pure BurnRequestToken)
        aes
    where
    constMono :: forall a. a -> Object {} -> a
    constMono a _ = a
