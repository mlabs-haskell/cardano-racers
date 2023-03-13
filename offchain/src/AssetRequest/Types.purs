module CardanoRacers.AssetRequest.Types where

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

data AssetRequestRedeemer = UserMintRequestToken | AdminMintRequestTokens

derive instance Generic AssetRequestRedeemer _
derive instance Eq AssetRequestRedeemer

instance
  HasPlutusSchema AssetRequestRedeemer
    ( "UserMintRequestToken" := PNil @@ Z
        :+ "AdminMintRequestTokens"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance ToData AssetRequestRedeemer where
  toData = genericToData

instance FromData AssetRequestRedeemer where
  fromData = genericFromData

instance Show AssetRequestRedeemer where
  show = genericShow

instance EncodeAeson AssetRequestRedeemer where
  encodeAeson UserMintRequestToken = wrapEncodeAeson "UserMintRequestToken" {}
  encodeAeson AdminMintRequestTokens = wrapEncodeAeson "AdminMintRequestTokens"
    {}

instance DecodeAeson AssetRequestRedeemer where
  decodeAeson aes =
    decodeWrappedAeson "UserMintRequestToken"
      (constMono $ pure UserMintRequestToken)
      aes
      <|> decodeWrappedAeson "AdminMintRequestTokens"
        (constMono $ pure AdminMintRequestTokens)
        aes
    where
    constMono :: forall a. a -> Object {} -> a
    constMono a _ = a

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
