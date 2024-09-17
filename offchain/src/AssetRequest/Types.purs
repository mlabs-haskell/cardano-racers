module CardanoRacers.AssetRequest.Types
  ( AirdropAddressDatum(AirdropAddressDatum)
  , AssetRequestRedeemer(MintRequestToken, BurnRequestToken)
  ) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, encodeAeson, (.:))
import Cardano.Plutus.DataSchema (S, Z)
import Cardano.Plutus.Types.Address (Address)
import CardanoRacers.Helpers
  ( decodeAesonString
  , decodeWrappedAeson
  , wrapEncodeAeson
  )
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
import Control.Alt ((<|>))

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
  encodeAeson MintRequestToken = encodeAeson "MintRequestToken"
  encodeAeson BurnRequestToken = encodeAeson "BurnRequestToken"

instance DecodeAeson AssetRequestRedeemer where
  decodeAeson aes =
    decodeAesonString "MintRequestToken" (const MintRequestToken) aes
      <|> decodeAesonString "BurnRequestToken" (const BurnRequestToken) aes
