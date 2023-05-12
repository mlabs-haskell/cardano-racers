module CardanoRacers.Nitro.Types
  ( NitroPolicyRedeemer(MintNitroToken, BuyNitroToken, BurnNitroToken)
  ) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, encodeAeson)
import CardanoRacers.Helpers (decodeAesonString, decodeWrappedAeson, wrapEncodeAeson)
import Contract.PlutusData (class FromData, class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), PNil, S, Z, genericFromData, genericToData)
import Control.Alt ((<|>))
import Data.BigInt (BigInt)

data NitroPolicyRedeemer
  = MintNitroToken BigInt
  | BuyNitroToken BigInt
  | BurnNitroToken

derive instance Generic NitroPolicyRedeemer _
derive instance Eq NitroPolicyRedeemer
instance
  HasPlutusSchema NitroPolicyRedeemer
    ( "MintNitroToken"
        := PNil
        @@ Z
        :+ "BuyNitroToken"
        := PNil
        @@ (S Z)
        :+ "BurnNitroToken"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

instance ToData NitroPolicyRedeemer where
  toData = genericToData

instance FromData NitroPolicyRedeemer where
  fromData = genericFromData

instance Show NitroPolicyRedeemer where
  show = genericShow

instance EncodeAeson NitroPolicyRedeemer where
  encodeAeson (MintNitroToken amt) = wrapEncodeAeson "MintNitroToken" amt
  encodeAeson (BuyNitroToken amt) = wrapEncodeAeson "BuyNitroToken" amt
  encodeAeson BurnNitroToken = encodeAeson "BurnNitroToken"

instance DecodeAeson NitroPolicyRedeemer where
  decodeAeson aes =
    decodeWrappedAeson "MintNitroToken" (pure <<< MintNitroToken) aes
      <|> decodeWrappedAeson "BuyNitroToken" (pure <<< BuyNitroToken) aes
      <|> decodeAesonString "BurnRequestToken" (const BurnNitroToken) aes
