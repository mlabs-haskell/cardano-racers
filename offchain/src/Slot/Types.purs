module CardanoRacers.Slot.Types where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import CardanoRacers.Helpers (decodeWrappedAeson, wrapEncodeAeson)
import Contract.PlutusData
  ( class FromData
  , class HasPlutusSchema
  , class ToData
  , type (:+)
  , type (:=)
  , type (@@)
  , PNil
  , S
  , Z
  , genericFromData
  , genericToData
  )
import Control.Alt ((<|>))
import Data.BigInt (BigInt)
import Foreign.Object (Object)

type RaceHash = String -- in hex

data SlotTokenPolicyRedeemer
  = MintSlotToken BigInt
  | BurnSlotToken

derive instance Generic SlotTokenPolicyRedeemer _
derive instance Eq SlotTokenPolicyRedeemer
instance
  HasPlutusSchema SlotTokenPolicyRedeemer
    ( "MintSlotToken"
        := PNil
        @@ Z
        :+ "BurnSlotToken"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance ToData SlotTokenPolicyRedeemer where
  toData = genericToData

instance FromData SlotTokenPolicyRedeemer where
  fromData = genericFromData

instance Show SlotTokenPolicyRedeemer where
  show = genericShow

instance EncodeAeson SlotTokenPolicyRedeemer where
  encodeAeson (MintSlotToken amt) = wrapEncodeAeson "MintSlotToken" amt
  encodeAeson BurnSlotToken = wrapEncodeAeson "BurnSlotToken" {}

instance DecodeAeson SlotTokenPolicyRedeemer where
  decodeAeson aes =
    decodeWrappedAeson "MintSlotToken" (pure <<< MintSlotToken) aes
      <|> decodeWrappedAeson "BurnSlotToken" (constMono $ pure BurnSlotToken)
        aes
    where
    constMono :: forall a. a -> Object {} -> a
    constMono a _ = a
