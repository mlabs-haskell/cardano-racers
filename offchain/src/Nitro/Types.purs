module CardanoRacers.Nitro.Types where

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
import Contract.Value (CurrencySymbol, TokenName)
import Control.Alt ((<|>))
import Data.BigInt (BigInt)

newtype NitroScriptParams = NitroScriptParams
  { adminToken :: (CurrencySymbol /\ TokenName)
  , stateToken :: (CurrencySymbol /\ TokenName)
  , nitroToken :: TokenName
  }

derive instance Generic NitroScriptParams _
derive instance Newtype NitroScriptParams _
derive instance Eq NitroScriptParams

instance
  HasPlutusSchema NitroScriptParams
    ( "NitroScriptParams"
        :=
          ( "adminToken" := I (CurrencySymbol /\ TokenName)
              :+ "stateToken"
              := I (CurrencySymbol /\ TokenName)
              :+ "nitroToken"
              := I TokenName
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData NitroScriptParams where
  toData = genericToData

instance FromData NitroScriptParams where
  fromData = genericFromData

instance Show NitroScriptParams where
  show = genericShow

instance EncodeAeson NitroScriptParams where
  encodeAeson = wrapEncodeAeson "NitroScriptParams" <<< unwrap

instance DecodeAeson NitroScriptParams where
  decodeAeson = decodeWrappedAeson "NitroScriptParams" \obj -> do
    adminToken <- obj .: "adminToken"
    stateToken <- obj .: "stateToken"
    nitroToken <- obj .: "nitroToken"
    pure $ NitroScriptParams { adminToken, stateToken, nitroToken }

newtype NitroState = NitroState
  { nitroPrice :: BigInt -- Nitro price in Lovelace
  , treasuryAddress :: Address
  , operatingAddress :: Address
  }

derive instance Generic NitroState _
derive instance Newtype NitroState _
derive instance Eq NitroState

instance
  HasPlutusSchema NitroState
    ( "NitroState"
        :=
          ( "nitroPrice" := I BigInt
              :+ "treasuryAddress"
              := I Address
              :+ "operatingAddress"
              := I Address
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData NitroState where
  toData = genericToData

instance FromData NitroState where
  fromData = genericFromData

instance Show NitroState where
  show = genericShow

instance EncodeAeson NitroState where
  encodeAeson = wrapEncodeAeson "NitroState" <<< unwrap

instance DecodeAeson NitroState where
  decodeAeson = decodeWrappedAeson "NitroState" \obj -> do
    nitroPrice <- obj .: "nitroPrice"
    treasuryAddress <- obj .: "treasuryAddress"
    operatingAddress <- obj .: "operatingAddress"
    pure $ NitroState { nitroPrice, treasuryAddress, operatingAddress }

data NitroPolicyRedeemer
  = MintNitroToken BigInt
  | BuyNitroToken BigInt

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

instance DecodeAeson NitroPolicyRedeemer where
  decodeAeson aes =
    decodeWrappedAeson "MintNitroToken" (pure <<< MintNitroToken) aes <|>
      decodeWrappedAeson "BuyNitroToken" (pure <<< BuyNitroToken) aes

newtype NitroStateRedeemer = SetNitroState NitroState

derive instance Generic NitroStateRedeemer _
derive instance Newtype NitroStateRedeemer _
derive instance Eq NitroStateRedeemer
instance
  HasPlutusSchema NitroStateRedeemer
    ("SetNitroState" := PNil @@ Z :+ PNil)

instance ToData NitroStateRedeemer where
  toData = genericToData

instance FromData NitroStateRedeemer where
  fromData = genericFromData

instance Show NitroStateRedeemer where
  show = genericShow

instance EncodeAeson NitroStateRedeemer where
  encodeAeson = wrapEncodeAeson "SetNitroState" <<< unwrap

instance DecodeAeson NitroStateRedeemer where
  decodeAeson = decodeWrappedAeson "SetNitroState" (pure <<< SetNitroState)
