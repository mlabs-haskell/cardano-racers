module CardanoRacers.Deposit.Types
  ( DepositValidatorParams(DepositValidatorParams)
  ) where

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
import Contract.Value (CurrencySymbol)

newtype DepositValidatorParams = DepositValidatorParams
  { assetPolicySymbol :: CurrencySymbol
  , assetRequestPolicySymbol :: CurrencySymbol
  }

derive instance Generic DepositValidatorParams _
derive instance Newtype DepositValidatorParams _
derive instance Eq DepositValidatorParams

instance
  HasPlutusSchema DepositValidatorParams
    ( "DepositValidatorParams"
        :=
          ( "assetPolicySymbol"
              := I CurrencySymbol
              :+ "assetRequestPolicySymbol"
              := I CurrencySymbol
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData DepositValidatorParams where
  toData = genericToData

instance FromData DepositValidatorParams where
  fromData = genericFromData

instance EncodeAeson DepositValidatorParams where
  encodeAeson = wrapEncodeAeson "DepositValidatorParams" <<< unwrap

instance DecodeAeson DepositValidatorParams where
  decodeAeson = decodeWrappedAeson "DepositValidatorParams" \obj -> do
    assetPolicySymbol <- obj .: "assetPolicySymbol"
    assetRequestPolicySymbol <- obj .: "assetRequestPolicySymbol"
    pure $ DepositValidatorParams
      { assetPolicySymbol, assetRequestPolicySymbol }
