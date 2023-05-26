module CardanoRacers.Deposit.Types
  ( DepositScriptParams(DepositScriptParams)
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

newtype DepositScriptParams = DepositScriptParams
  { driverPolicySymbol :: CurrencySymbol
  , carPolicySymbol :: CurrencySymbol
  , assetRequestPolicySymbol :: CurrencySymbol
  }

derive instance Generic DepositScriptParams _
derive instance Newtype DepositScriptParams _
derive instance Eq DepositScriptParams

instance
  HasPlutusSchema DepositScriptParams
    ( "DepositScriptParams"
        :=
          ( "driverPolicySymbol"
              := I CurrencySymbol
              :+ "carPolicySymbol"
              := I CurrencySymbol
              :+ "assetRequestPolicySymbol"
              := I CurrencySymbol
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData DepositScriptParams where
  toData = genericToData

instance FromData DepositScriptParams where
  fromData = genericFromData

instance EncodeAeson DepositScriptParams where
  encodeAeson = wrapEncodeAeson "DepositScriptParams" <<< unwrap

instance DecodeAeson DepositScriptParams where
  decodeAeson = decodeWrappedAeson "DepositScriptParams" \obj -> do
    driverPolicySymbol <- obj .: "driverPolicySymbol"
    carPolicySymbol <- obj .: "carPolicySymbol"
    assetRequestPolicySymbol <- obj .: "assetRequestPolicySymbol"
    pure $ DepositScriptParams
      { driverPolicySymbol, carPolicySymbol, assetRequestPolicySymbol }
