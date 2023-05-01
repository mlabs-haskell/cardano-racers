module CardanoRacers.RaceRegistry.Types
  ( RegistryParams(RegistryParams)
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
import Contract.Scripts (MintingPolicyHash)
import Data.BigInt (BigInt)

newtype RegistryParams = RegistryParams
  { raceHash :: String
  , nitroFee :: BigInt
  , nitroPolicyHash :: MintingPolicyHash
  }

derive instance Generic RegistryParams _
derive instance Newtype RegistryParams _
derive instance Eq RegistryParams

instance
  HasPlutusSchema RegistryParams
    ( "RegistryParams"
        :=
          ( "raceHash"
              := I String
              :+ "nitroFee"
              := I BigInt
              :+ "nitroPolicyHash"
              := I MintingPolicyHash
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData RegistryParams where
  toData = genericToData

instance FromData RegistryParams where
  fromData = genericFromData

instance EncodeAeson RegistryParams where
  encodeAeson = wrapEncodeAeson "RegistryParams" <<< unwrap

instance DecodeAeson RegistryParams where
  decodeAeson = decodeWrappedAeson "RegistryParams" \obj -> do
    raceHash <- obj .: "raceHash"
    nitroFee <- obj .: "nitroFee"
    nitroPolicyHash <- obj .: "nitroPolicyHash"
    pure $ RegistryParams
      { raceHash, nitroFee, nitroPolicyHash }
