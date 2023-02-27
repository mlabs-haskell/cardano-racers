module CardanoRacers.GameAsset.Types where

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

newtype Driver = Driver
  { driverId :: String
  , aggression :: BigInt
  , experience :: BigInt
  , reflexes :: BigInt
  , luck :: BigInt
  }

derive instance Generic Driver _
derive instance Newtype Driver _
derive instance Eq Driver

instance
  HasPlutusSchema Driver
    ( "Driver"
        :=
          ( "driverId" := I String
              :+ "aggression"
              := I BigInt
              :+ "experience"
              := I BigInt
              :+ "reflexes"
              := I BigInt
              :+ "luck"
              := I BigInt
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData Driver where
  toData = genericToData

instance FromData Driver where
  fromData = genericFromData

instance Show Driver where
  show = genericShow

instance EncodeAeson Driver where
  encodeAeson = wrapEncodeAeson "Driver" <<< unwrap

instance DecodeAeson Driver where
  decodeAeson = decodeWrappedAeson "Driver" \obj -> do
    driverId <- obj .: "driverId"
    aggression <- obj .: "aggression"
    experience <- obj .: "experience"
    reflexes <- obj .: "reflexes"
    luck <- obj .: "luck"
    pure $ Driver { driverId, aggression, experience, reflexes, luck }

newtype Car = Car
  { carId :: String
  , topSpeed :: BigInt
  , acceleration :: BigInt
  , cornering :: BigInt
  , aerodynamics :: BigInt
  }

derive instance Generic Car _
derive instance Newtype Car _
derive instance Eq Car

instance
  HasPlutusSchema Car
    ( "Car"
        :=
          ( "carId" := I String
              :+ "topSpeed"
              := I BigInt
              :+ "acceleration"
              := I BigInt
              :+ "cornering"
              := I BigInt
              :+ "aerodynamics"
              := I BigInt
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData Car where
  toData = genericToData

instance FromData Car where
  fromData = genericFromData

instance Show Car where
  show = genericShow

instance EncodeAeson Car where
  encodeAeson = wrapEncodeAeson "Car" <<< unwrap

instance DecodeAeson Car where
  decodeAeson = decodeWrappedAeson "Car" \obj -> do
    carId <- obj .: "carId"
    topSpeed <- obj .: "topSpeed"
    acceleration <- obj .: "acceleration"
    cornering <- obj .: "cornering"
    aerodynamics <- obj .: "aerodynamics"
    pure $ Car { carId, topSpeed, acceleration, cornering, aerodynamics }

data GameAsset
  = DriverAsset Driver
  | CarAsset Car

derive instance Generic GameAsset _
derive instance Eq GameAsset

instance
  HasPlutusSchema GameAsset
    ( "DriverAsset"
        := PNil
        @@ Z
        :+ "CarAsset"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance ToData GameAsset where
  toData = genericToData

instance FromData GameAsset where
  fromData = genericFromData

instance Show GameAsset where
  show = genericShow

instance EncodeAeson GameAsset where
  encodeAeson = case _ of
    DriverAsset driver -> wrapEncodeAeson "DriverAsset" driver
    CarAsset car -> wrapEncodeAeson "CarAsset" car

instance DecodeAeson GameAsset where
  decodeAeson aes =
    decodeWrappedAeson "DriverAsset" (pure <<< DriverAsset) aes <|>
      decodeWrappedAeson "CarAsset" (pure <<< CarAsset) aes

newtype GameAssetPolicyParams = GameAssetPolicyParams
  { adminToken :: (CurrencySymbol /\ TokenName)
  , botToken :: (CurrencySymbol /\ TokenName)
  , asset :: GameAsset
  }

derive instance Generic GameAssetPolicyParams _
derive instance Newtype GameAssetPolicyParams _
derive instance Eq GameAssetPolicyParams

instance
  HasPlutusSchema GameAssetPolicyParams
    ( "GameAssetPolicyParams"
        :=
          ( "adminToken" := I (CurrencySymbol /\ TokenName)
              :+ "botToken"
              := I (CurrencySymbol /\ TokenName)
              :+ "asset"
              := I GameAsset
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData GameAssetPolicyParams where
  toData = genericToData

instance FromData GameAssetPolicyParams where
  fromData = genericFromData

newtype AirdropAddressDatum = AirdropAddressDatum
  { airdropAddress :: Address }

derive instance Generic AirdropAddressDatum _
derive instance Newtype AirdropAddressDatum _
derive instance Eq AirdropAddressDatum

instance
  HasPlutusSchema AirdropAddressDatum
    ( "AirdropAddressDatum"
        :=
          ( "airdropAddress" := I Address
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData AirdropAddressDatum where
  toData = genericToData

instance FromData AirdropAddressDatum where
  fromData = genericFromData
