module CardanoRacers.RacersState.Types
  ( RacersState(RacersState)
  , AssetPrices(AssetPrices)
  , RacersStateRedeemer(SetRacersState)
  , getAssetPrice
  ) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, (.:))
import CardanoRacers.GameAsset.Types (Rarity(Common, Rare, Epic))
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
  , Z
  , genericFromData
  , genericToData
  )
import Contract.Scripts (ValidatorHash)
import Data.BigInt (BigInt)

newtype AssetPrices = AssetPrices
  { common :: BigInt
  , rare :: BigInt
  , epic :: BigInt
  }

derive instance Generic AssetPrices _
derive instance Newtype AssetPrices _
derive instance Eq AssetPrices

instance Show AssetPrices where
  show = genericShow

instance
  HasPlutusSchema
    AssetPrices
    ( "AssetPrices"
        :=
          ( "common" := I BigInt
              :+ "rare"
              := I BigInt
              :+ "epic"
              := I BigInt
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData AssetPrices where
  toData = genericToData

instance FromData AssetPrices where
  fromData = genericFromData

instance EncodeAeson AssetPrices where
  encodeAeson = wrapEncodeAeson "AssetPrices" <<< unwrap

instance DecodeAeson AssetPrices where
  decodeAeson = decodeWrappedAeson "AssetPrices" \obj -> do
    common <- obj .: "common"
    rare <- obj .: "rare"
    epic <- obj .: "epic"
    pure $ AssetPrices { common, rare, epic }

getAssetPrice :: Rarity -> AssetPrices -> BigInt
getAssetPrice r (AssetPrices ap) = case r of
  Common -> ap.common
  Rare -> ap.rare
  Epic -> ap.epic

newtype RacersState = RacersState
  { nitroPrice :: BigInt -- Nitro price in Lovelace
  , treasuryAddress :: Address
  , operatingAddress :: Address
  , assetPrices :: AssetPrices
  , depositScript :: ValidatorHash
  }

derive instance Generic RacersState _
derive instance Newtype RacersState _
derive instance Eq RacersState

instance
  HasPlutusSchema RacersState
    ( "RacersState"
        :=
          ( "nitroPrice" := I BigInt
              :+ "treasuryAddress"
              := I Address
              :+ "operatingAddress"
              := I Address
              :+ "assetPrices"
              := I AssetPrices
              :+ "depositScript"
              := I ValidatorHash
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData RacersState where
  toData = genericToData

instance FromData RacersState where
  fromData = genericFromData

instance Show RacersState where
  show = genericShow

instance EncodeAeson RacersState where
  encodeAeson = wrapEncodeAeson "RacersState" <<< unwrap

instance DecodeAeson RacersState where
  decodeAeson = decodeWrappedAeson "RacersState" \obj -> do
    nitroPrice <- obj .: "nitroPrice"
    treasuryAddress <- obj .: "treasuryAddress"
    operatingAddress <- obj .: "operatingAddress"
    assetPrices <- obj .: "assetPrices"
    depositScript <- obj .: "depositScript"
    pure $ RacersState
      { nitroPrice
      , treasuryAddress
      , operatingAddress
      , assetPrices
      , depositScript
      }

newtype RacersStateRedeemer = SetRacersState RacersState

derive instance Generic RacersStateRedeemer _
derive instance Newtype RacersStateRedeemer _
derive instance Eq RacersStateRedeemer
instance
  HasPlutusSchema RacersStateRedeemer
    ("SetRacersState" := PNil @@ Z :+ PNil)

instance ToData RacersStateRedeemer where
  toData = genericToData

instance FromData RacersStateRedeemer where
  fromData = genericFromData

instance Show RacersStateRedeemer where
  show = genericShow

instance EncodeAeson RacersStateRedeemer where
  encodeAeson = wrapEncodeAeson "SetRacersState" <<< unwrap

instance DecodeAeson RacersStateRedeemer where
  decodeAeson = decodeWrappedAeson "SetRacersState" (pure <<< SetRacersState)
