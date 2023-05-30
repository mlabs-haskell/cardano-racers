module CardanoRacers.RaceRegistry.Types
  ( RegistryParams(RegistryParams)
  , RaceParticipant(RaceParticipant)
  , RegistryEntry(PendingSelection, AssetSelection)
  , RegistryDatum(RegistryDatum)
  , RegistryRedeemer(Enroll, SelectAssets, Collect)
  ) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, encodeAeson, getField, (.:))
import CardanoRacers.Helpers
  ( decodeAesonString
  , decodeWrappedAeson
  , wrapEncodeAeson
  )
import Contract.Address (Address, PubKeyHash)
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
import Contract.Scripts (MintingPolicyHash)
import Contract.Value (CurrencySymbol, TokenName)
import Control.Alt ((<|>))
import Data.BigInt (BigInt)

newtype RegistryParams = RegistryParams
  { slotAssetClass :: (CurrencySymbol /\ TokenName)
  -- ^ can't reuse tokens across races, if that's desired an additional raceHash parameters should be included to ensure uniqueness
  , nitroPolicyHash :: MintingPolicyHash
  , driverAssetPolicyHash :: MintingPolicyHash
  , carAssetPolicyHash :: MintingPolicyHash
  , nitroFee :: BigInt
  }

derive instance Generic RegistryParams _
derive instance Newtype RegistryParams _
derive instance Eq RegistryParams

instance Show RegistryParams where
  show = genericShow

instance
  HasPlutusSchema RegistryParams
    ( "RegistryParams"
        :=
          ( "slotAssetClass"
              := I (CurrencySymbol /\ TokenName)
              :+ "nitroPolicyHash"
              := I MintingPolicyHash
              :+ "driverAssetPolicyHash"
              := I MintingPolicyHash
              :+ "carAssetPolicyHash"
              := I MintingPolicyHash
              :+ "nitroFee"
              := I BigInt
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
    slotAssetClass <- obj .: "slotAssetClass"
    nitroPolicyHash <- obj .: "nitroPolicyHash"
    driverAssetPolicyHash <- obj .: "driverAssetPolicyHash"
    carAssetPolicyHash <- obj .: "carAssetPolicyHash"
    nitroFee <- obj .: "nitroFee"
    pure $ RegistryParams
      { slotAssetClass
      , nitroPolicyHash
      , driverAssetPolicyHash
      , carAssetPolicyHash
      , nitroFee
      }

newtype RaceParticipant = RaceParticipant
  { car :: TokenName
  , driver :: TokenName
  , payoutAddress :: Address
  }

-- RaceParticipant type
derive instance Generic RaceParticipant _
derive instance Newtype RaceParticipant _
derive instance Eq RaceParticipant

instance Show RaceParticipant where
  show = genericShow

instance
  HasPlutusSchema RaceParticipant
    ( "RaceParticipant"
        :=
          ( "car"
              := I TokenName
              :+ "driver"
              := I TokenName
              :+ "payoutAddress"
              := I Address
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData RaceParticipant where
  toData = genericToData

instance FromData RaceParticipant where
  fromData = genericFromData

instance EncodeAeson RaceParticipant where
  encodeAeson = wrapEncodeAeson "RaceParticipant" <<< unwrap

instance DecodeAeson RaceParticipant where
  decodeAeson = decodeWrappedAeson "RaceParticipant" \obj -> do
    car <- obj .: "car"
    driver <- obj .: "driver"
    payoutAddress <- obj .: "payoutAddress"
    pure $ RaceParticipant { car, driver, payoutAddress }

-- RegistryEntry type
data RegistryEntry
  = PendingSelection PubKeyHash
  | AssetSelection RaceParticipant

derive instance Generic RegistryEntry _
derive instance Eq RegistryEntry

instance Show RegistryEntry where
  show = genericShow

instance
  HasPlutusSchema RegistryEntry
    ( "PendingSelection"
        := PNil
        @@ Z
        :+ "AssetSelection"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance ToData RegistryEntry where
  toData = genericToData

instance FromData RegistryEntry where
  fromData = genericFromData

instance EncodeAeson RegistryEntry where
  encodeAeson (PendingSelection pkh) = wrapEncodeAeson "PendingSelection"
    { pkh }
  encodeAeson (AssetSelection raceParticipant) = wrapEncodeAeson
    "AssetSelection"
    { raceParticipant }

instance DecodeAeson RegistryEntry where
  decodeAeson aes =
    decodeWrappedAeson "PendingSelection"
      (map PendingSelection <<< flip getField "pkh")
      aes
      <|> decodeWrappedAeson "AssetSelection"
        (map AssetSelection <<< flip getField "raceParticipant")
        aes

newtype RegistryDatum = RegistryDatum (Array RegistryEntry)

-- RegistryDatum type
derive instance Generic RegistryDatum _
derive instance Newtype RegistryDatum _
derive instance Eq RegistryDatum

instance Show RegistryDatum where
  show = genericShow

instance
  HasPlutusSchema RegistryDatum
    ( "RegistryDatum"
        :=
          ( "registryEntries"
              := I (Array RegistryEntry)
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData RegistryDatum where
  toData = genericToData

instance FromData RegistryDatum where
  fromData = genericFromData

instance EncodeAeson RegistryDatum where
  encodeAeson = wrapEncodeAeson "RegistryDatum" <<< unwrap

instance DecodeAeson RegistryDatum where
  decodeAeson = decodeWrappedAeson "RegistryDatum" \obj -> do
    registryEntries <- obj .: "registryEntries"
    pure $ RegistryDatum registryEntries

data RegistryRedeemer = Enroll (Array PubKeyHash) | SelectAssets | Collect

-- RegistryRedeemer type
derive instance Generic RegistryRedeemer _
derive instance Eq RegistryRedeemer

instance Show RegistryRedeemer where
  show = genericShow

instance
  HasPlutusSchema RegistryRedeemer
    ( "Enroll"
        := PNil
        @@ Z
        :+ "SelectAssets"
        := PNil
        @@ (S Z)
        :+ "Collect"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

instance ToData RegistryRedeemer where
  toData = genericToData

instance FromData RegistryRedeemer where
  fromData = genericFromData

instance EncodeAeson RegistryRedeemer where
  encodeAeson (Enroll pubKeyHashes) = wrapEncodeAeson "Enroll" { pubKeyHashes }
  encodeAeson SelectAssets = encodeAeson "SelectAssets"
  encodeAeson Collect = encodeAeson "Collect"

instance DecodeAeson RegistryRedeemer where
  decodeAeson aes =
    decodeWrappedAeson "Enroll"
      (map Enroll <<< flip getField "pubKeyHashes")
      aes
      <|> decodeAesonString "SelectAssets" (const SelectAssets) aes
      <|> decodeAesonString "Collect" (const Collect) aes
