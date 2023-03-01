module CardanoRacers.GameAsset.Types where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, (.:))
import CardanoRacers.Helpers (decodeWrappedAeson, wrapEncodeAeson)
import Contract.Address (Address)
import Contract.Metadata
  ( Cip25String
  , Cip25TokenName
  , TransactionMetadatum(MetadataMap)
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
  , S
  , Z
  , genericFromData
  , genericToData
  )
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArray, rawBytesToHex)
import Contract.Scripts (MintingPolicyHash)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , currencyMPSHash
  , getTokenName
  , mkTokenName
  , mpsSymbol
  )
import Control.Alt ((<|>))
import Ctl.Internal.Metadata.Cip25.Cip25String (toMetadataString)
import Ctl.Internal.Metadata.FromMetadata (class FromMetadata, fromMetadata)
import Ctl.Internal.Metadata.Helpers (lookupMetadata)
import Ctl.Internal.Metadata.MetadataType (class MetadataType)
import Ctl.Internal.Metadata.ToMetadata (class ToMetadata, toMetadata)
import Ctl.Internal.Serialization.Hash (scriptHashFromBytes, scriptHashToBytes)
import Data.Array (catMaybes, concat)
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Map (toUnfoldable) as Map

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

type CommonAssetNftMetadata r =
  { assetClass :: CurrencySymbol /\ TokenName
  , name :: Cip25String
  , image :: String
  , mediaType :: Maybe Cip25String
  , description :: Maybe String
  | r
  }

data GameAssetNftMetadataEntry
  = DriverNftMetadata
      { driver :: Driver
      , assetClass :: CurrencySymbol /\ TokenName
      , name :: Cip25String
      , image :: String
      , mediaType :: Maybe Cip25String
      , description :: Maybe String
      }
  | CarNftMetadata
      { car :: Car
      , assetClass :: CurrencySymbol /\ TokenName
      , name :: Cip25String
      , image :: String
      , mediaType :: Maybe Cip25String
      , description :: Maybe String
      }

gameAssetMetadataEntryToKeyValue
  :: GameAssetNftMetadataEntry -> Array (String /\ TransactionMetadatum)
gameAssetMetadataEntryToKeyValue ganme = case ganme of
  DriverNftMetadata dnm -> aux dnm attributeEntries
    where
    attributeEntries =
      [ "driverId" /\ toMetadata (unwrap dnm.driver).driverId
      , "aggression" /\ toMetadata (unwrap dnm.driver).aggression
      , "experience" /\ toMetadata (unwrap dnm.driver).experience
      , "reflexes" /\ toMetadata (unwrap dnm.driver).reflexes
      , "luck" /\ toMetadata (unwrap dnm.driver).luck
      ]

  CarNftMetadata cnm -> aux cnm attributeEntries
    where
    attributeEntries =
      [ "carId" /\ toMetadata (unwrap cnm.car).carId
      , "acceleration" /\ toMetadata (unwrap cnm.car).acceleration
      , "cornering" /\ toMetadata (unwrap cnm.car).cornering
      , "topSpeed" /\ toMetadata (unwrap cnm.car).topSpeed
      , "aerodynamics" /\ toMetadata (unwrap cnm.car).aerodynamics
      ]

  where
  aux
    :: forall (r :: Row Type)
     . CommonAssetNftMetadata r
    -> Array (String /\ TransactionMetadatum)
    -> Array (String /\ TransactionMetadatum)
  aux nm attributeEntries =
    let
      dataEntry = [ "attributes" /\ toMetadata attributeEntries ]
      cip25metadata =
        [ "name" /\ toMetadata nm.name
        , "image" /\ toMetadataString nm.image
        ]
          <>
            ( fold $ nm.mediaType <#> \mediaType ->
                [ "mediaType" /\ toMetadata mediaType ]
            )
          <>
            ( fold $ nm.description <#> \description ->
                [ "description" /\ toMetadataString description ]
            )
      assetEntry =
        [ (byteArrayToHex $ getTokenName $ snd nm.assetClass) /\ toMetadata
            (dataEntry <> cip25metadata)
        ]
      policyEntry =
        [ ( rawBytesToHex $ scriptHashToBytes $ unwrap $ currencyMPSHash
              (fst nm.assetClass)
          ) /\ toMetadata assetEntry
        ]

    in
      policyEntry

gameAssetMetadataEntryFromMetadata
  :: MintingPolicyHash
  -> Cip25TokenName
  -> TransactionMetadatum
  -> Maybe GameAssetNftMetadataEntry
gameAssetMetadataEntryFromMetadata policy tk md = do
  name <- lookupMetadata "name" md >>= fromMetadata
  image <- lookupMetadata "image" md >>= fromMetadata
  mbMediaType <- for (lookupMetadata "mediaType" md) fromMetadata
  mbDescription <- for (lookupMetadata "description" md) fromMetadata
  attrsMd <- lookupMetadata "attributes" md >>= fromMetadata
  cs <- mpsSymbol policy
  let
    decodeDriverAttributes attrs = do
      driverId <- lookupMetadata "driverId" attrs >>= fromMetadata
      aggression <- lookupMetadata "aggression" attrs >>= fromMetadata
      experience <- lookupMetadata "experience" attrs >>= fromMetadata
      reflexes <- lookupMetadata "reflexes" attrs >>= fromMetadata
      luck <- lookupMetadata "luck" attrs >>= fromMetadata
      pure $ DriverNftMetadata
        { driver: Driver { driverId, aggression, experience, reflexes, luck }
        , assetClass: cs /\ unwrap tk
        , name
        , image
        , mediaType: mbMediaType
        , description: mbDescription
        }
    decodeCarAttributes attrs = do
      carId <- lookupMetadata "carId" attrs >>= fromMetadata
      acceleration <- lookupMetadata "acceleration" attrs >>= fromMetadata
      cornering <- lookupMetadata "cornering" attrs >>= fromMetadata
      topSpeed <- lookupMetadata "topSpeed" attrs >>= fromMetadata
      aerodynamics <- lookupMetadata "aerodynamics" attrs >>= fromMetadata
      pure $ CarNftMetadata
        { car: Car { carId, acceleration, cornering, topSpeed, aerodynamics }
        , assetClass: cs /\ unwrap tk
        , name
        , image
        , mediaType: mbMediaType
        , description: mbDescription
        }
  decodeDriverAttributes attrsMd <|> decodeCarAttributes attrsMd

newtype GameAssetNftMetadata = GameAssetNftMetadata
  (Array GameAssetNftMetadataEntry)

derive instance Newtype GameAssetNftMetadata _

instance ToMetadata GameAssetNftMetadata where
  toMetadata (GameAssetNftMetadata ganmes) = toMetadata $
    let
      policyEntries = concat $ gameAssetMetadataEntryToKeyValue <$> ganmes
      versionEntry = [ "version" /\ toMetadata (BigInt.fromInt 2) ]
    in
      policyEntries <> versionEntry

instance FromMetadata GameAssetNftMetadata where
  fromMetadata (MetadataMap mp1) = do
    arrMbArrGanmes <- for (Map.toUnfoldable mp1 :: Array _)
      \(policy /\ assets) ->
        if policy == toMetadata "version" then
          ( if assets == toMetadata (BigInt.fromInt 2) then Just Nothing
            else Nothing
          )
        else
          Just case assets of
            MetadataMap mp2 ->
              for (Map.toUnfoldable mp2 :: Array _)
                \( assetName /\
                     contents
                 ) -> join $ gameAssetMetadataEntryFromMetadata
                  <$>
                    ( map wrap <<< scriptHashFromBytes <=< hexToByteArray
                        <=< fromMetadata
                        $ policy
                    )
                  <*>
                    ( map wrap <<< mkTokenName <=< hexToByteArray
                        <=< fromMetadata
                        $ assetName
                    )
                  <*> pure contents
            _ -> Nothing
    let ganmes = concat $ catMaybes arrMbArrGanmes
    pure $ GameAssetNftMetadata ganmes
  fromMetadata _ = Nothing

instance MetadataType GameAssetNftMetadata where
  metadataLabel _ = wrap $ BigInt.fromInt 721
