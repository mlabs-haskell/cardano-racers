module CardanoRacers.GameAsset.Types
  ( Rarity(Common, Rare, Epic)
  , DriverAttributes(DriverAttributes)
  , CarAttributes(CarAttributes)
  , GameAssetType(DriverType, CarType)
  , GameAssetAttributes(DriverAttrs, CarAttrs)
  , GameAsset
  , GameAssetObject
  , GameAssetNftMetadataEntry(GameAssetNftMetadataEntry)
  , GameAssetNftMetadata(GameAssetNftMetadata)
  , AssetOption
  , mkGameAsset
  , unGameAsset
  ) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, encodeAeson, (.:))
import CardanoRacers.Helpers
  ( decodeAesonString
  , decodeWrappedAeson
  , wrapEncodeAeson
  )
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
import Ctl.Internal.Metadata.Cip25.Cip25String
  ( fromMetadataString
  , toMetadataString
  )
import Ctl.Internal.Metadata.FromMetadata (class FromMetadata, fromMetadata)
import Ctl.Internal.Metadata.Helpers (lookupMetadata)
import Ctl.Internal.Metadata.MetadataType (class MetadataType)
import Ctl.Internal.Metadata.ToMetadata (class ToMetadata, toMetadata)
import Ctl.Internal.Serialization.Hash (scriptHashFromBytes, scriptHashToBytes)
import Data.Array (catMaybes, concat)
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Function (on)
import Data.Map (toUnfoldable) as Map

type AssetOption =
  { name :: Cip25String
  , assetType :: GameAssetType
  , imageUrl :: String
  , description :: String
  , nitroAmount :: BigInt
  }

data Rarity = Common | Rare | Epic

derive instance Generic Rarity _
derive instance Eq Rarity
derive instance Ord Rarity

instance Show Rarity where
  show = genericShow

instance EncodeAeson Rarity where
  encodeAeson Common = encodeAeson "Common"
  encodeAeson Rare = encodeAeson "Rare"
  encodeAeson Epic = encodeAeson "Epic"

instance DecodeAeson Rarity where
  decodeAeson aes =
    decodeAesonString "Common" (const Common) aes
      <|> decodeAesonString "Rare" (const Rare) aes
      <|> decodeAesonString "Epic" (const Epic) aes

instance
  HasPlutusSchema Rarity
    ( "Common"
        := PNil
        @@ Z
        :+ "Rare"
        := PNil
        @@ (S Z)
        :+ "Epic"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

instance ToData Rarity where
  toData = genericToData

instance FromData Rarity where
  fromData = genericFromData

newtype DriverAttributes = DriverAttributes
  { aggression :: BigInt
  , experience :: BigInt
  , reflexes :: BigInt
  , luck :: BigInt
  }

derive instance Generic DriverAttributes _
derive instance Newtype DriverAttributes _
derive instance Eq DriverAttributes

instance
  HasPlutusSchema DriverAttributes
    ( "DriverAttributes"
        :=
          ( "aggression"
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

instance ToData DriverAttributes where
  toData = genericToData

instance FromData DriverAttributes where
  fromData = genericFromData

instance Show DriverAttributes where
  show = genericShow

instance EncodeAeson DriverAttributes where
  encodeAeson = wrapEncodeAeson "DriverAttributes" <<< unwrap

instance DecodeAeson DriverAttributes where
  decodeAeson = decodeWrappedAeson "DriverAttributes" \obj -> do
    aggression <- obj .: "aggression"
    experience <- obj .: "experience"
    reflexes <- obj .: "reflexes"
    luck <- obj .: "luck"
    pure $ DriverAttributes { aggression, experience, reflexes, luck }

newtype CarAttributes = CarAttributes
  { topSpeed :: BigInt
  , acceleration :: BigInt
  , cornering :: BigInt
  , aerodynamics :: BigInt
  }

derive instance Generic CarAttributes _
derive instance Newtype CarAttributes _
derive instance Eq CarAttributes

instance
  HasPlutusSchema CarAttributes
    ( "CarAttributes"
        :=
          ( "topSpeed"
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

instance ToData CarAttributes where
  toData = genericToData

instance FromData CarAttributes where
  fromData = genericFromData

instance Show CarAttributes where
  show = genericShow

instance EncodeAeson CarAttributes where
  encodeAeson = wrapEncodeAeson "CarAttributes" <<< unwrap

instance DecodeAeson CarAttributes where
  decodeAeson = decodeWrappedAeson "CarAttributes" \obj -> do
    topSpeed <- obj .: "topSpeed"
    acceleration <- obj .: "acceleration"
    cornering <- obj .: "cornering"
    aerodynamics <- obj .: "aerodynamics"
    pure $ CarAttributes { topSpeed, acceleration, cornering, aerodynamics }

data GameAssetType = DriverType | CarType

derive instance Generic GameAssetType _
derive instance Eq GameAssetType

instance Ord GameAssetType where
  compare = compare `on` toInt
    where
    toInt DriverType = 0
    toInt CarType = 1

instance Show GameAssetType where
  show = genericShow

instance
  HasPlutusSchema GameAssetType
    ( "DriverType"
        := PNil
        @@ Z
        :+ "CarType"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance ToData GameAssetType where
  toData = genericToData

instance FromData GameAssetType where
  fromData = genericFromData

instance EncodeAeson GameAssetType where
  encodeAeson DriverType = encodeAeson "DriverType"
  encodeAeson CarType = encodeAeson "CarType"

instance DecodeAeson GameAssetType where
  decodeAeson aes =
    decodeAesonString "DriverType" (const DriverType) aes
      <|> decodeAesonString "CarType" (const CarType) aes

data GameAssetAttributes
  = DriverAttrs DriverAttributes
  | CarAttrs CarAttributes

driverAttrsFromAttributes :: GameAssetAttributes -> Maybe DriverAttributes
driverAttrsFromAttributes = case _ of
  DriverAttrs driver -> Just driver
  CarAttrs _ -> Nothing

carAttrsFromAttributes :: GameAssetAttributes -> Maybe CarAttributes
carAttrsFromAttributes = case _ of
  DriverAttrs _ -> Nothing
  CarAttrs car -> Just car

derive instance Generic GameAssetAttributes _
derive instance Eq GameAssetAttributes

instance
  HasPlutusSchema GameAssetAttributes
    ( "DriverAttrs"
        := PNil
        @@ Z
        :+ "CarAttrs"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance ToData GameAssetAttributes where
  toData = genericToData

instance FromData GameAssetAttributes where
  fromData = genericFromData

instance Show GameAssetAttributes where
  show = genericShow

instance EncodeAeson GameAssetAttributes where
  encodeAeson = case _ of
    DriverAttrs driver -> wrapEncodeAeson "DriverAttrs" driver
    CarAttrs car -> wrapEncodeAeson "CarAttrs" car

instance DecodeAeson GameAssetAttributes where
  decodeAeson aes =
    decodeWrappedAeson "DriverAttrs" (pure <<< DriverAttrs) aes <|>
      decodeWrappedAeson "CarAttrs" (pure <<< CarAttrs) aes

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

type GameAssetObject =
  { assetType :: GameAssetType
  , attributes :: GameAssetAttributes
  , imageUrl :: String
  , mediaType :: Maybe String
  , name :: Cip25String
  , tokenName :: TokenName
  , description :: String
  }

newtype GameAsset = GameAsset GameAssetObject

unGameAsset :: GameAsset -> GameAssetObject
unGameAsset (GameAsset ga) = ga

mkGameAsset
  :: GameAssetObject
  -> Maybe GameAsset
mkGameAsset
  { assetType, attributes, imageUrl, mediaType, name, description, tokenName }
  | assetType == DriverType && isJust (driverAttrsFromAttributes attributes) =
      Just $ GameAsset
        { assetType
        , attributes
        , imageUrl
        , mediaType
        , name
        , description
        , tokenName
        }
  | assetType == CarType && isJust (carAttrsFromAttributes attributes) = Just $
      GameAsset
        { assetType
        , attributes
        , imageUrl
        , mediaType
        , name
        , description
        , tokenName
        }
  | otherwise = Nothing

derive instance Eq GameAsset

instance Show GameAsset where
  show (GameAsset ga) = "(GameAsset " <> show ga <> ")"

instance EncodeAeson GameAsset where
  encodeAeson (GameAsset ga) = wrapEncodeAeson "GameAsset" ga

instance DecodeAeson GameAsset where
  decodeAeson = decodeWrappedAeson "GameAsset" \obj -> do
    assetType <- obj .: "assetType"
    attributes <- obj .: "attributes"
    imageUrl <- obj .: "imageUrl"
    mediaType <- obj .: "mediaType"
    name <- obj .: "name"
    description <- obj .: "description"
    tokenName <- obj .: "tokenName"
    pure $ GameAsset
      { assetType
      , attributes
      , imageUrl
      , mediaType
      , name
      , description
      , tokenName
      }

type GameAssetMeta =
  { asset :: GameAsset
  , assetClass :: CurrencySymbol /\ TokenName
  }

newtype GameAssetNftMetadataEntry = GameAssetNftMetadataEntry
  { asset :: GameAsset
  , assetClass :: CurrencySymbol /\ TokenName
  }

derive instance Newtype GameAssetNftMetadataEntry _

gameAssetMetadataEntryToKeyValue
  :: GameAssetNftMetadataEntry -> Array (String /\ TransactionMetadatum)
gameAssetMetadataEntryToKeyValue
  (GameAssetNftMetadataEntry { asset: GameAsset asset, assetClass }) =
  policyEntry
  where
  policyEntry =
    [ ( rawBytesToHex $ scriptHashToBytes $ unwrap $ currencyMPSHash
          (fst assetClass)
      ) /\ toMetadata assetEntry
    ]
  assetEntry =
    [ (byteArrayToHex $ getTokenName $ snd assetClass) /\ toMetadata dataEntry
    ]
  dataEntry =
    [ "name" /\ toMetadata (asset.name)
    , "image" /\ toMetadataString (asset.imageUrl)
    , "description" /\ toMetadataString (asset.description)
    , "attributes" /\ toMetadata attributesEntry
    , "type" /\ toMetadata
        ( case asset.assetType of
            DriverType -> "Driver"
            CarType -> "Car"
        )
    ] <> fromMaybe []
      (asset.mediaType <#> \mt -> [ "mediaType" /\ toMetadata mt ])

  attributesEntry = case asset.attributes of
    DriverAttrs (DriverAttributes driver) ->
      [ "aggression" /\ toMetadata (driver.aggression)
      , "experience" /\ toMetadata (driver.experience)
      , "reflexes" /\ toMetadata (driver.reflexes)
      , "luck" /\ toMetadata (driver.luck)
      ]
    CarAttrs (CarAttributes car) ->
      [ "topSpeed" /\ toMetadata (car.topSpeed)
      , "acceleration" /\ toMetadata (car.acceleration)
      , "cornering" /\ toMetadata (car.cornering)
      , "aerodynamics" /\ toMetadata (car.aerodynamics)
      ]

gameAssetMetadataEntryFromMetadata
  :: MintingPolicyHash
  -> Cip25TokenName
  -> TransactionMetadatum
  -> Maybe GameAssetNftMetadataEntry
gameAssetMetadataEntryFromMetadata policy tk md = do
  name <- lookupMetadata "name" md >>= fromMetadata
  imageUrl <- lookupMetadata "image" md >>= fromMetadataString
  assetType <- lookupMetadata "type" md >>= fromMetadata >>= case _ of
    "Driver" -> pure DriverType
    "Car" -> pure CarType
    _ -> Nothing
  description <- lookupMetadata "description" md >>= fromMetadataString
  mbMediaType <- for (lookupMetadata "mediaType" md) fromMetadata
  cs <- mpsSymbol policy
  attrsMd <- lookupMetadata "attributes" md >>= fromMetadata >>=
    ( \attrs ->
        case assetType of
          DriverType -> DriverAttrs <$> decodeDriverAttrs attrs
          CarType -> CarAttrs <$> decodeCarAttrs attrs
    )
  pure $ GameAssetNftMetadataEntry
    { asset: GameAsset
        { assetType
        , attributes: attrsMd
        , imageUrl
        , mediaType: mbMediaType
        , name
        , description
        , tokenName: (unwrap tk)
        }
    , assetClass: cs /\ (unwrap tk)
    }
  where
  decodeDriverAttrs :: TransactionMetadatum -> Maybe DriverAttributes
  decodeDriverAttrs attrs = do
    aggression <- lookupMetadata "aggression" attrs >>= fromMetadata
    experience <- lookupMetadata "experience" attrs >>= fromMetadata
    reflexes <- lookupMetadata "reflexes" attrs >>= fromMetadata
    luck <- lookupMetadata "luck" attrs >>= fromMetadata
    pure $ DriverAttributes { aggression, experience, reflexes, luck }

  decodeCarAttrs :: TransactionMetadatum -> Maybe CarAttributes
  decodeCarAttrs attrs = do
    acceleration <- lookupMetadata "acceleration" attrs >>= fromMetadata
    cornering <- lookupMetadata "cornering" attrs >>= fromMetadata
    topSpeed <- lookupMetadata "topSpeed" attrs >>= fromMetadata
    aerodynamics <- lookupMetadata "aerodynamics" attrs >>= fromMetadata
    pure $ CarAttributes { acceleration, cornering, topSpeed, aerodynamics }

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
