module CardanoRacers.GameAsset.Contract where

import CardanoRacers.GameAsset.Parameters
import CardanoRacers.GameAsset.Types
import Contract.Prelude

import Aeson (encodeAeson)
import CardanoRacers.Nft (mintNftConstraints)
import Contract.AuxiliaryData (setTxMetadata)
import Contract.Log (logInfo')
import Contract.Metadata
  ( Cip25Metadata(..)
  , Cip25MetadataEntry(..)
  , Cip25String
  , Cip25TokenName(..)
  , TransactionMetadatum(..)
  , mkCip25String
  )
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.Prim.ByteArray
  ( byteArrayFromAscii
  , byteArrayToHex
  , hexToByteArray
  , rawBytesToHex
  )
import Contract.ScriptLookups (mkUnbalancedTx)
import Contract.Scripts (MintingPolicyHash(..))
import Contract.Transaction
  ( TransactionHash(..)
  , TransactionInput(..)
  , awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  )
import Contract.Utxos (getWalletUtxos)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , currencyMPSHash
  , getTokenName
  , mkTokenName
  , mpsSymbol
  )
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Ctl.Internal.Metadata.Cip25.Cip25String (toMetadataString)
import Ctl.Internal.Metadata.FromMetadata (class FromMetadata, fromMetadata)
import Ctl.Internal.Metadata.Helpers (lookupMetadata)
import Ctl.Internal.Metadata.MetadataType (class MetadataType)
import Ctl.Internal.Metadata.ToMetadata (class ToMetadata, toMetadata)
import Ctl.Internal.Serialization.Hash (scriptHashFromBytes, scriptHashToBytes)
import Ctl.Internal.Serialization.Types (MetadataMap)
import Data.Array (catMaybes, concat)
import Data.Array (head) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Map (toUnfoldable) as Map
import Effect.Exception (error)

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

createNewDriver :: Rarity -> Effect Driver
createNewDriver rarity = do
  ps <- generateUniformParameters rarity
  (p1 /\ p2 /\ p3 /\ p4) <- case ps of
    [ p1, p2, p3, p4 ] -> pure (p1 /\ p2 /\ p3 /\ p4)
    _ -> throwError $ error
      "generateUniformParameters returned wrong number of parameters"

  let
    driver = Driver
      { driverId: show rarity <> "Driver"
      , aggression: BigInt.fromInt p1
      , experience: BigInt.fromInt p2
      , reflexes: BigInt.fromInt p3
      , luck: BigInt.fromInt p4
      }
  pure driver

createNewCar :: Rarity -> Effect Car
createNewCar rarity = do
  ps <- generateUniformParameters rarity
  (p1 /\ p2 /\ p3 /\ p4) <- case ps of
    [ p1, p2, p3, p4 ] -> pure (p1 /\ p2 /\ p3 /\ p4)
    _ -> throwError $ error
      "generateUniformParameters returned wrong number of parameters"

  let
    car = Car
      { carId: show rarity <> "Car"
      , acceleration: BigInt.fromInt p1
      , cornering: BigInt.fromInt p2
      , topSpeed: BigInt.fromInt p3
      , aerodynamics: BigInt.fromInt p4
      }
  pure car

mintNewDriverNft :: Rarity -> Contract TransactionHash
mintNewDriverNft rarity = do
  driver <- liftEffect $ createNewDriver rarity
  tk <- liftContractM "could not make token name"
    $ (mkTokenName <=< byteArrayFromAscii)
    $ (unwrap driver).driverId
  txi <- liftedM "could not get first txi" $ getWalletUtxos <#>
    (_ >>= Map.toUnfoldable >>> map fst >>> Array.head)
  (cs /\ constrants /\ lookups) <- mintNftConstraints txi tk
  cip25Name <- liftContractM "could not make cip25 name" $ mkCip25String
    (unwrap driver).driverId
  cip25Mime <- liftContractM "could not make cip25 mime" $ mkCip25String
    "image/png"
  let
    ganm = DriverNftMetadata
      { driver
      , assetClass: cs /\ tk
      , name: cip25Name
      , image:
          "ipfs://k2cwuee3arxg398hwxx6c0iferxitu126xntuzg8t765oo020h5y6npn"
      , mediaType: Just cip25Mime
      , description: Nothing
      }
  -- logInfo' $ show $ toMetadata ganm

  -- let
  --   metadata :: Cip25Metadata
  --   metadata = Cip25Metadata
  --     [ Cip25MetadataEntry
  --         { policyId: currencyMPSHash cs
  --         , assetName: Cip25TokenName tk
  --         , name: cip25Name
  --         , image:
  --             "ipfs://k2cwuee3arxg398hwxx6c0iferxitu126xntuzg8t765oo020h5y6npn"
  --         , mediaType: Nothing -- Just cip25Mime
  --         , description: Just $ show $ encodeAeson driver
  --         , files: []
  --         }
  --     ]
  unbalancedTx <- liftedE $ mkUnbalancedTx lookups constrants
  unbalancedTxWithMetadata <- setTxMetadata unbalancedTx $ GameAssetNftMetadata
    [ ganm ]
  balancedTx <- liftedE $ balanceTx unbalancedTxWithMetadata
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId
  logInfo' $ "Tx ID: " <> show txId
  pure txId

mintWithMetadata :: Contract TransactionHash
mintWithMetadata = do
  txi <- liftedM "could not get first txi" $ getWalletUtxos <#>
    (_ >>= Map.toUnfoldable >>> map fst >>> Array.head)
  tk <- liftContractM "could not make token name" $
    (mkTokenName <=< byteArrayFromAscii) "SomeAsset"
  (cs /\ constrants /\ lookups) <- mintNftConstraints txi tk
  cip25name <- liftContractM "could not make cip25 name" $ mkCip25String
    "Schumacher"
  let
    metadata :: Cip25Metadata
    metadata = Cip25Metadata
      [ Cip25MetadataEntry
          { policyId: currencyMPSHash cs
          , assetName: Cip25TokenName tk
          , name: cip25name
          , image:
              "ipfs://k2cwuee3arxg398hwxx6c0iferxitu126xntuzg8t765oo020h5y6npn"
          , mediaType: Nothing
          , description: Just "Cool Driver"
          , files: []
          }
      ]
  unbalancedTx <- liftedE $ mkUnbalancedTx lookups constrants
  unbalancedTxWithMetadata <- setTxMetadata unbalancedTx metadata
  balancedTx <- liftedE $ balanceTx unbalancedTxWithMetadata
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId
  logInfo' $ "Tx ID: " <> show txId
  pure txId
