module CardanoRacers.GameAsset.Contract
  ( mintAvailableAssetByRarity
  , mkGameAssetPolicy
  , generateAsset
  ) where

import Contract.Prelude

import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.GameAsset.Parameters (generateUniformParameters)
import CardanoRacers.GameAsset.Types
  ( AssetOption
  , CarAttributes(CarAttributes)
  , DriverAttributes(DriverAttributes)
  , GameAsset
  , GameAssetAttributes(DriverAttrs, CarAttrs)
  , GameAssetNftMetadataEntry(GameAssetNftMetadataEntry)
  , GameAssetType(CarType, DriverType)
  , Rarity
  , mkGameAsset
  )
import CardanoRacers.Helpers (paysToAddrConstraint)
import CardanoRacers.ScriptsFFI (gameAssetPolicy)
import Contract.Address (Address)
import Contract.Metadata (unCip25String)
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , MintingPolicyHash(..)
  , applyArgs
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionInput(..)
  , TransactionOutputWithRefScript(..)
  , mkTxUnspentOut
  )
import Contract.TxConstraints (InputWithScriptRef(..))
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName, mkTokenName)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe, throwError)
import Data.Array (singleton) as Array
import Data.BigInt (fromInt) as BigInt
import Data.Profunctor.Choice (left)
import Effect.Exception (error)
import Random.LCG (randomSeed)

generateAsset
  :: AssetOption -> String -> Rarity -> Effect (GameAsset /\ TokenName)
generateAsset ao nonce rarity = do
  attrs <- case ao.assetType of
    CarType -> CarAttrs <$> generateNewCar rarity
    DriverType -> DriverAttrs <$> generateNewDriver rarity

  ga <-
    liftMaybe (error "invalid game asset params, could not create game asset")
      $ mkGameAsset
          { assetType: ao.assetType
          , attributes: attrs
          , name: ao.name
          , imageUrl: ao.imageUrl
          , mediaType: Nothing
          , description: ao.description
          }

  nameByteArrayWithSep <- liftMaybe (error "could not create name byte array")
    $ byteArrayFromAscii
    $ (unCip25String ao.name)
    <> ":"
    <> nonce

  tkName <- liftMaybe (error "could not create token name") $ mkTokenName
    $ nameByteArrayWithSep

  pure (ga /\ tkName)

generateNewDriver :: Rarity -> Effect DriverAttributes
generateNewDriver rarity = do
  seed <- randomSeed
  let ps = generateUniformParameters seed rarity
  (p1 /\ p2 /\ p3 /\ p4) <- case ps of
    [ p1, p2, p3, p4 ] -> pure (p1 /\ p2 /\ p3 /\ p4)
    _ -> throwError $ error
      "generateUniformParameters returned wrong number of parameters"

  let
    driver = DriverAttributes
      { aggression: BigInt.fromInt p1
      , experience: BigInt.fromInt p2
      , reflexes: BigInt.fromInt p3
      , luck: BigInt.fromInt p4
      }
  pure driver

generateNewCar :: Rarity -> Effect CarAttributes
generateNewCar rarity = do
  seed <- randomSeed
  let ps = generateUniformParameters seed rarity
  (p1 /\ p2 /\ p3 /\ p4) <- case ps of
    [ p1, p2, p3, p4 ] -> pure (p1 /\ p2 /\ p3 /\ p4)
    _ -> throwError $ error
      "generateUniformParameters returned wrong number of parameters"

  let
    car = CarAttributes
      { acceleration: BigInt.fromInt p1
      , cornering: BigInt.fromInt p2
      , topSpeed: BigInt.fromInt p3
      , aerodynamics: BigInt.fromInt p4
      }
  pure car

type MintAssetNftOptions =
  { tokenNameStr :: String
  , nameStr :: String
  , image :: String
  , mediaType :: Maybe String
  , description :: Maybe String
  , rarity :: Rarity
  }

mintAvailableAssetByRarity
  :: Maybe
       (MintingPolicyHash /\ TransactionInput /\ TransactionOutputWithRefScript)
  -> AssetOption
  -> CurrencySymbol
  -> String
  -> Address
  -> Rarity
  -> Effect (Constraints.TxConstraints Void Void /\ GameAssetNftMetadataEntry)
mintAvailableAssetByRarity
  mAssetPolicyRef
  assetOption
  assetSymbol
  nonce
  targetAddress
  rarity = do
  (ga /\ tk) <- generateAsset assetOption nonce rarity

  let
    assetVal = Value.singleton assetSymbol tk $ BigInt.fromInt 1
    assetMintConstraints = maybe
      (Constraints.mustMintValue assetVal)
      ( \(mph /\ refTxi /\ refTxo) -> Constraints.mustMintCurrencyUsingScriptRef
          mph
          tk
          (BigInt.fromInt 1)
          (RefInput $ mkTxUnspentOut refTxi refTxo)
      )
      mAssetPolicyRef
    constraints = assetMintConstraints
      <> paysToAddrConstraint targetAddress assetVal
    metadata = GameAssetNftMetadataEntry
      { asset: ga
      , assetClass: assetSymbol /\ tk
      }

  pure $ constraints /\ metadata

mkGameAssetPolicy :: RacersParams -> Contract MintingPolicy
mkGameAssetPolicy np = do
  v2script <- liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope gameAssetPolicy
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ Array.singleton
    $ toData np
  pure $ PlutusMintingPolicy $ appliedScript
