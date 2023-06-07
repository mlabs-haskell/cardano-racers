module CardanoRacers.GameAsset.Contract
  ( mintAvailableAssetByRarity
  , mkGameAssetPolicy
  , generateAsset
  , mintGameAsset
  ) where

import Contract.Prelude

import CardanoRacers.GameAsset.Parameters (generateUniformParameters)
import CardanoRacers.GameAsset.Types
  ( AssetOption
  , CarAttributes(CarAttributes)
  , DriverAttributes(DriverAttributes)
  , GameAsset
  , GameAssetAttributes(DriverAttrs, CarAttrs)
  , GameAssetNftMetadata
  , GameAssetNftMetadataEntry(GameAssetNftMetadataEntry)
  , GameAssetType(DriverType, CarType)
  , Rarity
  , mkGameAsset
  )
import CardanoRacers.Helpers (paysToAddrConstraint)
import CardanoRacers.ScriptsFFI (gameAssetPolicy)
import Common.ContractHelpers (findAnyAuthUtxo)
import Contract.Address (Address)
import Contract.AuxiliaryData (setTxMetadata)
import Contract.Metadata (mkCip25String, unCip25String)
import Contract.Monad (liftContractM, liftedE, liftedM)
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , MintingPolicyHash
  , applyArgs
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
  , balanceTx
  , mkTxUnspentOut
  , signTransaction
  , submit
  )
import Contract.TxConstraints (InputWithScriptRef(RefInput))
import Contract.TxConstraints as Constraints
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , mkTokenName
  , scriptCurrencySymbol
  )
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe, throwError)
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)
import Data.BigInt (fromInt) as BigInt
import Data.Map (singleton) as Map
import Data.Profunctor.Choice (left)
import Effect.Exception (error)
import Racers (Racers, withContract)
import Random.LCG (randomSeed)
import Record.Builder (build, delete, modify)
import Type.Proxy (Proxy(Proxy))

type RawAssetOption =
  { name :: String
  , assetType :: GameAssetType
  , imageUrl :: String
  , description :: String
  }

generateAsset
  :: RawAssetOption -> String -> Rarity -> Effect (GameAsset /\ TokenName)
generateAsset ao nonce rarity = do
  attrs <- case ao.assetType of
    CarType -> CarAttrs <$> generateNewCar rarity
    DriverType -> DriverAttrs <$> generateNewDriver rarity

  cip25Name <- liftMaybe (error "could not create cip25 string from asset name")
    $ mkCip25String ao.name

  nameByteArray <- liftMaybe (error "could not create name byte array")
    $ byteArrayFromAscii
    $ (unCip25String cip25Name)
    <> ":"
    <> nonce

  tkName <- liftMaybe (error "could not create token name") $ mkTokenName
    $ nameByteArray

  ga <-
    liftMaybe (error "invalid game asset params, could not create game asset")
      $ mkGameAsset
          { assetType: ao.assetType
          , attributes: attrs
          , rarity
          , name: cip25Name
          , imageUrl: ao.imageUrl
          , description: ao.description
          , tokenName: tkName
          }

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

mintGameAsset :: RawAssetOption -> Rarity -> String -> Racers TransactionHash
mintGameAsset aoo r nonce = do
  (ga /\ tk) <- liftEffect $ generateAsset aoo nonce r

  gameAssetPolicy <- mkGameAssetPolicy aoo.assetType
  gameAssetSymbol <- lift $ liftContractM "Could not get game asset symbol" $
    scriptCurrencySymbol gameAssetPolicy

  (authTxi /\ authTxo) <- withContract
    (liftedM "Could not find auth admin/bot token in wallet")
    findAnyAuthUtxo

  let
    assetVal = Value.singleton gameAssetSymbol tk $ BigInt.fromInt 1

    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustMintValue assetVal <>
      Constraints.mustSpendPubKeyOutput authTxi

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy gameAssetPolicy <> Lookups.unspentOutputs
      (Map.singleton authTxi authTxo)

    metadata :: GameAssetNftMetadata
    metadata = wrap
      [ GameAssetNftMetadataEntry
          { asset: ga
          , assetClass: gameAssetSymbol /\ tk
          }
      ]

  lift do
    unbalancedTx <- liftedE $ mkUnbalancedTx lookups constraints
    unbalancedTxWithMetadata <- setTxMetadata unbalancedTx metadata
    balTx <- liftedE $ balanceTx unbalancedTxWithMetadata
    balSignedTx <- signTransaction balTx
    submit balSignedTx

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
  let
    buildRawAssetOption = build $ modify (Proxy :: Proxy "name") unCip25String
      <<< delete (Proxy :: Proxy "nitroAmount")

  (ga /\ tk) <- generateAsset (buildRawAssetOption assetOption) nonce rarity

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

mkGameAssetPolicy :: GameAssetType -> Racers MintingPolicy
mkGameAssetPolicy assetType = do
  rp <- asks _.params
  v2script <- lift $ liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope gameAssetPolicy
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData rp, toData assetType ]
  pure $ PlutusMintingPolicy $ appliedScript
