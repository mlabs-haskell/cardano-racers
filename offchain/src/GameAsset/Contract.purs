module CardanoRacers.GameAsset.Contract
  ( mintAvailableAssetByRarity
  , mkGameAssetPolicy
  , generateAsset
  , mintGameAsset
  ) where

import Contract.Prelude

import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Plutus.Types.Address as Address
import Cardano.Plutus.Types.MintingPolicyHash (MintingPolicyHash)
import Cardano.Plutus.Types.TokenName (TokenName) as Plutus
import Cardano.Plutus.Types.TokenName (mkTokenName)
import Cardano.Types (TransactionOutput)
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.PlutusScript as PlutusScript
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
  , Rarity(Common, Rare, Epic)
  , mkGameAsset
  )
import CardanoRacers.Helpers (paysToAddrConstraint)
import CardanoRacers.ScriptsFFI (gameAssetPolicy)
import Common.ContractHelpers (findAnyAuthUtxo)
import Contract.Address (Address)
import Contract.AuxiliaryData (setTxMetadata)
import Contract.Monad (liftContractM, liftedM)
import Contract.PlutusData (toData)
import Contract.ScriptLookups (ScriptLookups, plutusMintingPolicy)
import Contract.ScriptLookups as Lookups
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , balanceTx
  , signTransaction
  , submit
  )
import Contract.TxConstraints (InputWithScriptRef(RefInput), TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe, throwError)
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.Map (singleton) as Map
import Data.Profunctor.Choice (left)
import Data.Profunctor.Strong (first)
import Data.TextEncoder (encodeUtf8)
import Effect.Exception (error)
import JS.BigInt (fromInt) as JSBigInt
import Partial.Unsafe (unsafePartial)
import Racers (Racers, withContract)
import Racers.Metadata.Cip25.Cip25String (mkCip25String, unCip25String)
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
generateAsset ao nonce requestedRarity = do
  ((attrs /\ rarity) :: (GameAssetAttributes /\ Rarity)) <- case ao.assetType of
    CarType -> first CarAttrs <$> generateNewCar requestedRarity
    DriverType -> first DriverAttrs <$> generateNewDriver requestedRarity

  cip25Name <- liftMaybe (error "could not create cip25 string from asset name")
    $ mkCip25String ao.name

  let
    nameByteArray = wrap $ encodeUtf8 $ unCip25String cip25Name <> ":" <> nonce

  (tkName :: Plutus.TokenName) <-
    liftMaybe (error "could not create token name")
      <$> mkTokenName
      $ nameByteArray
  let
    (assetName :: AssetName) = unwrap tkName

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

  pure (ga /\ assetName)

generateNewDriver :: Rarity -> Effect (DriverAttributes /\ Rarity)
generateNewDriver rarity = do
  seed <- randomSeed
  let ps = generateUniformParameters seed rarity
  (p1 /\ p2 /\ p3 /\ p4) <- case ps of
    [ p1, p2, p3, p4 ] -> pure (p1 /\ p2 /\ p3 /\ p4)
    _ -> throwError $ error
      "generateUniformParameters returned wrong number of parameters"

  let
    driver = DriverAttributes
      { aggression: JSBigInt.fromInt p1
      , experience: JSBigInt.fromInt p2
      , reflexes: JSBigInt.fromInt p3
      , luck: JSBigInt.fromInt p4
      }
  pure (driver /\ getNewRarity ps)

generateNewCar :: Rarity -> Effect (CarAttributes /\ Rarity)
generateNewCar rarity = do
  seed <- randomSeed
  let ps = generateUniformParameters seed rarity
  (p1 /\ p2 /\ p3 /\ p4) <- case ps of
    [ p1, p2, p3, p4 ] -> pure (p1 /\ p2 /\ p3 /\ p4)
    _ -> throwError $ error
      "generateUniformParameters returned wrong number of parameters"

  let
    car = CarAttributes
      { acceleration: JSBigInt.fromInt p1
      , cornering: JSBigInt.fromInt p2
      , topSpeed: JSBigInt.fromInt p3
      , aerodynamics: JSBigInt.fromInt p4
      }
  pure (car /\ getNewRarity ps)

getNewRarity :: Array Int -> Rarity
getNewRarity params =
  let
    totalSum = sum params
  in
    if totalSum > 20000 then Epic
    else if totalSum > 10000 then Rare
    else Common

mintGameAsset :: RawAssetOption -> Rarity -> String -> Racers TransactionHash
mintGameAsset aoo r nonce = do
  (ga /\ tk) <- liftEffect $ generateAsset aoo nonce r

  gameAssetPolicy <- mkGameAssetPolicy aoo.assetType
  gameAssetScriptHash <- lift
    $ liftContractM "Could not get game asset script hash"
    $ head
    $ map PlutusScript.hash
    $ (unwrap gameAssetPolicy).plutusMintingPolicies

  (authTxi /\ authTxo) <- withContract
    (liftedM "Could not find auth admin/bot token in wallet")
    findAnyAuthUtxo

  let
    assetVal :: Mint.Mint
    assetVal = Mint.singleton gameAssetScriptHash tk Int.one

    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustMintValue assetVal <>
      Constraints.mustSpendPubKeyOutput authTxi

    lookups :: Lookups.ScriptLookups
    lookups = gameAssetPolicy <> Lookups.unspentOutputs
      (Map.singleton authTxi authTxo)

    metadata :: GameAssetNftMetadata
    metadata = wrap
      [ GameAssetNftMetadataEntry
          { asset: ga
          , assetClass: gameAssetScriptHash /\ wrap tk
          }
      ]

  (unbalancedTx /\ usedUtxos) <- lift $ mkUnbalancedTx lookups constraints
  let
    unbalancedTxWithMetadata = setTxMetadata unbalancedTx metadata
  balTx <- lift $ balanceTx unbalancedTxWithMetadata usedUtxos mempty
  balSignedTx <- lift $ signTransaction balTx
  lift $ submit balSignedTx

mintAvailableAssetByRarity
  :: Maybe
       (MintingPolicyHash /\ TransactionInput /\ TransactionOutput)
  -> AssetOption
  -> CurrencySymbol
  -> String
  -> Address
  -> Rarity
  -> Effect (Constraints.TxConstraints /\ GameAssetNftMetadataEntry)
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

  (ga /\ tk) <- generateAsset
    (buildRawAssetOption assetOption)
    nonce
    rarity

  let
    assetMint = Mint.singleton assetSymbol tk Int.one
    assetVal = Value.singleton assetSymbol tk BigNum.one

    (assetMintConstraints :: TxConstraints) = maybe
      (Constraints.mustMintValue assetMint)
      ( \((mph :: MintingPolicyHash) /\ refTxi /\ (refTxo :: TransactionOutput)) ->
          Constraints.mustMintCurrencyUsingScriptRef
            (unwrap mph)
            tk
            Int.one
            (RefInput $ wrap { input: refTxi, output: refTxo })
      )
      mAssetPolicyRef

    addr = unsafePartial $ fromJust $
      Address.fromCardano
        targetAddress
    constraints = assetMintConstraints
      <> paysToAddrConstraint addr assetVal
    metadata = GameAssetNftMetadataEntry
      { asset: ga
      , assetClass: assetSymbol /\ wrap tk
      }

  pure $ constraints /\ metadata

mkGameAssetPolicy :: GameAssetType -> Racers ScriptLookups
mkGameAssetPolicy assetType = do
  rp <- asks _.params
  v2script <- lift $ liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope gameAssetPolicy
    plutusScriptFromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData rp, toData assetType ]
  pure $ plutusMintingPolicy $ appliedScript
