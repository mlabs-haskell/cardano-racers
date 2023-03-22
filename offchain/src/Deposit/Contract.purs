module CardanoRacers.Deposit.Contract
  ( queryRequestsWithAirdropAddress
  , createDepositReferenceScriptOutput
  , consumeAndRedeemRequests
  , mkDepositValidator
  ) where

import Contract.Prelude

import CardanoRacers.AssetRequest.Contract (mkAssetRequestPolicy)
import CardanoRacers.AssetRequest.Types (AirdropAddressDatum)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Deposit.Types (DepositValidatorParams)
import CardanoRacers.GameAsset.Contract
  ( AssetOption
  , generateAsset
  , mintAvailableAssetByRarity
  , mkGameAssetPolicy
  )
import CardanoRacers.GameAsset.Types
  ( GameAssetNftMetadata
  , GameAssetType(CarType, DriverType)
  , Rarity(Common, Rare, Epic)
  )
import CardanoRacers.RacersState.Types (RacersState(..))
import CardanoRacers.ScriptsFFI (depositScript)
import Contract.Address (Address, scriptHashAddress)
import Contract.AuxiliaryData (setTxMetadata)
import Contract.CborBytes (cborBytesToByteArray)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData
  ( OutputDatum(..)
  , PlutusData
  , fromData
  , toData
  , unitDatum
  , unitRedeemer
  )
import Contract.Prim.ByteArray
  ( byteArrayFromAscii
  , byteArrayToIntArray
  , byteLength
  )
import Contract.ScriptLookups (mkUnbalancedTx)
import Contract.ScriptLookups as Lookup
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( Validator(..)
  , ValidatorHash(..)
  , applyArgs
  , validatorHash
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( ScriptRef(..)
  , TransactionHash
  , TransactionInput(..)
  , TransactionOutputWithRefScript
  , awaitTxConfirmed
  , balanceTx
  , mkTxUnspentOut
  , signTransaction
  , submit
  , submitTxFromConstraints
  )
import Contract.TxConstraints (DatumPresence(..), InputWithScriptRef(..))
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo, getWalletUtxos, utxosAt)
import Contract.Value (TokenName, Value)
import Contract.Value
  ( flattenValue
  , geq
  , getTokenName
  , lovelaceValueOf
  , mkTokenName
  , negation
  , scriptCurrencySymbol
  , singleton
  ) as Value
import Control.Apply (lift2)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Contract.QueryHandle (getQueryHandle)
import Ctl.Internal.Plutus.Conversion (toPlutusTxOutputWithRefScript)
import Ctl.Internal.Serialization (convertTransaction, toBytes)
import Data.Array (catMaybes, concatMap)
import Data.Array (elem, filter, find) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, toInt) as BigInt
import Data.Char (fromCharCode)
import Data.List.Lazy (replicateM)
import Data.List.Lazy as List
import Data.Map (Map)
import Data.Map (fromFoldable, singleton, toUnfoldable) as Map
import Data.Profunctor.Choice (left)
import Data.String.CodeUnits (fromCharArray)
import Effect.Exception (error)

type PendingAssetRequest =
  { airdropAddress :: Address
  , requestedAssets :: Array (Rarity /\ BigInt)
  }

queryRequestsWithAirdropAddress
  :: RacersParams
  -> RacersState
  -> Contract (Map TransactionInput PendingAssetRequest)
queryRequestsWithAirdropAddress rp st = do
  assetRequestPolicy <- mkAssetRequestPolicy rp
  assetRequestSymmol <- liftContractM "Could not get currency symbol"
    $ Value.scriptCurrencySymbol
    $ assetRequestPolicy

  utxosAtDeposit <- utxosAt $ scriptHashAddress (unwrap st).depositScript
    Nothing

  let
    requestUtxos =
      Array.filter
        ( Array.elem assetRequestSymmol <<< map fst <<< Value.flattenValue
            <<< _.amount
            <<< unwrap
            <<< _.output
            <<< unwrap
            <<< snd
        )
        $ Map.toUnfoldable utxosAtDeposit

    (pendingRequests :: Array (TransactionInput /\ PendingAssetRequest)) =
      catMaybes $ requestUtxos <#> \(requestTxIn /\ requestTxOut) -> do
        let
          requestOutput = (unwrap requestTxOut).output
          parsedRequestedAssets = catMaybes
            $ map
                ( \(_ /\ tk /\ a) -> ado
                    r <- parseRequestToken tk
                    in r /\ a
                )
            $ Value.flattenValue (unwrap requestOutput).amount
        airdropAddress <- case (unwrap requestOutput).datum of
          OutputDatum d -> (_.airdropAddress <<< unwrap) <$>
            (fromData (unwrap d) :: Maybe AirdropAddressDatum)
          _ -> Nothing

        pure $ requestTxIn /\
          { airdropAddress, requestedAssets: parsedRequestedAssets }

  pure $ Map.fromFoldable pendingRequests
  where
  parseRequestToken :: TokenName -> Maybe Rarity
  parseRequestToken tk = do
    let
      tkBytes = Value.getTokenName tk
      ia = byteArrayToIntArray tkBytes
    tkStr <- fromCharArray <$> traverse fromCharCode ia
    case tkStr of
      "Common" -> pure Common
      "Rare" -> pure Rare
      "Common" -> pure Common
      _ -> Nothing

-- todo: this contract does not need to computer scripts by itself as its
-- essentially a hepler, deposit validator params should be passed in
createDepositReferenceScriptOutput :: RacersParams -> Contract TransactionInput
createDepositReferenceScriptOutput rp = do
  assetRequestMP <- mkAssetRequestPolicy rp
  assetRequestSymbol <-
    liftContractM "could not get currency symbol of asset request policy"
      $ Value.scriptCurrencySymbol assetRequestMP

  gameAssetMP <- mkGameAssetPolicy rp
  gameAssetSymbol <- liftContractM "Could not get currency symbol" $
    Value.scriptCurrencySymbol gameAssetMP

  depositValidator <- mkDepositValidator rp
    $ wrap
        { assetPolicySymbol: gameAssetSymbol
        , assetRequestPolicySymbol: assetRequestSymbol
        }

  let
    vhash :: ValidatorHash
    vhash = validatorHash depositValidator

    scriptRef :: ScriptRef
    scriptRef = PlutusScriptRef (unwrap depositValidator)

    constraints :: Constraints.TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToScriptWithScriptRef vhash unitDatum DatumWitness
        scriptRef
        (Value.lovelaceValueOf $ BigInt.fromInt 2_000_000)

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

  txHash <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txHash
  pure $ wrap
    { transactionId: txHash
    , index: zero
    }

-- todo: parametrize by array of queried request tokens, so its the tx is easily
-- split based on chunks of txs returned by query contract
consumeAndRedeemRequests
  :: RacersParams
  -> RacersState
  -> Maybe TransactionInput
  -> Contract TransactionHash
consumeAndRedeemRequests rp st mDepScriptRef = do

  assetRequestMP <- mkAssetRequestPolicy rp
  assetRequestSymbol <-
    liftContractM "could not get currency symbol of asset request policy"
      $ Value.scriptCurrencySymbol assetRequestMP

  gameAssetMP <- mkGameAssetPolicy rp
  gameAssetSymbol <- liftContractM "Could not get currency symbol" $
    Value.scriptCurrencySymbol gameAssetMP

  pendingRequests <- queryRequestsWithAirdropAddress rp st
  utxosAtDeposit <- utxosAt $ scriptHashAddress (unwrap st).depositScript
    Nothing

  (authTxi /\ authTxo) <- liftedM "could not find admin or bot utxo in wallet" $
    findOwnAuthUtxo rp

  logInfo' $ show pendingRequests

  let
    payAssetConstraintsAndMetadata
      :: Effect (Constraints.TxConstraints Void Void /\ GameAssetNftMetadata)
    payAssetConstraintsAndMetadata = do
      cs <- for (Map.toUnfoldable pendingRequests)
        \(_ /\ { airdropAddress, requestedAssets }) -> do
          cs' <- map join $ for requestedAssets $ \(rarity /\ count) -> do
            countInt <- liftMaybe (error "could not convert BigInt to Int") $
              BigInt.toInt count
            List.toUnfoldable <$> replicateM countInt
              ( mintAvailableAssetByRarity availableAssets airdropAddress
                  gameAssetSymbol
                  rarity
              )
          pure $ foldMap fst cs' /\ map snd cs'
      pure $ foldMap fst cs /\ wrap (concatMap snd cs)

    burnsRequestTokensM :: Maybe (Constraints.TxConstraints Void Void)
    burnsRequestTokensM = fold <$> for
      (Map.toUnfoldable pendingRequests :: Array _)
      \(_ /\ { requestedAssets }) ->
        fold <$> for requestedAssets \(rarity /\ count) ->
          let
            tokenNameStr = show rarity
            tkNameM = Value.mkTokenName <=< byteArrayFromAscii $ tokenNameStr
          in
            tkNameM <#> \tkName -> Constraints.mustMintValue
              (Value.negation $ Value.singleton assetRequestSymbol tkName count)

  mintsAndPays /\ allMetadata <- liftEffect payAssetConstraintsAndMetadata
  burnsRequestTokens <- liftContractM "could not create token name"
    burnsRequestTokensM
  logInfo' $ show burnsRequestTokens

  spendsRequestTokenHandle <- maybe
    (pure $ \txIn -> Constraints.mustSpendScriptOutput txIn unitRedeemer)
    ( \scriptRefIn -> do
        queryHandle <- getQueryHandle
        txo <- liftedM "could not get script ref from txin" $ liftedE $ liftAff
          $ queryHandle.getUtxoByOref scriptRefIn
        txoWithScriptRef <-
          liftContractM
            "could not convert TransactionOutput to TransactionOutputWithScriptRef"
            $ toPlutusTxOutputWithRefScript txo
        pure $ \txIn ->
          Constraints.mustSpendScriptOutputUsingScriptRef
            txIn
            unitRedeemer
            (RefInput $ mkTxUnspentOut scriptRefIn txoWithScriptRef)
    )
    mDepScriptRef

  depositScriptLookups <- maybe
    ( do
        depositValidator <- mkDepositValidator rp
          $ wrap
              { assetPolicySymbol: gameAssetSymbol
              , assetRequestPolicySymbol: assetRequestSymbol
              }
        pure $ Lookup.validator depositValidator
    )
    (const $ pure mempty)
    mDepScriptRef

  let
    spendsRequestTokens =
      foldMap
        (\(txIn /\ _) -> spendsRequestTokenHandle txIn)
        $ (Map.toUnfoldable pendingRequests :: Array _)

    constraints :: Constraints.TxConstraints Void Void
    constraints = spendsRequestTokens
      <> Constraints.mustSpendPubKeyOutput authTxi
      <> mintsAndPays
      <> burnsRequestTokens

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookup.unspentOutputs utxosAtDeposit
      <> Lookup.mintingPolicy gameAssetMP
      <> Lookup.mintingPolicy assetRequestMP
      <> Lookup.unspentOutputs (Map.singleton authTxi authTxo)
      <> depositScriptLookups

  unbalancedTx <- liftedE $ mkUnbalancedTx lookups constraints
  unbalancedTxWithMetadata <- setTxMetadata unbalancedTx allMetadata
  balancedTx <- liftedE $ balanceTx unbalancedTxWithMetadata
  balancedSignedTx <- signTransaction balancedTx
  tx <- liftEffect $ convertTransaction $ unwrap balancedSignedTx
  logInfo' $ show $ byteLength $ cborBytesToByteArray $ toBytes tx
  -- logInfo' $ show balancedSignedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId
  pure txId

-- txId <- submitTxFromConstraints lookups constraints
-- awaitTxConfirmed txId
-- pure txId

availableAssets :: Map Rarity AssetOption
availableAssets = Map.fromFoldable
  [ Common /\
      { name: "CommonCar"
      , assetType: CarType
      , imageUrl:
          "https://cdn.pixabay.com/photo/31/19/17/comic-2026591_1280.png"
      , description: "Cool car with lots of experience"
      }
  , Rare /\
      { name: "RareDriver"
      , assetType: DriverType
      , imageUrl:
          "https://cdn.pixabay.com/photo/31/19/17/comic-2026591_1280.png"
      , description: "Cool car with lots of experience"
      }
  , Epic /\
      { name: "EpicCar"
      , assetType: CarType
      , imageUrl:
          "https://cdn.pixabay.com/photo/31/19/17/comic-2026591_1280.png"
      , description: "Cool car with lots of experience"
      }
  ]

findOwnAuthUtxo
  :: RacersParams
  -> Contract (Maybe (TransactionInput /\ TransactionOutputWithRefScript))
findOwnAuthUtxo rp = do
  utxos <- liftedM "could not get wallet utxos" $ getWalletUtxos

  let
    adminValue :: Value
    adminValue = uncurry Value.singleton (unwrap rp).adminToken $ BigInt.fromInt
      1

    botValue :: Value
    botValue = uncurry Value.singleton (unwrap rp).botToken $ BigInt.fromInt 1
    mUtxo =
      Array.find
        ( \(_ /\ txo) -> lift2 (||) (_ `Value.geq` adminValue)
            (_ `Value.geq` botValue)
            (unwrap (unwrap txo).output).amount
        ) $ Map.toUnfoldable utxos
  pure mUtxo

mkDepositValidator
  :: RacersParams -> DepositValidatorParams -> Contract Validator
mkDepositValidator rp dp = do
  v2script <- liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope depositScript
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData rp, toData dp ]
  pure $ Validator $ appliedScript
