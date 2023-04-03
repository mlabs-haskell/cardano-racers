module CardanoRacers.Deposit.Contract
  ( queryRequestsWithAirdropAddress
  , createDepositReferenceScriptOutput
  , queryOrCreateDepositReferenceScript
  , consumeAndRedeemRequests
  , redeemGameAsset
  , mkDepositValidator
  ) where

import Contract.Prelude

import CardanoRacers.AssetRequest.Contract (mkAssetRequestPolicy)
import CardanoRacers.AssetRequest.Types
  ( AirdropAddressDatum
  , AssetRequestRedeemer(BurnRequestToken)
  )
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Deposit.Types (DepositValidatorParams)
import CardanoRacers.GameAsset.Contract
  ( mintAvailableAssetByRarity
  , mkGameAssetPolicy
  )
import CardanoRacers.GameAsset.Types
  ( AssetOption
  , GameAssetNftMetadata
  , Rarity(Epic, Rare, Common)
  )
import CardanoRacers.Helpers (paysToAddrConstraint)
import CardanoRacers.Nitro.Contract
  ( mintNitroAndPayToAddressConstraints
  , paysNitroConstraints
  )
import CardanoRacers.RacersState.Contract (queryRacersState)
import CardanoRacers.RacersState.Types (RacersState)
import CardanoRacers.ScriptsFFI (depositScript)
import Common.ContractHelpers (findOwnAuthUtxo)
import Contract.Address (Address, scriptHashAddress)
import Contract.AuxiliaryData (setTxMetadata)
import Contract.CborBytes (cborBytesToByteArray)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData
  ( OutputDatum(OutputDatum)
  , PlutusData
  , Redeemer(Redeemer)
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
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( Validator(Validator)
  , ValidatorHash
  , applyArgs
  , validatorHash
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( ScriptRef(PlutusScriptRef)
  , TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
  , awaitTxConfirmed
  , balanceTx
  , mkTxUnspentOut
  , signTransaction
  , submit
  , submitTxFromConstraints
  )
import Contract.TxConstraints
  ( DatumPresence(DatumWitness)
  , InputWithScriptRef(RefInput)
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (TokenName)
import Contract.Value
  ( flattenValue
  , getTokenName
  , lovelaceValueOf
  , mkTokenName
  , negation
  , scriptCurrencySymbol
  , singleton
  ) as Value
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Ctl.Internal.Plutus.Conversion (toPlutusTxOutputWithRefScript)
import Ctl.Internal.Serialization (convertTransaction, toBytes)
import Data.Array (catMaybes, elem, filter) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, toInt) as BigInt
import Data.Char (fromCharCode)
import Data.FoldableWithIndex (findWithIndex)
import Data.List.Lazy (replicateM)
import Data.List.Lazy as List
import Data.Map (Map)
import Data.Map (fromFoldable, lookup, singleton, toUnfoldable) as Map
import Data.Profunctor.Choice (left)
import Data.String.CodeUnits (fromCharArray)
import Effect.Exception (error)

type PendingAssetRequest =
  { airdropAddress :: Address
  , requestTxo :: TransactionOutputWithRefScript
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
      Array.catMaybes $ requestUtxos <#> \(requestTxIn /\ requestTxOut) -> do
        let
          requestOutput = (unwrap requestTxOut).output
          parsedRequestedAssets = Array.catMaybes
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
          { airdropAddress
          , requestTxo: requestTxOut
          , requestedAssets: parsedRequestedAssets
          }

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
      "Epic" -> pure Epic
      _ -> Nothing

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

queryOrCreateDepositReferenceScript :: RacersParams -> Contract TransactionInput
queryOrCreateDepositReferenceScript rp = do
  mTxi <- queryDepositReferenceScriptOutput rp
  case mTxi of
    Nothing -> createDepositReferenceScriptOutput rp
    Just txi -> pure txi

queryDepositReferenceScriptOutput
  :: RacersParams -> Contract (Maybe TransactionInput)
queryDepositReferenceScriptOutput rp = do
  (rs /\ _) <- queryRacersState rp
  utxosAtDeposit <- utxosAt $ scriptHashAddress (unwrap rs).depositScript
    Nothing
  pure $ _.index <$> findWithIndex
    ( \_ txo -> maybe false (_ == unwrap (unwrap rs).depositScript)
        (unwrap (unwrap txo).output).referenceScript
    )
    utxosAtDeposit

redeemGameAsset
  :: RacersParams
  -> Map Rarity AssetOption
  -> Effect String
  -> Maybe TransactionInput
  -> (TransactionInput /\ PendingAssetRequest)
  -> Contract TransactionHash
redeemGameAsset
  rp
  availableAssets
  generateNonce
  mDepScriptRef
  (requestTxi /\ { airdropAddress, requestTxo, requestedAssets }) = do
  assetRequestMP <- mkAssetRequestPolicy rp
  assetRequestSymbol <-
    liftContractM "could not get currency symbol of asset request policy"
      $ Value.scriptCurrencySymbol assetRequestMP

  gameAssetMP <- mkGameAssetPolicy rp
  gameAssetSymbol <- liftContractM "Could not get currency symbol" $
    Value.scriptCurrencySymbol gameAssetMP

  (authTxi /\ authTxo) <- liftedM "could not find admin or bot utxo in wallet" $
    findOwnAuthUtxo rp

  paysNitro <- do
    cs <- for requestedAssets $ \(rarity /\ count) -> do
      assetOption <- liftContractM "could not find asset option" $ Map.lookup
        rarity
        availableAssets
      paysNitroConstraints rp airdropAddress (count * assetOption.nitroAmount)
    pure $ fold cs

  let

    -- Create and collect constraints to to mint game assets with metadata
    payAssetConstraintsAndMetadata
      :: Effect (Constraints.TxConstraints Void Void /\ GameAssetNftMetadata)
    payAssetConstraintsAndMetadata = do
      constraintsAndMetadata <- map join $ for requestedAssets $
        \(rarity /\ count) -> do
          assetOption <- liftMaybe (error "could not find rarity entry in map")
            $ Map.lookup rarity availableAssets
          countInt <- liftMaybe (error "could not convert BigInt to Int") $
            BigInt.toInt count
          List.toUnfoldable <$> replicateM countInt
            ( do
                nonce <- generateNonce
                mintAvailableAssetByRarity assetOption gameAssetSymbol nonce
                  airdropAddress
                  rarity
            )
      pure $ foldMap fst constraintsAndMetadata /\ wrap
        (map snd constraintsAndMetadata)

    -- Constraints to ensure burning of request tokens
    burnsRequestTokensM :: Maybe (Constraints.TxConstraints Void Void)
    burnsRequestTokensM =
      fold <$> for requestedAssets \(rarity /\ count) ->
        let
          tokenNameStr = show rarity
          tkNameM = Value.mkTokenName <=< byteArrayFromAscii $ tokenNameStr
          red = Redeemer $ toData $ BurnRequestToken
        in
          tkNameM <#> \tkName -> Constraints.mustMintValueWithRedeemer red
            (Value.negation $ Value.singleton assetRequestSymbol tkName count)

  mintsAndPaysNft /\ allMetadata <- liftEffect payAssetConstraintsAndMetadata
  burnsRequestTokens <- liftContractM "could not create token name"
    burnsRequestTokensM

  -- Constraints to ensure that the deposit script outputs are spent,
  -- favors reference script if passed, otherwise defaults to the validator
  -- script
  spendsRequestToken <- maybe
    (pure $ Constraints.mustSpendScriptOutput requestTxi unitRedeemer)
    ( \scriptRefIn -> do
        -- Need to use internal functions here to get
        -- a TransactionOutputWithRefScript
        -- otherwise, getUtxo uses toPlutusTxOutput which drops the script ref
        -- and attaches a script ref hash
        queryHandle <- getQueryHandle
        txo <- liftedM "could not get script ref from txin" $ liftedE $ liftAff
          $ queryHandle.getUtxoByOref scriptRefIn
        txoWithScriptRef <-
          liftContractM
            "could not convert TransactionOutput to TransactionOutputWithScriptRef"
            $ toPlutusTxOutputWithRefScript txo

        pure $
          Constraints.mustSpendScriptOutputUsingScriptRef
            requestTxi
            unitRedeemer
            (RefInput $ mkTxUnspentOut scriptRefIn txoWithScriptRef)
    )
    mDepScriptRef

  -- Conditinal lookup for the deposit script in case of absense of reference script
  depositScriptLookups <- maybe
    ( do
        depositValidator <- mkDepositValidator rp
          $ wrap
              { assetPolicySymbol: gameAssetSymbol
              , assetRequestPolicySymbol: assetRequestSymbol
              }
        pure $ Lookups.validator depositValidator
    )
    (const $ pure mempty)
    mDepScriptRef

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = spendsRequestToken
      <> Constraints.mustSpendPubKeyOutput authTxi
      <> paysNitro
      <> mintsAndPaysNft
      <> burnsRequestTokens

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.unspentOutputs (Map.singleton requestTxi requestTxo)
      <> Lookups.unspentOutputs (Map.singleton authTxi authTxo)
      <> Lookups.mintingPolicy gameAssetMP
      <> Lookups.mintingPolicy assetRequestMP
      <> depositScriptLookups

  unbalancedTx <- liftedE $ mkUnbalancedTx lookups constraints
  unbalancedTxWithMetadata <- setTxMetadata unbalancedTx allMetadata
  balancedTx <- liftedE $ balanceTx unbalancedTxWithMetadata
  balancedSignedTx <- signTransaction balancedTx
  tx <- liftEffect $ convertTransaction $ unwrap balancedSignedTx
  logInfo' $ "Tx size: " <>
    (show $ byteLength $ cborBytesToByteArray $ toBytes tx)
  -- logInfo' $ show balancedSignedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId
  pure txId

consumeAndRedeemRequests
  :: RacersParams
  -> Map Rarity AssetOption
  -> Effect String
  -> RacersState
  -> Maybe TransactionInput
  -> Contract (Array TransactionHash)
consumeAndRedeemRequests rp availableAssets generateNonce st mDepScriptRef = do
  pendingRequests <- (Map.toUnfoldable :: _ -> Array _) <$>
    queryRequestsWithAirdropAddress rp st
  txIds <- traverse
    (redeemGameAsset rp availableAssets generateNonce mDepScriptRef)
    pendingRequests
  pure txIds

mkDepositValidator
  :: RacersParams -> DepositValidatorParams -> Contract Validator
mkDepositValidator rp dp = do
  v2script <- liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope depositScript
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData rp, toData dp ]
  pure $ Validator $ appliedScript
