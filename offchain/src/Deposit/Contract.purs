module CardanoRacers.Deposit.Contract
  ( queryRequestsWithAirdropAddress
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
import CardanoRacers.Deposit.Types
  ( DepositValidatorParams(DepositValidatorParams)
  )
import CardanoRacers.GameAsset.Contract
  ( mintAvailableAssetByRarity
  , mkGameAssetPolicy
  )
import CardanoRacers.GameAsset.Types
  ( AssetOption
  , GameAssetNftMetadata
  , Rarity(Epic, Rare, Common)
  )
import CardanoRacers.Nitro.Contract (paysNitroConstraints)
import CardanoRacers.RacersState.Contract (queryRacersRefScriptOutput)
import CardanoRacers.RacersState.Types (RacersState)
import CardanoRacers.ScriptsFFI (depositScript)
import Common.ContractHelpers (findAuthInUtxosMap, findOwnAuthUtxo)
import Contract.Address (Address, getNetworkId, scriptHashAddress)
import Contract.AuxiliaryData (setTxMetadata)
import Contract.BalanceTxConstraints as BalanceTxConstraints
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractE, liftContractM, liftedE, liftedM)
import Contract.Numeric.Natural (fromBigInt')
import Contract.PlutusData
  ( OutputDatum(OutputDatum)
  , Redeemer(Redeemer)
  , fromData
  , toData
  , unitRedeemer
  )
import Contract.Prim.ByteArray (byteArrayFromAscii, byteArrayToIntArray)
import Contract.ScriptLookups (UnbalancedTx, mkUnbalancedTx)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( Validator(Validator)
  , applyArgs
  , mintingPolicyHash
  , validatorHash
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( BalancedSignedTransaction
  , TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
  , awaitTxConfirmed
  , createAdditionalUtxos
  , mkTxUnspentOut
  , signTransaction
  , submit
  , withBalancedTx
  , withBalancedTxWithConstraints
  )
import Contract.TxConstraints (InputWithScriptRef(RefInput))
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoMap, utxosAt)
import Contract.Value (TokenName)
import Contract.Value
  ( flattenValue
  , getTokenName
  , mkTokenName
  , negation
  , scriptCurrencySymbol
  , singleton
  ) as Value
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Ctl.Internal.Plutus.Conversion (fromPlutusUtxoMap)
import Ctl.Internal.TxOutput
  ( transactionInputToTxOutRef
  , transactionOutputToOgmiosTxOut
  )
import Data.Array
  ( catMaybes
  , concat
  , cons
  , drop
  , elem
  , filter
  , fromFoldable
  , take
  , uncons
  ) as Array
import Data.Bifunctor (bimap)
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, toInt) as BigInt
import Data.Char (fromCharCode)
import Data.List.Lazy (replicateM)
import Data.List.Lazy as List
import Data.Map (Map)
import Data.Map (empty, fromFoldable, lookup, singleton, toUnfoldable, values) as Map
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

redeemGameAsset
  :: RacersParams
  -> Map Rarity AssetOption
  -> Effect String
  -> Maybe (TransactionInput /\ TransactionOutputWithRefScript)
  -> Maybe (TransactionInput /\ TransactionOutputWithRefScript)
  -> Maybe (TransactionInput /\ TransactionOutputWithRefScript)
  -> (TransactionInput /\ UtxoMap)
  -> (TransactionInput /\ PendingAssetRequest)
  -> Contract UnbalancedTx
redeemGameAsset
  rp
  availableAssets
  generateNonce
  mAssetRequestPolicyRef
  mGameAssetPolicyRef
  mDepositRef
  (authTxi /\ additionalUtxos)
  (requestTxi /\ { airdropAddress, requestTxo, requestedAssets }) = do
  assetRequestMP <- mkAssetRequestPolicy rp
  assetRequestSymbol <-
    liftContractM "could not get currency symbol of asset request policy"
      $ Value.scriptCurrencySymbol assetRequestMP

  gameAssetMP <- mkGameAssetPolicy rp
  gameAssetSymbol <- liftContractM "Could not get currency symbol" $
    Value.scriptCurrencySymbol gameAssetMP

  -- (authTxi /\ authTxo) <- liftedM "could not find admin or bot utxo in wallet" $
  --   findOwnAuthUtxo rp

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
                mintAvailableAssetByRarity
                  ((mintingPolicyHash gameAssetMP /\ _) <$> mGameAssetPolicyRef)
                  assetOption
                  gameAssetSymbol
                  nonce
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
          tkNameM <#> \tkName -> maybe
            ( Constraints.mustMintValueWithRedeemer red
                ( Value.negation $ Value.singleton assetRequestSymbol tkName
                    count
                )
            )
            ( \(refTxi /\ refTxo) ->
                Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
                  (mintingPolicyHash assetRequestMP)
                  red
                  tkName
                  ((BigInt.fromInt (-1)) * count)
                  (RefInput $ mkTxUnspentOut refTxi refTxo)
            )
            mAssetRequestPolicyRef

  mintsAndPaysNft /\ allMetadata <- liftEffect payAssetConstraintsAndMetadata

  burnsRequestTokens <- liftContractM "could not create token name"
    burnsRequestTokensM

  -- Constraints to ensure that the deposit script outputs are spent,
  -- favors reference script if passed, otherwise defaults to the validator
  -- script
  (depositConstraints /\ depositLookups) <- maybe
    ( do
        depositValidator <- mkDepositValidator rp
        pure $ Constraints.mustSpendScriptOutput requestTxi unitRedeemer
          /\ Lookups.validator depositValidator
    )
    ( \(refTxi /\ refTxo) ->
        pure
          $ Constraints.mustSpendScriptOutputUsingScriptRef
              requestTxi
              unitRedeemer
              (RefInput $ mkTxUnspentOut refTxi refTxo)
          /\ mempty
    )
    mDepositRef

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = depositConstraints
      <> Constraints.mustSpendPubKeyOutput authTxi
      <> paysNitro
      <> mintsAndPaysNft
      <> burnsRequestTokens

    assetRequestPolicyLookups = maybe (Lookups.mintingPolicy assetRequestMP)
      (const mempty)
      mAssetRequestPolicyRef
    gameAssetLookup = maybe (Lookups.mintingPolicy gameAssetMP) (const mempty)
      mGameAssetPolicyRef

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.unspentOutputs (Map.singleton requestTxi requestTxo)
      <> Lookups.unspentOutputs additionalUtxos -- (Map.singleton authTxi authTxo)
      <> gameAssetLookup
      <> assetRequestPolicyLookups
      <> depositLookups

  unbalancedTx <- liftedE $ mkUnbalancedTx lookups constraints
  unbalancedTxWithMetadata <- setTxMetadata unbalancedTx allMetadata
  pure unbalancedTxWithMetadata

-- balancedTx <- liftedE $ balanceTx unbalancedTxWithMetadata
-- balancedSignedTx <- signTransaction balancedTx
-- txId <- submit balancedSignedTx
-- awaitTxConfirmed txId
-- pure txId

consumeAndRedeemRequests
  :: RacersParams
  -> Map Rarity AssetOption
  -> Effect String
  -> RacersState
  -> Contract (Array TransactionHash)
consumeAndRedeemRequests rp availableAssets generateNonce st =
  do
    assetRequestMP <- mkAssetRequestPolicy rp
    gameAssetMP <- mkGameAssetPolicy rp

    depositValidator <- mkDepositValidator rp

    mAssetRequestPolicyRef <- queryRacersRefScriptOutput rp
      (unwrap $ mintingPolicyHash assetRequestMP)
    mAssetPolicyRef <- queryRacersRefScriptOutput rp
      (unwrap $ mintingPolicyHash gameAssetMP)
    mDepositScriptRef <- queryRacersRefScriptOutput rp
      (unwrap $ validatorHash depositValidator)

    pendingRequestsChunked <- chunk 4 <<< (Map.toUnfoldable :: _ -> Array _) <$>
      queryRequestsWithAirdropAddress rp st

    txIds <- traverse
      ( \reqs -> do
          (authTxi /\ authTxo) <- liftedM "could not find own auth utxo" $
            findOwnAuthUtxo rp
          txs <- chainRedeem
            ( redeemGameAsset rp availableAssets generateNonce
                mAssetRequestPolicyRef
                mAssetPolicyRef
                mDepositScriptRef
            )
            (authTxi /\ authTxo)
            reqs
          txIds <- traverse submit txs
          traverse_ awaitTxConfirmed txIds
          pure txIds
      )
      pendingRequestsChunked

    pure $ Array.concat txIds

  where
  chainRedeem
    :: ( (TransactionInput /\ UtxoMap)
         -> (TransactionInput /\ PendingAssetRequest)
         -> Contract UnbalancedTx
       )
    -> (TransactionInput /\ TransactionOutputWithRefScript)
    -> Array (TransactionInput /\ PendingAssetRequest)
    -> Contract (Array BalancedSignedTransaction)
  chainRedeem redeemTx (authTxi /\ authTxo) reqs = case Array.uncons reqs of
    Nothing -> pure []
    Just { head: req, tail: rest } -> do
      unbalancedTx <- redeemTx (authTxi /\ Map.singleton authTxi authTxo) req
      withBalancedTx unbalancedTx
        ( \balancedTx -> do
            balSignedTx <- signTransaction balancedTx
            calculateExUnits balSignedTx Map.empty
            additionalUtxos <- createAdditionalUtxos balSignedTx
            (nextAuthTxi /\ _) <-
              liftContractM "could not find auth utxo in prev tx outputs" $
                findAuthInUtxosMap rp additionalUtxos
            txs <- recursiveChain redeemTx (nextAuthTxi /\ additionalUtxos) rest
            pure $ Array.cons balSignedTx txs
        )

  recursiveChain
    :: ( (TransactionInput /\ UtxoMap)
         -> (TransactionInput /\ PendingAssetRequest)
         -> Contract UnbalancedTx
       )
    -> (TransactionInput /\ UtxoMap)
    -> Array (TransactionInput /\ PendingAssetRequest)
    -> Contract (Array BalancedSignedTransaction)
  recursiveChain redeemTx (authTxi /\ additionalUtxos) reqs =
    case Array.uncons reqs of
      Nothing -> pure []
      Just { head: req, tail: rest } -> do
        unbalancedTx <- redeemTx (authTxi /\ additionalUtxos) req
        let
          balanceTxConstraints
            :: BalanceTxConstraints.BalanceTxConstraintsBuilder
          balanceTxConstraints = BalanceTxConstraints.mustUseAdditionalUtxos
            additionalUtxos
        withBalancedTxWithConstraints unbalancedTx balanceTxConstraints
          ( \balancedTx -> do
              balSignedTx <- signTransaction balancedTx
              calculateExUnits balSignedTx additionalUtxos
              additionalUtxos' <- createAdditionalUtxos balSignedTx
              (nextAuthTxi /\ _) <-
                liftContractM "could not find auth utxo in prev tx outputs" $
                  findAuthInUtxosMap rp additionalUtxos'
              txs <- recursiveChain redeemTx (nextAuthTxi /\ additionalUtxos')
                rest
              pure $ Array.cons balSignedTx txs
          )

  chunk :: forall a. Int -> Array a -> Array (Array a)
  chunk _ [] = []
  chunk n xs = Array.take n xs `Array.cons` chunk n (Array.drop n xs)

-- todo: remove this once memory limit problems are solved
calculateExUnits :: BalancedSignedTransaction -> UtxoMap -> Contract Unit
calculateExUnits tx additionalUtxos = do
  queryHandle <- getQueryHandle
  netId <- getNetworkId
  let
    ogmiosAdditionalUtxos = wrap $ Map.fromFoldable
      ( bimap transactionInputToTxOutRef transactionOutputToOgmiosTxOut
          <$> (Map.toUnfoldable :: _ -> Array _)
            (fromPlutusUtxoMap netId additionalUtxos)
      )
  res <- liftAff $ queryHandle.evaluateTx (unwrap tx) ogmiosAdditionalUtxos
  let
    memorySum = res # unwrap >>> map
      (unwrap >>> Map.values >>> Array.fromFoldable >>> map _.memory >>> sum)
  liftContractE memorySum >>= \mem -> do
    logInfo' $ show mem

-- when (mem < fromBigInt' (BigInt.fromInt 8000000)) $ do
--   logInfo' $ show tx
-- when (mem > fromBigInt' (BigInt.fromInt 9200000)) $ do
--   logInfo' $ show tx

-- when (mem > BigInt.fromInt 10000000) $ do
--   logInfo' "memory limit exceeded"
--   logInfo' $ show tx
-- logInfo' $ show res

mkDepositValidator
  :: RacersParams -> Contract Validator
mkDepositValidator rp = do
  assetRequestMP <- mkAssetRequestPolicy rp
  assetRequestSymbol <-
    liftContractM "could not get currency symbol of asset request policy"
      $ Value.scriptCurrencySymbol assetRequestMP

  gameAssetMP <- mkGameAssetPolicy rp
  gameAssetSymbol <- liftContractM "Could not get currency symbol" $
    Value.scriptCurrencySymbol gameAssetMP

  let
    depositParams = DepositValidatorParams
      { assetPolicySymbol: gameAssetSymbol
      , assetRequestPolicySymbol: assetRequestSymbol
      }

  v2script <- liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope depositScript
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData rp, toData depositParams ]
  pure $ Validator $ appliedScript
