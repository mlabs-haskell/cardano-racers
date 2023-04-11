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
import CardanoRacers.Helpers (getTxoWithRefScrpt)
import CardanoRacers.Nitro.Contract
  ( mintNitroAndPayToAddressConstraints
  , paysNitroConstraints
  )
import CardanoRacers.RacersState.Contract
  ( createRacersRefScriptOutput
  , queryRacersRefScriptOutput
  , queryRacersState
  )
import CardanoRacers.RacersState.Types (RacersState)
import CardanoRacers.ScriptsFFI (depositScript)
import Common.ContractHelpers (findOwnAuthUtxo)
import Contract.Address (Address, scriptHashAddress)
import Contract.AuxiliaryData (setTxMetadata)
import Contract.CborBytes (cborBytesToByteArray)
import Contract.Log (logInfo, logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData
  ( OutputDatum(OutputDatum)
  , Redeemer(Redeemer)
  , fromData
  , toData
  , unitRedeemer
  )
import Contract.Prim.ByteArray
  ( byteArrayFromAscii
  , byteArrayToIntArray
  , byteLength
  )
import Contract.ScriptLookups (mkUnbalancedTx)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator(Validator), applyArgs, mintingPolicyHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
  , awaitTxConfirmed
  , balanceTx
  , mkTxUnspentOut
  , signTransaction
  , submit
  )
import Contract.TxConstraints (InputWithScriptRef(..))
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
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
import Ctl.Internal.Serialization (convertTransaction, toBytes)
import Data.Array (catMaybes, elem, filter) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, toInt) as BigInt
import Data.Char (fromCharCode)
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

  createRacersRefScriptOutput rp (unwrap depositValidator)

queryOrCreateDepositReferenceScript :: RacersParams -> Contract TransactionInput
queryOrCreateDepositReferenceScript rp = do
  (rs /\ _) <- queryRacersState rp
  mTxiTxo <- queryRacersRefScriptOutput rp (unwrap (unwrap rs).depositScript)
  case mTxiTxo of
    Nothing -> createDepositReferenceScriptOutput rp
    Just (txi /\ _) -> pure txi

redeemGameAsset
  :: RacersParams
  -> Map Rarity AssetOption
  -> Effect String
  -> Maybe (TransactionInput /\ TransactionOutputWithRefScript)
  -> Maybe (TransactionInput /\ TransactionOutputWithRefScript)
  -> Maybe (TransactionInput /\ TransactionOutputWithRefScript)
  -> (TransactionInput /\ PendingAssetRequest)
  -> Contract TransactionHash
redeemGameAsset
  rp
  availableAssets
  generateNonce
  mAssetRequestPolicyRef
  mGameAssetPolicyRef
  mDepositRef
  (requestTxi /\ { airdropAddress, requestTxo, requestedAssets }) = do
  -- todo: can be passed as param
  assetRequestMP <- mkAssetRequestPolicy rp
  assetRequestSymbol <-
    liftContractM "could not get currency symbol of asset request policy"
      $ Value.scriptCurrencySymbol assetRequestMP

  -- todo: can be passed as param
  gameAssetMP <- mkGameAssetPolicy rp
  gameAssetSymbol <- liftContractM "Could not get currency symbol" $
    Value.scriptCurrencySymbol gameAssetMP

  (authTxi /\ authTxo) <- liftedM "could not find admin or bot utxo in wallet" $
    findOwnAuthUtxo rp

  (mintsNitroAndPaysConstraint /\ mintsNitroAndPaysLookup) <- do
    cs <- for requestedAssets $ \(rarity /\ count) -> do
      assetOption <- liftContractM "could not find asset option" $ Map.lookup
        rarity
        availableAssets
      mintNitroAndPayToAddressConstraints rp (count * assetOption.nitroAmount)
        airdropAddress
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
          $ wrap
              { assetPolicySymbol: gameAssetSymbol
              , assetRequestPolicySymbol: assetRequestSymbol
              }
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
      <> mintsNitroAndPaysConstraint
      <> mintsAndPaysNft
      <> burnsRequestTokens

    assetRequestPolicyLookups = maybe (Lookups.mintingPolicy assetRequestMP)
      (const mempty)
      mAssetRequestPolicyRef
    gameAssetLookup = maybe (Lookups.mintingPolicy gameAssetMP) (const mempty)
      mGameAssetPolicyRef

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.unspentOutputs (Map.singleton requestTxi requestTxo)
      <> Lookups.unspentOutputs (Map.singleton authTxi authTxo)
      <> mintsNitroAndPaysLookup
      <> gameAssetLookup
      <> assetRequestPolicyLookups
      <> depositLookups

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
  -> Maybe (TransactionInput /\ TransactionOutputWithRefScript)
  -> Contract (Array TransactionHash)
consumeAndRedeemRequests rp availableAssets generateNonce st mDepositScriptRef =
  do
    assetRequestMP <- mkAssetRequestPolicy rp
    gameAssetMP <- mkGameAssetPolicy rp

    mAssetRequestPolicyRef <- queryRacersRefScriptOutput rp
      (unwrap $ mintingPolicyHash assetRequestMP)
    mAssetPolicyRef <- queryRacersRefScriptOutput rp
      (unwrap $ mintingPolicyHash gameAssetMP)

    logInfo' $ show mAssetRequestPolicyRef
    logInfo' $ show mAssetPolicyRef

    pendingRequests <- (Map.toUnfoldable :: _ -> Array _) <$>
      queryRequestsWithAirdropAddress rp st

    txIds <- traverse
      ( redeemGameAsset rp availableAssets generateNonce mAssetRequestPolicyRef
          mAssetPolicyRef
          mDepositScriptRef
      )
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
