module CardanoRacers.HydraGroup.Contract
  ( disbandHydraGroup
  , registerHydraGroup
  ) where

import Contract.Prelude

import Cardano.FromData (fromData)
import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.ToData (toData)
import Cardano.Types
  ( Ed25519KeyHash
  , PaymentPubKeyHash
  , PlutusData
  , PlutusScript
  , RedeemerDatum
  , ScriptHash
  , TransactionHash
  , TransactionInput
  , TransactionOutput
  )
import Cardano.Types.Int (negate, one) as CTInt
import Cardano.Types.OutputDatum (outputDatumDatum)
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Cardano.Types.RedeemerDatum (unit) as Redeemer
import Cardano.Types.Value (empty) as Value
import CardanoRacers.HydraGroup.Types
  ( HydraGroupInfo(HydraGroupInfo)
  , HydraGroupRegistryRedeemer(DisbandGroup)
  , hydraGroupTokenName
  )
import CardanoRacers.ScriptsFFI (hydraGroupPolicy, hydraGroupRegistryScript)
import Contract.Monad (Contract, liftedM)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (plutusMintingPolicy, unspentOutputs, validator) as Lookups
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints
  ( mustBeSignedBy
  , mustMintCurrencyWithRedeemer
  , mustPayToScript
  , mustSpendPubKeyOutput
  , mustSpendScriptOutput
  ) as Constraints
import Contract.Utxos (getUtxo)
import Contract.Wallet (getWalletUtxos, ownPaymentPubKeyHash)
import Control.Monad.Error.Class (liftMaybe, throwError)
import Data.Array (elem, head) as Array
import Data.Bifunctor (lmap)
import Data.Map (fromFoldable, singleton, toUnfoldable) as Map
import Effect.Exception (error)

registerHydraGroup
  :: Array Ed25519KeyHash
  -> Array String
  -> String
  -> Contract TransactionHash
registerHydraGroup masterKeys httpServers metadata = do
  ownPkh <- checkMasterKeys masterKeys

  utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
  nonceUtxo@(nonceOref /\ _) <-
    liftMaybe (error "Could not get first utxo (to be used as nonce)") $
      Array.head (Map.toUnfoldable utxos)

  validator <- mkHydraGroupRegistryValidator
  let validatorHash = PlutusScript.hash validator
  mintingPolicy <- mkHydraGroupPolicy validatorHash nonceOref

  let
    groupId :: ScriptHash
    groupId = PlutusScript.hash mintingPolicy

    groupInfo :: PlutusData
    groupInfo =
      toData $ HydraGroupInfo
        { hydraGroupUniqueId: groupId
        , hydraGroupNonceOref: nonceOref
        , hydraGroupMasterKeys: masterKeys
        , hydraGroupHttpServers: httpServers
        , hydraGroupMetadata: metadata
        }

    constraints :: TxConstraints
    constraints = mconcat
      [ Constraints.mustBeSignedBy ownPkh
      , Constraints.mustSpendPubKeyOutput nonceOref
      , Constraints.mustPayToScript validatorHash groupInfo DatumInline
          Value.empty
      , Constraints.mustMintCurrencyWithRedeemer groupId Redeemer.unit
          hydraGroupTokenName
          CTInt.one
      ]

    lookups :: ScriptLookups
    lookups = mconcat
      [ Lookups.plutusMintingPolicy mintingPolicy
      , Lookups.unspentOutputs $ Map.fromFoldable [ nonceUtxo ]
      ]

  txHash <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txHash
  pure txHash

disbandHydraGroup :: TransactionInput -> Contract TransactionHash
disbandHydraGroup registryOref = do
  registryOut <- liftedM "Could not get registry utxo" $ getUtxo registryOref
  HydraGroupInfo groupInfo <-
    liftMaybe (error "Could not decode HydraGroupInfo") $
      decodeHydraGroupInfoDatum registryOut

  validator <- mkHydraGroupRegistryValidator
  let validatorHash = PlutusScript.hash validator
  mintingPolicy <- mkHydraGroupPolicy validatorHash
    groupInfo.hydraGroupNonceOref
  let
    mintingPolicyHash = PlutusScript.hash mintingPolicy
    groupId = groupInfo.hydraGroupUniqueId

  when (groupId /= mintingPolicyHash) do
    throwError $ error $ "Hydra Group ID mismatch. Expected: "
      <> show groupId
      <> ", computed: "
      <> show mintingPolicyHash

  ownPkh <- checkMasterKeys groupInfo.hydraGroupMasterKeys

  let
    redeemer :: RedeemerDatum
    redeemer = wrap $ toData DisbandGroup

    constraints :: TxConstraints
    constraints = mconcat
      [ Constraints.mustBeSignedBy ownPkh
      , Constraints.mustSpendScriptOutput registryOref redeemer
      , Constraints.mustMintCurrencyWithRedeemer groupId Redeemer.unit
          hydraGroupTokenName
          (CTInt.negate CTInt.one)
      ]

    lookups :: ScriptLookups
    lookups = mconcat
      [ Lookups.validator validator
      , Lookups.plutusMintingPolicy mintingPolicy
      , Lookups.unspentOutputs $ Map.singleton registryOref registryOut
      ]

  txHash <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txHash
  pure txHash

checkMasterKeys :: Array Ed25519KeyHash -> Contract PaymentPubKeyHash
checkMasterKeys masterKeys = do
  ownPkh <- liftedM "Could not get own payment pkh" ownPaymentPubKeyHash
  unless (Array.elem (unwrap ownPkh) masterKeys) do
    throwError $ error "The provided master key list doesn't contain own pkh"
  pure ownPkh

decodeHydraGroupInfoDatum :: TransactionOutput -> Maybe HydraGroupInfo
decodeHydraGroupInfoDatum txOut =
  case (unwrap txOut).datum of
    Just datum ->
      case outputDatumDatum datum of
        Just inlineDatum -> fromData inlineDatum
        Nothing -> Nothing
    Nothing -> Nothing

mkHydraGroupPolicy :: ScriptHash -> TransactionInput -> Contract PlutusScript
mkHydraGroupPolicy registryValidatorHash nonceOref = do
  v2script <-
    liftMaybe (error "Could not decode HydraGroup minting policy") do
      envelope <- decodeTextEnvelope hydraGroupPolicy
      plutusScriptFromEnvelope envelope
  appliedScript <-
    liftEither $ lmap (error <<< show) $ applyArgs v2script
      [ toData registryValidatorHash
      , toData nonceOref
      ]
  pure appliedScript

mkHydraGroupRegistryValidator :: Contract PlutusScript
mkHydraGroupRegistryValidator =
  liftMaybe (error "Could not decode HydraGroupRegistry validator") do
    envelope <- decodeTextEnvelope hydraGroupRegistryScript
    plutusScriptFromEnvelope envelope
