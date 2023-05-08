{-# LANGUAGE TemplateHaskell #-}

-- | A generic NFT minting policy. These NFTs are used for state treads.
-- https://github.com/Plutonomicon/plutonomicon/blob/main/statethread.md#state-thread-tokens
-- For game NFTs (cars/drivers), see GameAssetPolicy
module NftPolicy (policy, script) where

import PlutusTx.Prelude

import Ledger (TokenName)
import Ledger qualified as Scripts
import Ledger.Value (flattenValue)
import Plutus.V2.Ledger.Api (CurrencySymbol, ScriptContext (scriptContextTxInfo), ToData (toBuiltinData), TxInfo, TxOutRef, txInInfoOutRef, txInfoInputs, txInfoMint)
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol)
import PlutusTx (unsafeFromBuiltinData)
import PlutusTx qualified (applyCode, compile, liftCode)

{-# INLINEABLE mkPolicy #-}
mkPolicy :: TxOutRef -> ScriptContext -> Bool
mkPolicy txoref ctx =
  traceIfFalse "parameter TxOutRef not consumed in inputs" hasUtxo
    && traceIfFalse "amount minted is not 1" mintedOne
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    cs :: CurrencySymbol
    cs = ownCurrencySymbol ctx

    hasUtxo :: Bool
    hasUtxo = elem txoref . map txInInfoOutRef $ txInfoInputs info

    mintedOne :: Bool
    mintedOne = case filter (\(mintedCs, _, _) -> mintedCs == cs) $ flattenValue (txInfoMint info) of
      [(_, _, amt)] -> amt == 1
      _ -> False

{-# INLINEABLE mkPolicy' #-}
mkPolicy' :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy' params _nonce _redeemer context =
  --               ^ the nonce is to enable a Tx to use a single TxOutRef to mint
  --               multiple unique NFTs
  let
    result =
      mkPolicy
        (unsafeFromBuiltinData params)
        (unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Scripts.Script
script = Scripts.fromCompiledCode $$(PlutusTx.compile [||mkPolicy'||])

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy params tk =
  Scripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [||mkPolicy'||])
      `PlutusTx.applyCode` PlutusTx.liftCode (toBuiltinData params)
      `PlutusTx.applyCode` PlutusTx.liftCode (toBuiltinData tk)
