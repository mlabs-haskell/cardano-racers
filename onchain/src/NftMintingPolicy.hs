{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

module NftMintingPolicy (policy, script) where

import PlutusTx.Prelude

import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash), TokenName, TxId (TxId), TxOutRef (TxOutRef))
import Ledger qualified as Scripts
import Plutus.V1.Ledger.Value (flattenValue)
import Plutus.V2.Ledger.Api (CurrencySymbol (CurrencySymbol), Script, ScriptContext (scriptContextTxInfo), ToData (toBuiltinData), TokenName (TokenName), TxInfo, fromCompiledCode, getPubKeyHash, txInInfoOutRef, txInfoInputs, txInfoMint)
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol, txSignedBy)
import PlutusTx (unsafeFromBuiltinData)
import PlutusTx qualified (applyCode, compile, liftCode)

{-# INLINEABLE mkPolicy #-}
mkPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkPolicy txoref _red ctx =
    traceIfFalse badInput hasUtxo
        && traceIfFalse badAmount mintedOne
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    cs :: CurrencySymbol
    cs = ownCurrencySymbol ctx

    hasUtxo :: Bool
    hasUtxo = elem txoref . map txInInfoOutRef $ txInfoInputs info

    mintedOne :: Bool
    mintedOne = case filter (\(mintedCs, _, _) -> mintedCs == cs) $ flattenValue (txInfoMint info) of
        [(mintedCs, _, amt)] -> amt == 1
        _ -> False

    badInput = "parameter TxOutRef not consumed in inputs"
    badAmount = "amount minted is not 1 token of given token name and own currency symbol"

{-# INLINEABLE mkPolicy' #-}
mkPolicy' :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy' params redeemer context =
    let
        result =
            mkPolicy
                (unsafeFromBuiltinData params)
                (unsafeFromBuiltinData redeemer)
                (unsafeFromBuiltinData context)
     in
        if result then () else traceError "Failed verification"

script :: Scripts.Script
script = Scripts.fromCompiledCode $$(PlutusTx.compile [||mkPolicy'||])

policy :: TxOutRef -> Scripts.MintingPolicy
policy params =
    Scripts.mkMintingPolicyScript $
        $$(PlutusTx.compile [||mkPolicy'||])
            `PlutusTx.applyCode` PlutusTx.liftCode (toBuiltinData params)
