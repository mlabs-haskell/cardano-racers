{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

module NitroMintingPolicy (script) where

import PlutusTx.Prelude

import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash), CurrencySymbol)
import Plutus.V2.Ledger.Api (CurrencySymbol (CurrencySymbol), Script, ScriptContext (scriptContextTxInfo), ToData (toBuiltinData), TokenName (TokenName), TxInfo, fromCompiledCode, getPubKeyHash, txInInfoOutRef, txInfoInputs, txInfoMint)
import Plutus.V2.Ledger.Contexts (txSignedBy, valueSpent)
import PlutusTx (unsafeFromBuiltinData)
import PlutusTx qualified (compile)
import Ledger.Value (flattenValue)

{-# INLINEABLE mkPolicy #-}
mkPolicy :: CurrencySymbol -> () -> () -> ScriptContext -> Bool
mkPolicy cs _datum _redeemer ctx = traceIfFalse "Admin NFT not contained in inputs" inputContainsAdminToken
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    inputContainsAdminToken :: Bool
    inputContainsAdminToken = case filter (\(cs',_,_) -> cs' == cs) $ flattenValue (valueSpent info) of
      [(cs',_,amt)] -> amt == 1
      _ -> False

{-# INLINEABLE mkPolicy' #-}
mkPolicy' :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy' cs datum redeemer context =
    let
        result =
            mkPolicy
                (unsafeFromBuiltinData cs)
                (unsafeFromBuiltinData datum)
                (unsafeFromBuiltinData redeemer)
                (unsafeFromBuiltinData context)
     in
        if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $$(PlutusTx.compile [||mkPolicy'||])
