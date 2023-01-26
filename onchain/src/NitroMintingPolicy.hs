{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

module NitroMintingPolicy (script) where

import PlutusTx.Prelude

import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash))
import Plutus.V2.Ledger.Api (Script, ScriptContext (scriptContextTxInfo), fromCompiledCode, getPubKeyHash)
import Plutus.V2.Ledger.Contexts (txSignedBy)
import PlutusTx (unsafeFromBuiltinData)
import PlutusTx qualified (compile)

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _datum _redeemer ctx =
    traceIfFalse errMessage True
  where
    errMessage = "Failed verification"

{-# INLINEABLE mkValidator' #-}
mkValidator' :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator' datum redeemer context =
    let
        result =
            mkValidator
                (unsafeFromBuiltinData datum)
                (unsafeFromBuiltinData redeemer)
                (unsafeFromBuiltinData context)
     in
        if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $$(PlutusTx.compile [||mkValidator'||])
