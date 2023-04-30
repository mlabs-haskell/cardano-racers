{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-all #-}

module RaceConfirmationScript (script) where

import PlutusTx.Prelude

import Plutus.V2.Ledger.Api (Script, fromCompiledCode)
import PlutusTx (compile, unsafeFromBuiltinData)
import Plutonomy qualified (optimizeUPLC)

{-# INLINEABLE mkConfirmationScript #-}
mkConfirmationScript :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> Bool
mkConfirmationScript redeemer context _ _ = True
    -- Placeholder for logic implementation


{-# INLINEABLE mkScript #-}
mkScript :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkScript racersParams registryParams _dat red ctx =
  let
    result =
      mkConfirmationScript
        (PlutusTx.unsafeFromBuiltinData racersParams)
        (PlutusTx.unsafeFromBuiltinData registryParams)
        (PlutusTx.unsafeFromBuiltinData red)
        (PlutusTx.unsafeFromBuiltinData ctx)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $ Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkScript||])
