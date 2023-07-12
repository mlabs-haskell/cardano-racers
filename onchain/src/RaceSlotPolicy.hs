{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module RaceSlotPolicy (script) where

import PlutusTx.Prelude

import CommonTypes (RacersParams (RacersParams, adminToken, botToken))
import Ledger.Value (assetClassValue, geq)
import Plutonomy qualified (aggressiveOptimizerOptions, optimizeUPLCWith)
import Plutus.V2.Ledger.Api (
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInfo,
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (valueSpent)
import PlutusTx qualified (compile, unsafeFromBuiltinData)

mkSlotPolicy :: RacersParams -> ScriptContext -> Bool
mkSlotPolicy RacersParams {adminToken, botToken} ctx = inputContainsBotNft || inputContainsAdminNft
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    inputContainsAdminNft :: Bool
    inputContainsAdminNft = valueSpent info `geq` assetClassValue adminToken 1

    inputContainsBotNft :: Bool
    inputContainsBotNft = valueSpent info `geq` assetClassValue botToken 1

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy racersParams _raceHash _redeemer context =
  let
    result =
      mkSlotPolicy
        (PlutusTx.unsafeFromBuiltinData racersParams)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $ Plutonomy.optimizeUPLCWith Plutonomy.aggressiveOptimizerOptions $$(PlutusTx.compile [||mkPolicy||])
