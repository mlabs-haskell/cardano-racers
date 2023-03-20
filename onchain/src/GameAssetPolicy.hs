{-# LANGUAGE TemplateHaskell #-}

module GameAssetPolicy (script) where

import CommonTypes (RacersParams, adminToken, botToken)
import Ledger.Value (assetClassValue, geq)
import Plutus.V2.Ledger.Api (
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInfo,
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (valueSpent)
import PlutusTx qualified (compile, unsafeFromBuiltinData)
import PlutusTx.Prelude

{-# INLINEABLE mkGameAssetPolicy #-}
mkGameAssetPolicy :: RacersParams -> ScriptContext -> Bool
mkGameAssetPolicy gapp ctx =
  traceIfFalse "a" inputContainsAdminNft
    || traceIfFalse "b" inputContainsBotNft
  where
    info :: TxInfo
    !info = scriptContextTxInfo ctx

    inputContainsAdminNft :: Bool
    inputContainsAdminNft = valueSpent info `geq` assetClassValue (adminToken gapp) 1

    inputContainsBotNft :: Bool
    inputContainsBotNft = valueSpent info `geq` assetClassValue (botToken gapp) 1

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy gapp _redeemer context =
  let
    result =
      mkGameAssetPolicy
        (PlutusTx.unsafeFromBuiltinData gapp)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $$(PlutusTx.compile [||mkPolicy||])
