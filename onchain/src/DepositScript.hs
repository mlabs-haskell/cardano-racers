{-# LANGUAGE TemplateHaskell #-}

-- {-# OPTIONS_GHC -w #-}

module DepositScript (script) where

import Ledger (AssetClass)
import Ledger.Value (assetClassValue, geq)
import Plutus.V2.Ledger.Api (
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInfo,
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (valueSpent)
import PlutusTx qualified (compile, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.Prelude

data DepositScriptParams = DepositScriptParams
  { adminToken :: AssetClass
  , botToken :: AssetClass
  }
PlutusTx.unstableMakeIsData ''DepositScriptParams

{-# INLINEABLE mkDepositValidator #-}
mkDepositValidator :: DepositScriptParams -> ScriptContext -> Bool
mkDepositValidator dsp ctx =
  traceIfFalse "admin token not present" inputContainsAdminNft
    || traceIfFalse "bot token not present" inputContainsBotNft
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    inputContainsAdminNft :: Bool
    inputContainsAdminNft = valueSpent info `geq` assetClassValue (adminToken dsp) 1

    inputContainsBotNft :: Bool
    inputContainsBotNft = valueSpent info `geq` assetClassValue (botToken dsp) 1

{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator dsp _datum _redeemer context =
  let
    result =
      mkDepositValidator
        (PlutusTx.unsafeFromBuiltinData dsp)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $$(PlutusTx.compile [||mkValidator||])
