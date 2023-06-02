{-# LANGUAGE TemplateHaskell #-}

-- | A script where users lock their deposits to request game assets.
-- The minting bot processes these deposits and airdrops the NFTs.
module DepositScript (script) where

import CommonTypes (RacersParams, adminToken, botToken)
import Plutonomy qualified (optimizeUPLC)
import Plutus.V2.Ledger.Api (
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInfo,
  Value,
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (valueSpent)
import PlutusTx qualified (compile, unsafeFromBuiltinData)
import PlutusTx.Prelude
import Ledger.Value (assetClassValue, geq)

{-# INLINEABLE mkDepositValidator #-}
mkDepositValidator :: RacersParams -> ScriptContext -> Bool
mkDepositValidator rp ctx =
  traceIfFalse "admin token not present" inputContainsAdminNft
  || traceIfFalse "bot token not present" inputContainsBotNft
  where
    info :: TxInfo
    !info = scriptContextTxInfo ctx

    spentValue :: Value
    !spentValue = valueSpent info

    inputContainsAdminNft :: Bool
    inputContainsAdminNft = spentValue `geq` assetClassValue (adminToken rp) 1

    inputContainsBotNft :: Bool
    inputContainsBotNft = spentValue `geq` assetClassValue (botToken rp) 1

{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator rp _datum _redeemer context =
  let
    result =
      mkDepositValidator
        (PlutusTx.unsafeFromBuiltinData rp)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $ Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkValidator||])
