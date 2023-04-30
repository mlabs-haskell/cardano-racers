{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module RaceConfirmationScript (script) where

import PlutusTx.Prelude

import CommonTypes (RacersParams (RacersParams, adminToken, botToken))
import Ledger.Value (Value, assetClassValue, geq)
import Plutonomy qualified (optimizeUPLC)
import Plutus.V2.Ledger.Api (Script, ScriptContext (scriptContextTxInfo), TxInfo, fromCompiledCode)
import Plutus.V2.Ledger.Contexts (valueSpent)
import PlutusTx (compile, unsafeFromBuiltinData)

-- | The `RaceConfirmationScript` will hold Contender tokens, spending from
-- | this script is only permitted if the user is the admin or bot.
{-# INLINEABLE mkConfirmationScript #-}
mkConfirmationScript :: RacersParams -> ScriptContext -> Bool
mkConfirmationScript RacersParams {adminToken, botToken} ctx = inputContainsAdminNft || inputContainsBotNft
  where
    info :: TxInfo
    !info = scriptContextTxInfo ctx

    spentValue :: Value
    !spentValue = valueSpent info

    inputContainsAdminNft :: Bool
    inputContainsAdminNft = spentValue `geq` assetClassValue adminToken 1

    inputContainsBotNft :: Bool
    inputContainsBotNft = spentValue `geq` assetClassValue botToken 1

-- | The `RaceConfirmationScript` is parameterized by the `_raceHash`
-- | BuiltinByteString for unqiueness across multiple races.
{-# INLINEABLE mkScript #-}
mkScript :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkScript racersParams _raceHash _dat _red ctx =
  let
    result =
      mkConfirmationScript
        (PlutusTx.unsafeFromBuiltinData racersParams)
        (PlutusTx.unsafeFromBuiltinData ctx)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $ Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkScript||])
