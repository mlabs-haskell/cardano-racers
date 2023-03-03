{-# LANGUAGE TemplateHaskell #-}

module RacersStateScript (script) where

import PlutusTx.Prelude

import CommonTypes (RacersParams, RacersState, adminToken, stateToken)
import Ledger (Datum (getDatum))
import Ledger.Value (assetClassValue, geq)
import Plutus.V2.Ledger.Api (
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (scriptContextTxInfo),
  ToData (toBuiltinData),
  TxInfo,
  Value,
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (ownHash, scriptOutputsAt, valueSpent)
import PlutusTx qualified (compile, unsafeFromBuiltinData, unstableMakeIsData)

newtype RacersStateRedeemer = SetRacersState RacersState
PlutusTx.unstableMakeIsData ''RacersStateRedeemer

{-# INLINEABLE mkRacersStateValidator #-}
mkRacersStateValidator :: RacersParams -> RacersStateRedeemer -> ScriptContext -> Bool
mkRacersStateValidator nsp (SetRacersState ns) ctx =
  traceIfFalse "Admin token not present" inputContainsAdminNft
    && traceIfFalse "game state invalid: " (setsRacersStateTo ns)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    stateNftValue :: Value
    stateNftValue = assetClassValue (stateToken nsp) 1

    inputContainsAdminNft :: Bool
    inputContainsAdminNft = valueSpent info `geq` assetClassValue (adminToken nsp) 1

    outputsLockedByTheScript :: [(OutputDatum, Value)]
    outputsLockedByTheScript = scriptOutputsAt (ownHash ctx) info

    -- This will ensure that state is set to expected value and that stateNft
    -- is re-locked at the script
    setsRacersStateTo :: RacersState -> Bool
    setsRacersStateTo gs =
      case filter (\(_, val) -> val `geq` stateNftValue) outputsLockedByTheScript of
        [(OutputDatum odat, _)] ->
          traceIfFalse "game state is not equal to state provided by redeemer" $
            getDatum odat == toBuiltinData gs
        [(_, _)] -> traceError "game state datum must be inline"
        [] -> traceError "game state is not re-locked at the script"
        _ -> traceError "unexpected game state output"

{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator nsp _datum redeemer context =
  let
    result =
      mkRacersStateValidator
        (PlutusTx.unsafeFromBuiltinData nsp)
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $$(PlutusTx.compile [||mkValidator||])
