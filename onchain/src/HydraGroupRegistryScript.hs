{-# LANGUAGE TemplateHaskell #-}

module HydraGroupRegistryScript (script) where

import Constants (hydraGroupTokenName)
import GHC.Generics (Generic)
import GHC.Show (Show)
import HydraGroupPolicy (HydraGroupInfo (hydraGroupUniqueId))
import Plutonomy qualified (aggressiveOptimizerOptions, optimizeUPLCWith)
import Plutus.V1.Ledger.Value qualified as Value (singleton)
import Plutus.V1.Ledger.Value (geq, valueOf)
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoMint),
  TxOut (txOutValue),
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (findOwnInput)
import PlutusTx qualified (compile, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.Prelude hiding (toList)

data HydraGroupRegistryRedeemer
  = DisbandGroup
  deriving (Generic, Show)
PlutusTx.unstableMakeIsData ''HydraGroupRegistryRedeemer

{-# INLINEABLE mkHydraGroupRegistryScript #-}
mkHydraGroupRegistryScript ::
  HydraGroupInfo ->
  HydraGroupRegistryRedeemer ->
  ScriptContext ->
  Bool 
mkHydraGroupRegistryScript groupInfo red ctx =
  case red of
    DisbandGroup ->
      traceIfFalse "registry output missing auth token" registryOutputContainsAuthToken &&
        traceIfFalse "auth token not burned" txBurnsAuthToken 
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx 

    ownInput :: TxOut
    ownInput =
      maybe (traceError "could not get own input") txInInfoResolved $
        findOwnInput ctx

    groupId :: CurrencySymbol
    groupId = hydraGroupUniqueId groupInfo

    registryOutputContainsAuthToken :: Bool
    registryOutputContainsAuthToken =
      txOutValue ownInput `geq` Value.singleton groupId hydraGroupTokenName 1

    txBurnsAuthToken :: Bool
    txBurnsAuthToken = valueOf (txInfoMint txInfo) groupId hydraGroupTokenName == (-1)

{-# INLINEABLE mkScript #-}
mkScript :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkScript dat red ctx =
  let
    result =
      mkHydraGroupRegistryScript
        (PlutusTx.unsafeFromBuiltinData dat)
        (PlutusTx.unsafeFromBuiltinData red)
        (PlutusTx.unsafeFromBuiltinData ctx)
   in
    if result then () else traceError "Failed verification"

script :: Script
script =
  fromCompiledCode $
    Plutonomy.optimizeUPLCWith Plutonomy.aggressiveOptimizerOptions
      $$(PlutusTx.compile [||mkScript||])
