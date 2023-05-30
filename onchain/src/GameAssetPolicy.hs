{-# LANGUAGE TemplateHaskell #-}

-- | A minting policy for game NFTs - only bot can mint them (to airdrop later).
-- Requires a bot token or an admin token.
module GameAssetPolicy (script) where

import PlutusTx.Prelude

import CommonTypes (GameAsset, RacersParams, adminToken, botToken)
import Ledger.Value (assetClassValue, geq)
import Plutonomy qualified (optimizeUPLC)
import Plutus.V2.Ledger.Api (
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInfo,
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (valueSpent)
import PlutusTx qualified (compile, unsafeFromBuiltinData)

{-# INLINEABLE mkGameAssetPolicy #-}
mkGameAssetPolicy :: RacersParams -> GameAsset -> ScriptContext -> Bool
mkGameAssetPolicy gapp _assetType ctx =
  traceIfFalse "admin token not present" inputContainsAdminNft
    || traceIfFalse "bot token not present" inputContainsBotNft
  where
    info :: TxInfo
    !info = scriptContextTxInfo ctx

    inputContainsAdminNft :: Bool
    inputContainsAdminNft = valueSpent info `geq` assetClassValue (adminToken gapp) 1

    inputContainsBotNft :: Bool
    inputContainsBotNft = valueSpent info `geq` assetClassValue (botToken gapp) 1

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy gapp assetType _redeemer context =
  let
    result =
      mkGameAssetPolicy
        (PlutusTx.unsafeFromBuiltinData gapp)
        (PlutusTx.unsafeFromBuiltinData assetType)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $ Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkPolicy||])
