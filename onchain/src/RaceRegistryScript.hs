{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module RaceRegistryScript (script) where

import CommonTypes (RacersParams (RacersParams), adminToken)
import Constants (nitroTokenName, slotTokenName)
import Ledger.Address (scriptHashAddress)
import Plutonomy qualified (optimizeUPLC)
import Plutus.V1.Ledger.Value (assetClass, assetClassValue, assetClassValueOf, flattenValue, geq, mpsSymbol)
import Plutus.V2.Ledger.Api (
  MintingPolicyHash,
  Script,
  ScriptContext,
  TokenName (unTokenName),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoInputs),
  TxOut (txOutAddress),
  Value,
  fromCompiledCode,
  scriptContextTxInfo,
  txInfoMint,
 )
import Plutus.V2.Ledger.Contexts (ownHash, valueLockedBy, valueSpent)
import Plutus.V2.Ledger.Tx (TxOut (txOutValue))
import PlutusTx qualified (compile, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.Prelude

data RegistryParams = RegistryParams
  { raceHash :: BuiltinByteString
  , nitroFee :: Integer
  , nitroPolicyHash :: MintingPolicyHash
  }
PlutusTx.unstableMakeIsData ''RegistryParams

-- | The `RaceRegistryScript` holds minted Slot tokens that can be purchased by
-- | users. It allows users to spend Slot tokens provided they burn the
-- | parameter `nitroFee` of NITRO tokens.
{-# INLINEABLE mkRegistryScript #-}
mkRegistryScript :: RacersParams -> RegistryParams -> ScriptContext -> Bool
mkRegistryScript
  RacersParams {adminToken}
  RegistryParams {nitroFee, nitroPolicyHash}
  ctx =
    inputContainsAdminNft
      || traceIfFalse "bad amount of nitro burnt" burnsNitroPerSlotPurchased -- to collect leftover min-ada UTxO's
    where
      info :: TxInfo
      info = scriptContextTxInfo ctx

      inputContainsAdminNft :: Bool
      inputContainsAdminNft = valueSpent info `geq` assetClassValue adminToken 1

      -- Checks that value minted is less than expected nitro burn fee
      burnsNitroPerSlotPurchased :: Bool
      burnsNitroPerSlotPurchased = assetClassValueOf (txInfoMint info) (assetClass (mpsSymbol nitroPolicyHash) nitroTokenName) <= totalNitro
        where
          totalNitro = nitroFee * slotTokensSpentFromScript

      -- Only checks TokenName of spent value since it is not possible to get
      -- access to the Slot currency symbol due to cyclic dependency. It is up
      -- to the user to ensure spending of the correct AssetClass. Enrollment
      -- policy will reject minting of Contender tokens if Slot tokens policy
      -- symbol doesn't match.
      slotTokensSpentFromScript :: Integer
      slotTokensSpentFromScript = case flattenValue valueSpentFromScript of
        [(_, tk, amt)] ->
          if tk == slotTokenName
            then amt
            else traceError $ "expected '" <> decodeUtf8 (unTokenName slotTokenName) <> "' token name"
        _ -> traceError $ "expected single entry '" <> decodeUtf8 (unTokenName slotTokenName) <> "' token name spent"

      valueSpentFromScript :: Value
      valueSpentFromScript = totalScriptInputValue - totalScriptOutputValue
        where
          ownAddress = scriptHashAddress (ownHash ctx)
          totalScriptInputValue =
            foldMap (txOutValue . txInInfoResolved) $
              filter ((==) ownAddress . txOutAddress . txInInfoResolved) $
                txInfoInputs info
          totalScriptOutputValue = valueLockedBy info (ownHash ctx)

{-# INLINEABLE mkScript #-}
mkScript :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkScript racersParams registryParams _dat _red ctx =
  let
    result =
      mkRegistryScript
        (PlutusTx.unsafeFromBuiltinData racersParams)
        (PlutusTx.unsafeFromBuiltinData registryParams)
        -- (PlutusTx.unsafeFromBuiltinData red)
        (PlutusTx.unsafeFromBuiltinData ctx)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $ Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkScript||])
