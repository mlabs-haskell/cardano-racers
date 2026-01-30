{-# LANGUAGE TemplateHaskell #-}

module HydraGroupPolicy (
  HydraGroupInfo (..),
  HydraPeerServerInfo (..),
  script
 ) where

import Constants (hydraGroupTokenName)
import GHC.Generics (Generic)
import GHC.Show (Show)
import Plutonomy qualified (aggressiveOptimizerOptions, optimizeUPLCWith)
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Value qualified as Value (singleton)
import Plutus.V1.Ledger.Value (geq, valueOf)
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  PubKeyHash,
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoOutRef, txInInfoResolved),
  TxInfo (txInfoInputs, txInfoOutputs, txInfoMint),
  TxOut (txOutAddress, txOutValue),
  TxOutRef,
  ValidatorHash,
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol, txSignedBy)
import PlutusTx.Prelude
import PlutusTx qualified (compile, unsafeFromBuiltinData, unstableMakeIsData)
import Utils (getInlineDatumFromTxOut)

newtype HydraPeerServerInfo = HydraPeerServerInfo
  { httpServer :: BuiltinByteString
  }
  deriving (Generic, Show)
PlutusTx.unstableMakeIsData ''HydraPeerServerInfo

data HydraGroupInfo = HydraGroupInfo
  { hydraGroupUniqueId :: CurrencySymbol
  , hydraGroupNonceOref :: TxOutRef
  , hydraGroupMasterKeys :: [PubKeyHash]
  -- FIXME: `hydraGroupHttpServers :: [HydraPeerServerInfo]` causes
  -- "Reference to a name which is not a local, a builtin, or an external INLINABLE function"
  , hydraGroupHttpServers :: [BuiltinByteString]
  , hydraGroupApiVersion :: BuiltinByteString
  , hydraGroupMetadata :: BuiltinByteString
  }
  deriving (Generic, Show)
PlutusTx.unstableMakeIsData ''HydraGroupInfo

{-# INLINEABLE mkHydraGroupPolicy #-}
mkHydraGroupPolicy :: ValidatorHash -> TxOutRef -> ScriptContext -> Bool
mkHydraGroupPolicy registryValidatorHash nonceOref ctx = 
  case hydraGroupInfo of
    Nothing ->
      traceError "missing or invalid registry output"
    Just (isBurning, groupInfo) ->
      (isBurning || traceIfFalse "nonce utxo not spent" spendsNonceUtxo)
        && traceIfFalse "nonce oref mismatch" (hydraGroupNonceOref groupInfo == nonceOref)
        && traceIfFalse "group id mismatch" (hydraGroupUniqueId groupInfo == cs)
        && traceIfFalse "missing master signature" (txSignedByMasterKey groupInfo)
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    cs :: CurrencySymbol
    cs = ownCurrencySymbol ctx
    
    spendsNonceUtxo :: Bool
    spendsNonceUtxo = isJust $ find ((==) nonceOref . txInInfoOutRef) $ txInfoInputs txInfo 

    -- There must be exactly one output or input (depending on whether the state
    -- token is minted or burned, respectively) locked at the HydraGroupRegistry
    -- validator, containing the state token and a valid HydraGroupInfo inline
    -- datum.
    hydraGroupInfo :: Maybe (Bool, HydraGroupInfo)
    hydraGroupInfo
      | mintedQuantity == 1 =
          (False,) <$> getHydraGroupInfo (txInfoOutputs txInfo)
      | mintedQuantity == (-1) =
          (True,) <$> getHydraGroupInfo (txInInfoResolved <$> txInfoInputs txInfo)
      | otherwise =
          Nothing

    getHydraGroupInfo :: [TxOut] -> Maybe HydraGroupInfo
    getHydraGroupInfo utxos =
      let
        found =
          filter
            ( \out ->
                txOutAddress out == scriptHashAddress registryValidatorHash
                  && txOutValue out `geq` Value.singleton cs hydraGroupTokenName 1

            )
            utxos
       in
        case found of
          [ registryOut ] -> getInlineDatumFromTxOut registryOut 
          _ -> Nothing

    mintedQuantity :: Integer
    mintedQuantity = valueOf (txInfoMint txInfo) cs hydraGroupTokenName

    txSignedByMasterKey :: HydraGroupInfo -> Bool
    txSignedByMasterKey = any (txSignedBy txInfo) . hydraGroupMasterKeys 

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy registryValidatorHash nonceOref _red ctx =
  let
    result =
      mkHydraGroupPolicy
        (PlutusTx.unsafeFromBuiltinData registryValidatorHash)
        (PlutusTx.unsafeFromBuiltinData nonceOref)
        (PlutusTx.unsafeFromBuiltinData ctx)
   in
    if result then () else traceError "Failed verification"

script :: Script
script =
  fromCompiledCode $
    Plutonomy.optimizeUPLCWith Plutonomy.aggressiveOptimizerOptions
      $$(PlutusTx.compile [||mkPolicy||])
