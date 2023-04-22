{-# LANGUAGE TemplateHaskell #-}

module SlotTokenPolicy (script) where

import PlutusTx.Prelude

import CommonTypes (RacersParams (adminToken, botToken))
import GHC.Generics (Generic)
import GHC.Show (Show)
import Ledger.Value (AssetClass, TokenName (TokenName), assetClass, assetClassValue, assetClassValueOf, geq)
import Plutonomy qualified (optimizeUPLC)
import Plutus.V2.Ledger.Api (
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInfo (txInfoInputs, txInfoMint),
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (TxInInfo (txInInfoOutRef), TxOutRef, ownCurrencySymbol, valueSpent)
import PlutusTx qualified (compile, unsafeFromBuiltinData, unstableMakeIsData)

data SlotTokenPolicyRedeemer
  = MintSlotToken Integer
  | BurnSlotToken Integer
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''SlotTokenPolicyRedeemer

{-# INLINEABLE mkSlotTokenPolicy #-}
mkSlotTokenPolicy :: RacersParams -> TxOutRef -> BuiltinByteString -> SlotTokenPolicyRedeemer -> ScriptContext -> Bool
mkSlotTokenPolicy rp txoref raceId red ctx = case red of
  MintSlotToken i ->
    ( traceIfFalse "admin token not present" inputContainsAdminNft
        || traceIfFalse "bot token not present" inputContainsBotNft
    )
      && traceIfFalse "UTxO not present in inputs" hasUtxo
      && traceIfFalse "wrong amount minted" mintsSlotTokens
    where
      hasUtxo :: Bool
      hasUtxo = elem txoref . map txInInfoOutRef $ txInfoInputs info

      inputContainsAdminNft :: Bool
      inputContainsAdminNft = valueSpent info `geq` assetClassValue (adminToken rp) 1

      inputContainsBotNft :: Bool
      inputContainsBotNft = valueSpent info `geq` assetClassValue (botToken rp) 1

      mintsSlotTokens :: Bool
      mintsSlotTokens = i == mintedSlotToken
  BurnSlotToken _ -> traceIfFalse "wrong amount minted" burnsSlotTokens
    where
      burnsSlotTokens :: Bool
      burnsSlotTokens = mintedSlotToken < 0
  where
    info :: TxInfo
    !info = scriptContextTxInfo ctx

    mintedSlotToken :: Integer
    !mintedSlotToken = assetClassValueOf (txInfoMint info) slotTokenAssetClass

    slotTokenAssetClass :: AssetClass
    !slotTokenAssetClass = assetClass (ownCurrencySymbol ctx) (TokenName raceId)

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy rp utxo raceHash redeemer context =
  let
    result =
      mkSlotTokenPolicy
        (PlutusTx.unsafeFromBuiltinData rp)
        (PlutusTx.unsafeFromBuiltinData utxo)
        (PlutusTx.unsafeFromBuiltinData raceHash)
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $ Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkPolicy||])
