{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}
module NftMintingPolicy (policy, script) where

import PlutusTx.Prelude

import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash), TokenName, TxOutRef)
import Plutus.V2.Ledger.Api (Script, ScriptContext (scriptContextTxInfo), fromCompiledCode, getPubKeyHash, ToData (toBuiltinData), TxInfo, txInfoInputs, txInInfoOutRef, txInfoMint, CurrencySymbol (CurrencySymbol))
import Plutus.V2.Ledger.Contexts (txSignedBy, ownCurrencySymbol)
import PlutusTx (unsafeFromBuiltinData)
import PlutusTx qualified (compile, applyCode, liftCode)
import qualified Ledger as Scripts
import Plutus.V1.Ledger.Value (flattenValue)

{-# INLINEABLE mkPolicy #-}
mkPolicy :: (TxOutRef,TokenName) -> () -> ScriptContext -> Bool
mkPolicy (txoref,tk) _red ctx = traceIfFalse badInput  hasUtxo   &&
                              traceIfFalse badAmount mintedOne
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    cs :: CurrencySymbol
    cs = ownCurrencySymbol ctx

    hasUtxo :: Bool
    hasUtxo = any ((== txoref) . txInInfoOutRef) $ txInfoInputs info

    mintedOne :: Bool
    mintedOne = case filter (\(cs',_,_) -> cs' == cs) $ flattenValue (txInfoMint info) of
      [(cs',tk',amt)] -> cs' == cs && tk' == tk && amt == 1
      _               -> False

    badInput = "parameter TxOutRef not consumed in inputs"
    badAmount = "amount minted is not 1 token of given token name and own currency symbol"

{-# INLINEABLE mkPolicy' #-} 
mkPolicy' :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy' params redeemer context =
  let
    result = mkPolicy
      (unsafeFromBuiltinData params)
      (unsafeFromBuiltinData redeemer)
      (unsafeFromBuiltinData context)
  in
    if result then () else traceError "Failed verification"


script :: Scripts.Script
script = Scripts.fromCompiledCode $$(PlutusTx.compile [|| mkPolicy' ||])

policy :: (TxOutRef, TokenName) -> Scripts.MintingPolicy
policy params = Scripts.mkMintingPolicyScript $ 
  $$(PlutusTx.compile [|| mkPolicy' ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode (toBuiltinData params)

