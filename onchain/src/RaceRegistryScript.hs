{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module RaceRegistryScript (script) where

import CommonTypes (RacersParams (RacersParams, nitroToken))
import Plutonomy qualified (optimizeUPLC)
import Plutus.V1.Ledger.Value (assetClass, assetClassValue, assetClassValueOf, geq, mpsSymbol)
import Plutus.V2.Ledger.Api (
  CurrencySymbol (unCurrencySymbol),
  MintingPolicyHash,
  Script,
  ScriptContext,
  TokenName (..),
  TxInfo,
  Value,
  fromCompiledCode,
  scriptContextTxInfo,
  txInfoMint,
 )
import Plutus.V2.Ledger.Contexts (valueSpent)
import PlutusTx qualified (compile, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.Prelude

data RegistryParams = RegistryParams
  { raceHash :: BuiltinByteString
  , signerPubKey :: BuiltinByteString
  , slotPolicyHash :: MintingPolicyHash
  , nitroPolicyHash :: MintingPolicyHash
  , nitroFee :: Integer
  }
PlutusTx.unstableMakeIsData ''RegistryParams

data RegistryRedeemer = Register
  { carToken :: TokenName
  , driverToken :: TokenName
  , assetPolicyHash :: MintingPolicyHash
  , verifiableSignature :: BuiltinByteString
  }
PlutusTx.unstableMakeIsData ''RegistryRedeemer

{-# INLINEABLE mkRegistryScript #-}
mkRegistryScript :: RacersParams -> RegistryParams -> RegistryRedeemer -> ScriptContext -> Bool
mkRegistryScript
  RacersParams {nitroToken}
  RegistryParams {raceHash, signerPubKey, slotPolicyHash, nitroPolicyHash, nitroFee}
  Register {carToken, driverToken, verifiableSignature, assetPolicyHash}
  ctx =
    traceIfFalse "bad amount of nitro burnt" burnsNitroFee
      && traceIfFalse "slot token not burnt" burnsSlotToken
      && traceIfFalse "invalid registration signature" signatureIsValid
      && traceIfFalse "selected NFTs not present in inputs" nftsPresent
    where
      info :: TxInfo
      info = scriptContextTxInfo ctx

      assetSymbol :: CurrencySymbol
      !assetSymbol = mpsSymbol assetPolicyHash

      spentValue :: Value
      !spentValue = valueSpent info

      burnsNitroFee :: Bool
      burnsNitroFee =
        assetClassValueOf (txInfoMint info) (assetClass (mpsSymbol nitroPolicyHash) nitroToken) <= nitroFee

      burnsSlotToken :: Bool
      burnsSlotToken =
        assetClassValueOf (txInfoMint info) (assetClass (mpsSymbol slotPolicyHash) (TokenName raceHash)) == -1

      signatureIsValid :: Bool
      signatureIsValid = verifyEd25519Signature signerPubKey encodedMessage verifiableSignature
        where
          encodedMessage :: BuiltinByteString
          encodedMessage = raceHash <> unCurrencySymbol assetSymbol <> unTokenName carToken <> unTokenName driverToken

      nftsPresent :: Bool
      nftsPresent = inputContainsCarNft && inputContainsDriverNft
        where
          inputContainsCarNft = spentValue `geq` assetClassValue (assetClass assetSymbol carToken) 1
          inputContainsDriverNft = spentValue `geq` assetClassValue (assetClass assetSymbol driverToken) 1

{-# INLINEABLE mkScript #-}
mkScript :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkScript racersParams registryParams _dat red ctx =
  let
    result =
      mkRegistryScript
        (PlutusTx.unsafeFromBuiltinData racersParams)
        (PlutusTx.unsafeFromBuiltinData registryParams)
        (PlutusTx.unsafeFromBuiltinData red)
        (PlutusTx.unsafeFromBuiltinData ctx)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $ Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkScript||])
