{-# LANGUAGE TemplateHaskell #-}

module GameAssetPolicy (script) where

import GHC.Generics (Generic)
import GHC.Show (Show)
import Ledger (AssetClass)
import Ledger.Value (assetClass, assetClassValue, geq, flattenValue)
import Plutus.V2.Ledger.Api (
  Address,
  Datum (getDatum),
  FromData (fromBuiltinData),
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoOutRef, txInInfoResolved),
  TxInfo (txInfoInputs, txInfoMint),
  TxOut (txOutDatum),
  TxOutRef,
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol, valueSpent)
import PlutusTx qualified (compile, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.Prelude
import Utils (valueToAddr)

data GameAssetPolicyParams = GameAssetPolicyParams
  { adminToken :: AssetClass
  , botToken :: AssetClass
  }
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''GameAssetPolicyParams

newtype GameAssetPolicyDatum = GameAssetPolicyDatum
  {airdropAddress :: Address}
PlutusTx.unstableMakeIsData ''GameAssetPolicyDatum

{-# INLINEABLE mkGameAssetPolicy #-}
mkGameAssetPolicy :: TxOutRef -> GameAssetPolicyParams -> ScriptContext -> Bool
mkGameAssetPolicy oref gapp ctx =
  ( traceIfFalse "input does not contain admin token" inputContainsAdminNft
      || traceIfFalse "input does not contain bot token" inputContainsBotNft
  )
    && traceIfFalse "does not spend parameter TxOutRef" (isJust paramTxo)
    && traceIfFalse "does not mint asset NFT" (isJust mintedNftAssetClass)
    && traceIfFalse "doesn't send nft to airdrop address" paysNftToAirdrop
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    paramTxo :: Maybe TxOut
    paramTxo = fmap txInInfoResolved $ find ((== oref) . txInInfoOutRef) $ txInfoInputs info

    inputContainsAdminNft :: Bool
    inputContainsAdminNft = valueSpent info `geq` assetClassValue (adminToken gapp) 1

    inputContainsBotNft :: Bool
    inputContainsBotNft = valueSpent info `geq` assetClassValue (botToken gapp) 1

    paysNftToAirdrop :: Bool
    paysNftToAirdrop = fromMaybe False $ do
      ptxo <- paramTxo
      gapd <- case txOutDatum ptxo of
        OutputDatum d ->
          maybe (trace "failed to decode game asset policy datum" Nothing) pure $
            fromBuiltinData @GameAssetPolicyDatum $
              getDatum d
        _ -> trace "failed to get txo inline datum containing airdrop address" Nothing
      v <-
        maybe (trace "failed to get value paid to airdrop address" Nothing) pure $
          valueToAddr info (airdropAddress gapd)
      nftAssetClass <- mintedNftAssetClass
      pure $ v `geq` assetClassValue nftAssetClass 1

    mintedNftAssetClass :: Maybe AssetClass
    mintedNftAssetClass = case filter (\(mintedCs, _, _) -> mintedCs == ownCurrencySymbol ctx) $ flattenValue (txInfoMint info) of
      [(mintedCs, mintedTokenName, amt)] | amt == 1 -> Just $ assetClass mintedCs mintedTokenName
      _ -> Nothing

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy oref gapp _redeemer context =
  let
    result =
      mkGameAssetPolicy
        (PlutusTx.unsafeFromBuiltinData oref)
        (PlutusTx.unsafeFromBuiltinData gapp)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $$(PlutusTx.compile [||mkPolicy||])
