{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}

-- | A script where users lock their deposits to request game assets.
-- The minting bot processes these deposits and airdrops the NFTs.
module DepositScript (script) where

import CommonTypes (AirdropAddressDatum (airdropAddress), RacersParams, Rarity, adminToken, botToken)
import Ledger (Address)
import Ledger.Value (assetClass, assetClassValue, flattenValue, geq, leq)
import Plutonomy qualified (optimizeUPLC)
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoInputs, txInfoMint),
  TxOut (txOutValue),
  Value,
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (valueSpent)
import PlutusTx qualified (compile, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as AssocMap (empty, singleton, toList, unionWith)
import PlutusTx.Prelude
import Utils (getInlineDatumFromTxOut, parseToken, valueToAddr, withTraceM)

data DepositValidatorParams = DepositValidatorParams
  { assetPolicySymbol :: CurrencySymbol
  , assetRequestPolicySymbol :: CurrencySymbol
  }

PlutusTx.unstableMakeIsData ''DepositValidatorParams

{-# INLINEABLE mkDepositValidator #-}
mkDepositValidator :: RacersParams -> DepositValidatorParams -> ScriptContext -> Bool
mkDepositValidator rp dps ctx =
  ( -- traceIfFalse "admin token not present"
    inputContainsAdminNft
      || traceIfFalse "bot token not present" inputContainsBotNft
  )
    && traceIfFalse "not all asset nfts due are paid to airdrop address" mintsAndPaysAssetNfts
    && traceIfFalse "all input request tokens aro not burnt" burnsInputRequestTokens
  where
    info :: TxInfo
    !info = scriptContextTxInfo ctx

    spentValue :: Value
    !spentValue = valueSpent info

    inputContainsAdminNft :: Bool
    inputContainsAdminNft = spentValue `geq` assetClassValue (adminToken rp) 1

    inputContainsBotNft :: Bool
    inputContainsBotNft = spentValue `geq` assetClassValue (botToken rp) 1

    -- Filter out inputs that have airdrop address inline datum and get their
    -- locked request tokens parsed
    inputsWithAirdropAddr :: [(Address, [(Rarity, Integer)])]
    inputsWithAirdropAddr =
      mapMaybe
        ( ( \txo ->
              (,) <$> (airdropAddress <$> getInlineDatumFromTxOut txo) <*> getRequestEntriesGrouped (txOutValue txo)
          )
            . txInInfoResolved
        )
        . txInfoInputs
        $ info

    -- Checks that all request tokens are burnt
    burnsInputRequestTokens :: Bool
    burnsInputRequestTokens = assetRequestValueMint `leq` negate combinedRequestValue
      where
        -- Request tokens minted (burned)
        assetRequestValueMint :: Value
        assetRequestValueMint =
          foldMap (\(cs, tk, i) -> assetClassValue (assetClass cs tk) i) $
            filter (\(cs, _, _) -> cs == assetRequestPolicySymbol dps) $
              flattenValue $
                txInfoMint info
        -- Request tokens in inputs
        combinedRequestValue :: Value
        combinedRequestValue =
          foldMap (\(cs, tk, i) -> assetClassValue (assetClass cs tk) i)
            . filter (\(cs, _, _) -> cs == assetRequestPolicySymbol dps)
            . flattenValue
            $ spentValue

    -- Checks that request tokens are fulfilled with Game Assets
    mintsAndPaysAssetNfts :: Bool
    mintsAndPaysAssetNfts =
      maybe False and $
        for
          inputsWithAirdropAddr
          ( \(addr, assetsDue) -> do
              vToAddr <- valueToAddr info addr
              let
                actualAssetsPaid = sum $ map (\(_, _, i) -> i) $ filter (\(cs, _, _) -> cs == assetPolicySymbol dps) $ flattenValue vToAddr
                expectedAssetsPaid = sum $ map snd assetsDue
              -- expectedAssetsPaid = foldl (<>) mempty $ map (\(_, _, i) -> assetClassValue (gameAssetClass ga r) i) assetsDue
              pure $ actualAssetsPaid >= expectedAssetsPaid
          )

    getRequestEntriesGrouped :: Value -> Maybe [(Rarity, Integer)]
    getRequestEntriesGrouped =
      fmap (AssocMap.toList . toAssetRarityMap)
        . traverse (\(_, tk, i) -> withTraceM "could not parse token name" $ (,i) <$> parseToken tk)
        . filter (\(cs, _, _) -> cs == assetRequestPolicySymbol dps)
        . flattenValue

    -- helper to group like rarity class requests into single entry with
    -- corresponding counts
    toAssetRarityMap :: [(Rarity, Integer)] -> Map Rarity Integer
    toAssetRarityMap = foldr (\(r, i) acc -> AssocMap.singleton r i `unionWithPlus` acc) AssocMap.empty
      where
        unionWithPlus = AssocMap.unionWith (+)

{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator rp dps _datum _redeemer context =
  let
    result =
      mkDepositValidator
        (PlutusTx.unsafeFromBuiltinData rp)
        (PlutusTx.unsafeFromBuiltinData dps)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $ Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkValidator||])
