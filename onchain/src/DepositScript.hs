{-# LANGUAGE TemplateHaskell #-}

module DepositScript (script) where

import CommonTypes (AirdropAddressDatum (airdropAddress), RacersParams, Rarity, adminToken, botToken)
import Ledger (Address)
import Ledger.Value (assetClass, assetClassValue, flattenValue, geq, leq)
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
import PlutusTx.Prelude
import Utils (getInlineDatum, parseToken, valueToAddr, withTraceM)

data DepositValidatorParams = DepositValidatorParams
  { assetPolicySymbol :: CurrencySymbol
  , assetRequestPolicySymbol :: CurrencySymbol
  }
PlutusTx.unstableMakeIsData ''DepositValidatorParams

{-# INLINEABLE mkDepositValidator #-}
mkDepositValidator :: RacersParams -> DepositValidatorParams -> ScriptContext -> Bool
mkDepositValidator rp dps ctx =
  ( traceIfFalse "admin token not present" inputContainsAdminNft
      || traceIfFalse "bot token not present" inputContainsBotNft
  )
    && traceIfFalse "not all asset nfts due are paid to airdrop address" mintsAndPaysAssetNfts
    && traceIfFalse "all input request tokens aro not burnt" burnsInputRequestTokens
  where
    info :: TxInfo
    !info = scriptContextTxInfo ctx

    spentValue :: Value
    !spentValue = valueSpent info

    -- Filter out inputs that have airdrop address inline datum and get their
    -- locked request tokens parsed
    -- This is to ensure that all inputs that have an airdrop address receive
    -- their corresponding minted AssetNfts
    inputsWithAirdropAddr :: [(Address, [(Rarity, Integer)])]
    inputsWithAirdropAddr =
      mapMaybe
        ( ( \txo ->
              (,) <$> (airdropAddress <$> getInlineDatum txo) <*> getRequestEntriesGrouped (txOutValue txo)
          )
            . txInInfoResolved
        )
        . txInfoInputs
        $ info

    -- Checks that all request tokens are burnt
    burnsInputRequestTokens :: Bool
    burnsInputRequestTokens = assetRequestValueMint `leq` negate combinedRequestValue
      where
        assetRequestValueMint :: Value
        assetRequestValueMint = 
          foldMap (\(cs, tk, i) -> assetClassValue (assetClass cs tk) i) 
          $ filter (\(cs, _, _) -> cs == assetRequestPolicySymbol dps) 
          $ flattenValue 
          $ txInfoMint info
        combinedRequestValue :: Value
        combinedRequestValue =
          foldMap (\(cs, tk, i) -> assetClassValue (assetClass cs tk) i)
            . filter (\(cs, _, _) -> cs == assetRequestPolicySymbol dps)
            . flattenValue
            $ spentValue


    -- Checks that request tokens are fulfilled with AssetNfts
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
      fmap groupByAssetRarity
        . traverse (\(_, tk, i) -> withTraceM "could not parse token name" $ (,i) <$> parseToken tk)
        . filter (\(cs, _, _) -> cs == assetRequestPolicySymbol dps)
        . flattenValue

    groupByAssetRarity :: [(Rarity, Integer)] -> [(Rarity, Integer)]
    groupByAssetRarity [] = []
    groupByAssetRarity ((r, i) : xs) = (r, i + sum (map snd sames)) : groupByAssetRarity rest
      where
        (sames, rest) = partition ((== r) . fst) xs

    inputContainsAdminNft :: Bool
    inputContainsAdminNft = spentValue `geq` assetClassValue (adminToken rp) 1

    inputContainsBotNft :: Bool
    inputContainsBotNft = spentValue `geq` assetClassValue (botToken rp) 1

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
script = fromCompiledCode $$(PlutusTx.compile [||mkValidator||])
