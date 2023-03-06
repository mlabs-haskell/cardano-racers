{-# LANGUAGE TemplateHaskell #-}

module DepositScript (script) where

import CommonTypes (GameAsset, RacersParams, Rarity, adminToken, botToken)
import Ledger (Address)
import Ledger.Value (AssetClass, assetClass, assetClassValue, flattenValue, geq, leq)
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
import Utils (gameAssetTokenName, getInlineDatum, parseToken, valueToAddr, withTraceM)

data DepositValidatorParams = DepositValidatorParams
  { assetPolicySymbol :: CurrencySymbol
  , assetRequestPolicySymbol :: CurrencySymbol
  }
PlutusTx.unstableMakeIsData ''DepositValidatorParams

newtype AssetRequestDatum = AssetRequestDatum
  {airdropAddress :: Address}
PlutusTx.unstableMakeIsData ''AssetRequestDatum

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
    info = scriptContextTxInfo ctx

    -- Filter out inputs that have airdrop address inline datum and get their
    -- locked request tokens parsed
    -- This is to ensure that all inputs that have an airdrop address receive
    -- their corresponding minted AssetNfts
    inputsWithAirdropAddr :: [(Address, [(GameAsset, Rarity, Integer)])]
    inputsWithAirdropAddr =
      mapMaybe
        ( ( \txo ->
              (,) <$> getInlineDatum txo <*> getRequestEntriesGrouped (txOutValue txo)
          )
            . txInInfoResolved
        )
        . txInfoInputs
        $ info

    -- Checks that all request tokens are burnt
    burnsInputRequestTokens :: Bool
    burnsInputRequestTokens = txInfoMint info `leq` negate combinedRequestValue
      where
        combinedRequestValue :: Value
        combinedRequestValue =
          foldMap (\(cs, tk, i) -> assetClassValue (assetClass cs tk) i)
            . filter (\(cs, _, _) -> cs == assetRequestPolicySymbol dps)
            . flattenValue
            . valueSpent
            $ info

    -- Checks that request tokens are fulfilled with AssetNfts
    mintsAndPaysAssetNfts :: Bool
    mintsAndPaysAssetNfts =
      maybe False and $
        for
          inputsWithAirdropAddr
          ( \(addr, assetsDue) -> do
              vToAddr <- valueToAddr info addr
              let expectedVToAddr = foldl (<>) mempty $ map (\(ga, r, i) -> assetClassValue (gameAssetClass ga r) i) assetsDue
              pure $ vToAddr `geq` expectedVToAddr
          )

    -- Assuming Token name are of the format: <Rarity><GameAsset> e.g. -- CommonDriver
    -- we create the AssetClass using parameter assetPolicySymobl and
    -- corresponding tokennames
    gameAssetClass :: GameAsset -> Rarity -> AssetClass
    gameAssetClass ga r = assetClass (assetPolicySymbol dps) (gameAssetTokenName ga r)

    getRequestEntriesGrouped :: Value -> Maybe [(GameAsset, Rarity, Integer)]
    getRequestEntriesGrouped =
      fmap groupByAssetRarity
        . traverse (\(_, tk, i) -> withTraceM "could not parse token name" $ parseToken tk i)
        . filter (\(cs, _, _) -> cs == assetRequestPolicySymbol dps)
        . flattenValue

    groupByAssetRarity :: [(GameAsset, Rarity, Integer)] -> [(GameAsset, Rarity, Integer)]
    groupByAssetRarity [] = []
    groupByAssetRarity ((ga, r, i) : xs) = (ga, r, i + sum (map (\(_, _, i') -> i') sames)) : groupByAssetRarity rest
      where
        (sames, rest) = partition (\(ga', r', _) -> ga == ga' && r == r') xs

    inputContainsAdminNft :: Bool
    inputContainsAdminNft = valueSpent info `geq` assetClassValue (adminToken rp) 1

    inputContainsBotNft :: Bool
    inputContainsBotNft = valueSpent info `geq` assetClassValue (botToken rp) 1

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
