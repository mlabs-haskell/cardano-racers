{-# LANGUAGE TemplateHaskell #-}

module GameAssetPolicy (script) where

import CommonTypes (GameAsset, RacersParams, Rarity, adminToken, botToken)
import Ledger.Value (assetClassValue, flattenValue, geq)
import Plutus.V2.Ledger.Api (
  Address,
  Datum (getDatum),
  FromData (fromBuiltinData),
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoInputs),
  TxOut (txOutDatum),
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol, valueSpent)
import PlutusTx qualified (compile, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.Prelude
import Utils (valueToAddr)

data AssetRequestDatum = AssetRequestDatum
  { airdropAddress :: Address
  , asset :: GameAsset
  , rarity :: Rarity
  }
PlutusTx.unstableMakeIsData ''AssetRequestDatum

{-# INLINEABLE mkGameAssetPolicy #-}
mkGameAssetPolicy :: RacersParams -> ScriptContext -> Bool
mkGameAssetPolicy gapp ctx =
  ( traceIfFalse "input does not contain admin token" inputContainsAdminNft
      || traceIfFalse "input does not contain bot token" inputContainsBotNft
  )
    && traceIfFalse "Tx does not pay to expected airdrop addresses" paysNftsToAirdrops
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    airdropAddresses :: [Address]
    airdropAddresses = mapMaybe getDatumAirdropAddress $ txInfoInputs info

    groupByOccurences :: Eq a => [a] -> [(a, Integer)]
    groupByOccurences [] = []
    groupByOccurences (x : xs) = (x, length xs' + 1) : groupByOccurences ys'
      where
        (xs', ys') = partition (== x) xs

    -- Check to ensure corerct amount of Nfts are paid to airdrop addresses
    -- found in tx inputs
    paysNftsToAirdrops :: Bool
    paysNftsToAirdrops =
      maybe (trace "Couldn't get amount value to address" False) and $
        for (groupByOccurences airdropAddresses) $ \(addr, count) -> do
          totalVal <- valueToAddr info addr
          let nftToAirdropCount =
                length $
                  filter (\(cs, _, amt) -> cs == ownCurrencySymbol ctx && amt == 1) $
                    flattenValue totalVal
          pure $ nftToAirdropCount >= count

    getDatumAirdropAddress :: TxInInfo -> Maybe Address
    getDatumAirdropAddress txIn = do
      let txOut = txInInfoResolved txIn
      case txOutDatum txOut of
        OutputDatum d -> airdropAddress <$> fromBuiltinData @AssetRequestDatum (getDatum d)
        _ -> Nothing

    inputContainsAdminNft :: Bool
    inputContainsAdminNft = valueSpent info `geq` assetClassValue (adminToken gapp) 1

    inputContainsBotNft :: Bool
    inputContainsBotNft = valueSpent info `geq` assetClassValue (botToken gapp) 1

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy gapp _redeemer context =
  let
    result =
      mkGameAssetPolicy
        (PlutusTx.unsafeFromBuiltinData gapp)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $$(PlutusTx.compile [||mkPolicy||])
