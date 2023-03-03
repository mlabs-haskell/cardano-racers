{-# LANGUAGE TemplateHaskell #-}

module AssetRequestPolicy where

import CommonTypes (GameAsset (Car, Driver), RacersParams (stateToken), RacersState, Rarity, carPrices, driverPrices)
import Ledger.Value (flattenValue)
import Plutus.V2.Ledger.Api (
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInfo (txInfoMint),
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol)
import PlutusTx qualified (compile, unsafeFromBuiltinData)
import PlutusTx.AssocMap (lookup)
import PlutusTx.Prelude
import Utils (distributesToAddrs, findCurrentGameStateFromRefInputs, parseToken)

{-# INLINEABLE mkAssetRequestPolicy #-}
mkAssetRequestPolicy :: RacersParams -> ScriptContext -> Bool
mkAssetRequestPolicy rp ctx = paysToCorrectAddrs
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    currentStateFromRefInput :: Maybe RacersState
    currentStateFromRefInput = findCurrentGameStateFromRefInputs info (stateToken rp)

    mintedRequestTokens :: Maybe [(GameAsset, Rarity, Integer)]
    mintedRequestTokens =
      traverse (\(_, tk, i) -> parseToken tk i) $
        filter (\(cs, _, _) -> cs == ownCurrencySymbol ctx) $
          flattenValue $
            txInfoMint info

    totalLovelaceDue :: [(GameAsset, Rarity, Integer)] -> Maybe Integer
    totalLovelaceDue requestEntries = do
      st <- currentStateFromRefInput
      let lovelaceOfEntry (Driver, r, i) = (* i) <$> lookup r (driverPrices st)
          lovelaceOfEntry (Car, r, i) = (* i) <$> lookup r (carPrices st)
      sum <$> traverse lovelaceOfEntry requestEntries

    paysToCorrectAddrs :: Bool
    paysToCorrectAddrs = fromMaybe False $ do
      totalLovelace <- mintedRequestTokens >>= totalLovelaceDue
      state <- currentStateFromRefInput
      pure $ distributesToAddrs info state totalLovelace

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy gapp _redeemer context =
  let
    result =
      mkAssetRequestPolicy
        (PlutusTx.unsafeFromBuiltinData gapp)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $$(PlutusTx.compile [||mkPolicy||])
