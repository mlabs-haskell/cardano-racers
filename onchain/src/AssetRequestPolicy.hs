{-# LANGUAGE TemplateHaskell #-}

module AssetRequestPolicy where

import CommonTypes (GameAsset (Car, Driver), RacersParams (stateToken), RacersState, Rarity (Common, Epic, Rare), carPrices, driverPrices)
import Ledger.Value (TokenName (unTokenName), flattenValue)
import Plutus.V2.Ledger.Api (
  Address,
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInfo (txInfoMint),
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol)
import PlutusTx qualified (compile, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.AssocMap (lookup)
import PlutusTx.Builtins (equalsByteString)
import PlutusTx.Prelude
import Utils (distributesToAddrs, findCurrentGameStateFromRefInputs, safeIndex, splitOn)

data AssetRequestDatum = AssetRequestDatum
  { airdropAddress :: Address
  , asset :: GameAsset
  , rarity :: Rarity
  }
PlutusTx.unstableMakeIsData ''AssetRequestDatum

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

    parseToken :: TokenName -> Integer -> Maybe (GameAsset, Rarity, Integer)
    parseToken tn count = do
      let splitted = splitOn ":" $ unTokenName tn
      r <-
        splitted
          `safeIndex` 0
          >>= ( \x -> case x of
                  _ | equalsByteString x "Common" -> Just Common
                  _ | equalsByteString x "Rare" -> Just Rare
                  _ | equalsByteString x "Epic" -> Just Epic
                  _ | otherwise -> Nothing
              )
      a <-
        splitted
          `safeIndex` 1
          >>= ( \x -> case x of
                  _ | equalsByteString x "Driver" -> Just Driver
                  _ | equalsByteString x "Car" -> Just Car
                  _ | otherwise -> Nothing
              )

      pure (a, r, count)

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
