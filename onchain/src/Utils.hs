{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Utils where

import PlutusTx.Prelude

import CommonTypes (RacersState (operatingAddress, treasuryAddress), Rarity (Common, Epic, Rare))
import Control.Applicative ((<|>))
import Ledger (AssetClass, toPubKeyHash, toValidatorHash)
import Ledger.Ada (lovelaceValueOf)
import Ledger.Value (Value, assetClassValue, geq)
import Plutus.V2.Ledger.Api (
  Address,
  Datum (getDatum),
  OutputDatum (OutputDatum),
  TokenName (unTokenName),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoReferenceInputs),
  TxOut (txOutDatum, txOutValue),
 )
import Plutus.V2.Ledger.Contexts (valueLockedBy, valuePaidTo)
import PlutusTx qualified (fromBuiltinData)
import PlutusTx.Builtins (equalsByteString)
import PlutusTx.IsData (FromData)
import PlutusTx.Ratio (truncate)

{-# INLINEABLE valueToAddr #-}
valueToAddr :: TxInfo -> Address -> Maybe Value
valueToAddr info addr =
  (fmap (valuePaidTo info) . toPubKeyHash $ addr)
    <|> (fmap (valueLockedBy info) . toValidatorHash $ addr)

{-# INLINEABLE findCurrentGameStateFromRefInputs #-}
findCurrentGameStateFromRefInputs :: TxInfo -> AssetClass -> Maybe RacersState
findCurrentGameStateFromRefInputs info stateToken = do
  let stateNftValue = assetClassValue stateToken 1
  txo <- withTraceM "could not find state ref input" $ find ((`geq` stateNftValue) . txOutValue) . map txInInfoResolved $ txInfoReferenceInputs info
  getInlineDatumFromTxOut txo

{-# INLINEABLE getInlineDatumFromTxOut #-}
getInlineDatumFromTxOut :: FromData a => TxOut -> Maybe a
getInlineDatumFromTxOut txo = getInlineDatum $ txOutDatum txo

{-# INLINEABLE getInlineDatum #-}
getInlineDatum :: FromData a => OutputDatum -> Maybe a
getInlineDatum (OutputDatum d) = withTraceM "unexpected inline datum type" $ PlutusTx.fromBuiltinData $ getDatum d
getInlineDatum _ = Nothing

-- common helper that checks correct disitrbution of lovelace
-- 3/4 to treasury
-- 1/4 to operating
{-# INLINEABLE distributesToAddrs #-}
distributesToAddrs :: TxInfo -> RacersState -> Integer -> Bool
distributesToAddrs info state totalLovelace = fromMaybe False $ do
  let total = fromInteger totalLovelace
      treasuryValue = lovelaceValueOf . ceiling $ threeForths * total
      operatingValue = lovelaceValueOf . ceiling $ oneForth * total
  paysToTreasury <- (`geq` treasuryValue) <$> valueToAddr info (treasuryAddress state)
  paysToOperating <- (`geq` operatingValue) <$> valueToAddr info (operatingAddress state)
  -- This check is to cover the edge case where the operating and treasury
  -- addresses are the same, in this case we need to make sure that the total
  -- sum is paid to that address
  combinedValueCheck <- do
    addrV <- valueToAddr info (treasuryAddress state)
    operV <- valueToAddr info (operatingAddress state)
    pure $ (addrV <> operV) `geq` (treasuryValue <> operatingValue)
  pure $
    traceIfFalse "wrong amount paid to treasury" paysToTreasury
      && traceIfFalse "wrong amount paid to operating" paysToOperating
      && traceIfFalse "wrong combined amount paid to treasury and operating" combinedValueCheck
  where
    threeForths, oneForth :: Rational
    threeForths = unsafeRatio 3 4
    oneForth = unsafeRatio 1 4
    ceiling :: Rational -> Integer
    ceiling x =
      let floor = truncate x
       in if fromInteger floor == x then floor else floor + 1

{-# INLINEABLE withTraceM #-}
withTraceM :: BuiltinString -> Maybe a -> Maybe a
withTraceM msg Nothing = trace msg Nothing
withTraceM _ x = x

{-# INLINEABLE parseToken #-}
parseToken :: TokenName -> Maybe Rarity
parseToken tn = do
  case unTokenName tn of
    x | equalsByteString x "Common" -> Just Common
    x | equalsByteString x "Rare" -> Just Rare
    x | equalsByteString x "Epic" -> Just Epic
    _ -> Nothing
