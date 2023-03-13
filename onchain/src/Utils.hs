{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Utils where

import PlutusTx.Prelude

import CommonTypes (GameAsset (Car, Driver), RacersState (operatingAddress, treasuryAddress), Rarity (Common, Epic, Rare), gameAssetToBuiltinByteString, rarityToBuiltinByteString)
import Control.Applicative ((<|>))
import Ledger (AssetClass, toPubKeyHash, toValidatorHash)
import Ledger.Ada (lovelaceValueOf)
import Ledger.Value (TokenName (TokenName), Value, assetClassValue, geq)
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
  getInlineDatum txo

{-# INLINEABLE getInlineDatum #-}
getInlineDatum :: FromData a => TxOut -> Maybe a
getInlineDatum txo = case txOutDatum txo of
  OutputDatum d -> withTraceM "unexpected inline datum type" $ PlutusTx.fromBuiltinData $ getDatum d
  _ -> Nothing

{-# INLINEABLE distributesToAddrs #-}
distributesToAddrs :: TxInfo -> RacersState -> Integer -> Bool
distributesToAddrs info state totalLovelace = fromMaybe False $ do
  let total = fromInteger totalLovelace
      treasuryValue = lovelaceValueOf . ceiling $ threeForths * total
      operatingValue = lovelaceValueOf . ceiling $ oneForth * total
  paysToTreasury <- (`geq` treasuryValue) <$> valueToAddr info (treasuryAddress state)
  paysToOperating <- (`geq` operatingValue) <$> valueToAddr info (operatingAddress state)
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

{-# INLINEABLE safeIndex #-}
safeIndex :: [a] -> Integer -> Maybe a
safeIndex xs i
  | i < 0 = Nothing
  | otherwise = go xs i
  where
    go [] _ = Nothing
    go (x' : xs') i' = if i' == 0 then Just x' else go xs' (i' - 1)

{-# INLINEABLE splitOn #-}
splitOn :: BuiltinByteString -> BuiltinByteString -> [BuiltinByteString]
splitOn sep orig
  | equalsByteString orig "" = []
  | equalsByteString sep "" = [orig]
  | otherwise = h : splitOn sep t
  where
    (h, t) = span 0
    startsWith x xs = equalsByteString x $ sliceByteString 0 (lengthOfByteString x) xs
    span ptr
      | ptr > lengthOfByteString orig - lengthOfByteString sep = (orig, emptyByteString)
      | startsWith sep (sliceByteString ptr (lengthOfByteString orig) orig) =
          ( sliceByteString 0 ptr orig
          , sliceByteString (ptr + lengthOfByteString sep) (lengthOfByteString orig - ptr + lengthOfByteString sep) orig
          )
      | otherwise = span (ptr + 1)

{-# INLINEABLE parseToken #-}
parseToken :: TokenName -> Integer -> Maybe (GameAsset, Rarity, Integer)
parseToken tn count = do
  let tnStr = decodeUtf8 $ unTokenName tn
      splitted = splitOn ":" $ unTokenName tn
  r <-
    withTraceM "could not decode rarity" $
      splitted
        `safeIndex` 0
        >>= ( \x -> case x of
                _ | equalsByteString x "Common" -> Just Common
                _ | equalsByteString x "Rare" -> Just Rare
                _ | equalsByteString x "Epic" -> Just Epic
                _ | otherwise -> Nothing
            )
  a <-
    withTraceM ("could not decode asset type" <> tnStr) $
      splitted
        `safeIndex` 1
        >>= ( \case
                s | equalsByteString s "Driver" -> Just Driver
                s | equalsByteString s "Car" -> Just Car
                _ | otherwise -> Nothing
            )
  pure (a, r, count)

{-# INLINEABLE gameAssetTokenName #-}
gameAssetTokenName :: GameAsset -> Rarity -> TokenName
gameAssetTokenName asset rarity = TokenName $ gameAssetToBuiltinByteString asset <> rarityToBuiltinByteString rarity
