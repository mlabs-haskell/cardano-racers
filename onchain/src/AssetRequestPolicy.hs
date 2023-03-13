{-# LANGUAGE TemplateHaskell #-}

module AssetRequestPolicy where

import CommonTypes (GameAsset (Car, Driver), RacersParams (stateToken), RacersState (depositScript), Rarity, airdropAddress, carPrices, depositScript, driverPrices, rarityToBuiltinByteString)
import Ledger.Value (Value, assetClass, assetClassValue, flattenValue, geq)
import Plutus.V2.Ledger.Api (
  Datum (getDatum),
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInfo (txInfoMint),
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol, scriptOutputsAt, valueLockedBy)
import PlutusTx qualified (FromData (fromBuiltinData), compile, unsafeFromBuiltinData)
import PlutusTx.AssocMap (lookup)
import PlutusTx.Prelude
import Utils (distributesToAddrs, findCurrentGameStateFromRefInputs, parseToken, withTraceM)

{-# INLINEABLE mkAssetRequestPolicy #-}
mkAssetRequestPolicy :: RacersParams -> ScriptContext -> Bool
mkAssetRequestPolicy rp ctx =
  traceIfFalse "wrong ada value sent to treasury and operating" paysAdaDueToCorrectAddrs
    && traceIfFalse "does not lock minted request tokens at deposit script" locksRequestTokensAtDeposit
    && traceIfFalse "outputs at deposit with request token must have airdrop address datum" attachesAirdropAddrToDepositOutputs
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    currentStateFromRefInput :: Maybe RacersState
    currentStateFromRefInput = findCurrentGameStateFromRefInputs info (stateToken rp)

    attachesAirdropAddrToDepositOutputs :: Bool
    attachesAirdropAddrToDepositOutputs = isJust $ do
      st <- currentStateFromRefInput
      let depositOutputsWithRequest =
            filter
              ( \(_, v) ->
                  elem
                    (ownCurrencySymbol ctx)
                    $ map (\(cs, _, _) -> cs)
                    $ flattenValue v
              )
              $ scriptOutputsAt (depositScript st) info
      traverse
        ( \case
            (OutputDatum odat, _) -> fmap airdropAddress $ PlutusTx.fromBuiltinData $ getDatum odat
            _ -> Nothing
        )
        depositOutputsWithRequest

    mintedRequestTokensValue :: Value
    mintedRequestTokensValue =
      foldMap (\(cs, tk, i) -> assetClassValue (assetClass cs tk) i) $
        filter (\(cs, _, _) -> cs == ownCurrencySymbol ctx) $
          flattenValue $
            txInfoMint info

    mintedRequestTokensParsed :: Maybe [(GameAsset, Rarity, Integer)]
    mintedRequestTokensParsed =
      traverse (\(_, tk, i) -> parseToken tk i) $
        flattenValue mintedRequestTokensValue

    locksRequestTokensAtDeposit :: Bool
    locksRequestTokensAtDeposit = fromMaybe False $ do
      st <- currentStateFromRefInput
      pure $ valueLockedBy info (depositScript st) `geq` mintedRequestTokensValue

    paysAdaDueToCorrectAddrs :: Bool
    paysAdaDueToCorrectAddrs = fromMaybe False $ do
      totalLovelace <- mintedRequestTokensParsed >>= totalLovelaceDue
      state <- currentStateFromRefInput
      pure $ distributesToAddrs info state totalLovelace

    totalLovelaceDue :: [(GameAsset, Rarity, Integer)] -> Maybe Integer
    totalLovelaceDue requestEntries = do
      st <- currentStateFromRefInput
      let lovelaceOfEntry (Driver, r, i) =
            fmap (* i) $
              withTraceM ("state does not contain price entry for driver: " <> decodeUtf8 (rarityToBuiltinByteString r)) $
                lookup r (driverPrices st)
          lovelaceOfEntry (Car, r, i) =
            fmap (* i) $
              withTraceM ("state does not contain price entry for car: " <> decodeUtf8 (rarityToBuiltinByteString r)) $
                lookup r (carPrices st)
      sum <$> traverse lovelaceOfEntry requestEntries

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy gapp _red context =
  let
    result =
      mkAssetRequestPolicy
        (PlutusTx.unsafeFromBuiltinData gapp)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $$(PlutusTx.compile [||mkPolicy||])
