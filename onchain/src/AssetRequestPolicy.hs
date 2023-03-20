{-# LANGUAGE TemplateHaskell #-}

module AssetRequestPolicy where

import CommonTypes (RacersParams (adminToken, botToken, stateToken), RacersState (depositScript), Rarity, airdropAddress, depositScript, assetPrices)
import Ledger.Value (Value, assetClass, assetClassValue, flattenValue, geq, leq)
import Plutus.V2.Ledger.Api (
  Datum (getDatum),
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInfo (txInfoMint),
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol, scriptOutputsAt, valueLockedBy, valueSpent)
import PlutusTx qualified (FromData (fromBuiltinData), compile, unsafeFromBuiltinData)
import PlutusTx.AssocMap (lookup)
import PlutusTx.Prelude
import Utils (distributesToAddrs, findCurrentGameStateFromRefInputs, parseToken, withTraceM)

{-# INLINEABLE mkAssetRequestPolicy #-}
mkAssetRequestPolicy :: RacersParams -> ScriptContext -> Bool
mkAssetRequestPolicy rp ctx =
  traceIfFalse "admin or bot not present, value minted is not negative" adminOrBotBurns
    || ( traceIfFalse "wrong ada value sent to treasury and operating" paysAdaDueToCorrectAddrs
          && traceIfFalse "does not lock minted request tokens at deposit script" locksRequestTokensAtDeposit
          && traceIfFalse "outputs at deposit with request token must have airdrop address datum" attachesAirdropAddrToDepositOutputs
       )
  where
    info :: TxInfo
    !info = scriptContextTxInfo ctx

    spentValue :: Value
    !spentValue = valueSpent info

    currentStateFromRefInput :: Maybe RacersState
    !currentStateFromRefInput = findCurrentGameStateFromRefInputs info (stateToken rp)

    adminOrBotBurns :: Bool
    adminOrBotBurns = (traceIfFalse "admin token not present" inputContainsAdminNft || traceIfFalse "bot token not present" inputContainsBotNft) && mintedRequestTokensValue `leq` zero

    inputContainsAdminNft :: Bool
    inputContainsAdminNft = spentValue `geq` assetClassValue (adminToken rp) 1

    inputContainsBotNft :: Bool
    inputContainsBotNft = spentValue `geq` assetClassValue (botToken rp) 1

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

    mintedRequestTokensParsed :: Maybe [(Rarity, Integer)]
    mintedRequestTokensParsed =
      traverse (\(_, tk, i) -> (,i) <$> parseToken tk) $
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

    totalLovelaceDue :: [(Rarity, Integer)] -> Maybe Integer
    totalLovelaceDue requestEntries = do
      st <- currentStateFromRefInput
      let lovelaceOfEntry (r, i) =
            fmap (* i) $
              withTraceM "state does not contain price entry for given rarity" $
                lookup r (assetPrices st)
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
