{-# LANGUAGE TemplateHaskell #-}

module AssetRequestPolicy where

import CommonTypes (RacersParams (adminToken, botToken, stateToken), RacersState (depositScript), Rarity, airdropAddress, assetPrices, depositScript)
import Ledger.Value (Value, assetClass, assetClassValue, flattenValue, geq)
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
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

-- todo: will help readability to add redeemers representing admin/bot
-- burning, and user minting request tokens. Must be wary of Tx size though
{-# INLINEABLE mkAssetRequestPolicy #-}
mkAssetRequestPolicy :: RacersParams -> ScriptContext -> Bool
mkAssetRequestPolicy rp ctx =
  if burnsRequestTokens
    then traceIfFalse "admin token not present" inputContainsAdminNft || traceIfFalse "bot token not present" inputContainsBotNft
    else
      traceIfFalse "wrong ada value sent to treasury and operating" paysAdaDueToCorrectAddrs
        && traceIfFalse "does not lock minted request tokens at deposit script" locksRequestTokensAtDeposit
        && traceIfFalse "outputs at deposit with request token must have airdrop address datum" attachesAirdropAddrToDepositOutputs
  where
    info :: TxInfo
    !info = scriptContextTxInfo ctx

    spentValue :: Value
    !spentValue = valueSpent info

    currentStateFromRefInput :: Maybe RacersState
    !currentStateFromRefInput = findCurrentGameStateFromRefInputs info (stateToken rp)

    ownSymbol :: CurrencySymbol
    !ownSymbol = ownCurrencySymbol ctx

    burnsRequestTokens :: Bool
    burnsRequestTokens = any (\(_, _, i) -> i < 0) $ flattenValue mintedRequestTokensValue

    inputContainsAdminNft :: Bool
    inputContainsAdminNft = spentValue `geq` assetClassValue (adminToken rp) 1

    inputContainsBotNft :: Bool
    inputContainsBotNft = spentValue `geq` assetClassValue (botToken rp) 1

    -- Ensures that any outputs containing request tokens have a corresponding airdrop address datum
    attachesAirdropAddrToDepositOutputs :: Bool
    attachesAirdropAddrToDepositOutputs = isJust $ do
      st <- currentStateFromRefInput
      let depositOutputsWithRequest =
            -- filter outputs with request tokens
            filter
              ( \(_, v) ->
                  elem
                    ownSymbol
                    $ map (\(cs, _, _) -> cs)
                    $ flattenValue v
              )
              -- outputs at deposit script
              $ scriptOutputsAt (depositScript st) info

      traverse
        ( \case
            -- attempt to parse airdrop address from datum fails if datum is
            -- not inline or not of the expected form
            (OutputDatum odat, _) -> fmap airdropAddress $ PlutusTx.fromBuiltinData $ getDatum odat
            _ -> Nothing
        )
        depositOutputsWithRequest

    mintedRequestTokensValue :: Value
    mintedRequestTokensValue =
      foldMap (\(cs, tk, i) -> assetClassValue (assetClass cs tk) i) $
        filter (\(cs, _, _) -> cs == ownSymbol) $
          flattenValue $
            txInfoMint info

    -- parse requested rarity class from token name
    mintedRequestTokensParsed :: Maybe [(Rarity, Integer)]
    mintedRequestTokensParsed =
      traverse (\(_, tk, i) -> (,i) <$> parseToken tk) $
        flattenValue mintedRequestTokensValue

    -- check to ensure all minted request tokens are locked at deposit script
    locksRequestTokensAtDeposit :: Bool
    locksRequestTokensAtDeposit = fromMaybe False $ do
      st <- currentStateFromRefInput
      pure $ valueLockedBy info (depositScript st) `geq` mintedRequestTokensValue

    paysAdaDueToCorrectAddrs :: Bool
    paysAdaDueToCorrectAddrs = fromMaybe False $ do
      totalLovelace <- mintedRequestTokensParsed >>= totalLovelaceDue
      state <- currentStateFromRefInput
      pure $ distributesToAddrs info state totalLovelace

    -- computes total ada due by requested rarity class
    -- fails if state does not contain price entry for given rarity
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
