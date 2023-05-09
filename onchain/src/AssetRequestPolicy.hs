{-# LANGUAGE TemplateHaskell #-}
-- | A minting policy for asset requests.
module AssetRequestPolicy where

import CommonTypes (RacersParams (adminToken, botToken, stateToken), RacersState (depositScript), Rarity, airdropAddress, assetPrices, depositScript, getPrice)
import Ledger.Value (Value, assetClass, assetClassValue, flattenValue, geq)
import Plutonomy qualified (optimizeUPLC)
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
import PlutusTx qualified (FromData (fromBuiltinData), compile, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.Prelude
import Utils (distributesToAddrs, findCurrentGameStateFromRefInputs, parseToken)

data AssetRequestRedeemer = MintRequestToken | BurnRequestToken
PlutusTx.unstableMakeIsData ''AssetRequestRedeemer

{-# INLINEABLE mkAssetRequestPolicy #-}
mkAssetRequestPolicy :: RacersParams -> AssetRequestRedeemer -> ScriptContext -> Bool
mkAssetRequestPolicy rp red ctx =
  let
    info :: TxInfo
    !info = scriptContextTxInfo ctx

    ownSymbol :: CurrencySymbol
    !ownSymbol = ownCurrencySymbol ctx

    mintedRequestTokensValue :: Value
    !mintedRequestTokensValue =
      foldMap (\(cs, tk, i) -> assetClassValue (assetClass cs tk) i) $
        filter (\(cs, _, _) -> cs == ownSymbol) $
          flattenValue $
            txInfoMint info
   in
    case red of
      BurnRequestToken ->
        (traceIfFalse "admin token not present" inputContainsAdminNft
        || traceIfFalse "bot token not present" inputContainsBotNft)
        && traceIfFalse "all request tokens minted are not negative" burnsRequestTokens
        where
          spentValue :: Value
          !spentValue = valueSpent info

          burnsRequestTokens :: Bool
          burnsRequestTokens = all (\(_, _, i) -> i < 0) $ flattenValue mintedRequestTokensValue
          -- using ^(all) to disallow minting and burning in
          -- same tx, doing so would allow the admin/bot to
          -- mint request tokens freely which could result in
          -- the tokens leaving the closed system

          inputContainsAdminNft :: Bool
          inputContainsAdminNft = spentValue `geq` assetClassValue (adminToken rp) 1

          inputContainsBotNft :: Bool
          inputContainsBotNft = spentValue `geq` assetClassValue (botToken rp) 1
      MintRequestToken ->
        traceIfFalse "wrong ada value sent to treasury and operating" paysAdaDueToCorrectAddrs
          && traceIfFalse "does not lock minted request tokens at deposit script" locksRequestTokensAtDeposit
          && traceIfFalse "outputs at deposit with request token must have airdrop address datum" attachesAirdropAddrToDepositOutputs
        where
          currentStateFromRefInput :: Maybe RacersState
          !currentStateFromRefInput = findCurrentGameStateFromRefInputs info (stateToken rp)

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
            let lovelaceOfEntry (r, i) = i * getPrice r (assetPrices st)
            pure $ sum $ map lovelaceOfEntry requestEntries

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy gapp red context =
  let
    result =
      mkAssetRequestPolicy
        (PlutusTx.unsafeFromBuiltinData gapp)
        (PlutusTx.unsafeFromBuiltinData red)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $ Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkPolicy||])
