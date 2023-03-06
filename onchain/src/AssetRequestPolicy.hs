{-# LANGUAGE TemplateHaskell #-}

module AssetRequestPolicy where

import CommonTypes (GameAsset (Car, Driver), RacersParams (stateToken), RacersState, Rarity, adminToken, botToken, carPrices, depositScript, driverPrices, rarityToBuiltinByteString)
import Ledger.Value (Value, assetClass, assetClassValue, flattenValue, geq)
import Plutus.V2.Ledger.Api (
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInfo (txInfoMint),
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol, valueLockedBy, valueSpent)
import PlutusTx qualified (compile, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.AssocMap (lookup)
import PlutusTx.Prelude
import Utils (distributesToAddrs, findCurrentGameStateFromRefInputs, parseToken, withTraceM)

data AssetRequestRedeemer = UserMintRequestToken | AdminMintRequestTokens
PlutusTx.unstableMakeIsData ''AssetRequestRedeemer

{-# INLINEABLE mkAssetRequestPolicy #-}
mkAssetRequestPolicy :: RacersParams -> AssetRequestRedeemer -> ScriptContext -> Bool
mkAssetRequestPolicy rp red ctx =
  case red of
    UserMintRequestToken ->
      traceIfFalse "wrong ada value sent to treasury and operating" paysAdaDueToCorrectAddrs
        && traceIfFalse "does not lock minted request tokens at deposit script" locksRequestTokensAtDeposit
    AdminMintRequestTokens ->
      traceIfFalse "Admin token not present in inputs" inputContainsAdminNft
    || traceIfFalse "Bot token not present in inputs" inputContainsBotNft
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    currentStateFromRefInput :: Maybe RacersState
    currentStateFromRefInput = findCurrentGameStateFromRefInputs info (stateToken rp)

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

    inputContainsAdminNft :: Bool
    inputContainsAdminNft = valueSpent info `geq` assetClassValue (adminToken rp) 1

    inputContainsBotNft :: Bool
    inputContainsBotNft = valueSpent info `geq` assetClassValue (botToken rp) 1

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy gapp redeemer context =
  let
    result =
      mkAssetRequestPolicy
        (PlutusTx.unsafeFromBuiltinData gapp)
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $$(PlutusTx.compile [||mkPolicy||])
