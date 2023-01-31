{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

module NitroMintingPolicy (script) where

import PlutusTx.Prelude

import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import GHC.Real (RealFrac (ceiling))
import GHC.Show (Show)
import Ledger (Address, AssetClass, CurrencySymbol, Datum (getDatum), PaymentPubKeyHash (unPaymentPubKeyHash), toPubKeyHash, toValidatorHash)
import Ledger.Ada (lovelaceValueOf)
import Ledger.Value (assetClass, assetClassValue, flattenValue, geq)
import Plutus.V2.Ledger.Api (
  Address,
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (scriptContextTxInfo),
  ToData (toBuiltinData),
  TokenName,
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoReferenceInputs),
  TxOut (txOutDatum, txOutValue),
  Value,
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol, ownHash, scriptOutputsAt, txSignedBy, valueLockedBy, valuePaidTo, valueProduced, valueSpent)
import PlutusTx qualified (compile, makeLift, unsafeFromBuiltinData, unstableMakeIsData)

data GameState = GameState
  { gameTokenPrice :: Integer -- Nitro price in Lovelace
  , treasuryAddress :: Address
  , operatingAddress :: Address
  }
  deriving (Show, Generic, Eq)
PlutusTx.unstableMakeIsData ''GameState

data GameScriptParams = GameScriptParams
  { adminToken :: AssetClass
  , stateToken :: AssetClass
  , gameToken :: TokenName
  }
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''GameScriptParams

data GameScriptRedeemer
  = SetGameState GameState -- Requires AdminToken
  | MintGameToken Integer
  | BuyGameToken Integer
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''GameScriptRedeemer

{-# INLINEABLE mkPolicy #-}
mkPolicy :: GameScriptParams -> () -> GameScriptRedeemer -> ScriptContext -> Bool
mkPolicy gsp dat red ctx =
  traceIfFalse "state token not preserved" stateTokenPreserved && case red of
    SetGameState gs ->
      traceIfFalse "Admin token not present" inputContainsAdminToken
        && traceIfFalse "game state token not spent" inputContainsStateToken
        && traceIfFalse "game state not set" (setsGameStateTo gs)
    MintGameToken i ->
      traceIfFalse "admin token not present" inputContainsAdminToken
        && traceIfFalse "wrong amount minted" (mintedGameToken i)
    BuyGameToken i ->
      traceIfFalse "no ref input with game token" hasGameStateRefInput
        && traceIfFalse "wrong amount minted" (mintedGameToken i)
        && traceIfFalse "wrong amount spent" (sendsAdaToCorrectAddrs i)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    outputsLockedByTheScript :: [(OutputDatum, Value)]
    outputsLockedByTheScript = scriptOutputsAt (ownHash ctx) info

    hasGameStateRefInput :: Bool
    hasGameStateRefInput = isJust gameStateRefInput

    gameStateRefInput :: Maybe TxOut
    gameStateRefInput = find ((`geq` stateTokenValue) . txOutValue) . map txInInfoResolved $ txInfoReferenceInputs info

    currentStateFromRefInput :: Maybe GameState
    currentStateFromRefInput = do
      outDatum <- txOutDatum <$> gameStateRefInput
      dat <- case outDatum of
        OutputDatum d -> Just $ getDatum d
        _ -> Nothing
      PlutusTx.unsafeFromBuiltinData dat

    stateTokenPreserved :: Bool
    stateTokenPreserved = not inputContainsStateToken || stateTokenLocked -- if state token is in inputs then it must be locked again
    stateTokenValue :: Value
    stateTokenValue = assetClassValue (stateToken gsp) 1

    stateTokenLocked :: Bool
    stateTokenLocked = valueLockedBy info (ownHash ctx) `geq` stateTokenValue

    inputContainsAdminToken :: Bool
    inputContainsAdminToken = inputContainsValue $ assetClassValue (adminToken gsp) 1

    inputContainsStateToken :: Bool
    inputContainsStateToken = inputContainsValue $ stateTokenValue

    setsGameStateTo :: GameState -> Bool
    setsGameStateTo gs =
      case filter (\(odat, val) -> val `geq` stateTokenValue) outputsLockedByTheScript of
        [(OutputDatum odat, val)] -> getDatum odat == toBuiltinData gs
        _ -> False

    inputContainsValue :: Value -> Bool
    inputContainsValue v = valueSpent info `geq` v

    mintedGameToken :: Integer -> Bool
    mintedGameToken i = valueProduced info `geq` gameTokenValue i

    gameTokenValue :: Integer -> Value
    gameTokenValue amt = assetClassValue (assetClass (ownCurrencySymbol ctx) (gameToken gsp)) amt

    sendsAdaToCorrectAddrs :: Integer -> Bool
    sendsAdaToCorrectAddrs mintedAmount = fromMaybe False $ do
      gameState <- currentStateFromRefInput
      let totalPrice = fromInteger mintedAmount * fromInteger (gameTokenPrice gameState)
          treasuryValue = lovelaceValueOf . round $ unsafeRatio 1 4 * totalPrice
          operatingValue = lovelaceValueOf . round $ unsafeRatio 3 4 * totalPrice
      paysToTreasury <- (`geq` treasuryValue) <$> valueToAddr (treasuryAddress gameState)
      paysToOperating <- (`geq` operatingValue) <$> valueToAddr (operatingAddress gameState)
      pure $ paysToTreasury && paysToOperating

    valueToAddr :: Address -> Maybe Value
    valueToAddr addr = (fmap (valuePaidTo info) . toPubKeyHash $ addr) <|> (fmap (valueLockedBy info) . toValidatorHash $ addr)

{-# INLINEABLE mkPolicy' #-}
mkPolicy' :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy' gsp datum redeemer context =
  let
    result =
      mkPolicy
        (PlutusTx.unsafeFromBuiltinData gsp)
        (PlutusTx.unsafeFromBuiltinData datum)
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $$(PlutusTx.compile [||mkPolicy'||])
