{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

module NitroMintingPolicy (script, mintredeemer, setstatered) where

import PlutusTx.Prelude

import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import GHC.Real (RealFrac (ceiling))
import GHC.Show (Show)
import Ledger (Address, AssetClass, CurrencySymbol, Datum (getDatum), PaymentPubKeyHash (unPaymentPubKeyHash), Validator (Validator), fromSymbol, scriptHashAddress, toPubKeyHash, toValidatorHash, validatorHash, ScriptPurpose (Minting, Spending))
import Ledger.Ada (lovelaceValueOf)
import Ledger.Value (assetClass, assetClassValue, assetClassValueOf, flattenValue, geq)
import Plutus.V2.Ledger.Api (
  Address,
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (scriptContextTxInfo, scriptContextPurpose),
  ToData (toBuiltinData),
  TokenName,
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoReferenceInputs),
  TxOut (txOutDatum, txOutValue),
  ValidatorHash,
  Value,
  fromCompiledCode,
  toData,
 )
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol, scriptOutputsAt, txSignedBy, valueLockedBy, valuePaidTo, valueProduced, valueSpent, ownHash)
import PlutusTx qualified (compile, makeLift, unsafeFromBuiltinData, unstableMakeIsData, FromData (fromBuiltinData))

data NitroState = NitroState
  { nitroPrice :: Integer -- Nitro price in Lovelace
  , treasuryAddress :: Address
  , operatingAddress :: Address
  }
  deriving (Show, Generic, Eq)
PlutusTx.unstableMakeIsData ''NitroState

data NitroScriptParams = NitroScriptParams
  { adminToken :: AssetClass
  , stateToken :: AssetClass
  , nitroToken :: TokenName
  }
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''NitroScriptParams

data NitroScriptRedeemer
  = SetNitroState NitroState -- Requires AdminToken
  | MintNitroToken Integer
  | BuyNitroToken Integer
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''NitroScriptRedeemer

{-# INLINEABLE mkPolicy #-}
mkPolicy :: NitroScriptParams -> NitroScriptRedeemer -> ScriptContext -> Bool
mkPolicy gsp red ctx =
  traceIfFalse "state token not preserved" stateTokenPreserved && case red of
    SetNitroState ns ->
      traceIfFalse "Admin token not present" inputContainsAdminToken
        && traceIfFalse "game state token not spent" inputContainsStateToken
        && traceIfFalse "game state not set" (setsNitroStateTo ns)
    MintNitroToken i ->
      traceIfFalse "admin token not present" inputContainsAdminToken
        && traceIfFalse "wrong amount minted" (mintedNitroToken i)
    BuyNitroToken i ->
      traceIfFalse "no ref input with game token" hasNitroStateRefInput
        && traceIfFalse "wrong amount minted" (mintedNitroToken i)
        && traceIfFalse "wrong amount spent" (sendsAdaToCorrectAddrs i)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownValidatorHash :: ValidatorHash
    ownValidatorHash = case scriptContextPurpose ctx of
      Spending _ -> ownHash ctx
      Minting _ -> fromSymbol $ ownCurrencySymbol ctx
      _ -> traceError "unexpected script purpose"

    outputsLockedByTheScript :: [(OutputDatum, Value)]
    outputsLockedByTheScript = scriptOutputsAt ownValidatorHash info

    hasNitroStateRefInput :: Bool
    hasNitroStateRefInput = isJust gameStateRefInput

    gameStateRefInput :: Maybe TxOut
    gameStateRefInput = find ((`geq` stateTokenValue) . txOutValue) . map txInInfoResolved $ txInfoReferenceInputs info

    currentStateFromRefInput :: Maybe NitroState
    currentStateFromRefInput = do
      outDatum <- txOutDatum <$> gameStateRefInput
      dat <- case outDatum of
        OutputDatum d -> Just $ getDatum d
        _ -> Nothing
      PlutusTx.fromBuiltinData dat

    stateTokenPreserved :: Bool
    stateTokenPreserved = not inputContainsStateToken || stateTokenLocked -- if state token is in inputs then it must be locked again
    stateTokenValue :: Value
    stateTokenValue = assetClassValue (stateToken gsp) 1

    stateTokenLocked :: Bool
    stateTokenLocked = valueLockedBy info ownValidatorHash `geq` stateTokenValue

    inputContainsAdminToken :: Bool
    inputContainsAdminToken = inputContainsValue $ assetClassValue (adminToken gsp) 1

    inputContainsStateToken :: Bool
    inputContainsStateToken = inputContainsValue $ stateTokenValue

    setsNitroStateTo :: NitroState -> Bool
    setsNitroStateTo gs =
      case filter (\(odat, val) -> val `geq` stateTokenValue) outputsLockedByTheScript of
        [(OutputDatum odat, val)] -> getDatum odat == toBuiltinData gs
        _ -> False

    inputContainsValue :: Value -> Bool
    inputContainsValue v = valueSpent info `geq` v

    mintedNitroToken :: Integer -> Bool
    mintedNitroToken i = expectedMintedAmount == actualMintedAmount
      where
        expectedMintedAmount = i
        actualMintedAmount = assetClassValueOf (valueProduced info) nitroAssetClass

    nitroAssetClass :: AssetClass
    nitroAssetClass = assetClass (ownCurrencySymbol ctx) (nitroToken gsp)

    sendsAdaToCorrectAddrs :: Integer -> Bool
    sendsAdaToCorrectAddrs mintedAmount = fromMaybe False $ do
      gameState <- currentStateFromRefInput
      let totalPrice = fromInteger mintedAmount * fromInteger (nitroPrice gameState)
          treasuryValue = lovelaceValueOf . round $ unsafeRatio 1 4 * totalPrice
          operatingValue = lovelaceValueOf . round $ unsafeRatio 3 4 * totalPrice
      paysToTreasury <- (`geq` treasuryValue) <$> valueToAddr (treasuryAddress gameState)
      paysToOperating <- (`geq` operatingValue) <$> valueToAddr (operatingAddress gameState)
      pure $
        traceIfTrue "pays to treasury" paysToTreasury
          && traceIfTrue "pays to operating" paysToOperating
    -- pure $ paysToTreasury && paysToOperating

    valueToAddr :: Address -> Maybe Value
    valueToAddr addr = (fmap (valuePaidTo info) . toPubKeyHash $ addr) <|> (fmap (valueLockedBy info) . toValidatorHash $ addr)

{-# INLINEABLE mkPolicy' #-}
mkPolicy' :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy' gsp _datum redeemer context =
  let
    result =
      mkPolicy
        (PlutusTx.unsafeFromBuiltinData gsp)
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $$(PlutusTx.compile [||mkPolicy'||])


setstatered = toData $ SetNitroState $ NitroState 1000000 (scriptHashAddress $ validatorHash $ Validator script) (scriptHashAddress $ validatorHash $ Validator script)
-- gamestate :: BuiltinData
gamestate =
  toData $
    NitroState
      { nitroPrice = 1000000
      , treasuryAddress = scriptHashAddress $ validatorHash $ Validator script
      , operatingAddress = scriptHashAddress $ validatorHash $ Validator script
      }

gameparams =
  toData $
    NitroScriptParams
      {
      }

mintredeemer = toData $ MintNitroToken 1
