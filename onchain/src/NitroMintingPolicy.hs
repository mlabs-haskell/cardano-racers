{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

module NitroMintingPolicy (script) where

import PlutusTx.Prelude

import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import GHC.Real (RealFrac (ceiling))
import GHC.Show (Show)
import Ledger (Address, AssetClass, CurrencySymbol, Datum (getDatum), PaymentPubKeyHash (unPaymentPubKeyHash), ScriptPurpose (Minting, Spending), Validator (Validator), fromSymbol, scriptHashAddress, toPubKeyHash, toValidatorHash, validatorHash)
import Ledger.Ada (lovelaceValueOf)
import Ledger.Value (assetClass, assetClassValue, assetClassValueOf, flattenValue, geq, leq)
import Plutus.V2.Ledger.Api (
  Address,
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (scriptContextPurpose, scriptContextTxInfo),
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
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol, ownHash, scriptOutputsAt, txSignedBy, valueLockedBy, valuePaidTo, valueProduced, valueSpent)
import PlutusTx qualified (FromData (fromBuiltinData), compile, makeLift, unsafeFromBuiltinData, unstableMakeIsData)

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
    case (scriptContextPurpose ctx, red) of
      (Minting cs, BuyNitroToken i) ->
        traceIfFalse "wrong amount spent" (sendsAdaToCorrectAddrs i)
          && traceIfFalse "wrong amount minted" (mintedNitroToken i)
        where
          gameStateRefInput :: Maybe TxOut
          gameStateRefInput = find ((`geq` stateTokenValue) . txOutValue) . map txInInfoResolved $ txInfoReferenceInputs info

          currentStateFromRefInput :: Maybe NitroState
          currentStateFromRefInput = do
            outDatum <- txOutDatum <$> gameStateRefInput
            dat <- case outDatum of
              OutputDatum d -> Just $ getDatum d
              _ -> Nothing
            PlutusTx.fromBuiltinData dat

          sendsAdaToCorrectAddrs :: Integer -> Bool
          sendsAdaToCorrectAddrs mintedAmount = fromMaybe False $ do
            gameState <- currentStateFromRefInput
            let totalPrice = fromInteger mintedAmount * fromInteger (nitroPrice gameState)
                treasuryValue = lovelaceValueOf . round $ unsafeRatio 3 4 * totalPrice
                operatingValue = lovelaceValueOf . round $ unsafeRatio 1 4 * totalPrice
            paysToTreasury <- (`geq` treasuryValue) <$> valueToAddr (treasuryAddress gameState)
            paysToOperating <- (`geq` operatingValue) <$> valueToAddr (operatingAddress gameState)
            combinedValueCheck <- do
              addrV <- valueToAddr (treasuryAddress gameState)
              operV <- valueToAddr (operatingAddress gameState)
              pure $ (addrV <> operV) `geq` (treasuryValue <> operatingValue)
            pure $
              traceIfFalse "wrong amount paid to treasury" paysToTreasury
                && traceIfFalse "wrong amount paid to operating" paysToOperating
                && traceIfFalse "wrong combined amount paid to treasury and operating" combinedValueCheck

          valueToAddr :: Address -> Maybe Value
          valueToAddr addr =
            (fmap (valuePaidTo info) . toPubKeyHash $ addr)
              <|> (fmap (valueLockedBy info) . toValidatorHash $ addr)

      (Minting cs, MintNitroToken i) ->
        traceIfFalse "admin token not present" inputContainsAdminToken
          && traceIfFalse "wrong amount minted" (mintedNitroToken i)

      (Spending _, SetNitroState ns) ->
        traceIfFalse "Admin token not present" inputContainsAdminToken
          && traceIfFalse "game state invalid: " (setsNitroStateTo ns)
        where
          outputsLockedByTheScript :: [(OutputDatum, Value)]
          outputsLockedByTheScript = scriptOutputsAt (ownHash ctx) info

          setsNitroStateTo :: NitroState -> Bool
          setsNitroStateTo gs =
            case filter (\(odat, val) -> val `geq` stateTokenValue) outputsLockedByTheScript of
              [(OutputDatum odat, val)] ->
                traceIfFalse "game state is not equal to state provided by redeemer" $
                  getDatum odat == toBuiltinData gs
              _ -> traceError "game state not set"
      _ -> traceError "unexpected script purpose"
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    stateTokenValue :: Value
    stateTokenValue = assetClassValue (stateToken gsp) 1

    inputContainsAdminToken :: Bool
    inputContainsAdminToken = valueSpent info `geq` assetClassValue (adminToken gsp) 1

    nitroAssetClass :: AssetClass
    nitroAssetClass = assetClass (ownCurrencySymbol ctx) (nitroToken gsp)

    mintedNitroToken :: Integer -> Bool
    mintedNitroToken i = i == assetClassValueOf (valueProduced info) nitroAssetClass

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
