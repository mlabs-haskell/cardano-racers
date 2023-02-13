{-# LANGUAGE TemplateHaskell #-}

-- {-# OPTIONS_GHC -w #-}

module NitroPolicy (nitroPolicyScript, nitroStateValidatorScript) where

import PlutusTx.Prelude

import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import GHC.Show (Show)
import Ledger (Address, AssetClass, Datum (getDatum), toPubKeyHash, toValidatorHash)
import Ledger.Ada (lovelaceValueOf)
import Ledger.Value (assetClass, assetClassValue, assetClassValueOf, geq)
import Plutus.V2.Ledger.Api (
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
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol, ownHash, scriptOutputsAt, valueLockedBy, valuePaidTo, valueProduced, valueSpent)
import PlutusTx qualified (FromData (fromBuiltinData), compile, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.Ratio (truncate)

data NitroState = NitroState
  { nitroPrice :: Integer -- Nitro price in Lovelace
  , treasuryAddress :: Address
  , operatingAddress :: Address
  }
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''NitroState

data NitroScriptParams = NitroScriptParams
  { adminToken :: AssetClass
  , stateToken :: AssetClass
  , nitroToken :: TokenName
  }
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''NitroScriptParams

data NitroPolicyRedeemer
  = MintNitroToken Integer
  | BuyNitroToken Integer
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''NitroPolicyRedeemer

newtype NitroStateRedeemer = SetNitroState NitroState

PlutusTx.unstableMakeIsData ''NitroStateRedeemer

{-# INLINEABLE mkNitroStateValidator #-}
mkNitroStateValidator :: NitroScriptParams -> NitroStateRedeemer -> ScriptContext -> Bool
mkNitroStateValidator nsp (SetNitroState ns) ctx =
  traceIfFalse "Admin token not present" inputContainsAdminToken
    && traceIfFalse "state token is not locked again" stateTokenLocked
    && traceIfFalse "game state invalid: " (setsNitroStateTo ns)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    stateTokenValue :: Value
    stateTokenValue = assetClassValue (stateToken nsp) 1

    stateTokenLocked :: Bool
    stateTokenLocked = valueLockedBy info (ownHash ctx) `geq` stateTokenValue

    inputContainsAdminToken :: Bool
    inputContainsAdminToken = valueSpent info `geq` assetClassValue (adminToken nsp) 1

    outputsLockedByTheScript :: [(OutputDatum, Value)]
    outputsLockedByTheScript = scriptOutputsAt (ownHash ctx) info

    setsNitroStateTo :: NitroState -> Bool
    setsNitroStateTo gs =
      case filter (\(_, val) -> val `geq` stateTokenValue) outputsLockedByTheScript of
        [(OutputDatum odat, _)] ->
          traceIfFalse "game state is not equal to state provided by redeemer" $
            getDatum odat == toBuiltinData gs
        _ -> traceError "game state not set"

mkNitroMintiingPolicy :: NitroScriptParams -> NitroPolicyRedeemer -> ScriptContext -> Bool
mkNitroMintiingPolicy nsp red ctx = case red of
  MintNitroToken i ->
    traceIfFalse "admin token not present" inputContainsAdminToken
      && traceIfFalse "wrong amount minted" (mintedNitroToken i)
  BuyNitroToken i ->
    traceIfFalse "wrong amount spent" (sendsAdaToCorrectAddrs i)
      && traceIfFalse "minted amount is less than or equal to 0" (i > 0)
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

      ceiling :: Rational -> Integer
      ceiling x =
        let floor = truncate x
         in if fromInteger floor == x then floor else floor + 1

      sendsAdaToCorrectAddrs :: Integer -> Bool
      sendsAdaToCorrectAddrs mintedAmount = fromMaybe False $ do
        gameState <- currentStateFromRefInput
        let totalPrice = fromInteger mintedAmount * fromInteger (nitroPrice gameState)
            treasuryValue = lovelaceValueOf . ceiling $ unsafeRatio 3 4 * totalPrice
            operatingValue = lovelaceValueOf . ceiling $ unsafeRatio 1 4 * totalPrice
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
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    stateTokenValue :: Value
    stateTokenValue = assetClassValue (stateToken nsp) 1

    inputContainsAdminToken :: Bool
    inputContainsAdminToken = valueSpent info `geq` assetClassValue (adminToken nsp) 1

    nitroAssetClass :: AssetClass
    nitroAssetClass = assetClass (ownCurrencySymbol ctx) (nitroToken nsp)

    mintedNitroToken :: Integer -> Bool
    mintedNitroToken i = i == assetClassValueOf (valueProduced info) nitroAssetClass

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy nsp redeemer context =
  let
    result =
      mkNitroMintiingPolicy
        (PlutusTx.unsafeFromBuiltinData nsp)
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator nsp _datum redeemer context =
  let
    result =
      mkNitroStateValidator
        (PlutusTx.unsafeFromBuiltinData nsp)
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

nitroPolicyScript :: Script
nitroPolicyScript = fromCompiledCode $$(PlutusTx.compile [||mkPolicy||])

nitroStateValidatorScript :: Script
nitroStateValidatorScript = fromCompiledCode $$(PlutusTx.compile [||mkValidator||])
