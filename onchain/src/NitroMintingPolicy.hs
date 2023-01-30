{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

module NitroMintingPolicy (script) where

import PlutusTx.Prelude

import GHC.Generics (Generic)
import GHC.Show (Show)
import Ledger (Address, AssetClass, CurrencySymbol, Datum (getDatum), PaymentPubKeyHash (unPaymentPubKeyHash))
import Ledger.Value (assetClassValue, assetClass, flattenValue, geq)
import Plutus.V2.Ledger.Api (CurrencySymbol (CurrencySymbol), FromData (fromBuiltinData), OutputDatum (OutputDatum), Script, ScriptContext (scriptContextTxInfo), ToData (toBuiltinData), TokenName (TokenName), TxInfo, Value, fromCompiledCode, getPubKeyHash, txInInfoOutRef, txInfoInputs, txInfoMint)
import Plutus.V2.Ledger.Contexts (ownHash, scriptOutputsAt, txSignedBy, valueLockedBy, valueSpent, ownCurrencySymbol,  valueProduced)
import PlutusTx qualified (compile, makeLift, unsafeFromBuiltinData, unstableMakeIsData)

data GameState = GameState
    { gameTokenPrice :: Integer -- Nitro price in Ada
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
mkPolicy gsp dat red ctx = case red of
    SetGameState gs -> traceIfFalse "Admin token not present" inputContainsAdminToken
                && traceIfFalse "game state token not spent" inputContainsStateToken
                && traceIfFalse "state token is not preserved" stateTokenPreserved
                && traceIfFalse "game state not set" (setsGameStateTo gs)
    MintGameToken i -> traceIfFalse "admin token not present" inputContainsAdminToken
                    && traceIfFalse "" (mintedGameToken i)
                    && traceIfFalse "" stateTokenPreserved
    BuyGameToken i -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    outputsLockedByTheScript :: [(OutputDatum, Value)]
    outputsLockedByTheScript = scriptOutputsAt (ownHash ctx) info

    setsGameStateTo :: GameState -> Bool
    setsGameStateTo gs =
        case filter (\(odat, val) -> val `geq` stateTokenValue) outputsLockedByTheScript of
            [(OutputDatum odat, val)] -> (getDatum odat) == toBuiltinData gs
            _ -> False

    inputContainsValue :: Value -> Bool
    inputContainsValue v = valueSpent info `geq` v

    mintedGameToken :: Integer -> Bool
    mintedGameToken i = valueProduced info `geq`  gameTokenValue i 

    gameTokenValue :: Integer -> Value
    gameTokenValue amt = assetClassValue  (assetClass (ownCurrencySymbol ctx) (gameToken gsp)) amt

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
