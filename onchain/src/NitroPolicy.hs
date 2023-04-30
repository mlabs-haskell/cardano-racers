{-# LANGUAGE TemplateHaskell #-}
-- | A minting policy for NITRO
module NitroPolicy (script) where

import PlutusTx.Prelude

import Utils (distributesToAddrs, findCurrentGameStateFromRefInputs)

import CommonTypes (RacersParams, RacersState, adminToken, botToken, nitroPrice, nitroToken, stateToken)
import GHC.Generics (Generic)
import GHC.Show (Show)
import Ledger (AssetClass)
import Ledger.Value (assetClass, assetClassValue, assetClassValueOf, geq)
import Plutonomy qualified (optimizeUPLC)
import Plutus.V2.Ledger.Api (
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInfo (txInfoMint),
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol, valueSpent)
import PlutusTx qualified (compile, unsafeFromBuiltinData, unstableMakeIsData)

data NitroPolicyRedeemer
  = MintNitroToken Integer
  | BuyNitroToken Integer
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''NitroPolicyRedeemer

{-# INLINEABLE mkNitroMintiingPolicy #-}
mkNitroMintiingPolicy :: RacersParams -> NitroPolicyRedeemer -> ScriptContext -> Bool
mkNitroMintiingPolicy nsp red ctx = case red of
  MintNitroToken i ->
    ( traceIfFalse "admin token not present" inputContainsAdminNft
        || traceIfFalse "bot token not present" inputContainsBotNft
    )
      && traceIfFalse "wrong amount minted" (mintedNitroToken i)
  BuyNitroToken i ->
    traceIfFalse "wrong ada value sent to treasury and operating" (sendsAdaToCorrectAddrs i)
      && traceIfFalse "minted amount is less than or equal to 0" (i > 0)
      && traceIfFalse "wrong amount minted" (mintedNitroToken i)
    where
      currentStateFromRefInput :: Maybe RacersState
      currentStateFromRefInput = findCurrentGameStateFromRefInputs info (stateToken nsp)

      sendsAdaToCorrectAddrs :: Integer -> Bool
      sendsAdaToCorrectAddrs mintedAmount = fromMaybe False $ do
        gameState <- currentStateFromRefInput
        let totalLovelace = mintedAmount * nitroPrice gameState
        pure $ distributesToAddrs info gameState totalLovelace
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    inputContainsAdminNft :: Bool
    inputContainsAdminNft = valueSpent info `geq` assetClassValue (adminToken nsp) 1

    inputContainsBotNft :: Bool
    inputContainsBotNft = valueSpent info `geq` assetClassValue (botToken nsp) 1

    nitroAssetClass :: AssetClass
    nitroAssetClass = assetClass (ownCurrencySymbol ctx) (nitroToken nsp)

    mintedNitroToken :: Integer -> Bool
    mintedNitroToken i = i == assetClassValueOf (txInfoMint info) nitroAssetClass

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

script :: Script
script = fromCompiledCode $ Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkPolicy||])
