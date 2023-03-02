{-# LANGUAGE TemplateHaskell #-}

-- {-# OPTIONS_GHC -w #-}

module NitroPolicy (nitroPolicyScript) where

import PlutusTx.Prelude

import Utils (valueToAddr)

import GHC.Generics (Generic)
import GHC.Show (Show)
import Ledger (AssetClass, Datum (getDatum))
import Ledger.Ada (lovelaceValueOf)
import Ledger.Value (assetClass, assetClassValue, assetClassValueOf, geq)
import Plutus.V2.Ledger.Api (
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoReferenceInputs, txInfoMint),
  TxOut (txOutDatum, txOutValue),
  Value,
  fromCompiledCode
 )
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol, valueSpent)
import PlutusTx qualified (FromData (fromBuiltinData), compile, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.Ratio (truncate)
import CommonTypes (RacersParams, RacersState, stateToken, adminToken, botToken, nitroToken, treasuryAddress, operatingAddress, nitroPrice)

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
    traceIfFalse "wrong amount spent" (sendsAdaToCorrectAddrs i)
      && traceIfFalse "minted amount is less than or equal to 0" (i > 0)
      && traceIfFalse "wrong amount minted" (mintedNitroToken i)
    where
      gameStateRefInput :: Maybe TxOut
      gameStateRefInput = find ((`geq` stateNftValue) . txOutValue) . map txInInfoResolved $ txInfoReferenceInputs info

      currentStateFromRefInput :: Maybe RacersState
      currentStateFromRefInput = do
        outDatum <- txOutDatum <$> gameStateRefInput
        dat <- case outDatum of
          OutputDatum d -> Just $ getDatum d
          _ -> Nothing
        PlutusTx.fromBuiltinData dat

      threeForths, oneForth :: Rational
      threeForths = unsafeRatio 3 4
      oneForth = unsafeRatio 1 4

      ceiling :: Rational -> Integer
      ceiling x =
        let floor = truncate x
         in if fromInteger floor == x then floor else floor + 1

      sendsAdaToCorrectAddrs :: Integer -> Bool
      sendsAdaToCorrectAddrs mintedAmount = fromMaybe False $ do
        gameState <- currentStateFromRefInput
        let totalPrice = fromInteger mintedAmount * fromInteger (nitroPrice gameState)
            treasuryValue = lovelaceValueOf . ceiling $ threeForths * totalPrice
            operatingValue = lovelaceValueOf . ceiling $ oneForth * totalPrice
        paysToTreasury <- (`geq` treasuryValue) <$> valueToAddr info (treasuryAddress gameState)
        paysToOperating <- (`geq` operatingValue) <$> valueToAddr info (operatingAddress gameState)
        combinedValueCheck <- do
          addrV <- valueToAddr info (treasuryAddress gameState)
          operV <- valueToAddr info (operatingAddress gameState)
          pure $ (addrV <> operV) `geq` (treasuryValue <> operatingValue)
        pure $
          traceIfFalse "wrong amount paid to treasury" paysToTreasury
            && traceIfFalse "wrong amount paid to operating" paysToOperating
            && traceIfFalse "wrong combined amount paid to treasury and operating" combinedValueCheck
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    stateNftValue :: Value
    stateNftValue = assetClassValue (stateToken nsp) 1

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

nitroPolicyScript :: Script
nitroPolicyScript = fromCompiledCode $$(PlutusTx.compile [||mkPolicy||])
