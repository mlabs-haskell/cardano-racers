{-# LANGUAGE TemplateHaskell #-}

-- {-# OPTIONS_GHC -w #-}

module NitroPolicy (nitroPolicyScript, nitroStateValidatorScript) where

import PlutusTx.Prelude

import Utils (valueToAddr)

import GHC.Generics (Generic)
import GHC.Show (Show)
import Ledger (Address, AssetClass, Datum (getDatum))
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
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol, ownHash, scriptOutputsAt, valueProduced, valueSpent)
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
  -- ^ Admin NFT AssetClass that allows free minting and state modification
  , botToken :: AssetClass
  -- ^ Bot NFT AssetClass that allows bot to mint Nitro tokens only
  , stateToken :: AssetClass
  -- ^ State NFT AssetClass that reprensents the current NitroState
  -- | see https://github.com/Plutonomicon/plutonomicon/blob/main/statethread.md
  , nitroToken :: TokenName
  -- ^ TokenName of Nitro token
  }
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''NitroScriptParams

newtype NitroStateRedeemer = SetNitroState NitroState
PlutusTx.unstableMakeIsData ''NitroStateRedeemer

{-# INLINEABLE mkNitroStateValidator #-}
mkNitroStateValidator :: NitroScriptParams -> NitroStateRedeemer -> ScriptContext -> Bool
mkNitroStateValidator nsp (SetNitroState ns) ctx =
  traceIfFalse "Admin token not present" inputContainsAdminNft
    && traceIfFalse "game state invalid: " (setsNitroStateTo ns)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    stateNftValue :: Value
    stateNftValue = assetClassValue (stateToken nsp) 1

    inputContainsAdminNft :: Bool
    inputContainsAdminNft = valueSpent info `geq` assetClassValue (adminToken nsp) 1

    outputsLockedByTheScript :: [(OutputDatum, Value)]
    outputsLockedByTheScript = scriptOutputsAt (ownHash ctx) info

    -- This will ensure that state is set to expected value and that stateNft
    -- is re-locked at the script
    setsNitroStateTo :: NitroState -> Bool
    setsNitroStateTo gs =
      case filter (\(_, val) -> val `geq` stateNftValue) outputsLockedByTheScript of
        [(OutputDatum odat, _)] ->
          traceIfFalse "game state is not equal to state provided by redeemer" $
            getDatum odat == toBuiltinData gs
        [(_, _)] -> traceError "game state datum must be inline"
        [] -> traceError "game state is not re-locked at the script"
        _ -> traceError "unexpected game state output"

data Driver = Driver 
      { driveId :: BuiltinByteString
      , aggression :: Integer
      , experience :: Integer
      , reflexes :: Integer
      , luck :: Integer
      }
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''Driver

data Car = Car 
      { carId :: BuiltinByteString
      , topSpeed :: Integer
      , acceleration :: Integer
      , cornering :: Integer
      , aerodynamics :: Integer
      }
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''Car

data GameAsset
  = DriverAsset Driver
  | CarAsset Car
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''GameAsset

data NitroPolicyRedeemer
  = MintNitroToken Integer
  | MintGameAsset GameAsset
  | BuyNitroToken Integer
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''NitroPolicyRedeemer

{-# INLINEABLE mkNitroMintiingPolicy #-}
mkNitroMintiingPolicy :: NitroScriptParams -> NitroPolicyRedeemer -> ScriptContext -> Bool
mkNitroMintiingPolicy nsp red ctx = case red of
  MintGameAsset a -> ( traceIfFalse "admin token not present" inputContainsAdminNft
                    || traceIfFalse "bot token not present" inputContainsBotNft)
                    && traceIfFalse "wrong asset minted" (mintedGameAsset a)
    where
      mintedGameAsset :: GameAsset -> Bool
      mintedGameAsset _ = True
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

      currentStateFromRefInput :: Maybe NitroState
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
