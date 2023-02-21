{-# LANGUAGE TemplateHaskell #-}

module GameAssetPolicy (script) where

import GHC.Generics (Generic)
import GHC.Show (Show)
import Ledger (AssetClass)
import Ledger.Value (assetClass, assetClassValue, geq)
import Plutus.V2.Ledger.Api (
  Address,
  Datum (getDatum),
  FromData (fromBuiltinData),
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (scriptContextTxInfo),
  TokenName (TokenName),
  TxInInfo (txInInfoOutRef, txInInfoResolved),
  TxInfo (txInfoInputs),
  TxOut (txOutDatum),
  TxOutRef,
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol, valueProduced, valueSpent)
import PlutusTx qualified (compile, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.Prelude
import Utils (valueToAddr)

data Driver = Driver
  { driverId :: BuiltinByteString
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

data GameAssetPolicyParams = GameAssetPolicyParams
  { adminToken :: AssetClass
  , botToken :: AssetClass
  , asset :: GameAsset
  }
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''GameAssetPolicyParams

newtype GameAssetPolicyDatum = GameAssetPolicyDatum
  {airdropAddress :: Address}
PlutusTx.unstableMakeIsData ''GameAssetPolicyDatum

{-# INLINEABLE mkGameAssetPolicy #-}
mkGameAssetPolicy :: TxOutRef -> GameAssetPolicyParams -> ScriptContext -> Bool
mkGameAssetPolicy oref gapp ctx =
  ( traceIfFalse "input does not contain admin token" inputContainsAdminNft
      || traceIfFalse "input does not contain bot token" inputContainsBotNft
  )
    && traceIfFalse "does not spend parameter TxOutRef" (isJust paramTxo)
    && traceIfFalse "does not mint asset NFT" mintsAssetNft
    && traceIfFalse "doesn't send nft to airdrop address" paysNftToAirdrop
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    paramTxo :: Maybe TxOut
    paramTxo = fmap txInInfoResolved $ find ((== oref) . txInInfoOutRef) $ txInfoInputs info

    inputContainsAdminNft :: Bool
    inputContainsAdminNft = valueSpent info `geq` assetClassValue (adminToken gapp) 1

    inputContainsBotNft :: Bool
    inputContainsBotNft = valueSpent info `geq` assetClassValue (botToken gapp) 1

    nftTokenName :: TokenName
    nftTokenName = TokenName $ case asset gapp of
      DriverAsset d -> driverId d
      CarAsset c -> carId c

    nftAssetClass :: AssetClass
    nftAssetClass = assetClass (ownCurrencySymbol ctx) nftTokenName

    paysNftToAirdrop :: Bool
    paysNftToAirdrop = fromMaybe False $ do
      ptxo <- paramTxo
      gapd <- case txOutDatum ptxo of
        OutputDatum d ->
          maybe (trace "failed to decode game asset policy datum" Nothing) pure $
            fromBuiltinData @GameAssetPolicyDatum $
              getDatum d
        _ -> trace "failed to get txo inline datum containing airdrop address" Nothing
      v <-
        maybe (trace "failed to get value paid to airdrop address" Nothing) pure $
          valueToAddr info (airdropAddress gapd)
      pure $ v `geq` assetClassValue nftAssetClass 1

    -- todo: don't use valueProduced, use txInfoMint
    mintsAssetNft :: Bool
    mintsAssetNft = valueProduced info == assetClassValue nftAssetClass 1

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy oref gapp _redeemer context =
  let
    result =
      mkGameAssetPolicy
        (PlutusTx.unsafeFromBuiltinData oref)
        (PlutusTx.unsafeFromBuiltinData gapp)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $$(PlutusTx.compile [||mkPolicy||])
