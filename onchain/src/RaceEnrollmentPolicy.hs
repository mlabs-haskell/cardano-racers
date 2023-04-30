{-# LANGUAGE TemplateHaskell #-}

module RaceEnrollmentPolicy (script) where

import PlutusTx.Prelude

import CommonTypes (RacersParams (adminToken, botToken))
import GHC.Generics (Generic)
import GHC.Show (Show)
import Ledger.Value (AssetClass, TokenName (TokenName), assetClass, assetClassValue, assetClassValueOf, geq)
import Plutonomy qualified (optimizeUPLC)
import Plutus.V2.Ledger.Api (
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInfo (txInfoInputs, txInfoMint),
  fromCompiledCode, Value, CurrencySymbol, ValidatorHash, OutputDatum
 )
import Plutus.V2.Ledger.Contexts (TxInInfo (txInInfoOutRef), TxOutRef, ownCurrencySymbol, valueSpent, valueLockedBy, scriptOutputsAt)
import PlutusTx qualified (compile, unsafeFromBuiltinData, unstableMakeIsData)
import Plutus.V1.Ledger.Value (flattenValue)
import Utils (getInlineDatum)


data ConfirmedRegistrationEntry = ConfirmedRegistrationEntry
  { car :: TokenName
  , driver :: TokenName
  , payoutAddress :: TokenName
  }
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''ConfirmedRegistrationEntry

newtype ConfirmedRegistrationDatum = ConfirmedRegistrationDatum [ConfirmedRegistrationEntry]
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''ConfirmedRegistrationDatum

data EnrollmentPolicyRedeemer
  = MintInitialSlotTokens
  | ConfirmParticipation --  ConfirmedRegistrationDatum
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''EnrollmentPolicyRedeemer

{-# INLINEABLE mkEnrollmentPolicy #-}
mkEnrollmentPolicy :: TxOutRef -> RacersParams -> ValidatorHash -> ValidatorHash -> EnrollmentPolicyRedeemer -> ScriptContext -> Bool
mkEnrollmentPolicy txoref rp registryVHash confirmationVHash red ctx = case red of
  MintInitialSlotTokens ->
    ( traceIfFalse "admin token not present" inputContainsAdminNft
        || traceIfFalse "bot token not present" inputContainsBotNft
    )
      && traceIfFalse "UTxO not present in inputs" hasUtxo
      && traceIfFalse "mints only multiple slot tokens" mintsOnlySlotTokens
      && traceIfFalse "does not lock slot tokens at registry script" locksOwnMintedValueAtRegistry
    where
      hasUtxo :: Bool
      hasUtxo = elem txoref . map txInInfoOutRef $ txInfoInputs info

      inputContainsAdminNft :: Bool
      inputContainsAdminNft = valueSpent info `geq` assetClassValue (adminToken rp) 1

      inputContainsBotNft :: Bool
      inputContainsBotNft = valueSpent info `geq` assetClassValue (botToken rp) 1

      mintsOnlySlotTokens :: Bool
      mintsOnlySlotTokens = case flattenValue ownMintedValue of
        [(cs, tn, amount)] -> cs == ownSymbol && tn == slotTokenName && amount > 1
        _ -> False

      -- The script will check that the only token in own minted value is the slot token
      -- and so it is sufficient to check that the value locked by the registry script is greater than or equal to `ownMintedValue`
      locksOwnMintedValueAtRegistry :: Bool
      locksOwnMintedValueAtRegistry = valueLockedBy info registryVHash `geq` ownMintedValue

  ConfirmParticipation ->
    traceIfFalse "burns slot tokens" (burntSlotTokens > 0)
    && traceIfFalse "mismatch between slot tokens burnt and contender tokens minted" (burntSlotTokens == mintedContenderTokens)
    && traceIfFalse "mismatch between contender tokens minted vs contender tokens locked" (mintedContenderTokens == length totalValidConfirmedRegistrations)
    where
      burntSlotTokens :: Integer
      !burntSlotTokens = assetClassValueOf (negate ownMintedValue) slotTokenAssetClass

      mintedContenderTokens :: Integer
      !mintedContenderTokens = assetClassValueOf ownMintedValue contenderTokenAssetClass

      totalValidConfirmedRegistrations :: [ConfirmedRegistrationEntry]
      totalValidConfirmedRegistrations = concat $ mapMaybe parseValidConfirmationOutput $ scriptOutputsAt confirmationVHash info

      parseValidConfirmationOutput :: (OutputDatum, Value) -> Maybe [ConfirmedRegistrationEntry]
      parseValidConfirmationOutput (dat, v) = registrationEntries >>= \entries -> if length entries == contenderTokens then pure entries else Nothing
        where
          contenderTokens = assetClassValueOf v contenderTokenAssetClass
          registrationEntries :: Maybe [ConfirmedRegistrationEntry]
          registrationEntries = (\(ConfirmedRegistrationDatum entries) -> entries) <$> getInlineDatum dat


  where
    info :: TxInfo
    !info = scriptContextTxInfo ctx

    ownSymbol :: CurrencySymbol
    !ownSymbol = ownCurrencySymbol ctx

    slotTokenName :: TokenName
    !slotTokenName = TokenName "Slot"

    contenderTokenName :: TokenName
    !contenderTokenName = TokenName "Contender"

    slotTokenAssetClass :: AssetClass
    !slotTokenAssetClass = assetClass ownSymbol slotTokenName

    contenderTokenAssetClass :: AssetClass
    !contenderTokenAssetClass = assetClass ownSymbol contenderTokenName

    ownMintedValue :: Value
    !ownMintedValue = foldMap (\(cs, tk, i) -> assetClassValue (assetClass cs tk) i) $ filter (\(cs, _, _) -> cs == ownSymbol) $ flattenValue $ txInfoMint info

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPolicy utxo rp registryVHash confirmationVHash redeemer context =
  let
    result =
      mkEnrollmentPolicy
        (PlutusTx.unsafeFromBuiltinData utxo)
        (PlutusTx.unsafeFromBuiltinData rp)
        (PlutusTx.unsafeFromBuiltinData registryVHash)
        (PlutusTx.unsafeFromBuiltinData confirmationVHash)
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData context)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $ Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkPolicy||])
