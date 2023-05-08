{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-all -fno-specialise #-}

module RaceRegistryScript (script) where

import CommonTypes (RacersParams (RacersParams), adminToken, botToken)
import Constants (nitroTokenName)
import Data.Function (on)
import GHC.Generics
import GHC.Show (Show)
import Plutonomy qualified (optimizeUPLC)
import Plutus.V1.Ledger.Value (AssetClass(AssetClass), assetClass, assetClassValue, assetClassValueOf, geq, mpsSymbol)
import Plutus.V2.Ledger.Api (
  Address,
  MintingPolicyHash,
  PubKeyHash,
  Script,
  ScriptContext,
  ToData (toBuiltinData),
  TokenName,
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoInputs),
  TxOut (txOutAddress),
  Value,
  fromCompiledCode,
  scriptContextTxInfo,
  txInfoMint, CurrencySymbol
 )
import Plutus.V2.Ledger.Contexts (findOwnInput, getContinuingOutputs, txSignedBy, valueSpent)
import Plutus.V2.Ledger.Tx (TxOut (txOutValue))
import PlutusTx qualified (compile, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.Builtins (serialiseData)
import PlutusTx.Prelude
import Utils (getInlineDatumFromTxOut)

data RaceParticipant = RaceParticipant
  { car :: TokenName
  , driver :: TokenName
  , payoutAddress :: Address
  }
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''RaceParticipant

instance Eq RaceParticipant where
  RaceParticipant car1 driver1 payoutAddress1 == RaceParticipant car2 driver2 payoutAddress2 =
    car1 == car2 && driver1 == driver2 && payoutAddress1 == payoutAddress2

data RegistryEntry = PendingSelection PubKeyHash | AssetSelection RaceParticipant
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''RegistryEntry

instance Eq RegistryEntry where
  PendingSelection pkh1 == PendingSelection pkh2 = pkh1 == pkh2
  AssetSelection rp1 == AssetSelection rp2 = rp1 == rp2
  _ == _ = False

newtype RegistryDatum = RegistryDatum [RegistryEntry]
PlutusTx.unstableMakeIsData ''RegistryDatum

data RegistryRedeemer = Enroll [PubKeyHash] | SelectAssets | Collect
PlutusTx.unstableMakeIsData ''RegistryRedeemer

data RegistryParams = RegistryParams
  { slotAssetClass :: (CurrencySymbol, TokenName)
  -- ^ can't reuse tokens across races, if that's desired an additional raceHash parameters should be included to ensure uniqueness
  , nitroPolicyHash :: MintingPolicyHash
  , gameAssetPolicyHash :: MintingPolicyHash
  , nitroFee :: Integer
  }
PlutusTx.unstableMakeIsData ''RegistryParams

{-# INLINEABLE mkRegistryScript #-}
mkRegistryScript :: RacersParams -> RegistryParams -> RegistryRedeemer -> ScriptContext -> Bool
mkRegistryScript
  RacersParams {adminToken, botToken}
  RegistryParams {slotAssetClass, nitroPolicyHash, gameAssetPolicyHash, nitroFee}
  red
  ctx = case red of
    Collect ->
      traceIfFalse "admin token not present" inputContainsAdminNft
        || traceIfFalse "bot token not present" inputContainsBotNft
      where
        inputContainsAdminNft :: Bool
        inputContainsAdminNft = valueSpent info `geq` assetClassValue adminToken 1

        inputContainsBotNft :: Bool
        inputContainsBotNft = valueSpent info `geq` assetClassValue botToken 1

    Enroll pkhs ->
      traceIfFalse "value at registry not conserved" valueAtRegistryIsConserved
        && traceIfFalse "existing registry entries must not be altered" doesNotAlterExistingEntries
        && traceIfFalse "only one new signer entry must be added" addsPkhsToRegistry
        && traceIfFalse "does not burn enough NITRO" burnsNitroPerSlotPurchased
        && traceIfFalse "registry entries don't exceed slot counts per output" outputsWithRegistryDatumDontExceedSlotCounts
      where
        (removedEntries, commonEntries, addedEntries) = diffedRegistry

        -- When removedEntries is empty, the input registry is a subset of the output registry and so the registry is unaltered
        doesNotAlterExistingEntries :: Bool
        doesNotAlterExistingEntries = null removedEntries

        addsPkhsToRegistry :: Bool
        addsPkhsToRegistry = all aux addedEntries
          where
            aux (PendingSelection pkh) = pkh `elem` pkhs -- this check is possible not needed, Enroll can be modified to take no constructor params
            aux _ = False -- Only pending selections can be added

        burnsNitroPerSlotPurchased :: Bool
        burnsNitroPerSlotPurchased = totalNitroBurnt >= requiredNitroBurnt
          where
            requiredNitroBurnt = nitroFee * length addedEntries
            totalNitroBurnt = assetClassValueOf (negate $ txInfoMint info) (assetClass (mpsSymbol nitroPolicyHash) nitroTokenName)

    SelectAssets ->
      traceIfFalse "value at registry not conserved" valueAtRegistryIsConserved
        && traceIfFalse "not signed by all removed pending pubkey hashes" signedByRemovedEntries
        -- && traceIfFalse "does not have 1 to 1 mapping of entries removed and added" (length removedEntries == length addedEntries)
        && traceIfFalse "input does not contain all selected assets " inputContainsAssetSelections
        && traceIfFalse "registry entries exceed slot counts per output" outputsWithRegistryDatumDontExceedSlotCounts
      where
        (removedEntries, _, addedEntries) = diffedRegistry

        signedByRemovedEntries :: Bool
        signedByRemovedEntries = all aux removedEntries
          where
            aux (PendingSelection pkh) = txSignedBy info pkh
            aux _ = False -- Only pending selections can be removed
        inputContainsAssetSelections :: Bool
        inputContainsAssetSelections = all aux addedEntries
          where
            spentValue = valueSpent info
            gameAssetSymbol = mpsSymbol gameAssetPolicyHash
            aux (AssetSelection RaceParticipant {car, driver}) =
              spentValue `geq` assetClassValue (assetClass gameAssetSymbol car) 1
              -- ((<>) `on` (flip assetClassValue 1 . assetClass gameAssetSymbol)) car driver
            aux _ = False
    where
      info :: TxInfo
      !info = scriptContextTxInfo ctx

      inputBeingValidated :: TxOut
      !inputBeingValidated = maybe (traceError "could not get own input") txInInfoResolved $ findOwnInput ctx

      ownAddress :: Address
      !ownAddress = txOutAddress inputBeingValidated

      ownInputs :: [TxOut]
      !ownInputs = filter ((== ownAddress) . txOutAddress) $ map txInInfoResolved $ txInfoInputs info

      singletonSlot :: Value
      !singletonSlot = assetClassValue (AssetClass slotAssetClass) 1

      valueAtRegistryIsConserved :: Bool
      !valueAtRegistryIsConserved = foldMap txOutValue (getContinuingOutputs ctx) `geq` foldMap txOutValue ownInputs

      !inputRegistryEntries = foldMap snd inputRegistry
      !outputRegistryEntries = foldMap snd outputRegistry

      inputRegistry :: [(TxOut, [RegistryEntry])]
      !inputRegistry = map (\txo -> (txo, getTxoRegistry txo)) $ filter ((`geq` singletonSlot) . txOutValue) ownInputs

      outputRegistry :: [(TxOut, [RegistryEntry])]
      !outputRegistry = map (\txo -> (txo, getTxoRegistry txo)) $ filter ((`geq` singletonSlot) . txOutValue) $ getContinuingOutputs ctx

      diffedRegistry :: ([RegistryEntry], [RegistryEntry], [RegistryEntry])
      !diffedRegistry = diffDatas inputRegistryEntries outputRegistryEntries

      -- previous checks operate on total input and output registry entries, this check ensures that an attacker can't move datums around in such a way to break the correspondence between locked slot tokens and attached entries per utxo
      outputsWithRegistryDatumDontExceedSlotCounts :: Bool
      !outputsWithRegistryDatumDontExceedSlotCounts = all aux outputRegistry
        where
          aux (txo, registry) = assetClassValueOf (txOutValue txo) (AssetClass slotAssetClass) >= length registry

      getTxoRegistry :: TxOut -> [RegistryEntry]
      getTxoRegistry txo = case getInlineDatumFromTxOut txo of
        Just (RegistryDatum registry) -> registry
        Nothing -> traceError "could not get registry from txo inline datum"

{-# INLINEABLE diffDatas #-}
diffDatas :: ToData a => [a] -> [a] -> ([a], [a], [a])
diffDatas as bs =
  go (sorted as) (sorted bs) [] [] []
  where
    -- Hacky sort leveraging Ord on BuiltinByteString on RegistryEntry. This is
    -- done as deriving/implementing Ord seems to be broken
    -- TODO: find out how to derive/implement Ord on the custom data types
    sorted = sortBy (compare `on` snd) . map (\x -> (x, serialiseData (toBuiltinData x)))

    go [] [] left commons right = (reverse left, reverse commons, reverse right)
    go xs [] left commons right = (reverse left ++ map fst xs , reverse commons, reverse right)
    go [] ys left commons right = (reverse left, reverse commons, reverse right ++ map fst ys)
    go ((x, xSerialized) : xs) ((y, ySerialized) : ys) left commons right
      | xSerialized < ySerialized = go xs ((y, ySerialized) : ys) (x : left) commons right
      | xSerialized > ySerialized = go ((x, xSerialized) : xs) ys left commons (y : right)
      | otherwise = go xs ys left (x : commons) right

{-# INLINEABLE mkScript #-}
mkScript :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkScript racersParams registryParams _dat red ctx =
  let
    result =
      mkRegistryScript
        (PlutusTx.unsafeFromBuiltinData racersParams)
        (PlutusTx.unsafeFromBuiltinData registryParams)
        (PlutusTx.unsafeFromBuiltinData red)
        (PlutusTx.unsafeFromBuiltinData ctx)
   in
    if result then () else traceError "Failed verification"

script :: Script
script = fromCompiledCode $ Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkScript||])
