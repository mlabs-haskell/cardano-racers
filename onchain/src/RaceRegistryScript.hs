{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-all -fno-specialise #-}

module RaceRegistryScript (script, r, rr) where

import CommonTypes (RacersParams (RacersParams), adminToken, botToken)
import Constants (nitroTokenName)
import Data.Function (on)
import GHC.Generics
import GHC.Show (Show)
import Plutonomy qualified (optimizeUPLC)
import Plutus.V1.Ledger.Value (AssetClass, assetClass, assetClassValue, assetClassValueOf, geq, mpsSymbol)
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
  txInfoMint,
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
  { slotAssetClass :: AssetClass
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
        && traceIfFalse "existing regitry entries must not be altered" doesNotAlterExistingEntries
        && traceIfFalse "only one new signer entry must be added" addsPkhsToRegistry
        && traceIfFalse "does not burn enought NITRO" burnsNitroPerSlotPurchased
      where
        (removedEntries, _, addedEntries) = diffedRegistry

        -- When removedEntries is empty, the input registry is a subset of the output registry and so the registry is unaltered
        doesNotAlterExistingEntries :: Bool
        doesNotAlterExistingEntries = null removedEntries

        addsPkhsToRegistry :: Bool
        addsPkhsToRegistry = all aux addedEntries
          where
            aux (PendingSelection pkh) = pkh `elem` pkhs
            aux _ = False -- Only pending selections can be added
        burnsNitroPerSlotPurchased :: Bool
        burnsNitroPerSlotPurchased = assetClassValueOf (txInfoMint info) (assetClass (mpsSymbol nitroPolicyHash) nitroTokenName) <= totalNitro
          where
            totalNitro = nitroFee * length addedEntries
    SelectAssets ->
      traceIfFalse "value at registry not conserved" valueAtRegistryIsConserved
        && traceIfFalse "not signed by all removed pending pubkey hashes" signedByRemovedEntries
        && traceIfFalse "input does not contain all selected assets " inputContainsAssetSelections
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
              spentValue `geq` ((<>) `on` (flip assetClassValue 1 . assetClass gameAssetSymbol)) car driver
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
      !singletonSlot = assetClassValue slotAssetClass 1

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

      getTxoRegistry :: TxOut -> [RegistryEntry]
      getTxoRegistry txo = case getInlineDatumFromTxOut txo of
        Just (RegistryDatum registry) -> registry
        Nothing -> traceError "could not get registry from txo inline datum"

{-# INLINEABLE diffDatas #-}
diffDatas :: ToData a => [a] -> [a] -> ([a], [a], [a])
diffDatas as bs =
  go (sorted as) (sorted bs) [] [] []
  where
    sorted = sortBy (compare `on` snd) . map (\x -> (x, serialiseData (toBuiltinData x)))

    go [] [] left commons right = (reverse left, reverse commons, reverse right)
    go xs [] left commons right = (reverse left ++ map fst xs , reverse commons, reverse right)
    go [] ys left commons right = (reverse left, reverse commons, reverse right ++ map fst ys)
    go ((x, xSerialized) : xs) ((y, ySerialized) : ys) left commons right
      | xSerialized < ySerialized = go xs ((y, ySerialized) : ys) (x : left) commons right
      | xSerialized > ySerialized = go ((x, xSerialized) : xs) ys left commons (y : right)
      | otherwise = go xs ys left (x : commons) right

tests :: [(([Integer],[Integer]), ([Integer],[Integer],[Integer]))]
tests = [
    (([], []), ([], [], [])),
    (([1], [1]), ([], [1], [])),
    (([1], [2]), ([1], [], [2])),
    (([1, 2, 3], [1, 2, 3]), ([], [1, 2, 3], [])),
    (([1, 2, 3], [4, 5, 6]), ([1, 2, 3], [], [4, 5, 6])),
    (([1, 2, 3], [2, 3, 4]), ([1], [2, 3], [4])),
    (([1, 2], [1, 2, 3, 4]), ([], [1, 2], [3, 4])),
    (([1, 2, 2, 3], [2, 2, 3, 4]), ([1], [2, 2, 3], [4])),
    (([3, 2, 1], [1, 2, 3]), ([], [1, 2, 3], [])),
    (([1, 2, 3, 4, 5], [4, 5, 6, 7, 8]), ([1, 2, 3], [4, 5], [6, 7, 8])) 
    ]

rr = diffDatas @Integer [1,2,3] [4,5,6]
r = map (\((a, b), (ar, ir, br)) -> let (a',i,b') = diffDatas a b in a' == ar && i == ir && b' == br) tests


{-# INLINEABLE mkScript #-}
mkScript :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkScript racersParams registryParams _raceHash _dat red ctx =
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
