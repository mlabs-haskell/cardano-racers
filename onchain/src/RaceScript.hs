{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module RaceScript (script) where

import GHC.Generics (Generic)
import GHC.Show (Show)
import Plutonomy qualified (aggressiveOptimizerOptions, optimizeUPLCWith)
import Plutus.V1.Ledger.Value (AssetClass (AssetClass), assetClassValueOf)
import Plutus.V2.Ledger.Api (
  Address,
  CurrencySymbol,
  Map,
  POSIXTime,
  PubKeyHash,
  Script,
  ScriptContext (scriptContextTxInfo),
  TokenName,
  TxInInfo (txInInfoResolved),
  TxInfo,
  TxOut (txOutValue),
  Value,
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts (findOwnInput, getContinuingOutputs, txSignedBy)
import PlutusTx qualified (compile, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.AssocMap (elems)
import PlutusTx.Prelude
import Utils (getInlineDatumFromTxOut)

data RaceParams = RaceParams
  { stateAssetClass :: (CurrencySymbol, TokenName) 
  , totalRewardValue :: Value
  , delegates :: [PubKeyHash]
  , escrowTtl :: POSIXTime
  }
  deriving (Generic, Show)
PlutusTx.unstableMakeIsData ''RaceParams

data RaceDatum
  = ValueEscrow
  | RaceState
      { distribution :: Maybe (Map Address Value)
      }
  deriving (Generic, Show)
PlutusTx.unstableMakeIsData ''RaceDatum

data RaceRedeemer = MoveL2 | AnnounceDistribution | Distribute | ClaimTTL
  deriving (Generic, Show)
PlutusTx.unstableMakeIsData ''RaceRedeemer

{-# INLINEABLE mkRaceScript #-}
mkRaceScript :: RaceParams -> RaceDatum -> RaceRedeemer -> ScriptContext -> Bool  
mkRaceScript RaceParams{stateAssetClass, totalRewardValue, delegates} dat red ctx =
  case dat of
    RaceState {distribution=distr} ->
      case red of
        MoveL2 ->
          traceIfFalse "must be signed by all delegates" txSignedByDelegates
            && traceIfFalse "distribution already announced" (currentDistributionUndefined distr)
            && traceIfFalse "state token missing" stateTokenPresent
        AnnounceDistribution ->
          traceIfFalse "must be signed by all delegates" txSignedByDelegates
            && traceIfFalse "distribution already announced" (currentDistributionUndefined distr)
            && traceIfFalse "total reward value not conserved in distribution" totalRewardValueDistributed
            && traceIfFalse "continuing value not conserved" valueConserved 
            && traceIfFalse "state token missing" stateTokenPresent
            -- TODO: check participants
            -- TODO: check value escrow
        Distribute ->
          traceError "not implemented"
        ClaimTTL ->
          -- TODO: can be cleaned up by admin or bot after ttl
          traceError "not implemented"
    ValueEscrow ->
      case red of
        Distribute ->
          -- TODO: check presence of RaceState input (delegate validation)
          traceError "not implemented"
        ClaimTTL ->
          -- TODO: can only be claimed by admin or bot
          -- TODO: check ttl
          traceError "not implemented"
        _ -> traceError "incompatible redeemer"
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    -- The transaction must be signed by all delegates.
    txSignedByDelegates :: Bool
    txSignedByDelegates = all (txSignedBy txInfo) delegates

    -- The distribution can only be announced once and cannot be changed afterwards.
    currentDistributionUndefined :: Maybe (Map Address Value) -> Bool
    currentDistributionUndefined = isNothing

    -- The combined distributed value must be equal to the total reward value.
    totalRewardValueDistributed :: Bool
    totalRewardValueDistributed =
      case snd newOutputWithDatum of
        RaceState {distribution=Just distr} -> fold (elems distr) == totalRewardValue
        _ -> False

    valueConserved :: Bool
    valueConserved = txOutValue (fst newOutputWithDatum) == ownInputValue

    stateTokenPresent :: Bool
    stateTokenPresent = assetClassValueOf ownInputValue (AssetClass stateAssetClass) == 1

    ownInputValue :: Value
    ownInputValue = txOutValue ownInput

    ownInput :: TxOut
    ownInput =
      maybe (traceError "could not get own input") txInInfoResolved $
        findOwnInput ctx

    newOutputWithDatum :: (TxOut, RaceDatum)
    newOutputWithDatum =
      case getContinuingOutputs ctx of
        [ txOut ] ->
          case getInlineDatumFromTxOut txOut of
            Just raceDatum -> (txOut, raceDatum)
            Nothing -> traceError "invalid datum in continuing output"
        _ -> traceError "multiple continuing outputs"

{-# INLINEABLE mkScript #-}
mkScript :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkScript params dat red ctx =
  let
    result =
      mkRaceScript
        (PlutusTx.unsafeFromBuiltinData params)
        (PlutusTx.unsafeFromBuiltinData dat)
        (PlutusTx.unsafeFromBuiltinData red)
        (PlutusTx.unsafeFromBuiltinData ctx)
   in
    if result then () else traceError "Failed verification"

script :: Script
script =
  fromCompiledCode $
    Plutonomy.optimizeUPLCWith Plutonomy.aggressiveOptimizerOptions
      $$(PlutusTx.compile [||mkScript||])
