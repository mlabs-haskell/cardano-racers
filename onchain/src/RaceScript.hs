{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module RaceScript (script) where

import CommonTypes (RacersParams (adminToken, botToken, stateToken), RacersState)
import Constants (raceStateTokenName, valueEscrowTokenName)
import GHC.Generics (Generic)
import GHC.Show (Show)
import Plutonomy qualified (aggressiveOptimizerOptions, optimizeUPLCWith)
import Plutus.V1.Ledger.Interval (before)
import Plutus.V1.Ledger.Value qualified as Value (singleton)
import Plutus.V1.Ledger.Value (
  AssetClass (AssetClass),
  adaSymbol,
  adaToken,
  assetClassValue,
  assetClassValueOf,
  geq,
  valueOf
 )
import Plutus.V2.Ledger.Api (
  Address,
  CurrencySymbol,
  Map,
  POSIXTime,
  PubKeyHash,
  Redeemer (Redeemer),
  Script,
  ScriptContext (scriptContextTxInfo),
  ScriptPurpose (Spending),
  TokenName,
  TxInInfo (txInInfoOutRef, txInInfoResolved),
  TxInfo (txInfoInputs, txInfoOutputs, txInfoRedeemers, txInfoValidRange),
  TxOut (txOutAddress, txOutValue),
  TxOutRef,
  Value,
  fromCompiledCode,
  toBuiltinData,
 )
import Plutus.V2.Ledger.Contexts (findOwnInput, txSignedBy, valueSpent)
import PlutusTx qualified (compile, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.AssocMap (elems, keys, lookup, toList)
import PlutusTx.Prelude hiding (toList)
import Utils (distributesToAddrs, findCurrentGameStateFromRefInputs, getInlineDatumFromTxOut)
import Plutus.V1.Ledger.Address (toPubKeyHash)

data RaceParams = RaceParams
  { stateCurrencySymbol :: CurrencySymbol 
  , totalRewardValue :: Value
  , participants :: [Address]
  , delegates :: [PubKeyHash]
  , escrowTtl :: POSIXTime
  , feePerDelegate :: Maybe Value 
  }
  deriving (Generic, Show)
PlutusTx.unstableMakeIsData ''RaceParams

data RaceDatum
  = ValueEscrow
  | RaceState
      { distribution :: Maybe (Map Address Value)
      }
  | TokenBin
  deriving (Generic, Show)
PlutusTx.unstableMakeIsData ''RaceDatum

data RaceRedeemer
  = MoveL2
  | DistributeRewards
  | ClaimTTL
  | CleanupUsedTokens
  deriving (Generic, Show)
PlutusTx.unstableMakeIsData ''RaceRedeemer

-- TODO: optimize error messages, use codes
-- TODO: extract separate functions for each sub-validator

{-# INLINEABLE mkRaceScript #-}
mkRaceScript :: RacersParams -> RaceParams -> RaceDatum -> RaceRedeemer -> ScriptContext -> Bool  
mkRaceScript rp params dat red ctx =
  case dat of
    RaceState {distribution=mDistr} ->
      case red of
        -- The MoveL2 redeemer should be used both to commit a UTxO from
        -- the mainchain to the Hydra Head and to announce reward distribution.
        -- Introducing a separate AnnounceDistribution redeemer would be
        -- excessive and counterproductive, as its additional checks could be
        -- bypassed by using the MoveL2 redeemer instead. Therefore, it is
        -- preferable to incorporate these additional checks into the
        -- DistributeRewards redeemer.
        MoveL2 ->
          traceIfFalse "must be signed by all delegates" txSignedByDelegates
            && traceIfFalse "distribution already announced" (rewardDistributionNotAnnounced mDistr)
            && traceIfFalse "state token missing in validated input" ownInputContainsRaceStateToken

        -- Reward distribution is permissionless, but if performed
        -- automatically, introducing a bot priority interval may be necessary
        -- to avoid race conditions.
        --
        -- We don't actually care where the distributed Value comes from.
        DistributeRewards ->
          case mDistr of
            Nothing ->
              traceError "missing distribution"
            Just distr ->
              and
                [ traceIfFalse "state token missing in validated input" ownInputContainsRaceStateToken
                , traceIfFalse "invalid combined distributed value" $ totalRewardValueDistributed distr
                , traceIfFalse "unexpected addresses in distribution" $ rewardsDistributedToRaceParticipants distr
                , traceIfFalse "rewards not sent to race participants" $ rewardsSentToRaceParticipants distr
                , traceIfFalse "hosting fees not sent to delegates" feesSentToDelegates 
                , traceIfFalse "locked ada not returned to admin" lockedAdaReturnedToAdmin
                , traceIfFalse "state token not disposed correctly" raceStateTokenSentToTokenBin 
                ]

        ClaimTTL ->
          traceIfFalse "escrow ttl not expired" afterEscrowTtl
            && traceIfFalse "locked ada not returned to admin" lockedAdaReturnedToAdmin
            && traceIfFalse "state token not disposed correctly" raceStateTokenSentToTokenBin

        _ ->
          traceError "incompatible redeemer"

    ValueEscrow ->
      case red of 
        -- Delegate validation to the DistributeRewards sub-validator of the
        -- corresponding input with RaceState datum.
        DistributeRewards ->
          case findUniqueRaceStateInput of
            Nothing ->
              traceError "race state input missing or not unique"
            Just raceStateInput ->
              -- This check is not strictly necessary beyond improving on-chain
              -- discoverability.
              traceIfFalse "state token missing in validated input" ownInputContainsValueEscrowToken
                && traceIfFalse "state token not disposed correctly" valueEscrowTokenSentToTokenBin 
                && traceIfFalse "race state input missing state token"
                     (containsRaceStateToken $ txOutValue $ txInInfoResolved raceStateInput)
                && traceIfFalse "race state input not spent using DistributeRewards redeemer"
                     (inputSpentUsingRaceRedeemer txInfo (txInInfoOutRef raceStateInput) DistributeRewards)
                    
        ClaimTTL ->
          traceIfFalse "escrow ttl not expired" afterEscrowTtl
            && traceIfFalse "total reward value not returned to admin" lockedAdaReturnedToAdmin
            && traceIfFalse "state token not disposed correctly" valueEscrowTokenSentToTokenBin
        
        _ ->
          traceError "incompatible redeemer"

    TokenBin ->
      case red of
        CleanupUsedTokens ->
          traceIfFalse "not initiated by admin or bot" initiatedByAdminOrBot

        _ -> 
          traceError "incompatible redeemer"
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    initiatedByAdminOrBot :: Bool
    initiatedByAdminOrBot =
      valueSpent txInfo `geq`
        (assetClassValue (adminToken rp) 1 <> assetClassValue (botToken rp) 1)

    raceStateTokenSentToTokenBin :: Bool
    raceStateTokenSentToTokenBin = stateTokenSentToTokenBin raceStateTokenName

    valueEscrowTokenSentToTokenBin :: Bool
    valueEscrowTokenSentToTokenBin = stateTokenSentToTokenBin valueEscrowTokenName

    stateTokenSentToTokenBin :: TokenName -> Bool
    stateTokenSentToTokenBin tn =
      isJust $ find
        ( \out ->
            -- TODO: optimize?
            case getInlineDatumFromTxOut out of
              Just TokenBin ->
                txOutAddress out == ownScriptAddress
                  && txOutValue out `geq` Value.singleton (stateCurrencySymbol params) tn 1
              _ ->
                False
        )
        (txInfoOutputs txInfo)
            
    ownInputContainsRaceStateToken :: Bool
    ownInputContainsRaceStateToken = containsRaceStateToken ownInputValue

    ownInputContainsValueEscrowToken :: Bool
    ownInputContainsValueEscrowToken = containsValueEscrowToken ownInputValue

    containsRaceStateToken :: Value -> Bool
    containsRaceStateToken val = containsStateToken val raceStateTokenName

    containsValueEscrowToken :: Value -> Bool
    containsValueEscrowToken val = containsStateToken val valueEscrowTokenName 

    containsStateToken :: Value -> TokenName -> Bool
    containsStateToken val tn =
      assetClassValueOf val (AssetClass (stateCurrencySymbol params, tn)) == 1

    -- The transaction must be signed by all delegates.
    txSignedByDelegates :: Bool
    txSignedByDelegates = all (txSignedBy txInfo) $ delegates params

    -- The distribution can only be announced once and cannot be changed afterwards.
    rewardDistributionNotAnnounced :: Maybe (Map Address Value) -> Bool
    rewardDistributionNotAnnounced = isNothing

    -- The combined distributed Value must be equal to the initially announced
    -- total reward Value.
    totalRewardValueDistributed :: Map Address Value -> Bool
    totalRewardValueDistributed distr = fold (elems distr) == totalRewardValue params

    -- Rewards are distributed among race participants.
    rewardsDistributedToRaceParticipants :: Map Address Value -> Bool
    rewardsDistributedToRaceParticipants distr = all (`elem` participants params) $ keys distr 

    rewardsSentToRaceParticipants :: Map Address Value -> Bool
    rewardsSentToRaceParticipants distr =
      all
        ( \(recipientAddr, reward) ->
            isJust $ find
              (\out -> txOutAddress out == recipientAddr && txOutValue out `geq` reward)
              (txInfoOutputs txInfo)
        )
        (toList distr)

    feesSentToDelegates :: Bool
    feesSentToDelegates =
      case feePerDelegate params of
        Just feeValue ->
          all
            ( \delegatePkh ->
                isJust $ find
                  ( \out ->
                      toPubKeyHash (txOutAddress out) == Just delegatePkh &&
                        txOutValue out `geq` feeValue
                  )
                  (txInfoOutputs txInfo)
            )
            (delegates params)
        Nothing ->
          True

    lockedAdaReturnedToAdmin :: Bool
    lockedAdaReturnedToAdmin =
      maybe False (\s -> distributesToAddrs txInfo s ownInputLovelace)
        racersStateFromRefInput

    racersStateFromRefInput :: Maybe RacersState
    racersStateFromRefInput = findCurrentGameStateFromRefInputs txInfo $ stateToken rp

    afterEscrowTtl :: Bool
    afterEscrowTtl = escrowTtl params `before` txInfoValidRange txInfo

    findUniqueRaceStateInput :: Maybe TxInInfo
    findUniqueRaceStateInput =
      let
        found =
          filter
            ( \inp ->
                let
                  resolved = txInInfoResolved inp
                 in
                  -- TODO: optimize?
                  case getInlineDatumFromTxOut resolved of
                    Just RaceState {distribution=Just _} ->
                      txOutAddress (txInInfoResolved inp) == ownScriptAddress
                    _ ->
                      False
            )
            (txInfoInputs txInfo)
       in
        case found of
          [ x ] -> Just x
          _ -> Nothing

    ownScriptAddress :: Address
    ownScriptAddress = txOutAddress ownInput 

    ownInputLovelace :: Integer
    ownInputLovelace = valueOf ownInputValue adaSymbol adaToken
 
    ownInputValue :: Value
    ownInputValue = txOutValue ownInput

    ownInput :: TxOut
    ownInput =
      maybe (traceError "could not get own input") txInInfoResolved $
        findOwnInput ctx

{-# INLINEABLE inputSpentUsingRedeemer #-}
inputSpentUsingRedeemer :: TxInfo -> TxOutRef -> Redeemer -> Bool
inputSpentUsingRedeemer txInfo oref red =
  lookup (Spending oref) (txInfoRedeemers txInfo) == Just red

{-# INLINEABLE inputSpentUsingRaceRedeemer #-}
inputSpentUsingRaceRedeemer :: TxInfo -> TxOutRef -> RaceRedeemer -> Bool
inputSpentUsingRaceRedeemer txInfo oref = 
  inputSpentUsingRedeemer txInfo oref
    . Redeemer
    . toBuiltinData
 
{-# INLINEABLE mkScript #-}
mkScript :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkScript rp params dat red ctx =
  let
    result =
      mkRaceScript
        (PlutusTx.unsafeFromBuiltinData rp)
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
