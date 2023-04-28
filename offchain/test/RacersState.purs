module Test.CardanoRacers.RacersState.Contract (suite) where

import Contract.Prelude

import CardanoRacers.Common.Types (RacersParams(RacersParams))
import CardanoRacers.Deposit.Contract (mkDepositValidator)
import CardanoRacers.Nitro.Helpers (createRacersParams, mintBotNft) as NitroHelpers
import CardanoRacers.RacersState.Contract
  ( initRacersStateContract
  , mkRacersStateValidator
  , modifyRacersStateContract
  , queryRacersState
  ) as RacersState
import CardanoRacers.RacersState.Types
  ( RacersState(RacersState)
  , RacersStateRedeemer(SetRacersState)
  )
import Contract.AssocMap as AssocMap
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Contract.Transaction (submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value (geq, singleton) as Value
import Contract.Wallet (KeyWallet, getWalletAddresses, getWalletUtxos)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Data.Array (head) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Map (singleton, toUnfoldable) as Map
import Mote (group, test)
import Racers (Racers, runRacers, withContract)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

suite :: TestPlanM PlutipTest Unit
suite = group "RacersState script:" do
  group "Racers state:" do
    test "Admin initialises RacersState" do
      withWallets (walletUtxoDistr /\ walletUtxoDistr) \(admin /\ treasury) ->
        do
          let nitroPrice = BigInt.fromInt 1000000
          adminAddr <- withKeyWallet admin
            $ liftedM "Could not get admin address"
            $ Array.head
            <$> getWalletAddresses
          treasuryAddr <- withKeyWallet treasury
            $ liftedM "Could not get treasury address"
            $ Array.head
            <$> getWalletAddresses
          rp <- withKeyWallet admin createRacersParamsHelper
          runRacers rp do
            _ <- initRacersStateWithAdminAndTreasury (admin /\ treasury)
              nitroPrice
            depositScriptHash <- validatorHash <$> mkDepositValidator
            let
              expectedRacersState = RacersState
                { nitroPrice: nitroPrice
                , treasuryAddress: treasuryAddr
                , operatingAddress: adminAddr
                , assetPrices: AssocMap.empty
                , depositScript: depositScriptHash
                }
            onchainRacersState /\ _ <- RacersState.queryRacersState
            onchainRacersState `shouldEqual` expectedRacersState
    test "Admin modifies RacersState" do
      withWallets (walletUtxoDistr /\ walletUtxoDistr) \(admin /\ treasury) ->
        do
          rp <- withKeyWallet admin createRacersParamsHelper
          runRacers rp do
            prevState <-
              initRacersStateWithAdminAndTreasury (admin /\ treasury) $
                BigInt.fromInt
                  1000000
            withContract (withKeyWallet admin) do
              let
                newState = wrap $ (unwrap prevState)
                  { nitroPrice = BigInt.fromInt 2000000 }
              _ <- RacersState.modifyRacersStateContract newState
              updatedRacersState /\ _ <- RacersState.queryRacersState
              newState `shouldEqual` updatedRacersState
    test "Attempt to change RacersState fails without admin token" do
      withWallets (walletUtxoDistr /\ walletUtxoDistr) \(admin /\ eve) -> do
        rpBeforeUpdate <- withKeyWallet admin createRacersParamsHelper
        botTk <- withKeyWallet eve $ mintBotNftHelper
        let
          rp = RacersParams $ (unwrap rpBeforeUpdate)
            { botToken = botTk }
        runRacers rp do
          prevState <- initRacersStateWithAdminAndTreasury (admin /\ admin) $
            BigInt.fromInt
              1000000
          withContract (withKeyWallet eve) do
            nitroVal <- RacersState.mkRacersStateValidator
            let
              newState = wrap $ (unwrap prevState)
                { nitroPrice = BigInt.fromInt 2000000 }
              vhash = validatorHash nitroVal
              datum = Datum $ toData newState
              red = Redeemer $ toData $ SetRacersState newState
              stateVal = uncurry Value.singleton (unwrap rp).stateToken one
            (_ /\ stateTxi /\ stateTxo) <- RacersState.queryRacersState
            let
              constraints :: Constraints.TxConstraints Void Void
              constraints = Constraints.mustSpendScriptOutput stateTxi red
                <> Constraints.mustPayToScript vhash datum
                  Constraints.DatumInline
                  stateVal

              lookups :: Lookups.ScriptLookups Void
              lookups = Lookups.validator nitroVal
                <> Lookups.unspentOutputs (Map.singleton stateTxi stateTxo)

            resE <- try $ lift $ submitTxFromConstraints lookups constraints
            resE `shouldSatisfy` isLeft

            -- Continue to test with bot token
            ownUtxos <- lift $ liftedM "Could not get wallet utxos"
              getWalletUtxos
            let
              botVal = uncurry Value.singleton (unwrap rp).botToken $
                BigInt.fromInt 1
            (botTxi /\ _) <- lift
              $ liftContractM "Could not find bot token in wallet"
              $ find
                  ( \(_ /\ txo) -> (unwrap (unwrap txo).output).amount
                      `Value.geq`
                        botVal
                  )
              $ (Map.toUnfoldable ownUtxos :: Array _)
            let
              constraints' = Constraints.mustSpendPubKeyOutput botTxi
                <> constraints
              lookups' = lookups <> Lookups.unspentOutputs ownUtxos

            resE' <- try $ lift $ submitTxFromConstraints lookups' constraints'
            resE' `shouldSatisfy` isLeft
  where
  walletUtxoDistr :: InitialUTxOs
  walletUtxoDistr =
    [ BigInt.fromInt 5_000_000
    , BigInt.fromInt 2_000_000_000
    ]

  mintBotNftHelper :: Contract (CurrencySymbol /\ TokenName)
  mintBotNftHelper = do
    utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
    (txi /\ _) <- liftContractM "Could not get first utxo" $ Array.head $
      Map.toUnfoldable utxos
    NitroHelpers.mintBotNft txi

  createRacersParamsHelper :: Contract RacersParams
  createRacersParamsHelper = do
    utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
    (txi /\ _) <- liftContractM "Could not get first utxo" $ Array.head $
      Map.toUnfoldable utxos
    NitroHelpers.createRacersParams txi "NITRO"

  initRacersStateWithAdminAndTreasury
    :: (KeyWallet /\ KeyWallet)
    -> BigInt
    -> Racers RacersState
  initRacersStateWithAdminAndTreasury (admin /\ treasury) nitroPrice = do
    treasuryAddr <- lift $ withKeyWallet treasury
      $ liftedM "Could not get address"
      $ Array.head
      <$> getWalletAddresses

    withContract (withKeyWallet admin) do
      ownAddr <- lift $ liftedM "Could not get address" $ Array.head <$>
        getWalletAddresses

      depositScriptHash <- validatorHash <$> mkDepositValidator

      let
        rs = RacersState
          { nitroPrice: nitroPrice
          , treasuryAddress: treasuryAddr
          , operatingAddress: ownAddr
          , assetPrices: AssocMap.empty
          , depositScript: depositScriptHash
          }
      _ <- RacersState.initRacersStateContract rs
      pure rs

