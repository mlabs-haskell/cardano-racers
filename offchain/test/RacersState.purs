module Test.CardanoRacers.RacersState.Contract (suite) where

import Contract.Prelude

import CardanoRacers.Common.Types
  ( RacersParams(RacersParams)
  )
import CardanoRacers.RacersState.Types (RacersState(RacersState), RacersStateRedeemer(SetRacersState))
import CardanoRacers.Nitro.Helpers (createRacersParams, mintBotNft) as NitroHelpers
import CardanoRacers.RacersState.Contract
  ( initRacersStateContract
  , mkRacersStateValidator
  , modifyRacersStateContract
  , queryRacersState
  ) as RacersState
import Contract.Address (getWalletAddresses)
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
import Contract.Utxos (getWalletUtxos)
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value (geq, singleton) as Value
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (try)
import Data.Array (head) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Map (singleton, toUnfoldable) as Map
import Mote (group, test)
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
          nsp <- withKeyWallet admin createStateParamsHelper
          initRacersStateWithAdminAndTreasury (admin /\ treasury) nsp nitroPrice
          let
            expectedRacersState = RacersState
              { nitroPrice: nitroPrice
              , treasuryAddress: treasuryAddr
              , operatingAddress: adminAddr
              , driverPrices: AssocMap.empty
              , carPrices: AssocMap.empty
              }
          onchainRacersState /\ _ <- RacersState.queryRacersState nsp
          onchainRacersState `shouldEqual` expectedRacersState
    test "Admin modifies RacersState" do
      withWallets (walletUtxoDistr /\ walletUtxoDistr) \(admin /\ treasury) ->
        do
          nsp <- withKeyWallet admin createStateParamsHelper
          initRacersStateWithAdminAndTreasury (admin /\ treasury) nsp $
            BigInt.fromInt
              1000000
          withKeyWallet admin do
            addr <- liftedM "Could not get address" $ Array.head <$>
              getWalletAddresses
            let
              nitroState = RacersState
                { nitroPrice: BigInt.fromInt 2000000
                , treasuryAddress: addr
                , operatingAddress: addr
                , driverPrices: AssocMap.empty
                , carPrices: AssocMap.empty
                }
            void $ RacersState.modifyRacersStateContract nsp nitroState
            updatedRacersState /\ _ <- RacersState.queryRacersState nsp
            nitroState `shouldEqual` updatedRacersState
    test "Attempt to change RacersState fails without admin token" do
      withWallets (walletUtxoDistr /\ walletUtxoDistr) \(admin /\ eve) -> do
        nspBeforeUpdate <- withKeyWallet admin createStateParamsHelper
        botTk <- withKeyWallet eve $ mintBotNftHelper
        let
          nsp = RacersParams $ (unwrap nspBeforeUpdate)
            { botToken = botTk }
        initRacersStateWithAdminAndTreasury (admin /\ admin) nsp $
          BigInt.fromInt
            1000000
        withKeyWallet eve do
          nitroVal <- RacersState.mkRacersStateValidator nsp
          botAddress <- liftedM "Could not get address"
            $ Array.head
            <$> getWalletAddresses
          let
            newState = RacersState
              { nitroPrice: BigInt.fromInt 2000000
              , treasuryAddress: botAddress
              , operatingAddress: botAddress
              , driverPrices: AssocMap.empty
              , carPrices: AssocMap.empty
              }
            vhash = validatorHash nitroVal
            datum = Datum $ toData newState
            red = Redeemer $ toData $ SetRacersState newState
            stateVal = uncurry Value.singleton (unwrap nsp).stateToken one
          (_ /\ stateTxi /\ stateTxo) <- RacersState.queryRacersState nsp
          let
            constraints :: Constraints.TxConstraints Void Void
            constraints = Constraints.mustSpendScriptOutput stateTxi red
              <> Constraints.mustPayToScript vhash datum Constraints.DatumInline
                stateVal

            lookups :: Lookups.ScriptLookups Void
            lookups = Lookups.validator nitroVal
              <> Lookups.unspentOutputs (Map.singleton stateTxi stateTxo)

          resE <- try $ submitTxFromConstraints lookups constraints
          resE `shouldSatisfy` isLeft

          -- Continue to test with bot token
          ownUtxos <- liftedM "Could not get wallet utxos" getWalletUtxos
          let
            botVal = uncurry Value.singleton (unwrap nsp).botToken $
              BigInt.fromInt 1
          (botTxi /\ _) <- liftContractM "Could not find bot token in wallet"
            $ find
                ( \(_ /\ txo) -> (unwrap (unwrap txo).output).amount `Value.geq`
                    botVal
                )
            $ (Map.toUnfoldable ownUtxos :: Array _)
          let
            constraints' = Constraints.mustSpendPubKeyOutput botTxi
              <> constraints
            lookups' = lookups <> Lookups.unspentOutputs ownUtxos

          resE' <- try $ submitTxFromConstraints lookups' constraints'
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

  createStateParamsHelper :: Contract RacersParams
  createStateParamsHelper = do
    utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
    (txi /\ _) <- liftContractM "Could not get first utxo" $ Array.head $
      Map.toUnfoldable utxos
    NitroHelpers.createRacersParams txi "NITRO"

  initRacersStateWithAdminAndTreasury
    :: (KeyWallet /\ KeyWallet)
    -> RacersParams
    -> BigInt
    -> Contract Unit
  initRacersStateWithAdminAndTreasury (admin /\ treasury) nsp nitroPrice = do
    treasuryAddr <- withKeyWallet treasury
      $ liftedM "Could not get address"
      $ Array.head
      <$> getWalletAddresses
    withKeyWallet admin do
      ownAddr <- liftedM "Could not get address" $ Array.head <$>
        getWalletAddresses
      let
        ns = RacersState
          { nitroPrice: nitroPrice
          , treasuryAddress: treasuryAddr
          , operatingAddress: ownAddr
          , driverPrices: AssocMap.empty
          , carPrices: AssocMap.empty
          }
      void $ RacersState.initRacersStateContract nsp ns

