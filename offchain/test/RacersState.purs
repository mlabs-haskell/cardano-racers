module Test.CardanoRacers.RacersState.Contract (suite) where

import Contract.Prelude

import Cardano.Plutus.Types.Address as PlutusAddress
import Cardano.Plutus.Types.CurrencySymbol (fromScriptHash)
import Cardano.Plutus.Types.CurrencySymbol as Plutus
import Cardano.ToData (toData)
import Cardano.Types.BigInt as JSBigInt
import Cardano.Types.BigNum as BigNum
import Cardano.Types.PlutusScript (hash)
import Cardano.Types.Value (Value)
import CardanoRacers.Common.Types (RacersParams(RacersParams))
import CardanoRacers.Nitro.Helpers (mintBotNft) as NitroHelpers
import CardanoRacers.RacersState.Contract
  ( mkRacersStateValidator
  , modifyRacersStateContract
  , queryRacersState
  ) as RacersState
import CardanoRacers.RacersState.Types
  ( AssetPrices(AssetPrices)
  , RacersState(RacersState)
  , RacersStateRedeemer(SetRacersState)
  )
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (RedeemerDatum(RedeemerDatum))
import Contract.ScriptLookups as Lookups
import Contract.Scripts (ScriptHash)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Testnet
  ( ContractTest
  , InitialUTxOs
  , withKeyWallet
  , withWallets
  )
import Contract.Transaction (submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value (geq, singleton) as Value
import Contract.Wallet (getWalletAddresses, getWalletUtxos)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Data.Array (head) as Array
import Data.BigInt (fromInt) as BigInt
import Data.BigInt as DataBigInt
import Data.Map (singleton, toUnfoldable) as Map
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Racers (runRacers, withContract)
import Test.CardanoRacers.Helpers
  ( createRacersParamsHelper
  , initRacersStateWithAdminAndTreasury
  )
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

suite :: TestPlanM ContractTest Unit
suite = group "RacersState script:" do
  group "Racers state:" do
    test "Admin initialises RacersState" do
      withWallets (walletUtxoDistr /\ walletUtxoDistr) \(admin /\ treasury) ->
        do
          let nitroPrice = DataBigInt.fromInt 1000000
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
              defaultAssetPrices

            treasuryAddrPlutus <- lift
              $ liftContractM "Could not convert treasury address to Plutus"
              $ PlutusAddress.fromCardano treasuryAddr
            adminAddrPlutus <- lift
              $ liftContractM "Could not convert own address to Plutus"
              $ PlutusAddress.fromCardano adminAddr

            let
              expectedRacersState = RacersState
                { nitroPrice: toBI nitroPrice
                , treasuryAddress: treasuryAddrPlutus
                , operatingAddress: adminAddrPlutus
                , assetPrices: defaultAssetPrices
                }

            onchainRacersState /\ _ <- RacersState.queryRacersState
            onchainRacersState `shouldEqual` expectedRacersState
    test "Admin modifies RacersState" do
      withWallets (walletUtxoDistr /\ walletUtxoDistr) \(admin /\ treasury) ->
        do
          rp <- withKeyWallet admin createRacersParamsHelper
          runRacers rp do
            prevState <-
              initRacersStateWithAdminAndTreasury (admin /\ treasury)
                (BigInt.fromInt 1000000)
                defaultAssetPrices
            withContract (withKeyWallet admin) do
              let
                newState = wrap $ (unwrap prevState)
                  { nitroPrice = JSBigInt.fromInt 2000000 }
              _ <- RacersState.modifyRacersStateContract $ const newState
              updatedRacersState /\ _ <- RacersState.queryRacersState
              newState `shouldEqual` updatedRacersState
    test "Attempt to change RacersState fails without admin token" do
      withWallets (walletUtxoDistr /\ walletUtxoDistr) \(admin /\ eve) -> do
        rpBeforeUpdate <- withKeyWallet admin createRacersParamsHelper
        botTk <- withKeyWallet eve $ mintBotNftHelper
        let
          rp = RacersParams $ (unwrap rpBeforeUpdate)
            { botToken = (fromScriptHash (fst botTk) /\ wrap (snd botTk)) }
        runRacers rp do
          prevState <-
            initRacersStateWithAdminAndTreasury (admin /\ admin)
              (BigInt.fromInt 1000000)
              defaultAssetPrices

          (stateScriptHash :: ScriptHash) <- lift
            $ liftContractM "Could get ScriptHash from State CurrencySymbol"
            $ Plutus.toCardano
            $ fst (unwrap rp).stateToken

          (botScriptHash :: ScriptHash) <- lift
            $ liftContractM "Could get ScriptHash from Bot CurrencySymbol"
            $ Plutus.toCardano
            $ fst (unwrap rp).botToken

          withContract (withKeyWallet eve) do
            nitroVal <- RacersState.mkRacersStateValidator
            let
              newState = wrap $ (unwrap prevState)
                { nitroPrice = JSBigInt.fromInt 2000000 }
              vhash = hash $ unwrap nitroVal
              datum = toData newState
              red = RedeemerDatum $ toData $ SetRacersState newState
              (stateVal :: Value) = Value.singleton stateScriptHash
                (unwrap $ snd (unwrap rp).stateToken)
                BigNum.one
            -- stateVal = uncurry Value.singleton (unwrap rp).stateToken one
            (_ /\ stateTxi /\ stateTxo) <- RacersState.queryRacersState
            let
              constraints :: Constraints.TxConstraints
              constraints = Constraints.mustSpendScriptOutput stateTxi red
                <> Constraints.mustPayToScript vhash datum
                  Constraints.DatumInline
                  stateVal

              lookups :: Lookups.ScriptLookups
              lookups = Lookups.validator (unwrap nitroVal)
                <> Lookups.unspentOutputs (Map.singleton stateTxi stateTxo)

            resE <- try $ lift $ submitTxFromConstraints lookups constraints
            resE `shouldSatisfy` isLeft

            -- Continue to test with bot token
            ownUtxos <- lift $ liftedM "Could not get wallet utxos"
              getWalletUtxos
            let
              (botVal :: Value) = Value.singleton botScriptHash
                (unwrap $ snd (unwrap rp).botToken)
                BigNum.one
            (botTxi /\ _) <- lift
              $ liftContractM "Could not find bot token in wallet"
              $ find
                  ( \(_ /\ txo) -> (unwrap txo).amount
                      `Value.geq`
                        botVal
                  )
              $ (Map.toUnfoldable ownUtxos :: Array _)
            let
              constraints' = Constraints.mustSpendPubKeyOutput botTxi
                <> constraints
              lookups' = lookups <> Lookups.unspentOutputs ownUtxos

            resE' <- try $ lift $ submitTxFromConstraints lookups' constraints'
            resE' `shouldSatisfy` isRight
  where
  walletUtxoDistr :: InitialUTxOs
  walletUtxoDistr =
    [ BigNum.fromInt 5_000_000
    , BigNum.fromInt 2_000_000_000
    ]

  defaultAssetPrices :: AssetPrices
  defaultAssetPrices = AssetPrices
    { common: JSBigInt.fromInt 1_000_000
    , rare: JSBigInt.fromInt 2_000_000
    , epic: JSBigInt.fromInt 3_000_000
    }

  mintBotNftHelper :: Contract (CurrencySymbol /\ TokenName)
  mintBotNftHelper = do
    utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
    (txi /\ _) <- liftContractM "Could not get first utxo" $ Array.head $
      Map.toUnfoldable utxos
    NitroHelpers.mintBotNft txi

  toBI :: DataBigInt.BigInt -> JSBigInt.BigInt
  toBI = unsafePartial fromJust <<< JSBigInt.fromString <<< DataBigInt.toString
