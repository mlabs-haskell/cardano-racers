module Test.CardanoRacers.Nitro.Contract (nitroTokenSuite) where

import Contract.Prelude

import CardanoRacers.Nitro.Contract
  ( adminMintsNitroContract
  , botMintsNitroContract
  , buyNitroContract
  , initNitroStateContract
  , mkNitroPolicy
  , modifyNitroStateContract
  , queryNitroState
  ) as Nitro
import CardanoRacers.Nitro.Contract
  ( mkNitroPolicy
  , mkNitroValidator
  , queryNitroState
  )
import CardanoRacers.Nitro.Helpers (createNitroScriptParams, mintBotNft) as NitroHelpers
import CardanoRacers.Nitro.Types
  ( NitroPolicyRedeemer(BuyNitroToken)
  , NitroScriptParams(NitroScriptParams)
  , NitroState(NitroState)
  , NitroStateRedeemer(SetNitroState)
  )
import Contract.Address (Address, getWalletAddresses)
import Contract.Credential (Credential(PubKeyCredential, ScriptCredential))
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), toData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Contract.Test.Utils
  ( ContractWrapAssertion
  , assertGainAtAddress'
  , assertTokenGainAtAddress
  , label
  , withAssertions
  )
import Contract.Transaction (TransactionHash, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(DatumWitness))
import Contract.TxConstraints as Constraints
import Contract.Utxos (getWalletUtxos)
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value (geq, lovelaceValueOf, scriptCurrencySymbol, singleton) as Value
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (try)
import Data.Array (head) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, toNumber) as BigInt
import Data.Int (ceil)
import Data.Map (singleton, toUnfoldable) as Map
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

nitroTokenSuite :: TestPlanM PlutipTest Unit
nitroTokenSuite = group "NitroToken script" do
  group "Nitro minting:" do
    test "Admin can mint Nitro" do
      withWallets walletUtxoDistr \w ->
        withKeyWallet w do
          ownAddress <- liftedM "Couldn't get wallet address" $ Array.head <$>
            getWalletAddresses
          nsp <- createNitroParamsHelper
          nitroSymbol <-
            liftedM "Couldn't create currency symbol from NitroPolicy"
              $ Value.scriptCurrencySymbol
              <$> Nitro.mkNitroPolicy nsp
          let
            amountToMint = BigInt.fromInt 100

            withAssertionsMono
              :: forall (r :: Row Type)
               . Array (ContractWrapAssertion r TransactionHash)
              -> Contract r TransactionHash
              -> Contract r TransactionHash
            withAssertionsMono = withAssertions
          void
            $ withAssertionsMono
                [ assertTokenGainAtAddress (label ownAddress "Admin")
                    (nitroSymbol /\ (unwrap nsp).nitroToken)
                    (const $ pure amountToMint)
                ]
            $ Nitro.adminMintsNitroContract nsp amountToMint
    test "Bot can mint Nitro" do
      withWallets (walletUtxoDistr /\ walletUtxoDistr) \(admin /\ bot) -> do
        nsp <- withKeyWallet admin createNitroParamsHelper
        withKeyWallet bot do
          botTk <- mintBotNftHelper
          let
            nspWithBotToken = NitroScriptParams $ (unwrap nsp)
              { botToken = botTk }
          botAddress <- liftedM "Couldn't get wallet address" $ Array.head <$>
            getWalletAddresses
          nitroSymbol <-
            liftedM "Couldn't create currency symbol from NitroPolicy"
              $ Value.scriptCurrencySymbol
              <$> Nitro.mkNitroPolicy nspWithBotToken
          let
            amountToMint = BigInt.fromInt 100

            withAssertionsMono
              :: forall (r :: Row Type)
               . Array (ContractWrapAssertion r TransactionHash)
              -> Contract r TransactionHash
              -> Contract r TransactionHash
            withAssertionsMono = withAssertions
          void
            $ withAssertionsMono
                [ assertTokenGainAtAddress (label botAddress "Admin")
                    (nitroSymbol /\ (unwrap nspWithBotToken).nitroToken)
                    (const $ pure amountToMint)
                ]
            $ Nitro.botMintsNitroContract nspWithBotToken amountToMint
    test "User buys Nitro" do
      withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
        \(admin /\ treasury /\ bob) -> do
          let nitroPrice = BigInt.fromInt 1000000
          treasuryAddr <- withKeyWallet treasury
            $ liftedM "Could not get treasury address"
            $ Array.head
            <$> getWalletAddresses
          operatingAddress <- withKeyWallet admin
            $ liftedM "Could not get treasury address"
            $ Array.head
            <$> getWalletAddresses
          nsp <- withKeyWallet admin createNitroParamsHelper
          initNitroPolicyWithAdminAndTreasury (admin /\ treasury) nsp nitroPrice
          nitroCs <- liftedM "Could not get currency symbol"
            $ Value.scriptCurrencySymbol
            <$> mkNitroPolicy nsp
          void $ withKeyWallet bob do
            bobAddress <- liftedM "Could not get bob address" $ Array.head <$>
              getWalletAddresses
            let
              amountToBuy = BigInt.fromInt 100
              amountToTreasury = BigInt.fromInt <<< ceil
                $ BigInt.toNumber (amountToBuy * nitroPrice)
                * 0.75
              amountToOperating = BigInt.fromInt <<< ceil
                $ BigInt.toNumber (amountToBuy * nitroPrice)
                * 0.25
              assertions =
                [ assertGainAtAddress' (label treasuryAddr "Treasury")
                    amountToTreasury
                , assertGainAtAddress' (label operatingAddress "Operating")
                    amountToOperating
                , assertTokenGainAtAddress (label bobAddress "Bob")
                    (nitroCs /\ (unwrap nsp).nitroToken)
                    (const $ pure amountToBuy)
                ]

              withAssertionsMono
                :: forall (r :: Row Type)
                 . Array (ContractWrapAssertion r TransactionHash)
                -> Contract r TransactionHash
                -> Contract r TransactionHash
              withAssertionsMono = withAssertions
            withAssertionsMono assertions $
              Nitro.buyNitroContract nsp amountToBuy
    test
      "User fails to mint Nitro with incorrect amount paid to operating/treasury"
      do
        withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
          \(admin /\ treasury /\ bob) -> do
            nsp <- withKeyWallet admin do
              nsp <- createNitroParamsHelper
              initNitroPolicyWithAdminAndTreasury (admin /\ treasury) nsp
                (BigInt.fromInt 1000000)
              pure nsp

            withKeyWallet bob do
              nitroMp <- mkNitroPolicy nsp
              let
                nitroAmount = BigInt.fromInt 100
                red = Redeemer $ toData $ BuyNitroToken nitroAmount
              ns /\ stateTxi /\ stateTxo <- queryNitroState nsp
              cs <- liftContractM "Could not get currency symbol"
                $ Value.scriptCurrencySymbol
                $ nitroMp

              let
                totalAmount = (unwrap ns).nitroPrice * nitroAmount
                -- Bad treausry
                treasuryAmt = BigInt.fromInt <<< ceil
                  $ BigInt.toNumber totalAmount
                  * 0.74
                operatingAmt = BigInt.fromInt <<< ceil
                  $ BigInt.toNumber totalAmount
                  * 0.25
                treasuryVal = Value.lovelaceValueOf treasuryAmt
                operatingVal = Value.lovelaceValueOf operatingAmt

                paysToAddrConstraint
                  :: Address -> Value -> Constraints.TxConstraints Void Void
                paysToAddrConstraint a v = case (unwrap a).addressCredential of
                  PubKeyCredential pkh ->
                    Constraints.mustPayToPubKey (wrap pkh) v
                  ScriptCredential vh ->
                    Constraints.mustPayToScript vh unitDatum DatumWitness v

                constraints :: Constraints.TxConstraints Void Void
                constraints =
                  Constraints.mustReferenceOutput stateTxi
                    <> paysToAddrConstraint (unwrap ns).treasuryAddress
                      treasuryVal
                    <> paysToAddrConstraint (unwrap ns).operatingAddress
                      operatingVal
                    <> Constraints.mustMintValueWithRedeemer red
                      (Value.singleton cs (unwrap nsp).nitroToken nitroAmount)

                lookups :: Lookups.ScriptLookups Void
                lookups = Lookups.mintingPolicy nitroMp
                  <> Lookups.unspentOutputs (Map.singleton stateTxi stateTxo)

              resE <- try $ submitTxFromConstraints lookups constraints
              resE `shouldSatisfy` isLeft

              let
                treasuryAmt' = BigInt.fromInt <<< ceil
                  $ BigInt.toNumber totalAmount
                  * 0.75
                -- Bad operating
                operatingAmt' = BigInt.fromInt <<< ceil
                  $ BigInt.toNumber totalAmount
                  * 0.24
                treasuryVal' = Value.lovelaceValueOf treasuryAmt'
                operatingVal' = Value.lovelaceValueOf operatingAmt'

                constraints' :: Constraints.TxConstraints Void Void
                constraints' =
                  Constraints.mustReferenceOutput stateTxi
                    <> paysToAddrConstraint (unwrap ns).treasuryAddress
                      treasuryVal'
                    <> paysToAddrConstraint (unwrap ns).operatingAddress
                      operatingVal'
                    <> Constraints.mustMintValueWithRedeemer red
                      (Value.singleton cs (unwrap nsp).nitroToken nitroAmount)
              resE' <- try $ submitTxFromConstraints lookups constraints'
              resE' `shouldSatisfy` isLeft
  group "Nitro state:" do
    test "Admin initialises NitroState" do
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
          nsp <- withKeyWallet admin createNitroParamsHelper
          initNitroPolicyWithAdminAndTreasury (admin /\ treasury) nsp nitroPrice
          let
            expectedNitroState = NitroState
              { nitroPrice: nitroPrice
              , treasuryAddress: treasuryAddr
              , operatingAddress: adminAddr
              }
          onchainNitroState /\ _ <- Nitro.queryNitroState nsp
          onchainNitroState `shouldEqual` expectedNitroState
    test "Admin modifies NitroState" do
      withWallets (walletUtxoDistr /\ walletUtxoDistr) \(admin /\ treasury) ->
        do
          nsp <- withKeyWallet admin createNitroParamsHelper
          initNitroPolicyWithAdminAndTreasury (admin /\ treasury) nsp $
            BigInt.fromInt
              1000000
          withKeyWallet admin do
            addr <- liftedM "Could not get address" $ Array.head <$>
              getWalletAddresses
            let
              nitroState = NitroState
                { nitroPrice: BigInt.fromInt 2000000
                , treasuryAddress: addr
                , operatingAddress: addr
                }
            void $ Nitro.modifyNitroStateContract nsp nitroState
            updatedNitroState /\ _ <- Nitro.queryNitroState nsp
            nitroState `shouldEqual` updatedNitroState
    test "Attempt to change NitroState fails without admin token" do
      withWallets (walletUtxoDistr /\ walletUtxoDistr) \(admin /\ eve) -> do
        nspBeforeUpdate <- withKeyWallet admin createNitroParamsHelper
        botTk <- withKeyWallet eve $ mintBotNftHelper
        let
          nsp = NitroScriptParams $ (unwrap nspBeforeUpdate)
            { botToken = botTk }
        initNitroPolicyWithAdminAndTreasury (admin /\ admin) nsp $
          BigInt.fromInt
            1000000
        withKeyWallet eve do
          nitroVal <- mkNitroValidator nsp
          botAddress <- liftedM "Could not get address"
            $ Array.head
            <$> getWalletAddresses
          let
            newState = NitroState
              { nitroPrice: BigInt.fromInt 2000000
              , treasuryAddress: botAddress
              , operatingAddress: botAddress
              }
            vhash = validatorHash nitroVal
            datum = Datum $ toData newState
            red = Redeemer $ toData $ SetNitroState newState
            stateVal = uncurry Value.singleton (unwrap nsp).stateToken one
          (_ /\ stateTxi /\ stateTxo) <- queryNitroState nsp
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

  mintBotNftHelper :: Contract () (CurrencySymbol /\ TokenName)
  mintBotNftHelper = do
    utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
    (txi /\ _) <- liftContractM "Could not get first utxo" $ Array.head $
      Map.toUnfoldable utxos
    NitroHelpers.mintBotNft txi

  createNitroParamsHelper :: Contract () NitroScriptParams
  createNitroParamsHelper = do
    utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
    (txi /\ _) <- liftContractM "Could not get first utxo" $ Array.head $
      Map.toUnfoldable utxos
    NitroHelpers.createNitroScriptParams txi "NITRO"

  initNitroPolicyWithAdminAndTreasury
    :: (KeyWallet /\ KeyWallet)
    -> NitroScriptParams
    -> BigInt
    -> Contract () Unit
  initNitroPolicyWithAdminAndTreasury (admin /\ treasury) nsp nitroPrice = do
    treasuryAddr <- withKeyWallet treasury
      $ liftedM "Could not get address"
      $ Array.head
      <$> getWalletAddresses
    withKeyWallet admin do
      ownAddr <- liftedM "Could not get address" $ Array.head <$>
        getWalletAddresses
      let
        ns = NitroState
          { nitroPrice: nitroPrice
          , treasuryAddress: treasuryAddr
          , operatingAddress: ownAddr
          }
      void $ Nitro.initNitroStateContract nsp ns

