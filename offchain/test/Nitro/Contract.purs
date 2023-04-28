module Test.CardanoRacers.Nitro.Contract (suite) where

import Contract.Prelude

import CardanoRacers.Common.Types (RacersParams(RacersParams))
import CardanoRacers.Deposit.Contract (mkDepositValidator)
import CardanoRacers.Nitro.Contract
  ( adminMintsNitroContract
  , botMintsNitroContract
  , buyNitroContract
  , mkNitroPolicy
  ) as Nitro
import CardanoRacers.Nitro.Helpers (createRacersParams, mintBotNft) as NitroHelpers
import CardanoRacers.Nitro.Types (NitroPolicyRedeemer(BuyNitroToken))
import CardanoRacers.RacersState.Contract
  ( initRacersStateContract
  , queryRacersState
  ) as RacersState
import CardanoRacers.RacersState.Types (RacersState(RacersState))
import Contract.Address (Address)
import Contract.AssocMap as AssocMap
import Contract.Credential (Credential(PubKeyCredential, ScriptCredential))
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (Redeemer(Redeemer), toData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Test.Assert
  ( checkGainAtAddress'
  , checkTokenGainAtAddress'
  , label
  , runChecks
  )
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Contract.Transaction (submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(DatumWitness))
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value (lovelaceValueOf, scriptCurrencySymbol, singleton) as Value
import Contract.Wallet (KeyWallet, getWalletAddresses, getWalletUtxos)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Data.Array (head) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, toNumber) as BigInt
import Data.Int (ceil)
import Data.Map (singleton, toUnfoldable) as Map
import Mote (group, test)
import Racers (Racers, runRacers, withContract)
import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM PlutipTest Unit
suite = group "NitroToken script" do
  group "Nitro minting:" do
    test "Admin can mint Nitro" do
      withWallets walletUtxoDistr \w ->
        withKeyWallet w do
          ownAddress <- liftedM "Couldn't get wallet address" $ Array.head <$>
            getWalletAddresses
          rp <- createNitroParamsHelper
          runRacers rp do
            nitroSymbol <- withContract
              (liftedM "Couldn't create currency symbol from NitroPolicy")
              (Value.scriptCurrencySymbol <$> Nitro.mkNitroPolicy)

            let
              amountToMint = BigInt.fromInt 100
            void
              $ withContract
                  ( runChecks
                      [ checkTokenGainAtAddress' (label ownAddress "Admin")
                          ( nitroSymbol /\ (unwrap rp).nitroToken /\
                              amountToMint
                          )
                      ] <<< lift
                  )
              $ Nitro.adminMintsNitroContract amountToMint
    test "Bot can mint Nitro" do
      withWallets (walletUtxoDistr /\ walletUtxoDistr) \(admin /\ bot) -> do
        rp <- withKeyWallet admin createNitroParamsHelper
        withKeyWallet bot $ runRacers rp do
          botTk <- lift mintBotNftHelper
          let
            nspWithBotToken = RacersParams $ (unwrap rp)
              { botToken = botTk }
          botAddress <- lift $ liftedM "Couldn't get wallet address"
            $ Array.head
            <$>
              getWalletAddresses
          nitroSymbol <- withContract
            (liftedM "Couldn't create currency symbol from NitroPolicy")
            (Value.scriptCurrencySymbol <$> Nitro.mkNitroPolicy)
          let
            amountToMint = BigInt.fromInt 100

          void
            $ withContract
                ( runChecks
                    [ checkTokenGainAtAddress' (label botAddress "Admin")
                        ( nitroSymbol /\ (unwrap nspWithBotToken).nitroToken /\
                            amountToMint
                        )
                    ] <<< lift
                )
            $ Nitro.botMintsNitroContract amountToMint
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
          rp <- withKeyWallet admin createNitroParamsHelper
          runRacers rp do
            _ <- initNitroPolicyWithAdminAndTreasury (admin /\ treasury)
              nitroPrice
            nitroSymbol <- withContract
              (liftedM "Couldn't create currency symbol from NitroPolicy")
              (Value.scriptCurrencySymbol <$> Nitro.mkNitroPolicy)
            withContract (withKeyWallet bob) do
              bobAddress <- lift $ liftedM "Could not get bob address"
                $ Array.head
                <$>
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
                  [ checkGainAtAddress' (label treasuryAddr "Treasury")
                      amountToTreasury
                  , checkGainAtAddress' (label operatingAddress "Operating")
                      amountToOperating
                  , checkTokenGainAtAddress' (label bobAddress "Bob")
                      (nitroSymbol /\ (unwrap rp).nitroToken /\ amountToBuy)
                  ]

              void $ withContract (runChecks assertions <<< lift) $
                Nitro.buyNitroContract amountToBuy
    test
      "User fails to mint Nitro with incorrect amount paid to operating/treasury"
      do
        withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
          \(admin /\ treasury /\ bob) -> do
            rp <- withKeyWallet admin do
              rp <- createNitroParamsHelper
              runRacers rp do
                _ <- initNitroPolicyWithAdminAndTreasury (admin /\ treasury)
                  (BigInt.fromInt 1000000)
                pure rp

            withKeyWallet bob $ runRacers rp do
              nitroMp <- Nitro.mkNitroPolicy
              let
                nitroAmount = BigInt.fromInt 100
                red = Redeemer $ toData $ BuyNitroToken nitroAmount
              ns /\ stateTxi /\ stateTxo <- RacersState.queryRacersState
              cs <- lift $ liftContractM "Could not get currency symbol"
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
                      (Value.singleton cs (unwrap rp).nitroToken nitroAmount)

                lookups :: Lookups.ScriptLookups Void
                lookups = Lookups.mintingPolicy nitroMp
                  <> Lookups.unspentOutputs (Map.singleton stateTxi stateTxo)

              resE <- try $ lift $ submitTxFromConstraints lookups constraints
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
                      (Value.singleton cs (unwrap rp).nitroToken nitroAmount)
              resE' <- try $ lift $ submitTxFromConstraints lookups constraints'
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

  createNitroParamsHelper :: Contract RacersParams
  createNitroParamsHelper = do
    utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
    (txi /\ _) <- liftContractM "Could not get first utxo" $ Array.head $
      Map.toUnfoldable utxos
    NitroHelpers.createRacersParams txi "NITRO"

  initNitroPolicyWithAdminAndTreasury
    :: (KeyWallet /\ KeyWallet)
    -> BigInt
    -> Racers RacersState
  initNitroPolicyWithAdminAndTreasury (admin /\ treasury) nitroPrice = do
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
