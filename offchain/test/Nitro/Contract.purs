module Test.CardanoRacers.Nitro.Contract (suite) where

import Contract.Prelude

import Cardano.Plutus.Types.Address as Plutus
import Cardano.Plutus.Types.Address as PlutusAddres
import Cardano.Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  )
import Cardano.Plutus.Types.CurrencySymbol as CurrencySymbol
import Cardano.Plutus.Types.Value as PlutusValue
import Cardano.ToData (toData)
import Cardano.Types.BigInt as JSBigInt
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Mint as Mint
import CardanoRacers.Common.Types (RacersParams(RacersParams), nitroToken)
import CardanoRacers.Helpers
  ( fromBIToBigNum
  , fromBIToInt
  , fromBIToJSBI
  , fromJSBIToBI
  , mkMint
  , paysToAddrConstraint
  )
import CardanoRacers.Nitro.Contract
  ( adminMintsNitroContract
  , buyNitroContract
  , mkNitroPolicy
  ) as Nitro
import CardanoRacers.Nitro.Helpers (mintBotNft) as NitroHelpers
import CardanoRacers.Nitro.Types
  ( NitroPolicyRedeemer(BurnNitroToken, BuyNitroToken)
  )
import CardanoRacers.RacersState.Contract (queryRacersState) as RacersState
import CardanoRacers.RacersState.Types (AssetPrices(AssetPrices))
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (RedeemerDatum(..), unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Test.Assert
  ( checkGainAtAddress'
  , checkTokenGainAtAddress'
  , checkTokenLossAtAddress'
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
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(DatumWitness))
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value (lovelaceValueOf, singleton) as Value
import Contract.Wallet (getWalletAddresses, getWalletUtxos)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Data.Array (head) as Array
import Data.BigInt (fromInt, toNumber) as BigInt
import Data.BigInt as Data
import Data.BigInt as DataBigInt
import Data.Int (ceil)
import Data.Map (singleton, toUnfoldable) as Map
import Lib.CardanoRacers.Common (mintingPolicyHash)
import Mote (group, test)
import Racers (runRacers, withContract)
import Test.CardanoRacers.Helpers
  ( createRacersParamsHelper
  , initRacersStateWithAdminAndTreasury
  )
import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM PlutipTest Unit
suite = group "NitroToken script" do
  group "Nitro minting:" do
    test "Admin can mint Nitro" do
      withWallets walletUtxoDistr \w ->
        withKeyWallet w do
          ownAddress <- liftedM "Couldn't get wallet address" $ Array.head <$>
            getWalletAddresses
          rp <- createRacersParamsHelper
          runRacers rp do
            nitroPolicy <- Nitro.mkNitroPolicy
            nitroPolicyHash <- lift
              $ liftContractM "could not get nitro hash from policy"
              $ mintingPolicyHash nitroPolicy

            let
              amountToMint = 100
            void
              $ withContract
                  ( runChecks
                      [ checkTokenGainAtAddress' (label ownAddress "Admin")
                          ( unwrap nitroPolicyHash /\ unwrap nitroToken /\
                              JSBigInt.fromInt amountToMint
                          )
                      ] <<< lift
                  )
              $ Nitro.adminMintsNitroContract
              $ DataBigInt.fromInt amountToMint
    test "Bot can mint Nitro" do
      withWallets (walletUtxoDistr /\ walletUtxoDistr) \(admin /\ bot) -> do
        rp <- withKeyWallet admin createRacersParamsHelper
        withKeyWallet bot do
          (botCs /\ botTk) <- mintBotNftHelper
          let
            rpWithBotToken = RacersParams $ (unwrap rp)
              { botToken = (CurrencySymbol.fromScriptHash botCs /\ wrap botTk) }
          runRacers rpWithBotToken do
            botAddress <- lift $ liftedM "Couldn't get wallet address"
              $ Array.head
              <$>
                getWalletAddresses
            nitroPolicy <- Nitro.mkNitroPolicy
            nitroPolicyHash <- lift
              $ liftContractM "could not get nitro hash from policy"
              $ mintingPolicyHash nitroPolicy
            let
              amountToMint = 100

            void
              $ withContract
                  ( runChecks
                      [ checkTokenGainAtAddress' (label botAddress "Admin")
                          ( unwrap nitroPolicyHash /\ unwrap nitroToken /\
                              JSBigInt.fromInt amountToMint
                          )
                      ] <<< lift
                  )
              $ Nitro.adminMintsNitroContract
              $ DataBigInt.fromInt amountToMint
    test "User burns Nitro" do
      withWallets (walletUtxoDistr /\ walletUtxoDistr) \(admin /\ user) -> do
        userAddress <- withKeyWallet user
          $ liftedM "Couldn't get wallet address"
          $ Array.head
          <$>
            getWalletAddresses
        (_ /\ nitroPolicy /\ nitroSymbol) <- withKeyWallet admin do
          rp <- createRacersParamsHelper
          runRacers rp do
            let amountToMint = BigInt.fromInt 100
            nitroPolicy <- Nitro.mkNitroPolicy
            nitroSymbol <- lift
              $ liftContractM "could not get nitro hash from policy"
              $ mintingPolicyHash nitroPolicy
            _ <- Nitro.adminMintsNitroContract amountToMint

            userAddressPlutus <- lift
              $ liftContractM "could convert address from Cardano to Plutus"
              $ PlutusAddres.fromCardano userAddress

            txId <- lift $ submitTxFromConstraints
              (mempty :: Lookups.ScriptLookups)
              ( paysToAddrConstraint userAddressPlutus
                  ( Value.singleton (unwrap nitroSymbol) (unwrap nitroToken)
                      (BigNum.fromInt 50)
                  )
              )
            lift $ awaitTxConfirmed txId
            pure (rp /\ nitroPolicy /\ nitroSymbol)
        withKeyWallet user do
          let
            fiftyNitro = Value.singleton (unwrap nitroSymbol)
              (unwrap nitroToken)
              (BigNum.fromInt 50)

            mintContract :: PlutusValue.Value -> Contract Unit
            mintContract valueToMint =
              let
                amountToMint = mkMint valueToMint
              in
                submitTxFromConstraints (nitroPolicy :: Lookups.ScriptLookups)
                  ( Constraints.mustMintValueWithRedeemer
                      (RedeemerDatum $ toData BurnNitroToken)
                      amountToMint
                  ) >>= awaitTxConfirmed

          resE <- try $ mintContract $ PlutusValue.fromCardano fiftyNitro
          resE `shouldSatisfy` isLeft

          void
            $ runChecks
                [ checkTokenLossAtAddress' (label userAddress "User")
                    ( unwrap nitroSymbol /\ unwrap nitroToken /\
                        JSBigInt.fromInt 50
                    )
                ]
            $ lift
            $ mintContract
            $ PlutusValue.negation
            $ PlutusValue.fromCardano fiftyNitro
    test "User buys Nitro" do
      withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
        \(admin /\ treasury /\ user) -> do
          let nitroPrice = BigInt.fromInt 1000000
          treasuryAddr <- withKeyWallet treasury
            $ liftedM "Could not get treasury address"
            $ Array.head
            <$> getWalletAddresses
          operatingAddress <- withKeyWallet admin
            $ liftedM "Could not get treasury address"
            $ Array.head
            <$> getWalletAddresses
          rp <- withKeyWallet admin createRacersParamsHelper
          runRacers rp do
            _ <- initRacersStateWithAdminAndTreasury (admin /\ treasury)
              nitroPrice
              defaultAssetPrices
            nitroPolicy <- Nitro.mkNitroPolicy
            nitroPolicyHash <- lift
              $ liftContractM "could not get nitro hash from policy"
              $ mintingPolicyHash nitroPolicy
            withContract (withKeyWallet user) do
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
                      (fromBIToJSBI amountToTreasury)
                  , checkGainAtAddress' (label operatingAddress "Operating")
                      (fromBIToJSBI amountToOperating)
                  , checkTokenGainAtAddress' (label bobAddress "Bob")
                      ( unwrap nitroPolicyHash /\ unwrap nitroToken /\
                          fromBIToJSBI amountToBuy
                      )
                  ]

              void $ withContract (runChecks assertions <<< lift) $
                Nitro.buyNitroContract amountToBuy
    test
      "User fails to mint Nitro with incorrect amount paid to operating/treasury"
      do
        withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
          \(admin /\ treasury /\ bob) -> do
            rp <- withKeyWallet admin do
              rp <- createRacersParamsHelper
              runRacers rp do
                _ <- initRacersStateWithAdminAndTreasury (admin /\ treasury)
                  (BigInt.fromInt 1000000)
                  defaultAssetPrices
                pure rp

            withKeyWallet bob $ runRacers rp do
              nitroMp <- Nitro.mkNitroPolicy
              cs <- lift
                $ liftContractM
                    "could not get nitro currency symbol from policy"
                $ mintingPolicyHash nitroMp
              let
                nitroAmount = BigInt.fromInt 100
                red = RedeemerDatum $ toData $ BuyNitroToken $ fromBIToJSBI
                  nitroAmount
              ns /\ stateTxi /\ stateTxo <- RacersState.queryRacersState

              let
                (totalAmount :: Data.BigInt) =
                  (fromJSBIToBI (unwrap ns).nitroPrice * nitroAmount)
                -- Bad treausry
                treasuryAmt = BigInt.fromInt <<< ceil
                  $ BigInt.toNumber totalAmount
                  * 0.74
                operatingAmt = BigInt.fromInt <<< ceil
                  $ BigInt.toNumber totalAmount
                  * 0.25
                treasuryVal = Value.lovelaceValueOf $ fromBIToBigNum treasuryAmt
                operatingVal = Value.lovelaceValueOf $ fromBIToBigNum
                  operatingAmt

                paysToAddrConstraint
                  :: Plutus.Address -> Value -> Constraints.TxConstraints
                paysToAddrConstraint a v = case (unwrap a).addressCredential of
                  PubKeyCredential pkh ->
                    Constraints.mustPayToPubKey (wrap $ unwrap pkh) v
                  ScriptCredential vh ->
                    Constraints.mustPayToScript (unwrap vh) unitDatum
                      DatumWitness
                      v

                constraints :: Constraints.TxConstraints
                constraints =
                  Constraints.mustReferenceOutput stateTxi
                    <> paysToAddrConstraint (unwrap ns).treasuryAddress
                      treasuryVal
                    <> paysToAddrConstraint (unwrap ns).operatingAddress
                      operatingVal
                    <> Constraints.mustMintValueWithRedeemer red
                      ( Mint.singleton (unwrap cs) (unwrap nitroToken)
                          (fromBIToInt nitroAmount)
                      )

                lookups :: Lookups.ScriptLookups
                lookups = nitroMp <> Lookups.unspentOutputs
                  (Map.singleton stateTxi stateTxo)

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
                treasuryVal' = Value.lovelaceValueOf $ fromBIToBigNum
                  treasuryAmt'
                operatingVal' = Value.lovelaceValueOf $ fromBIToBigNum
                  operatingAmt'

                constraints' :: Constraints.TxConstraints
                constraints' =
                  Constraints.mustReferenceOutput stateTxi
                    <> paysToAddrConstraint (unwrap ns).treasuryAddress
                      treasuryVal'
                    <> paysToAddrConstraint (unwrap ns).operatingAddress
                      operatingVal'
                    <> Constraints.mustMintValueWithRedeemer red
                      ( Mint.singleton (unwrap cs) (unwrap nitroToken)
                          (fromBIToInt $ nitroAmount)
                      )
              resE' <- try $ lift $ submitTxFromConstraints lookups constraints'
              resE' `shouldSatisfy` isLeft
  where
  walletUtxoDistr :: InitialUTxOs
  walletUtxoDistr =
    [ BigNum.fromInt 5_000_000
    , BigNum.fromInt 2_000_000_000
    ]

  defaultAssetPrices :: AssetPrices
  defaultAssetPrices = AssetPrices
    { common: JSBigInt.fromInt 1000000
    , rare: JSBigInt.fromInt 2000000
    , epic: JSBigInt.fromInt 3000000
    }

  mintBotNftHelper :: Contract (CurrencySymbol /\ TokenName)
  mintBotNftHelper = do
    utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
    (txi /\ _) <- liftContractM "Could not get first utxo" $ Array.head $
      Map.toUnfoldable utxos
    NitroHelpers.mintBotNft txi

