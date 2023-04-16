module Test.CardanoRacers.AssetRequest (suite) where

import Contract.Prelude

import CardanoRacers.AssetRequest.Contract
  ( mkAssetRequestPolicy
  , requestAssetByRarity
  )
import CardanoRacers.AssetRequest.Types
  ( AirdropAddressDatum(..)
  , AssetRequestRedeemer(..)
  )
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Deposit.Contract (mkDepositValidator)
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types (Rarity(Common, Rare, Epic))
import CardanoRacers.Helpers (paysToAddrConstraint)
import CardanoRacers.Nitro.Helpers (createRacersParams) as NitroHelpers
import CardanoRacers.RacersState.Contract (initRacersStateContract) as RacersState
import CardanoRacers.RacersState.Contract (queryRacersState)
import CardanoRacers.RacersState.Types (RacersState(RacersState))
import Contract.Address (scriptHashAddress)
import Contract.Wallet (getWalletAddresses, getWalletUtxos)
import Contract.AssocMap (Map)
import Contract.AssocMap (empty, insert, lookup) as AssocMap
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (Datum(..), Redeemer(..), toData)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (ValidatorHash, validatorHash)
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
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Value (scriptCurrencySymbol)
import Contract.Value as Value
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Data.Array (head) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, toNumber) as BigInt
import Data.Int (ceil)
import Data.Map (singleton, toUnfoldable) as Map
import Mote (group, test)
import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM PlutipTest Unit
suite = group "AssetRequest" do
  test "User requests asset by rarity" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(admin /\ treasury /\ alice) -> do
        treasuryAddr <- withKeyWallet treasury
          $ liftedM "Could not get treasury address"
          $ Array.head
          <$> getWalletAddresses
        operatingAddress <- withKeyWallet admin
          $ liftedM "Could not get treasury address"
          $ Array.head
          <$> getWalletAddresses
        rp <- withKeyWallet admin createRacersParamsHelper
        rs <- initRacersStateWithAdminAndTreasury (admin /\ treasury) rp
        _ <- withKeyWallet admin $ RacersState.initRacersStateContract rp rs
        assetRequestCs <- liftedM "Could not get currency symbol"
          $ Value.scriptCurrencySymbol
          <$> mkAssetRequestPolicy rp

        let rarities = [ Common, Rare, Epic ]

        for_ rarities $ \rarity -> do
          withKeyWallet alice do
            assetRequestTokenName <-
              liftContractM "Could not make required token names" $
                (Value.mkTokenName <=< byteArrayFromAscii) (show rarity)
            assetPrice <- liftContractM "could not get asset price from state" $
              AssocMap.lookup rarity (unwrap rs).assetPrices
            let
              depositAddress = scriptHashAddress (unwrap rs).depositScript
                Nothing
              amountToTreasury = BigInt.fromInt <<< ceil
                $ BigInt.toNumber assetPrice
                * 0.75
              amountToOperating = BigInt.fromInt <<< ceil
                $ BigInt.toNumber assetPrice
                * 0.25
              assertions =
                [ checkGainAtAddress' (label treasuryAddr "Treasury")
                    amountToTreasury
                , checkGainAtAddress' (label operatingAddress "Operating")
                    amountToOperating
                , checkTokenGainAtAddress' (label depositAddress "Deposit")
                    ( assetRequestCs /\ assetRequestTokenName /\ BigInt.fromInt
                        1
                    )
                ]

            runChecks assertions $ lift $
              requestAssetByRarity rp rarity
  test
    "User fails to request asset by rarity with incorrect amount paid to operating/treasury"
    do
      withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
        \(admin /\ treasury /\ alice) -> do
          let rarity = Rare
          rp <- withKeyWallet admin createRacersParamsHelper
          rs <- initRacersStateWithAdminAndTreasury (admin /\ treasury) rp
          _ <- withKeyWallet admin $ RacersState.initRacersStateContract rp rs

          assetRequestPolicy <- mkAssetRequestPolicy rp
          assetRequestCs <- liftedM "Could not get currency symbol"
            $ Value.scriptCurrencySymbol
            <$> mkAssetRequestPolicy rp

          withKeyWallet alice do
            ownAddr <- liftedM "Could not get own address"
              $ Array.head
              <$> getWalletAddresses
            assetRequestTokenName <-
              liftContractM "Could not make required token names" $
                (Value.mkTokenName <=< byteArrayFromAscii) (show rarity)
            rarePrice <-
              liftContractM "could not get rare asset price from state" $
                AssocMap.lookup rarity (unwrap rs).assetPrices

            let
              incorrectPayments = [ (0.74 /\ 0.25), (0.75 /\ 0.24) ]

              dat = Datum $ toData $ AirdropAddressDatum
                { airdropAddress: ownAddr }
              red = Redeemer $ toData $ MintRequestToken

              testIncorrectPayment (treasuryRatio /\ operatingRatio) = do
                (_ /\ stateTxi /\ stateTxo) <- queryRacersState rp
                let
                  amountToTreasury = BigInt.fromInt <<< ceil
                    $ BigInt.toNumber rarePrice
                    * treasuryRatio
                  amountToOperating = BigInt.fromInt <<< ceil
                    $ BigInt.toNumber rarePrice
                    * operatingRatio
                  treasuryVal = Value.lovelaceValueOf amountToTreasury
                  operatingVal = Value.lovelaceValueOf amountToOperating

                  lockedVal =
                    Value.singleton assetRequestCs assetRequestTokenName $
                      BigInt.fromInt 1

                  constraints :: Constraints.TxConstraints Void Void
                  constraints = Constraints.mustReferenceOutput stateTxi
                    <> paysToAddrConstraint (unwrap rs).treasuryAddress
                      treasuryVal
                    <> paysToAddrConstraint (unwrap rs).operatingAddress
                      operatingVal
                    <> Constraints.mustMintValueWithRedeemer red
                      ( Value.singleton assetRequestCs assetRequestTokenName
                          (BigInt.fromInt 1)
                      )
                    <> Constraints.mustPayToScript (unwrap rs).depositScript dat
                      DatumInline
                      lockedVal

                  lookups :: Lookups.ScriptLookups Void
                  lookups = Lookups.mintingPolicy assetRequestPolicy
                    <> Lookups.unspentOutputs (Map.singleton stateTxi stateTxo)

                resE <- try $ submitTxFromConstraints lookups constraints
                resE `shouldSatisfy` isLeft

            traverse_ testIncorrectPayment incorrectPayments

  where
  walletUtxoDistr :: InitialUTxOs
  walletUtxoDistr =
    [ BigInt.fromInt 5_000_000
    , BigInt.fromInt 2_000_000_000
    , BigInt.fromInt 2_000_000_000
    ]

  createRacersParamsHelper :: Contract RacersParams
  createRacersParamsHelper = do
    utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
    (txi /\ _) <- liftContractM "Could not get first utxo" $ Array.head $
      Map.toUnfoldable utxos
    NitroHelpers.createRacersParams txi "NITRO"

  depositScriptHashHelper :: RacersParams -> Contract ValidatorHash
  depositScriptHashHelper rp = do
    assetRequestPolicySymbol <- liftedM "could not get asset request symbol"
      $ mkAssetRequestPolicy rp
      <#> scriptCurrencySymbol
    assetPolicySymbol <- liftedM "could not get game asset symbol"
      $ mkGameAssetPolicy rp
      <#> scriptCurrencySymbol
    depositVal <- mkDepositValidator rp $
      wrap
        { assetPolicySymbol
        , assetRequestPolicySymbol
        }
    pure $ validatorHash depositVal

  defaultAssetPrices :: Map Rarity BigInt
  defaultAssetPrices = foldl (flip $ uncurry AssocMap.insert) AssocMap.empty
    [ (Common /\ BigInt.fromInt 5_000_000)
    , (Rare /\ BigInt.fromInt 10_000_000)
    , (Epic /\ BigInt.fromInt 20_000_000)
    ]

  initRacersStateWithAdminAndTreasury
    :: (KeyWallet /\ KeyWallet)
    -> RacersParams
    -> Contract RacersState
  initRacersStateWithAdminAndTreasury
    (admin /\ treasury)
    rp = do
    treasuryAddr <- withKeyWallet treasury
      $ liftedM "Could not get address"
      $ Array.head
      <$> getWalletAddresses
    withKeyWallet admin do
      ownAddr <- liftedM "Could not get address" $ Array.head <$>
        getWalletAddresses

      depositScriptHash <- depositScriptHashHelper rp

      let
        rs = RacersState
          { nitroPrice: BigInt.fromInt 1_000_000
          , treasuryAddress: treasuryAddr
          , operatingAddress: ownAddr
          , assetPrices: defaultAssetPrices
          , depositScript: depositScriptHash
          }
      pure rs

