module Test.CardanoRacers.AssetRequest (suite) where

import Contract.Prelude

import CardanoRacers.AssetRequest.Contract
  ( mkAssetRequestPolicy
  , requestAssetByRarity
  )
import CardanoRacers.Common.Types (RacersParams(RacersParams))
import CardanoRacers.Deposit.Contract
  ( consumeAndRedeemRequests
  , mkDepositValidator
  , queryRequestsWithAirdropAddress
  )
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types (Rarity(Common, Rare, Epic))
import CardanoRacers.Nitro.Helpers (createRacersParams, mintBotNft) as NitroHelpers
import CardanoRacers.RacersState.Contract
  ( initRacersStateContract
  , mkRacersStateValidator
  , modifyRacersStateContract
  , queryRacersState
  ) as RacersState
import CardanoRacers.RacersState.Contract (queryRacersState)
import CardanoRacers.RacersState.Types
  ( RacersState(RacersState)
  , RacersStateRedeemer(SetRacersState)
  )
import CardanoRacers.ScriptsFFI (assetRequestPolicy, gameAssetPolicy)
import Contract.Address (getWalletAddresses, scriptHashAddress)
import Contract.AssocMap (Map)
import Contract.AssocMap (empty, insert) as AssocMap
import Contract.AssocMap as AssocMap
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (ValidatorHash(..), mintingPolicyHash, validatorHash)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Contract.Transaction (submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getWalletUtxos, utxosAt)
import Contract.Value (CurrencySymbol, TokenName, scriptCurrencySymbol)
import Contract.Value (geq, singleton) as Value
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (liftMaybe, try)
import Data.Array (head) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Map (toUnfoldable) as Map
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

suite :: TestPlanM PlutipTest Unit
suite = group "AssetRequest" do
  test "User mints request token" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ treasuryKey /\ userKey) -> do
        rp <- withKeyWallet adminKey createRacersParamsHelper
        let
          assetPrices = foldl (flip $ uncurry AssocMap.insert) AssocMap.empty
            [ (Common /\ BigInt.fromInt 5_000_000)
            , (Rare /\ BigInt.fromInt 10_000_000)
            , (Epic /\ BigInt.fromInt 20_000_000)
            ]
        st <- initRacersStateWithAdminAndTreasury (adminKey /\ treasuryKey) rp
          assetPrices
        _ <- withKeyWallet userKey $ requestAssetByRarity rp Common
        withKeyWallet adminKey $ do
          reqs <- queryRequestsWithAirdropAddress rp st
          _ <- consumeAndRedeemRequests rp st
          -- utxos <- utxosAt $ scriptHashAddress (unwrap st).depositScript Nothing
          logInfo' $ "Utxos at deposit script: " <> show reqs
        pure unit

  where
  walletUtxoDistr :: InitialUTxOs
  walletUtxoDistr =
    [ BigInt.fromInt 5_000_000
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

  initRacersStateWithAdminAndTreasury
    :: (KeyWallet /\ KeyWallet)
    -> RacersParams
    -> Map Rarity BigInt
    -> Contract RacersState
  initRacersStateWithAdminAndTreasury
    (admin /\ treasury)
    rp
    assetPrices = do
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
          , assetPrices: assetPrices
          , depositScript: depositScriptHash
          }
      _ <- RacersState.initRacersStateContract rp rs
      pure rs

