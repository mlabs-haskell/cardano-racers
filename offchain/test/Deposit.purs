module Test.CardanoRacers.Deposit (suite) where

import Contract.Prelude

import CardanoRacers.AssetRequest.Contract
  ( mkAssetRequestPolicy
  , requestAssetByRarity
  )
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Deposit.Contract
  ( consumeAndRedeemRequests
  , mkDepositValidator
  )
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types
  ( AssetOption
  , GameAssetType(CarType, DriverType)
  , Rarity(Common, Rare, Epic)
  )
import CardanoRacers.Nitro.Contract (adminMintsNitroContract)
import CardanoRacers.Nitro.Helpers (createRacersParams) as NitroHelpers
import CardanoRacers.RacersState.Contract (createRacersRefScriptOutput)
import CardanoRacers.RacersState.Contract (initRacersStateContract) as RacersState
import CardanoRacers.RacersState.Types (RacersState(RacersState))
import Contract.AssocMap (Map, empty, insert) as AssocMap
import Contract.Metadata (mkCip25String, unCip25String)
import Contract.Monad (Contract, liftContractM, liftedM, throwContractError)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy), validatorHash)
import Contract.Test.Assert (checkTokenGainAtAddress', label, runChecks)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Contract.Value (CurrencySymbol, mkTokenName)
import Contract.Value as Value
import Contract.Wallet (KeyWallet, getWalletAddresses, getWalletUtxos)
import Control.Monad.Trans.Class (lift)
import Data.Array (head) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Map (Map, fromFoldable, lookup, toUnfoldable) as Map
import Mote (group, test)
import Partial.Unsafe (unsafePartial)

suite :: TestPlanM PlutipTest Unit
suite = group "AssetRequest" do
  test "User mints request token" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ treasuryKey /\ userKey) -> do
        let uniquenessNonce = "0"

        rp <- withKeyWallet adminKey do
          rp' <- createRacersParamsHelper
          _ <- adminMintsNitroContract rp' (BigInt.fromInt 1_000_000)
          pure rp'

        (gameAssetSymbol :: CurrencySymbol) <- withKeyWallet adminKey $ do
          assetRequestPolicy <- mkAssetRequestPolicy rp
          gameAssetPolicy <- mkGameAssetPolicy rp

          assetRequestScriptRef <- case assetRequestPolicy of
            PlutusMintingPolicy s -> pure s
            _ -> throwContractError "Not plutus script"
          gameAssetScriptRef <- case gameAssetPolicy of
            PlutusMintingPolicy s -> pure s
            _ -> throwContractError "Not plutus script"

          depositAssetScriptRef <- unwrap <$> mkDepositValidator rp

          _ <- createRacersRefScriptOutput rp assetRequestScriptRef
          _ <- createRacersRefScriptOutput rp gameAssetScriptRef
          _ <- createRacersRefScriptOutput rp depositAssetScriptRef

          liftContractM "could not get currency symbol" $
            Value.scriptCurrencySymbol gameAssetPolicy

        let
          assetPrices = foldl (flip $ uncurry AssocMap.insert) AssocMap.empty
            [ (Common /\ BigInt.fromInt 5_000_000)
            , (Rare /\ BigInt.fromInt 10_000_000)
            , (Epic /\ BigInt.fromInt 20_000_000)
            ]

        st <- initRacersStateWithAdminAndTreasury (adminKey /\ treasuryKey) rp
          assetPrices

        let
          requests =
            [ Common
            , Rare
            , Epic
            ]

        _ <- withKeyWallet userKey do
          traverse_ (requestAssetByRarity rp) requests

        userAddress <- withKeyWallet userKey
          $ liftedM "Could not get user address"
          $ Array.head
          <$> getWalletAddresses

        _ <- withKeyWallet adminKey $ do
          tokenNames <- liftContractM "could not create string token names" $
            for requests \r -> do
              { name } <- Map.lookup r availableAssets
              pure $ unCip25String name <> ":" <> uniquenessNonce

          assertions <- for tokenNames $ \name -> do
            tkName <-
              liftContractM ("could not create token name from " <> name) $
                (mkTokenName <=< byteArrayFromAscii) name
            pure $ checkTokenGainAtAddress' (label userAddress "User")
              (gameAssetSymbol /\ tkName /\ BigInt.fromInt 1)

          runChecks assertions $ lift $
            consumeAndRedeemRequests rp availableAssets (pure uniquenessNonce)
              st

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

  initRacersStateWithAdminAndTreasury
    :: (KeyWallet /\ KeyWallet)
    -> RacersParams
    -> AssocMap.Map Rarity BigInt
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

      depositScriptHash <- validatorHash <$> mkDepositValidator rp

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

  availableAssets :: Map.Map Rarity AssetOption
  availableAssets = Map.fromFoldable
    [ Common /\
        { name: unsafePartial $ fromJust $ mkCip25String "CommonCar"
        , assetType: CarType
        , imageUrl:
            "https://cdn.pixabay.com/photo/31/19/17/comic-2026591_1280.png"
        , description: "Cool car with lots of experience"
        , nitroAmount: BigInt.fromInt 100
        }
    , Rare /\
        { name: unsafePartial $ fromJust $ mkCip25String "RareDriver"
        , assetType: DriverType
        , imageUrl:
            "https://cdn.pixabay.com/photo/31/19/17/comic-2026591_1280.png"
        , description: "Cool car with lots of experience"
        , nitroAmount: BigInt.fromInt 200
        }
    , Epic /\
        { name: unsafePartial $ fromJust $ mkCip25String "EpicCar"
        , assetType: CarType
        , imageUrl:
            "https://cdn.pixabay.com/photo/31/19/17/comic-2026591_1280.png"
        , description: "Cool car with lots of experience"
        , nitroAmount: BigInt.fromInt 300
        }
    ]
