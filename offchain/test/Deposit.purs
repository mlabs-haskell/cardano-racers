module Test.CardanoRacers.Deposit (suite) where

import Contract.Prelude

import CardanoRacers.AssetRequest.Contract
  ( mkAssetRequestPolicy
  , requestAssetByRarity
  )
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Deposit.Contract
  ( consumeAndRedeemRequests
  , createDepositReferenceScriptOutput
  , mkDepositValidator
  , queryRequestsWithAirdropAddress
  )
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types
  ( AssetOption
  , GameAssetType(CarType, DriverType)
  , Rarity(Common, Rare, Epic)
  )
import CardanoRacers.Helpers (counterNonce, getTxoWithRefScrpt)
import CardanoRacers.Nitro.Contract (adminMintsNitroContract)
import CardanoRacers.Nitro.Helpers (createRacersParams) as NitroHelpers
import CardanoRacers.RacersState.Contract (createRacersRefScriptOutput)
import CardanoRacers.RacersState.Contract (initRacersStateContract) as RacersState
import CardanoRacers.RacersState.Types (RacersState(RacersState))
import Contract.AssocMap (Map, empty, insert) as AssocMap
import Contract.Log (logInfo')
import Contract.Metadata (mkCip25String)
import Contract.Monad (Contract, liftContractM, liftedM, throwContractError)
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , ValidatorHash
  , validatorHash
  )
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Contract.Value (scriptCurrencySymbol)
import Contract.Wallet
  ( KeyWallet
  , getWalletAddresses
  , getWalletBalance
  , getWalletUtxos
  )
import Data.Array (concatMap)
import Data.Array (head) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Map (Map, fromFoldable, toUnfoldable) as Map
import Effect.Ref (new) as Ref
import Mote (group, test)
import Partial.Unsafe (unsafePartial)

suite :: TestPlanM PlutipTest Unit
suite = group "AssetRequest" do
  test "User mints request token" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ treasuryKey /\ userKey) -> do
        cRef <- liftEffect $ Ref.new 1
        rp <- withKeyWallet adminKey createRacersParamsHelper
        _ <- withKeyWallet adminKey $ do
          assetRequestScriptRef <- mkAssetRequestPolicy rp >>= case _ of
            PlutusMintingPolicy s -> pure s
            _ -> throwContractError "Not plutus script"
          gameAssetScriptRef <- mkGameAssetPolicy rp >>= case _ of
            PlutusMintingPolicy s -> pure s
            _ -> throwContractError "Not plutus script"
          requestTxi <- createRacersRefScriptOutput rp assetRequestScriptRef
          gameTxi <- createRacersRefScriptOutput rp gameAssetScriptRef
          pure $ requestTxi /\ gameTxi
        let
          assetPrices = foldl (flip $ uncurry AssocMap.insert) AssocMap.empty
            [ (Common /\ BigInt.fromInt 5_000_000)
            , (Rare /\ BigInt.fromInt 10_000_000)
            , (Epic /\ BigInt.fromInt 20_000_000)
            ]
        -- withKeyWallet adminKey do
        --    col <- getWalletCollateral
        --    logInfo' $ show col
        st <- initRacersStateWithAdminAndTreasury (adminKey /\ treasuryKey) rp
          assetPrices
        _ <- withKeyWallet userKey $ requestAssetByRarity rp Rare
        _ <- withKeyWallet userKey $ requestAssetByRarity rp Epic
        -- let scriptAddr = scriptHashAddress (unwrap st).depositScript Nothing
        depRefOref <- withKeyWallet adminKey $
          createDepositReferenceScriptOutput rp
        _ <- withKeyWallet adminKey $ do
          reqs <- queryRequestsWithAirdropAddress rp st
          logInfo' $ "========== Requests\n" <>
            ( show $ concatMap (_.requestedAssets <<< snd) $
                (Map.toUnfoldable :: _ -> Array _) reqs
            )
          _ <- adminMintsNitroContract rp (BigInt.fromInt 1_000_000)
          refTxo <- getTxoWithRefScrpt depRefOref
          consumeAndRedeemRequests rp availableAssets (counterNonce cRef) st $
            Just (depRefOref /\ refTxo)
        -- withKeyWallet treasuryKey do
        --   bal <- getWalletBalance
        --   logInfo' $ "========== Treasury\n" <> show bal
        withKeyWallet userKey do
          bal <- getWalletBalance
          logInfo' $ "========== User\n" <> show bal
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
