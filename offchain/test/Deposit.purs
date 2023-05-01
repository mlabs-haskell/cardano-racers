module Test.CardanoRacers.Deposit (suite) where

import Contract.Prelude

import CardanoRacers.AssetRequest.Contract
  ( mkAssetRequestPolicy
  , requestAssetByRarity
  )
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
import CardanoRacers.RacersState.Contract (createRacersRefScriptOutput)
import Contract.AssocMap (empty, insert) as AssocMap
import Contract.Log (logInfo')
import Contract.Metadata (mkCip25String, unCip25String)
import Contract.Monad (liftContractM, liftedM, throwContractError)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
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
import Contract.Wallet (getWalletAddresses)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Data.Array (head) as Array
import Data.BigInt (fromInt) as BigInt
import Data.Map (Map, fromFoldable, lookup) as Map
import Effect.Aff (delay)
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Racers (Racers, runRacers, withContract)
import Test.CardanoRacers.Helpers
  ( createRacersParamsHelper
  , initRacersStateWithAdminAndTreasury
  )

suite :: TestPlanM PlutipTest Unit
suite = group "AssetRequest" do
  test "User mints request token" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ treasuryKey /\ userKey) -> do
        let uniquenessNonce = "0"

        rp <- withKeyWallet adminKey do
          rp <- createRacersParamsHelper
          _ <- runRacers rp $ adminMintsNitroContract (BigInt.fromInt 1_000_000)
          pure rp

        runRacers rp do
          (gameAssetSymbol :: CurrencySymbol) <- withContract
            (withKeyWallet adminKey)
            do
              assetRequestPolicy <- mkAssetRequestPolicy
              gameAssetPolicy <- mkGameAssetPolicy

              assetRequestScriptRef <- lift $ case assetRequestPolicy of
                PlutusMintingPolicy s -> pure s
                _ -> throwContractError "Not plutus script"
              gameAssetScriptRef <- lift $ case gameAssetPolicy of
                PlutusMintingPolicy s -> pure s
                _ -> throwContractError "Not plutus script"

              depositAssetScriptRef <- unwrap <$> mkDepositValidator

              _ <- createRacersRefScriptOutput assetRequestScriptRef
              _ <- createRacersRefScriptOutput gameAssetScriptRef
              _ <- createRacersRefScriptOutput depositAssetScriptRef

              lift $ liftContractM "could not get currency symbol" $
                Value.scriptCurrencySymbol gameAssetPolicy

          let
            assetPrices = foldl (flip $ uncurry AssocMap.insert) AssocMap.empty
              [ (Common /\ BigInt.fromInt 5_000_000)
              , (Rare /\ BigInt.fromInt 10_000_000)
              , (Epic /\ BigInt.fromInt 20_000_000)
              ]

          st <- initRacersStateWithAdminAndTreasury (adminKey /\ treasuryKey)
            (BigInt.fromInt 1_000_000)
            assetPrices

          let
            requests =
              [ Common
              , Rare
              , Epic
              ]

          _ <- withContract (withKeyWallet userKey) do
            traverse_ requestAssetByRarity requests

          userAddress <- lift $ withKeyWallet userKey
            $ liftedM "Could not get user address"
            $ Array.head
            <$> getWalletAddresses

          _ <- withContract (withKeyWallet adminKey) do
            tokenNames <- lift
              $ liftContractM "could not create string token names"
              $ for requests \r -> do
                  { name } <- Map.lookup r availableAssets
                  pure $ unCip25String name <> ":" <> uniquenessNonce

            assertions <- lift $ for tokenNames $ \name -> do
              tkName <-
                liftContractM ("could not create token name from " <> name) $
                  (mkTokenName <=< byteArrayFromAscii) name
              pure $ checkTokenGainAtAddress' (label userAddress "User")
                (gameAssetSymbol /\ tkName /\ BigInt.fromInt 1)

            withContract (runChecks assertions <<< lift) $
              retryCount
                ( consumeAndRedeemRequests 5 availableAssets
                    (pure uniquenessNonce)
                    st
                )
                3

          pure unit
  where
  retryCount :: forall (a :: Type). Racers a -> Int -> Racers a
  retryCount c 0 = c
  retryCount c n = do
    x <- try c
    case x of
      Left e -> do
        logInfo' ("threw " <> show e)
        liftAff $ delay (wrap 1000.0)
        logInfo' ("retrying " <> show n)
        retryCount c (n - 1)
      Right r -> pure r

  walletUtxoDistr :: InitialUTxOs
  walletUtxoDistr =
    [ BigInt.fromInt 5_000_000
    , BigInt.fromInt 2_000_000_000
    ]

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
