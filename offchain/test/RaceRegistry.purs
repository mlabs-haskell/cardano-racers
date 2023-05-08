module Test.CardanoRacers.RaceRegistry (suite) where

import Contract.Prelude

import CardanoRacers.AssetRequest.Contract (mkAssetRequestPolicy, requestAssetByRarity)
import CardanoRacers.Deposit.Contract (consumeAndRedeemRequests, mkDepositValidator)
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types (AssetOption, GameAssetType(CarType, DriverType), Rarity(Common, Rare, Epic))
import CardanoRacers.Helpers (counterNonce)
import CardanoRacers.Nitro.Contract (adminMintsNitroContract, buyNitroContract, mkNitroPolicy)
import CardanoRacers.RaceRegistry.Contract (confirmParticipatingAssets, findUtxoWithAvailableSlotToken, initRace, queryRegistryUtxos, registerPositionInRace)
import CardanoRacers.RacersState.Contract (createRacersRefScriptOutput)
import CardanoRacers.RacersState.Types (AssetPrices(AssetPrices))
import Contract.Log (logInfo')
import Contract.Metadata (mkCip25String, unCip25String)
import Contract.Monad (liftContractM, liftedM, throwContractError)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
import Contract.Test.Assert (checkTokenGainAtAddress', label, runChecks)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip (InitialUTxOs, PlutipTest, withKeyWallet, withWallets)
import Contract.Value (CurrencySymbol, mkTokenName)
import Contract.Value as Value
import Contract.Wallet (getWalletAddresses, getWalletUtxos)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.Contract.Wallet (ownPubKeyHashes)
import Data.Array (head) as Array
import Data.BigInt (fromInt) as BigInt
import Data.Map (Map, fromFoldable, lookup) as Map
import Effect.Aff (delay)
import Effect.Ref as Ref
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Racers (Racers, runRacers, withContract)
import Test.CardanoRacers.Helpers (createRacersParamsHelper, initRacersStateWithAdminAndTreasury)

suite :: TestPlanM PlutipTest Unit
suite = group "Race Registry" do
  test "playground" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ treasuryKey /\ userKey) -> do
        rp <- withKeyWallet adminKey do
          rp <- createRacersParamsHelper
          _ <- runRacers rp $ adminMintsNitroContract (BigInt.fromInt 1_000_000)
          pure rp

        runRacers rp do
          _gameAssetSymbol <- withContract (withKeyWallet adminKey)
            do
              assetRequestPolicy <- mkAssetRequestPolicy
              gameAssetPolicy <- mkGameAssetPolicy
              nitroPolicy <- mkNitroPolicy

              nitroScriptRef <- lift $ case nitroPolicy of
                PlutusMintingPolicy s -> pure s
                _ -> throwContractError "Not plutus script"
              assetRequestScriptRef <- lift $ case assetRequestPolicy of
                PlutusMintingPolicy s -> pure s
                _ -> throwContractError "Not plutus script"
              gameAssetScriptRef <- lift $ case gameAssetPolicy of
                PlutusMintingPolicy s -> pure s
                _ -> throwContractError "Not plutus script"
              depositAssetScriptRef <- unwrap <$> mkDepositValidator

              _ <- createRacersRefScriptOutput nitroScriptRef
              _ <- createRacersRefScriptOutput assetRequestScriptRef
              _ <- createRacersRefScriptOutput gameAssetScriptRef
              _ <- createRacersRefScriptOutput depositAssetScriptRef

              lift $ liftContractM "could not get currency symbol" $
                Value.scriptCurrencySymbol gameAssetPolicy

          let
            assetPrices :: AssetPrices
            assetPrices = AssetPrices
              { common: BigInt.fromInt 5000000
              , rare: BigInt.fromInt 1000000
              , epic: BigInt.fromInt 20000000
              }

          st <- initRacersStateWithAdminAndTreasury (adminKey /\ treasuryKey)
            (BigInt.fromInt 1_000_000)
            assetPrices

          let
            requests =
              [ Common
              -- , Rare
              -- , Epic
              ]

          counterRef <- liftEffect $ Ref.new 0

          _ <- withContract (withKeyWallet userKey) do
            traverse_ requestAssetByRarity requests

          logInfo' $ "submitted request"

          _ <- withContract (withKeyWallet adminKey) $ consumeAndRedeemRequests
            5
            availableAssets
            (counterNonce counterRef)
            st

          logInfo' $ "redeemed requests"

          (rgp /\ _) <- withContract (withKeyWallet adminKey) $ initRace "abcde"
            (BigInt.fromInt 10)
            (BigInt.fromInt 2)
          logInfo' $ show rgp

          logInfo' "initialized race"

          -- _ <- withContract (withKeyWallet adminKey)
          --   $ lift (liftedM "asdf" $ ownPubKeyHashes <#> Array.head)
          --   >>= registerPositionInRace rgp

          -- logInfo' $ "registreing admin"

          withContract (withKeyWallet userKey) do
            _ <- buyNitroContract $ BigInt.fromInt 100

            lift getWalletUtxos >>= logInfo' <<< show

            firstPkh <- lift $ liftedM "Could not get first own public key hash"
              $ ownPubKeyHashes
              <#> Array.head
            firstAddr <- lift $ liftedM "Could not get first address"
              $ getWalletAddresses
              <#> Array.head

            testTokenName <- lift $ liftContractM "not token name" $ mkTokenName
              <=< byteArrayFromAscii
              $ "CommonCar:0"

            _ <- registerPositionInRace rgp firstPkh

            logInfo' $ "registering user"

            _ <- confirmParticipatingAssets rgp firstPkh
              ( wrap
                  { car: testTokenName
                  , driver: testTokenName
                  , payoutAddress: firstAddr
                  }
              )

            us <- queryRegistryUtxos rgp
            logInfo' $ show us

            pure unit
  where
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
