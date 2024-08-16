module Test.CardanoRacers.Deposit (suite) where

import Contract.Prelude

import Cardano.Types.AssetName (mkAssetName)
import Cardano.Types.BigInt as JSBigInt
import Cardano.Types.BigNum as BigNum
import Cardano.Types.PlutusScript as PlutusScript
import CardanoRacers.AssetRequest.Contract
  ( mkAssetRequestPolicy
  , requestAssetByRarity
  )
import CardanoRacers.Deposit.Contract (consumeAndRedeemRequests)
import CardanoRacers.Deposit.Validator (mkDepositValidator)
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types
  ( AssetOption
  , GameAssetType(CarType, DriverType)
  , Rarity(Common, Rare, Epic)
  )
import CardanoRacers.Nitro.Contract (adminMintsNitroContract, mkNitroPolicy)
import CardanoRacers.RacersState.Contract (createRacersRefScriptOutputs)
import CardanoRacers.RacersState.Types (AssetPrices(AssetPrices))
import Contract.Log (logInfo')
import Contract.Monad (liftContractM, liftedM, throwContractError)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Test.Assert (checkTokenGainAtAddress', label, runChecks)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Testnet
  ( InitialUTxOs
  , TestnetTest
  , withKeyWallet
  , withWallets
  )
import Contract.Wallet (getWalletAddresses)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.Array (head) as Array
import Data.BigInt (fromInt) as BigInt
import Data.Map (Map, fromFoldable, lookup) as Map
import Effect.Aff (delay)
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Racers (Racers, runRacers, withContract)
import Racers.Metadata.Cip25.Cip25String (mkCip25String, unCip25String)
import Test.CardanoRacers.Helpers
  ( createRacersParamsHelper
  , initRacersStateWithAdminAndTreasury
  )

suite :: TestPlanM TestnetTest Unit
suite = group "Deposit" do
  test "admin redeems user asset requests" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ treasuryKey /\ userKey) -> do
        let uniquenessNonce = "0"

        rp <- withKeyWallet adminKey do
          rp <- createRacersParamsHelper
          _ <- runRacers rp $ adminMintsNitroContract (BigInt.fromInt 1_000_000)
          pure rp

        runRacers rp do
          (driverAssetSymbol /\ carAssetSymbol) <- withContract
            (withKeyWallet adminKey)
            do
              assetRequestPolicy <- mkAssetRequestPolicy
              driverAssetPolicy <- mkGameAssetPolicy DriverType
              carAssetPolicy <- mkGameAssetPolicy CarType
              nitroPolicy <- mkNitroPolicy

              nitroScriptRef <- lift $
                case head (unwrap nitroPolicy).plutusMintingPolicies of
                  Just s -> pure s
                  Nothing -> throwContractError "Not plutus script"

              assetRequestScriptRef <- lift $
                case head (unwrap assetRequestPolicy).plutusMintingPolicies of
                  Just s -> pure s
                  Nothing -> throwContractError "Not plutus script"

              driverPolicyRef <- lift $
                case head (unwrap driverAssetPolicy).plutusMintingPolicies of
                  Just s -> pure s
                  Nothing -> throwContractError "Not plutus script"

              carPolicyRef <- lift $
                case head (unwrap carAssetPolicy).plutusMintingPolicies of
                  Just s -> pure s
                  Nothing -> throwContractError "Not plutus script"

              depositAssetScriptRef <- unwrap <$> mkDepositValidator

              traverse_ createRacersRefScriptOutputs
                [ [ nitroScriptRef
                  , assetRequestScriptRef
                  ]
                , [ driverPolicyRef
                  , carPolicyRef
                  , depositAssetScriptRef
                  ]
                ]

              let
                driverSymbol = PlutusScript.hash driverPolicyRef
                carSymbol = PlutusScript.hash carPolicyRef

              pure $ driverSymbol /\ carSymbol

          let
            assetPrices :: AssetPrices
            assetPrices = AssetPrices
              { common: JSBigInt.fromInt 5000000
              , rare: JSBigInt.fromInt 1000000
              , epic: JSBigInt.fromInt 20000000
              }

          _ <- initRacersStateWithAdminAndTreasury (adminKey /\ treasuryKey)
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
            assetsAndNames <- lift
              $ liftContractM "could not create string token names"
              $ for requests \r -> do
                  { name, assetType } <- Map.lookup r availableAssets
                  pure $ assetType /\
                    (unCip25String name <> ":" <> uniquenessNonce)

            assertions <- lift $ for assetsAndNames $ \(assetType /\ name) -> do
              tkName <-
                liftContractM ("could not create token name from " <> name) $
                  (mkAssetName <=< byteArrayFromAscii) name
              let
                gameAssetSymbol = case assetType of
                  DriverType -> driverAssetSymbol
                  CarType -> carAssetSymbol

              pure $ checkTokenGainAtAddress' (label userAddress "User")
                (gameAssetSymbol /\ tkName /\ JSBigInt.fromInt 1)

            withContract (runChecks assertions <<< lift) $
              retryCount
                ( consumeAndRedeemRequests 2 Nothing availableAssets
                    (const $ pure uniquenessNonce)

                )
                2

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
    [ BigNum.fromInt 5_000_000
    , BigNum.fromInt 5_000_000
    , BigNum.fromInt 5_000_000
    , BigNum.fromInt 2_000_000_000
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
