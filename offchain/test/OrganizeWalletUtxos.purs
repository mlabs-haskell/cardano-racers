module Test.CardanoRacers.OrganizeWalletUtxos (suite) where

import Contract.Prelude

import Cardano.Plutus.Types.MintingPolicyHash (MintingPolicyHash)
import Cardano.Types (UtxoMap, Value(Value))
import Cardano.Types.Asset (Asset(Asset))
import Cardano.Types.BigInt as JSBigInt
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Coin as Coin
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.Value (valueOf)
import CardanoRacers.AssetRequest.Contract
  ( mkAssetRequestPolicy
  , requestAssetByRarity
  )
import CardanoRacers.Common.Types (nitroToken)
import CardanoRacers.Deposit.Contract (consumeAndRedeemRequests)
import CardanoRacers.Deposit.Validator (mkDepositValidator)
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types
  ( AssetOption
  , GameAssetType(CarType, DriverType)
  , Rarity(Common, Rare, Epic)
  )
import CardanoRacers.Nitro.Contract (adminMintsNitroContract, mkNitroPolicy)
import CardanoRacers.Nitro.Contract (buyNitroContract, mkNitroPolicy) as Nitro
import CardanoRacers.RacersState.Contract (createRacersRefScriptOutputs)
import CardanoRacers.RacersState.Types (AssetPrices(AssetPrices))
import Common.OrganizeWalletUtxos (organizeUTXOsByAssetClass)
import Contract.Log (logInfo')
import Contract.Monad (liftContractM, liftedM, throwContractError)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Testnet
  ( ContractTest
  , InitialUTxOs
  , withKeyWallet
  , withWallets
  )
import Contract.Wallet (getWalletUtxos)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Data.Array (filter, fromFoldable, head, partition) as Array
import Data.Array (head)
import Data.BigInt (fromInt) as BigInt
import Data.Map (Map, fromFoldable, member, values) as Map
import Data.Ord (greaterThan, lessThan)
import Effect.Aff (delay)
import Lib.CardanoRacers.Common (mintingPolicyHash)
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Racers (Racers, runRacers, withContract)
import Racers.Metadata.Cip25.Cip25String (mkCip25String)
import Test.CardanoRacers.Helpers
  ( createRacersParamsHelper
  , initRacersStateWithAdminAndTreasury
  )
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

suite :: TestPlanM ContractTest Unit
suite = group "Organize wallet Utxos" do

  test "Collects dust" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(admin /\ treasury /\ user) -> do
        let nitroPrice = BigInt.fromInt 1_000_000
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

            _txId <- organizeUTXOsByAssetClass

            utxosFinal <- lift $ liftedM "Could not get wallet utxos"
              getWalletUtxos

            testGeneralChecks true 4_000_000 1_100_000 utxosFinal
              nitroPolicyHash
              0

  test "Works with Nitro only" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(admin /\ treasury /\ user) -> do
        let nitroPrice = BigInt.fromInt 1_000_000
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
            let amountsToBuy = [ 100, 200, 300 ]
            traverse_ (void <<< Nitro.buyNitroContract <<< BigInt.fromInt)
              amountsToBuy
            _txId <- organizeUTXOsByAssetClass

            utxosFinal <- lift $ liftedM "Could not get wallet utxos"
              getWalletUtxos

            testGeneralChecks true 4_000_000 1_100_000 utxosFinal
              nitroPolicyHash
              (sum amountsToBuy)

  test "Can organize user's non-native assets" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ treasuryKey /\ userKey) -> do
        let uniquenessNonce = "0"

        rp <- withKeyWallet adminKey do
          rp <- createRacersParamsHelper
          _ <- runRacers rp $ adminMintsNitroContract (BigInt.fromInt 1_000_000)
          pure rp

        runRacers rp do
          (_driverAssetSymbol /\ _carAssetSymbol) <- withContract
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
              { common: JSBigInt.fromInt 5_000_000
              , rare: JSBigInt.fromInt 1_000_000
              , epic: JSBigInt.fromInt 20_000_000
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

          _ <- withContract (withKeyWallet adminKey) do
            retryCount
              ( consumeAndRedeemRequests 2 Nothing availableAssets
                  (const $ pure uniquenessNonce)
              )
              2

          withContract (withKeyWallet userKey) do

            nitroPolicy <- Nitro.mkNitroPolicy
            nitroPolicyHash <- lift
              $ liftContractM "could not get nitro hash from policy"
              $ mintingPolicyHash nitroPolicy

            _txId <- organizeUTXOsByAssetClass

            utxosFinal <- lift $ liftedM "Could not get wallet utxos"
              getWalletUtxos

            testGeneralChecks false 4_000_000 1_100_000 utxosFinal
              nitroPolicyHash
              600

            pure unit
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

    , BigNum.fromInt 1_000_000
    , BigNum.fromInt 2_000_000
    , BigNum.fromInt 3_000_000

    , BigNum.fromInt 2_000_000_000
    ]

  defaultAssetPrices :: AssetPrices
  defaultAssetPrices = AssetPrices
    { common: JSBigInt.fromInt 1_000_000
    , rare: JSBigInt.fromInt 2_000_000
    , epic: JSBigInt.fromInt 3_000_000
    }

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

testGeneralChecks
  :: Boolean -> Int -> Int -> UtxoMap -> MintingPolicyHash -> Int -> Racers Unit
testGeneralChecks
  checkDust
  dustThreshold
  maxAdaInt
  utxosFinal
  nitroPolicyHash
  expectedNitro = do
  let
    maxAda = Coin.fromInt maxAdaInt
    values = Array.fromFoldable $ map (\to -> (unwrap to).amount)
      (Map.values utxosFinal)

    nitroAsset = Asset (unwrap nitroPolicyHash)
      (unwrap nitroToken)

    { yes: assets, no: adaOnly } = Array.partition
      (\(Value _ ma) -> length (unwrap ma) > 0)
      values

  for_ assets \(Value c ma) -> do
    -- Check that we have each non-native asset into their own utxo (considering ~minUtxoAda)
    c `shouldSatisfy` (_ `lessThan` maxAda)
    -- Single ScriptHash
    length (unwrap ma) `shouldEqual` 1

  when checkDust do
    for_ adaOnly \(Value c _ma) -> do
      -- No dust
      c `shouldSatisfy` (_ `greaterThan` Coin.fromInt dustThreshold)

  let
    isNitro (Value _ ma) = (unwrap nitroPolicyHash) `Map.member`
      (unwrap ma)

  case (Array.head $ Array.filter isNitro assets) of
    Nothing -> pure unit
    Just nitroVal ->
      BigNum.fromInt expectedNitro `shouldEqual`
        (nitroAsset `valueOf` nitroVal)

