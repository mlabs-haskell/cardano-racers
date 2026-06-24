module Test.CardanoRacers.Hydra.Integration
  ( suite
  ) where

import Prelude

import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Address (fromCardano) as Plutus.Address
import Cardano.Types (NetworkId(TestnetId), ScriptHash, TransactionHash)
import Cardano.Types.BigNum (fromInt) as BigNum
import Cardano.Types.Value (lovelaceValueOf)
import Cardano.Wallet.Key (KeyWallet)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Helpers (assetNameFromAsciiUnsafe)
import CardanoRacers.Hydra.Config (AppQueryBackend(Kupmios))
import CardanoRacers.Hydra.Lib.Retry (retryOnError, retryOnFalse)
import CardanoRacers.Hydra.Main (cleanupHandler)
import CardanoRacers.Hydra.Monad (appLogger, initApp, readHeadStatus, runApp)
import CardanoRacers.Hydra.Node (startHydraNode)
import CardanoRacers.Hydra.Server (httpServer)
import CardanoRacers.HydraGroup.Contract (findHydraGroupById, registerHydraGroup)
import CardanoRacers.HydraGroup.Types (HydraGroupInfo)
import CardanoRacers.Nitro.Helpers (createRacersParams)
import CardanoRacers.Race.Contract (distributeRewardsReturningErrors, startRace)
import CardanoRacers.Race.Types
  ( DistributeRewardsContractError(CouldNotFindRaceStateUtxo)
  , RaceParams
  , StartRaceParams(StartRaceParams)
  , StartRaceResult(StartRaceResult)
  )
import CardanoRacers.RaceRegistry.Types (RaceParticipant)
import CardanoRacers.RacersState.Contract (initRacersStateContract)
import CardanoRacers.RacersState.Types (AssetPrices(AssetPrices), RacersState(RacersState))
import CardanoRacers.Services.HydraDelegate
  ( HostRaceError
      ( HostRace_CollateralUtxoNotAvailableOnL2
      , HostRace_CollateralUtxoNotSpentOnL1
      , HostRace_CommitContractFailed
      )
  , HostRaceRequest
  , SubmitPlayerInputError(SubmitInput_PlayerInputSubmitWindowNotActive)
  , hostRaceRequest
  )
import CardanoRaces.Hydra.Lib.Print (printHex)
import Contract.Address (getNetworkId)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM, runContractInEnv)
import Contract.Test (ContractTest, InitialUTxOs, withKeyWallet, withWallets)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Testnet (TestnetConfig)
import Contract.Transaction (awaitTxConfirmed)
import Contract.Wallet (getWalletAddress, getWalletUtxos, ownPaymentPubKeyHash)
import Control.Monad.Error.Class (liftMaybe, throwError)
import Control.Monad.Reader (ask)
import Control.Parallel (parTraverse_)
import Data.Array (concat, cons, head, range, replicate, singleton, splitAt) as Array
import Data.Array.NonEmpty (length, unsafeIndex) as NEArray
import Data.ByteArray (byteArrayFromAscii)
import Data.Either (Either(Left, Right))
import Data.Int (toNumber) as Int
import Data.Log.Level (LogLevel(Info))
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust, isJust)
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Seconds(Seconds), fromDuration)
import Data.Traversable (traverse, traverse_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested ((/\))
import Data.UInt (fromInt) as UInt
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref (read) as Ref
import HydraSdk.Test (defaultHydraClusterTimeParamsForCardanoTestnet, withHydraCluster)
import HydraSdk.Types (HttpError, HydraHeadStatus(HeadStatus_Open))
import JS.BigInt (fromInt) as BigInt
import Lib.CardanoRacers.Client (submitPlayerInputToDelegates)
import Mote (group, test, only)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafePartial)
import Racers (runRacers)
import Test.QuickCheck.Gen (chooseInt, randomSampleOne, shuffle)
import URI.Port (unsafeFromInt) as Port

type TestParams =
  { numHydraNodes :: Int
  , numRaces :: Int
  , numRaceParticipants :: Int
  }

-- NOTE: Update these values with caution. Tests submit player inputs in
-- parallel, causing up to (`numHydraNodes` * `numRaces` * `numRaceParticipants`)
-- parallel Unity engine invocations, which can be heavy on CPU and RAM.
tp :: TestParams
tp =
  { numHydraNodes: 2
  , numRaces: 2
  , numRaceParticipants: 2
  }

defaultUtxoDistribution :: InitialUTxOs
defaultUtxoDistribution =
  [ BigNum.fromInt 2_000_000_000
  , BigNum.fromInt 2_000_000_000
  ]

suite :: Ref FilePath -> TestnetConfig -> TestPlanM ContractTest Unit
suite nodeSocketPathRef testnetConfig =
  group "Integration" do
    only $ test "1 race, 1 participant" do
      withWallets
        ( defaultUtxoDistribution /\ defaultUtxoDistribution /\ defaultUtxoDistribution /\
            Array.replicate tp.numHydraNodes defaultUtxoDistribution
        )
        \(adminWallet /\ treasuryWallet /\ playerWallet /\ delegateWallets) -> do
          let timeParams = defaultTimeParams
          hydraTest nodeSocketPathRef testnetConfig delegateWallets timeParams \app -> do
            withKeyWallet adminWallet do
              { racersParams, groupInfo } <- doInitialSetup { adminWallet, treasuryWallet }
              { raceParams } <- hostTestRace zero app racersParams groupInfo timeParams
                { playerWallets: Array.singleton playerWallet
                , delegateWallets
                }
              withKeyWallet playerWallet do
                submitTestPlayerInput raceParams groupInfo timeParams
              distributeTestRewards racersParams raceParams timeParams

    test ("1 race, " <> show tp.numRaceParticipants <> " participants") do
      withWallets
        ( defaultUtxoDistribution /\ defaultUtxoDistribution
            /\ Array.replicate tp.numRaceParticipants defaultUtxoDistribution
            /\ Array.replicate tp.numHydraNodes defaultUtxoDistribution
        )
        \(adminWallet /\ treasuryWallet /\ playerWallets /\ delegateWallets) -> do
          let timeParams = defaultTimeParams
          hydraTest nodeSocketPathRef testnetConfig delegateWallets timeParams \app -> do
            withKeyWallet adminWallet do
              { racersParams, groupInfo } <- doInitialSetup { adminWallet, treasuryWallet }
              { raceParams } <- hostTestRace zero app racersParams groupInfo timeParams
                { playerWallets
                , delegateWallets
                }
              -- all players submit their inputs simultaneously
              parTraverse_
                ( \kw -> withKeyWallet kw $ submitTestPlayerInput raceParams groupInfo
                    timeParams
                )
                playerWallets
              distributeTestRewards racersParams raceParams timeParams

    test
      ( show tp.numRaces <> " races, " <> show tp.numRaceParticipants <>
          " participants per race"
      )
      do
        withWallets
          ( defaultUtxoDistribution /\ defaultUtxoDistribution
              /\ Array.replicate (tp.numRaces * tp.numRaceParticipants) defaultUtxoDistribution
              /\ Array.replicate tp.numHydraNodes defaultUtxoDistribution
          )
          \(adminWallet /\ treasuryWallet /\ allPlayerWallets /\ delegateWallets) -> do
            let
              timeParams =
                defaultTimeParams
                  { playerInputSubmitWindowSec = 60
                  }
            hydraTest nodeSocketPathRef testnetConfig delegateWallets timeParams \app -> do
              withKeyWallet adminWallet do
                { racersParams, groupInfo } <- doInitialSetup { adminWallet, treasuryWallet }
                let
                  playerWalletsPerRace =
                    let
                      worker :: Array KeyWallet -> Array (Array KeyWallet)
                      worker xs =
                        let
                          { before, after } = Array.splitAt tp.numRaceParticipants xs
                        in
                          case after of
                            [] -> Array.singleton before
                            _ -> before `Array.cons` worker after
                    in
                      worker allPlayerWallets
                hostedRaces <- traverseWithIndex
                  ( \idx playerWallets -> do
                      { raceParams } <- hostTestRace idx app racersParams groupInfo timeParams
                        { playerWallets
                        , delegateWallets
                        }
                      pure
                        { raceParams
                        , playerWallets
                        }
                  )
                  playerWalletsPerRace
                players <-
                  liftEffect $ randomSampleOne $ shuffle $ Array.concat $ hostedRaces <#>
                    \{ raceParams, playerWallets } ->
                      playerWallets <#> \kw ->
                        kw /\ raceParams
                -- all players submit their inputs simultaneously
                parTraverse_
                  ( \(kw /\ rp) -> withKeyWallet kw $ submitTestPlayerInput rp groupInfo
                      timeParams
                  )
                  players
                traverse_
                  (flip (distributeTestRewards racersParams) timeParams <<< _.raceParams)
                  hostedRaces

type HydraTestInterface =
  { getHeadStatus :: Aff HydraHeadStatus
  }

mkHttpServerPort :: Int -> Int
mkHttpServerPort idx = 7080 + idx

type TestTimeParams =
  { depositPeriodSec :: Int
  , playerInputSubmitWindowSec :: Int
  , defaultRetryDelaySec :: Int
  }

defaultTimeParams :: TestTimeParams
defaultTimeParams =
  { depositPeriodSec: 40
  , playerInputSubmitWindowSec: 20
  , defaultRetryDelaySec: 2
  }

hydraTest
  :: Ref FilePath
  -> TestnetConfig
  -> Array KeyWallet
  -> TestTimeParams
  -> (HydraTestInterface -> Contract Unit)
  -> Contract Unit
hydraTest nodeSocketPathRef testnetConfig wallets timeParams action = do
  nodeSocketPath <- liftEffect $ Ref.read nodeSocketPathRef
  contractEnv <- ask
  mockRaceSimulator <- liftEffect $ isJust <$> lookupEnv "MOCK_RACE_SIMULATOR"
  liftAff $ withHydraCluster wallets
    { mkClusterSpec: \workdir ->
        { workdir
        , nodeSocketPath
        , testnetMagic: testnetConfig.clusterConfig.testnetMagic
        , protocolParametersPath: "pparams-cardano-testnet.json"
        , hydraNodeFirstPort: 7060
        , hydraNodeApiFirstPort: 7070
        , timeParams: defaultHydraClusterTimeParamsForCardanoTestnet
            { contestPeriodSec = Just $ timeParams.depositPeriodSec / 4
            , depositPeriodSec = Just $ timeParams.depositPeriodSec
            }
        }
    , mkPeerExtra: \_ idx ->
        { httpServer:
            { port: UInt.fromInt $ mkHttpServerPort idx
            , host: "127.0.0.1"
            , secure: false
            , path: Nothing
            }
        }
    , startHydraApp: \hydraNodeStartupParams idx -> do
        state <- initApp
          { hydraNodeStartupParams
          , serverPort: Port.unsafeFromInt $ mkHttpServerPort idx
          , queryBackend:
              Kupmios
                { network: TestnetId
                , kupoConfig: testnetConfig.kupoConfig
                , ogmiosConfig: testnetConfig.ogmiosConfig
                }
          , logLevel: Info
          , isHeadLeader: idx == 0
          , timeParams:
              { playerInputSubmitWindowSec: timeParams.playerInputSubmitWindowSec
              }
          , devParams:
              Just
                { mockRaceSimulator
                }
          }
        let logger = appLogger
        hydraNodeHandle <- runApp state logger startHydraNode
        closeHttpServer <- liftEffect $ httpServer state logger
        pure
          { cleanupHandler: cleanupHandler state hydraNodeHandle closeHttpServer
          , state
          , logger
          }
    , runCleanupForHydraApp: liftEffect <<< _.cleanupHandler
    , action: \appHandles -> do
        let
          getRandomApp = do
            idx <- liftEffect $ randomSampleOne $ chooseInt 0 $ NEArray.length appHandles - 1
            pure $ unsafePartial $ NEArray.unsafeIndex appHandles idx -- safe
          runApp' x = do
            app <- getRandomApp
            runApp app.state app.logger x
        runContractInEnv contractEnv $
          action
            { getHeadStatus: runApp' readHeadStatus
            }
    }

distributeTestRewards :: RacersParams -> RaceParams -> TestTimeParams -> Contract Unit
distributeTestRewards racersParams raceParams timeParams = do
  txHash <-
    retryOnError
      "distributeRewards"
      (pure <<< eq CouldNotFindRaceStateUtxo)
      { timeout: Seconds $ Int.toNumber $ timeParams.playerInputSubmitWindowSec * 2
      , delay: Seconds $ Int.toNumber timeParams.defaultRetryDelaySec
      }
      (runRacers racersParams $ distributeRewardsReturningErrors raceParams)
  logInfo' $ "distributeRewards success: " <> printHex txHash

submitTestPlayerInput :: RaceParams -> HydraGroupInfo -> TestTimeParams -> Contract Unit
submitTestPlayerInput raceParams groupInfo timeParams = do
  csv <- liftAff $ readTextFile UTF8 "simulator/test-input.csv"
  retryOnError
    "submitPlayerInput"
    (pure <<< eq SubmitInput_PlayerInputSubmitWindowNotActive)
    { timeout: Seconds $ Int.toNumber $ timeParams.depositPeriodSec * 2
    , delay: Seconds $ Int.toNumber timeParams.defaultRetryDelaySec
    }
    ( submitPlayerInputToDelegates (unwrap raceParams).stateCurrencySymbol
        (unwrap groupInfo).hydraGroupHttpServers
        csv
    )

hostTestRace
  :: Int
  -> HydraTestInterface
  -> RacersParams
  -> HydraGroupInfo
  -> TestTimeParams
  -> { playerWallets :: Array KeyWallet
     , delegateWallets :: Array KeyWallet
     }
  -> Contract
       { raceParams :: RaceParams
       }
hostTestRace idx app racersParams groupInfo timeParams { playerWallets, delegateWallets } = do
  delegatePkhs <-
    traverse
      ( \kw ->
          withKeyWallet kw do
            unwrap <$> liftedM "Could not get delegate pkh" ownPaymentPubKeyHash
      )
      delegateWallets

  playerAddresses <-
    traverse
      ( \kw ->
          withKeyWallet kw do
            addr <- liftedM "Could not get player wallet address" getWalletAddress
            liftContractM "Could not convert player wallet address" $
              Plutus.Address.fromCardano addr
      )
      playerWallets

  -- Start race
  let
    participants = mkTestRaceParticipant <$> playerAddresses
    rewardWeights = [ 50_000, 30_000, 20_000 ] <#> wrap <<< { numerator: _ } <<<
      BigInt.fromInt
  StartRaceResult { txHash: startRaceTxHash, raceParams } <-
    runRacers racersParams $
      startRace
        ( StartRaceParams
            { raceId: unsafePartial fromJust $ byteArrayFromAscii $ "TestRaceId" <> show idx
            , totalRewardValue: lovelaceValueOf $ BigNum.fromInt 7_000_000
            , rewardWeights
            , participants
            , delegates: delegatePkhs
            , feePerDelegate: Just $ lovelaceValueOf $ BigNum.fromInt 3_000_000
            }
        )
  logInfo' $ "startRace success: " <> printHex startRaceTxHash

  -- Wait until the Head is open
  do
    success <- retryOnFalse
      { timeout: Seconds 30.0
      , delay: Seconds $ Int.toNumber timeParams.defaultRetryDelaySec
      }
      (eq HeadStatus_Open <$> liftAff app.getHeadStatus)
    unless success do
      throwError $ error "Head is not in Open state after timeout"

  void $ retryOnError
    "hostRace"
    ( \err -> pure $ err == HostRace_CollateralUtxoNotAvailableOnL2
        || err == HostRace_CollateralUtxoNotSpentOnL1
        || err == HostRace_CommitContractFailed
    )
    { timeout: Seconds $ Int.toNumber $ timeParams.depositPeriodSec * 2
    , delay: Seconds $ Int.toNumber timeParams.defaultRetryDelaySec
    }
    ( hostRace groupInfo startRaceTxHash racersParams raceParams >>=
        case _ of
          Left httpErr ->
            throwError $ error $ "hostRace request failed with error: "
              <> show httpErr
          Right x ->
            pure x
    )
  liftAff $ delay $ fromDuration $ Seconds 10.0
  pure { raceParams }

doInitialSetup
  :: { adminWallet :: KeyWallet
     , treasuryWallet :: KeyWallet
     }
  -> Contract
       { racersParams :: RacersParams
       , groupInfo :: HydraGroupInfo
       }
doInitialSetup { adminWallet, treasuryWallet } = do
  treasuryAddress <-
    withKeyWallet treasuryWallet do
      addr <- liftedM "Could not get treasury address" getWalletAddress
      liftContractM "Could not convert treasury wallet address" $
        Plutus.Address.fromCardano addr

  adminAddress <-
    withKeyWallet adminWallet do
      addr <- liftedM "Could not get admin address" getWalletAddress
      liftContractM "Could not convert admin wallet address" $
        Plutus.Address.fromCardano addr

  withKeyWallet adminWallet do
    racersParams <- createTestRacersParams

    initTestRacersState racersParams
      { operatingAddress: adminAddress
      , treasuryAddress
      }

    hydraGroupId <- registerTestHydraGroup
    logInfo' $ "Registered new Hydra group with ID: " <> printHex hydraGroupId

    -- Discover Hydra group
    { groupInfo } <- liftedM "Could not find Hydra group by ID" $
      findHydraGroupById hydraGroupId
    logInfo' $ "Found valid Hydra group with ID: "
      <> printHex hydraGroupId
      <> ", group info: "
      <> show groupInfo

    pure
      { racersParams
      , groupInfo
      }

createTestRacersParams :: Contract RacersParams
createTestRacersParams = do
  utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
  (nonceOref /\ _) <-
    liftContractM "Could not get first utxo" $ Array.head $
      Map.toUnfoldable utxos
  createRacersParams nonceOref

initTestRacersState
  :: RacersParams
  -> { operatingAddress :: Plutus.Address
     , treasuryAddress :: Plutus.Address
     }
  -> Contract Unit
initTestRacersState racersParams { operatingAddress, treasuryAddress } = do
  let
    racersState = RacersState
      { nitroPrice: BigInt.fromInt 1000000
      , treasuryAddress
      , operatingAddress
      , assetPrices:
          AssetPrices
            { common: BigInt.fromInt 1_000_000
            , rare: BigInt.fromInt 2_000_000
            , epic: BigInt.fromInt 3_000_000
            }
      }
  void $ runRacers racersParams $ initRacersStateContract racersState

registerTestHydraGroup :: Contract ScriptHash
registerTestHydraGroup = do
  ownPkh <- unwrap <$> liftedM "Could not get own pkh" ownPaymentPubKeyHash
  let
    masterKeys = Array.singleton ownPkh
    httpServers =
      Array.range 0 (tp.numHydraNodes - 1) <#> \idx ->
        "http://127.0.0.1:" <> show (mkHttpServerPort idx)
    metadata = "Demo group"
  { txHash, groupId } <- registerHydraGroup masterKeys httpServers metadata
  awaitTxConfirmed txHash
  pure groupId

hostRace
  :: HydraGroupInfo
  -> TransactionHash
  -> RacersParams
  -> RaceParams
  -> Contract (Either HttpError (Either HostRaceError TransactionHash))
hostRace groupInfo startRaceTxHash racersParams raceParams = do
  network <- getNetworkId
  httpServer <- liftMaybe (error "Could not get httpServer") $ Array.head
    (unwrap groupInfo).hydraGroupHttpServers
  liftAff $ hostRaceRequest httpServer network reqBody
  where
  reqBody :: HostRaceRequest
  reqBody = wrap
    { raceOref: wrap { transactionId: startRaceTxHash, index: zero }
    , racersParams: Just racersParams
    , raceParams
    }

mkTestRaceParticipant :: Plutus.Address -> RaceParticipant
mkTestRaceParticipant payoutAddress =
  wrap
    { car: assetNameFromAsciiUnsafe "TestCar"
    , driver: assetNameFromAsciiUnsafe "TestDriver"
    , payoutAddress
    }
