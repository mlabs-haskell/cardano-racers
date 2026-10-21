module CardanoRacers.Hydra.Demo.HydraRace
  ( main
  ) where

import Prelude

import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Address (fromCardano) as Plutus.Address
import Cardano.Types (Ed25519KeyHash, ScriptHash, TransactionHash, Value)
import Cardano.Types.BigNum (fromInt) as BigNum
import Cardano.Types.Value (lovelaceValueOf)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Helpers (assetNameFromAsciiUnsafe)
import CardanoRacers.Hydra.Config (AppQueryBackend(Blockfrost))
import CardanoRacers.Hydra.Lib.Retry (retryOnError)
import CardanoRacers.Hydra.Monad (initContractEnv)
import CardanoRacers.HydraGroup.Contract (disbandHydraGroup, findHydraGroupById)
import CardanoRacers.HydraGroup.Contract (registerHydraGroup) as HydraGroup
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
import CardanoRacers.RaceSlot.Types (RaceHash)
import CardanoRacers.RacersState.Contract (initRacersStateContract)
import CardanoRacers.RacersState.Types (AssetPrices(AssetPrices), RacersState(RacersState))
import CardanoRacers.Services.HydraDelegate
  ( HostRaceError
      ( HostRace_CollateralUtxoNotAvailableOnL2
      , HostRace_CollateralUtxoNotSpentOnL1
      , HostRace_CommitContractFailed
      , HostRace_InvalidHeadStatus
      )
  , HostRaceRequest
  , SubmitPlayerInputError(SubmitInput_PlayerInputSubmitWindowNotActive)
  , hostRaceRequest
  )
import CardanoRaces.Hydra.Lib.Print (printHex)
import Contract.Address (getNetworkId)
import Contract.Monad (Contract, liftContractM, liftedM, runContractInEnv)
import Contract.Wallet (getWalletAddress, getWalletUtxos, ownPaymentPubKeyHash)
import Control.Monad.Error.Class (liftMaybe, throwError)
import Ctl.Internal.Contract.AwaitTxConfirmed (awaitTxConfirmed)
import Data.Array (head, singleton) as Array
import Data.ByteArray (byteArrayFromAscii)
import Data.Codec.Argonaut (JsonCodec, array, object, printJsonDecodeError, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Either (Either(Left, Right))
import Data.Log.Level (LogLevel(Trace))
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(Just), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Minutes(Minutes), Seconds(Seconds), convertDuration)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, throw)
import HydraSdk.Lib (caDecodeFile, ed25519KeyHashCodec)
import HydraSdk.Types (HttpError)
import JS.BigInt (fromInt) as BigInt
import Lib.CardanoRacers.Client (submitPlayerInputToDelegates)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)
import Node.Process (argv)
import Partial.Unsafe (unsafePartial)
import Racers (runRacers)

type Config =
  { blockfrostApiKeyFile :: FilePath
  , signingKeyFile :: FilePath
  , hydraGroupHttpServers :: Array String
  , delegatePkhs :: Array Ed25519KeyHash
  }

configCodec :: CA.JsonCodec Config
configCodec =
  CA.object "Config" $ CAR.record
    { blockfrostApiKeyFile: CA.string
    , signingKeyFile: CA.string
    , hydraGroupHttpServers: CA.array CA.string
    , delegatePkhs: CA.array ed25519KeyHashCodec
    }

main :: Effect Unit
main = do
  args <- argv
  case args of
    [ _, _, configPath ] ->
      caDecodeFile configCodec configPath >>=
        case _ of
          Right cfg ->
            launchAff_ do
              contractEnv <- initContractEnv
                (Blockfrost { apiKeyFile: cfg.blockfrostApiKeyFile })
                cfg.signingKeyFile
                Trace
              runContractInEnv contractEnv do
                -- Register Hydra group
                groupId <- registerHydraGroup cfg.hydraGroupHttpServers
                logAndDelay $ "Registered new Hydra group with ID: " <> printHex groupId

                -- Create RacersParams
                utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
                (nonceOref /\ _) <-
                  liftContractM "Could not get first utxo" $ Array.head $
                    Map.toUnfoldable utxos
                racersParams <- createRacersParams nonceOref

                -- Init Racers state
                plutusAddr <- do
                  addr <- liftedM "Could not get wallet address" getWalletAddress
                  liftMaybe (error "Could not convert wallet address") $
                    Plutus.Address.fromCardano addr
                let
                  racersState = RacersState
                    { nitroPrice: BigInt.fromInt 1000000
                    , treasuryAddress: plutusAddr
                    , operatingAddress: plutusAddr
                    , assetPrices:
                        AssetPrices
                          { common: BigInt.fromInt 1_000_000
                          , rare: BigInt.fromInt 2_000_000
                          , epic: BigInt.fromInt 3_000_000
                          }
                    }
                void $ runRacers racersParams $ initRacersStateContract racersState

                -- Start race
                let
                  -- one participant
                  participants = Array.singleton $ mkRaceParticipantFixture plutusAddr
                  rewardWeights = [ 50_000, 30_000, 20_000 ] <#> wrap <<< { numerator: _ } <<<
                    BigInt.fromInt
                StartRaceResult { txHash: startRaceTxHash, raceParams } <-
                  runRacers racersParams $
                    startRace
                      ( StartRaceParams
                          { raceId: raceHashFixture
                          , totalRewardValue: totalRewardValueFixture
                          , rewardWeights
                          , participants
                          , delegates: cfg.delegatePkhs
                          , feePerDelegate: Just $ lovelaceValueOf $ BigNum.fromInt 3_000_000
                          }
                      )
                logAndDelay $ "startRace success: "
                  <> printHex startRaceTxHash

                -- Discover Hydra group
                groupEntry@{ groupInfo } <- liftedM "Could not find Hydra group by ID" $
                  findHydraGroupById groupId
                logAndDelay $ "Found valid Hydra group with ID: "
                  <> printHex groupId
                  <> ", group info: "
                  <> show groupInfo

                -- Host L2 race
                hostRaceTxHash <-
                  retryOnError
                    "hostRace"
                    ( \err -> pure $ case err of
                        HostRace_CollateralUtxoNotAvailableOnL2 -> true
                        HostRace_CollateralUtxoNotSpentOnL1 -> true
                        HostRace_CommitContractFailed -> true
                        HostRace_InvalidHeadStatus _ -> true
                        _ -> false
                    )
                    { timeout: Minutes 10.0, delay: Seconds 30.0 }
                    ( hostRace groupInfo startRaceTxHash racersParams raceParams >>=
                        case _ of
                          Left httpErr ->
                            throwError $ error $ "hostRace request failed: "
                              <> show httpErr
                          Right x -> pure x
                    )
                liftEffect $ log $ "hostRace success: " <> printHex hostRaceTxHash

                -- Submit player input
                do
                  csv <- liftAff $ readTextFile UTF8 "simulator/test-input.csv"
                  retryOnError
                    "submitPlayerInput"
                    (pure <<< eq SubmitInput_PlayerInputSubmitWindowNotActive)
                    { timeout: Minutes 10.0, delay: Seconds 30.0 } $
                    submitPlayerInputToDelegates (unwrap raceParams).stateCurrencySymbol
                      (unwrap groupInfo).hydraGroupHttpServers
                      csv

                -- Distribute rewards
                do
                  txHash <-
                    retryOnError
                      "distributeRewards"
                      (pure <<< eq CouldNotFindRaceStateUtxo)
                      { timeout: Minutes 20.0, delay: Seconds 30.0 }
                      (runRacers racersParams $ distributeRewardsReturningErrors raceParams)
                  logAndDelay $ "distributeRewards success: "
                    <> printHex txHash

                -- Disband Hydra group
                disbandTxHash <- disbandHydraGroup groupEntry.oref
                logAndDelay $ "Successfully disbanded Hydra group with ID: "
                  <> printHex groupId
                  <> ", TX hash: "
                  <> printHex disbandTxHash
                pure unit
          Left decodeErr ->
            throw $ "could not decode config: " <>
              CA.printJsonDecodeError decodeErr
    _ ->
      throw "invalid command-line arguments"

registerHydraGroup :: Array String -> Contract ScriptHash
registerHydraGroup httpServers = do
  ownPkh <- unwrap <$> liftedM "Could not get own pkh" ownPaymentPubKeyHash
  let
    masterKeys = Array.singleton ownPkh
    metadata = "Demo group"
  { txHash, groupId } <- HydraGroup.registerHydraGroup masterKeys httpServers metadata
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

raceHashFixture :: RaceHash
raceHashFixture = unsafePartial fromJust $ byteArrayFromAscii "TestRaceHash"

totalRewardValueFixture :: Value
totalRewardValueFixture = lovelaceValueOf $ BigNum.fromInt 7_000_000

mkRaceParticipantFixture :: Plutus.Address -> RaceParticipant
mkRaceParticipantFixture payoutAddress =
  wrap
    { car: assetNameFromAsciiUnsafe "TestCar"
    , driver: assetNameFromAsciiUnsafe "TestDriver"
    , payoutAddress
    }

logAndDelay :: forall (m :: Type -> Type). MonadAff m => String -> m Unit
logAndDelay str = do
  liftEffect $ log str
  liftAff $ delay $ convertDuration $ Seconds 2.0
