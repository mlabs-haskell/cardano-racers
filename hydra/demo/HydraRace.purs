module CardanoRacers.Hydra.Demo.HydraRace
  ( main
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor)
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
  , HostRaceRequest
  , SubmitPlayerInputError(SubmitInput_PlayerInputSubmitWindowNotActive)
  , hostRaceRequest
  )
import CardanoRaces.Hydra.Lib.Print (printHex)
import Contract.Address (getNetworkId)
import Contract.CborBytes (hexToCborBytes)
import Contract.Monad (Contract, liftContractM, liftedM, runContractInEnv)
import Contract.Wallet (getWalletAddress, getWalletUtxos, ownPaymentPubKeyHash)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Contract.AwaitTxConfirmed (awaitTxConfirmed)
import Data.Array (head, singleton) as Array
import Data.ByteArray (byteArrayFromAscii)
import Data.Codec.Argonaut (JsonCodec, object, printJsonDecodeError, string) as CA
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
import HydraSdk.Lib (caDecodeFile)
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
  }

configCodec :: CA.JsonCodec Config
configCodec =
  CA.object "Config" $ CAR.record
    { blockfrostApiKeyFile: CA.string
    , signingKeyFile: CA.string
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
                groupId <- registerHydraGroup
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
                          , delegates: delegatesFixture
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
                resp <- hostRace groupInfo startRaceTxHash racersParams raceParams
                liftEffect case resp of
                  Left httpErr ->
                    throw $ "hostRace request failed: " <> show httpErr
                  Right (Left domainErr) ->
                    throw $ "hostRace endpoint returned error: " <> show domainErr
                  Right (Right txHash) ->
                    log $ "hostRace success: " <> printHex txHash

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

registerHydraGroup :: Contract ScriptHash
registerHydraGroup = do
  ownPkh <- unwrap <$> liftedM "Could not get own pkh" ownPaymentPubKeyHash
  let
    masterKeys = Array.singleton ownPkh
    httpServers = [ "http://127.0.0.1:7010", "http://127.0.0.1:7012" ]
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

delegatesFixture :: Array Ed25519KeyHash
delegatesFixture =
  keyHashFromHex <$>
    [ "0e0607203c2ab6f2f729e2317502277191c681283637d5ee6b2a9933"
    , "35c92e61b4f915ce7615ea8b8ece661843e6bc5f591fe99036d388a9"
    ]

mkRaceParticipantFixture :: Plutus.Address -> RaceParticipant
mkRaceParticipantFixture payoutAddress =
  wrap
    { car: assetNameFromAsciiUnsafe "TestCar"
    , driver: assetNameFromAsciiUnsafe "TestDriver"
    , payoutAddress
    }

keyHashFromHex :: String -> Ed25519KeyHash
keyHashFromHex str = unsafePartial fromJust $ decodeCbor =<< hexToCborBytes str

logAndDelay :: forall (m :: Type -> Type). MonadAff m => String -> m Unit
logAndDelay str = do
  liftEffect $ log str
  liftAff $ delay $ convertDuration $ Seconds 2.0
