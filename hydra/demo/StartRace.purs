module CardanoRacers.Hydra.Demo.StartRace
  ( main
  ) where

import Prelude

import Cardano.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.ToData (toData)
import Cardano.Types (Ed25519KeyHash, ScriptHash, TransactionHash, Value)
import Cardano.Types.BigNum (fromInt) as BigNum
import Cardano.Types.Value (lovelaceValueOf)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Hydra.Handlers.HostRace
  ( HostRaceRequest
  , HostRaceResponse
  , hostRaceRequestCodec
  , hostRaceResponseCodec
  )
import CardanoRacers.Hydra.Monad (initContractEnv)
import CardanoRacers.Hydra.Services.Utils (handleResponse, postRequest)
import CardanoRacers.Hydra.Types.ServerResponse
  ( ServerResponse(ServerResponseError, ServerResponseSuccess)
  )
import CardanoRacers.HydraGroup.Contract (disbandHydraGroup, findHydraGroupById)
import CardanoRacers.HydraGroup.Contract (registerHydraGroup) as HydraGroup
import CardanoRacers.HydraGroup.Types (HydraGroupInfo)
import CardanoRacers.Nitro.Helpers (createRacersParams)
import CardanoRacers.Race.Contract (startRace)
import CardanoRacers.Race.Types (RaceParams)
import CardanoRacers.RaceSlot.Types (RaceHash)
import Contract.CborBytes (cborBytesToHex, hexToCborBytes)
import Contract.Monad (Contract, liftContractM, liftedM, runContractInEnv)
import Contract.Wallet (getWalletUtxos, ownPaymentPubKeyHash)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Contract.AwaitTxConfirmed (awaitTxConfirmed)
import Ctl.Internal.Helpers ((<</>>))
import Data.Array (head, singleton) as Array
import Data.ByteArray (byteArrayFromAscii)
import Data.Codec.Argonaut (JsonCodec, encode, object, printJsonDecodeError, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Either (Either(Left, Right))
import Data.Log.Level (LogLevel(Trace))
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Seconds(Seconds), convertDuration)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, throw)
import HydraSdk.Lib (caDecodeFile)
import HydraSdk.Types (HttpError)
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
              contractEnv <- initContractEnv cfg.blockfrostApiKeyFile cfg.signingKeyFile Trace
              runContractInEnv contractEnv do
                -- Register Hydra group
                groupId <- registerHydraGroup
                logAndDelay $ "Registered new Hydra group with ID: " <> toHex groupId

                -- Create RacersParams
                utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
                (nonceOref /\ _) <-
                  liftContractM "Could not get first utxo" $ Array.head $
                    Map.toUnfoldable utxos
                racersParams <- createRacersParams nonceOref

                -- Start race
                { txHash, raceParams } <-
                  runRacers racersParams $
                    startRace Nothing raceHashFixture totalRewardValueFixture
                      participantsFixture
                      delegatesFixture
                logAndDelay $ "startRace success: " <> toHex txHash

                -- Discover Hydra group
                groupEntry@{ groupInfo } <- liftedM "Could not find Hydra group by ID" $
                  findHydraGroupById groupId
                logAndDelay $ "Found valid Hydra group with ID: "
                  <> toHex groupId
                  <> ", group info: "
                  <> show groupInfo

                -- Host L2 race
                resp <- hostRace groupInfo txHash racersParams raceParams
                liftEffect case resp of
                  Left httpError ->
                    throw $ "host request failed: " <> show httpError
                  Right (ServerResponseError hostRaceError) ->
                    throw $ "could not host race: " <> show hostRaceError
                  Right (ServerResponseSuccess { commitTxHash }) ->
                    log $ "hostRace success: " <> toHex commitTxHash

                -- Disband Hydra group
                disbandTxHash <- disbandHydraGroup groupEntry.oref
                logAndDelay $ "Successfully disbanded Hydra group with ID: "
                  <> toHex groupId
                  <> ", TX hash: "
                  <> toHex disbandTxHash
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
  -> Contract (Either HttpError HostRaceResponse)
hostRace groupInfo startRaceTxHash racersParams raceParams = do
  httpServer <- liftMaybe (error "Could not get httpServer") $ Array.head
    (unwrap groupInfo).hydraGroupHttpServers
  liftAff $ handleResponse hostRaceResponseCodec <$>
    postRequest
      { url: httpServer <</>> "hostRace"
      , content: Just $ CA.encode hostRaceRequestCodec reqBody
      , headers: mempty
      }
  where
  reqBody :: HostRaceRequest
  reqBody =
    { raceOref: wrap { transactionId: startRaceTxHash, index: zero }
    , racersParams
    , raceParams: encodeCbor $ toData raceParams
    }

raceHashFixture :: RaceHash
raceHashFixture = unsafePartial fromJust $ byteArrayFromAscii "TestRaceHash"

totalRewardValueFixture :: Value
totalRewardValueFixture = lovelaceValueOf $ BigNum.fromInt 7_000_000

-- TODO: Add participants
participantsFixture :: Array Plutus.Address
participantsFixture = []

delegatesFixture :: Array Ed25519KeyHash
delegatesFixture =
  keyHashFromHex <$>
    [ "0e0607203c2ab6f2f729e2317502277191c681283637d5ee6b2a9933"
    , "35c92e61b4f915ce7615ea8b8ece661843e6bc5f591fe99036d388a9"
    ]

keyHashFromHex :: String -> Ed25519KeyHash
keyHashFromHex str = unsafePartial fromJust $ decodeCbor =<< hexToCborBytes str

logAndDelay :: forall (m :: Type -> Type). MonadAff m => String -> m Unit
logAndDelay str = do
  liftEffect $ log str
  liftAff $ delay $ convertDuration $ Seconds 2.0

toHex :: forall (a :: Type). AsCbor a => a -> String
toHex = cborBytesToHex <<< encodeCbor
