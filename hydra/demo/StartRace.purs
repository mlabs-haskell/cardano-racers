module CardanoRacers.Hydra.Demo.StartRace
  ( main
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.ToData (toData)
import Cardano.Types (Ed25519KeyHash, TransactionHash, Value)
import Cardano.Types.BigNum (fromInt) as BigNum
import Cardano.Types.Value (lovelaceValueOf)
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
import CardanoRacers.Nitro.Helpers (createRacersParams)
import CardanoRacers.Race.Contract (startRace)
import CardanoRacers.Race.Types (RaceParams)
import CardanoRacers.RaceSlot.Types (RaceHash)
import Contract.CborBytes (hexToCborBytes)
import Contract.Monad (liftContractM, liftedM, runContractInEnv)
import Contract.Wallet (getWalletUtxos)
import Data.Array (head) as Array
import Data.ByteArray (byteArrayFromAscii)
import Data.Codec.Argonaut (JsonCodec, encode, object, printJsonDecodeError, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Either (Either(Left, Right))
import Data.Log.Level (LogLevel(Trace))
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(Just), fromJust)
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
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
                utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
                (nonceOref /\ _) <-
                  liftContractM "Could not get first utxo" $ Array.head $
                    Map.toUnfoldable utxos
                racersParams <- createRacersParams nonceOref
                { txHash, raceParams } <- runRacers racersParams $ startRace raceHashFixture
                  totalRewardValueFixture
                  participantsFixture
                  delegatesFixture
                liftEffect $ log $ "startRace success: " <> show txHash
                resp <- liftAff $ hostRace txHash raceParams
                liftEffect case resp of
                  Left httpError ->
                    throw $ "host request failed: " <> show httpError
                  Right (ServerResponseError hostRaceError) ->
                    throw $ "could not host race: " <> show hostRaceError
                  Right (ServerResponseSuccess { commitTxHash }) ->
                    log $ "hostRace success: " <> show commitTxHash
          Left decodeErr ->
            throw $ "could not decode config: " <>
              CA.printJsonDecodeError decodeErr
    _ ->
      throw "invalid command-line arguments"

hostRace :: TransactionHash -> RaceParams -> Aff (Either HttpError HostRaceResponse)
hostRace startRaceTxHash raceParams =
  handleResponse hostRaceResponseCodec <$>
    postRequest
      { url: "http://127.0.0.1:7010/hostRace"
      , content: Just $ CA.encode hostRaceRequestCodec reqBody
      , headers: mempty
      }
  where
  reqBody :: HostRaceRequest
  reqBody =
    { raceOref: wrap { transactionId: startRaceTxHash, index: zero }
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
