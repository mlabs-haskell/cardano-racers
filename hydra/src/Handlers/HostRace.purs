module CardanoRacers.Hydra.Handlers.HostRace
  ( HostRaceError
      ( CouldNotDecodeRequestBody
      , RacersParamsNotProvided
      , CouldNotResolveRaceOref
      , InvalidHeadStatus
      )
  , hostRaceErrorCodec
  , hostRaceHandler
  , hostRaceHandlerImpl
  ) where

import Prelude

import Aeson (stringifyAeson)
import Cardano.Types (TransactionHash)
import CardanoRacers.Hydra.Contracts.Commit (commitRaceUtxoToHydra)
import CardanoRacers.Hydra.Monad (AppM, liftContract, readHeadStatus, setRaceData)
import CardanoRacers.Services.HydraDelegate
  ( HostRaceRequest(HostRaceRequest)
  , hostRaceRequestCodec
  )
import Contract.Address (getNetworkId)
import Contract.Log (logInfo')
import Contract.Utxos (getUtxo)
import Control.Error.Util ((!?), (??))
import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, encode, printJsonDecodeError, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Sum (sumFlat) as CAS
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import HTTPure (Response) as HTTPure
import HTTPure (Status, response)
import HTTPure.Status (badRequest, conflict, created) as Status
import HydraSdk.Lib (caDecodeString, txHashCodec)
import HydraSdk.Types (HydraHeadStatus(HeadStatus_Initializing), headStatusCodec)

hostRaceHandler :: String -> AppM HTTPure.Response
hostRaceHandler bodyStr = do
  resp <- hostRaceHandlerImpl bodyStr
  case resp of
    Left err ->
      response (errorStatus err) $ stringifyAeson $ CA.encode hostRaceErrorCodec err
    Right txHash ->
      response Status.created $ stringifyAeson $ CA.encode txHashCodec txHash

-- TODO: check if another hosting request is currently being processed
hostRaceHandlerImpl :: String -> AppM (Either HostRaceError TransactionHash)
hostRaceHandlerImpl bodyStr =
  runExceptT do
    network <- lift $ liftContract getNetworkId
    HostRaceRequest reqBody <-
      liftEither $
        lmap (CouldNotDecodeRequestBody <<< { decodeErr: _ } <<< CA.printJsonDecodeError)
          (caDecodeString (hostRaceRequestCodec network) bodyStr)
    racersParams <- reqBody.racersParams ?? RacersParamsNotProvided
    headStatus <- lift readHeadStatus
    let headStatusExpected = HeadStatus_Initializing
    when (headStatus /= headStatusExpected) do
      throwError $ InvalidHeadStatus
        { expected: headStatusExpected
        , actual: headStatus
        }
    raceOut <- liftContract (getUtxo reqBody.raceOref) !? CouldNotResolveRaceOref
    { txHash, raceValidator } <- lift $ commitRaceUtxoToHydra (reqBody.raceOref /\ raceOut)
      racersParams
      reqBody.raceParams
    logInfo' $ "Successfully commited RaceState utxo: " <> show txHash
    lift $ setRaceData
      { racersParams
      , raceParams: reqBody.raceParams
      , raceValidator
      }
    pure txHash

-- Error

data HostRaceError
  = CouldNotDecodeRequestBody { decodeErr :: String }
  | RacersParamsNotProvided
  | CouldNotResolveRaceOref
  | InvalidHeadStatus { expected :: HydraHeadStatus, actual :: HydraHeadStatus }

derive instance Generic HostRaceError _
derive instance Eq HostRaceError

instance Show HostRaceError where
  show = genericShow

hostRaceErrorCodec :: CA.JsonCodec HostRaceError
hostRaceErrorCodec =
  CAS.sumFlat "HostRaceError"
    { "CouldNotDecodeRequestBody":
        CAR.record
          { decodeErr: CA.string
          }
    , "RacersParamsNotProvided": unit
    , "CouldNotResolveRaceOref": unit
    , "InvalidHeadStatus":
        CAR.record
          { expected: headStatusCodec
          , actual: headStatusCodec
          }
    }

errorStatus :: HostRaceError -> Status
errorStatus =
  case _ of
    CouldNotDecodeRequestBody _ ->
      Status.badRequest
    RacersParamsNotProvided ->
      Status.badRequest
    CouldNotResolveRaceOref ->
      Status.badRequest
    InvalidHeadStatus _ ->
      Status.conflict
