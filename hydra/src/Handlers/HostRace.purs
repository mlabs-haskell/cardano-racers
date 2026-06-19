module CardanoRacers.Hydra.Handlers.HostRace
  ( HostRaceError
      ( CouldNotDecodeReqBody
      , RacersParamsNotProvided
      , CollateralUtxoNotAvailable
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
import CardanoRacers.Hydra.Contracts.Common (findCollateralUtxo)
import CardanoRacers.Hydra.Monad
  ( AppM
  , initRace
  , liftContract
  , readHeadStatus
  , readHydraSnapshot
  )
import CardanoRacers.Services.HydraDelegate
  ( HostRaceRequest(HostRaceRequest)
  , hostRaceRequestCodec
  )
import CardanoRaces.Hydra.Lib.Print (printHex)
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
import Data.Map (union) as Map
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import HTTPure (Response) as HTTPure
import HTTPure (Status, response)
import HTTPure.Status (badRequest, conflict, created) as Status
import HydraSdk.Lib (caDecodeString, txHashCodec)
import HydraSdk.Types (HydraHeadStatus(HeadStatus_Open), headStatusCodec, toUtxoMap)

hostRaceHandler :: String -> AppM HTTPure.Response
hostRaceHandler bodyStr = do
  resp <- hostRaceHandlerImpl bodyStr
  case resp of
    Left err ->
      response (errorStatus err) $ stringifyAeson $ CA.encode hostRaceErrorCodec err
    Right txHash ->
      response Status.created $ stringifyAeson $ CA.encode txHashCodec txHash

hostRaceHandlerImpl :: String -> AppM (Either HostRaceError TransactionHash)
hostRaceHandlerImpl bodyStr =
  runExceptT do
    network <- lift $ liftContract getNetworkId
    HostRaceRequest reqBody <-
      liftEither $
        lmap (CouldNotDecodeReqBody <<< { decodeError: _ } <<< CA.printJsonDecodeError)
          (caDecodeString (hostRaceRequestCodec network) bodyStr)
    racersParams <- reqBody.racersParams ?? RacersParamsNotProvided
    {-
    _ <- do
      snapshot <- lift readHydraSnapshot
      let
        finalizedUtxos = toUtxoMap (unwrap snapshot).utxo
        utxos = maybe finalizedUtxos (Map.union finalizedUtxos <<< toUtxoMap)
          (unwrap snapshot).utxoToCommit
      findCollateralUtxo utxos ?? CollateralUtxoNotAvailable
    -}
    headStatus <- lift readHeadStatus
    let headStatusExpected = HeadStatus_Open
    when (headStatus /= headStatusExpected) do
      throwError $ InvalidHeadStatus
        { expected: headStatusExpected
        , actual: headStatus
        }
    raceOut <- liftContract (getUtxo reqBody.raceOref) !? CouldNotResolveRaceOref
    { txHash: depositTxId, raceValidator } <- lift $ commitRaceUtxoToHydra
      (reqBody.raceOref /\ raceOut)
      racersParams
      reqBody.raceParams
    logInfo' $ "Successfully commited RaceState utxo: " <> printHex depositTxId
    lift $ initRace depositTxId
      { racersParams
      , raceParams: reqBody.raceParams
      , raceValidator
      }
    pure depositTxId

-- Error

data HostRaceError
  = CouldNotDecodeReqBody { decodeError :: String }
  | RacersParamsNotProvided
  | CollateralUtxoNotAvailable
  | CouldNotResolveRaceOref
  | InvalidHeadStatus { expected :: HydraHeadStatus, actual :: HydraHeadStatus }

derive instance Generic HostRaceError _
derive instance Eq HostRaceError

instance Show HostRaceError where
  show = genericShow

hostRaceErrorCodec :: CA.JsonCodec HostRaceError
hostRaceErrorCodec =
  CAS.sumFlat "HostRaceError"
    { "CouldNotDecodeReqBody":
        CAR.record
          { decodeError: CA.string
          }
    , "RacersParamsNotProvided": unit
    , "CollateralUtxoNotAvailable": unit
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
    CouldNotDecodeReqBody _ ->
      Status.badRequest
    RacersParamsNotProvided ->
      Status.badRequest
    CollateralUtxoNotAvailable ->
      Status.conflict
    CouldNotResolveRaceOref ->
      Status.badRequest
    InvalidHeadStatus _ ->
      Status.conflict
