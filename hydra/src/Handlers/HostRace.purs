module CardanoRacers.Hydra.Handlers.HostRace
  ( hostRaceHandler
  , hostRaceHandlerImpl
  ) where

import Prelude

import Aeson (stringifyAeson)
import Cardano.Types (TransactionHash)
import CardanoRacers.Hydra.Contracts.Commit (commitRaceUtxoToHydra)
import CardanoRacers.Hydra.Contracts.Common (findCollateralUtxo)
import CardanoRacers.Hydra.Monad
  ( AppM
  , getHydraNodeBaseUrl
  , initRace
  , liftContract
  , readHeadStatus
  )
import CardanoRacers.Services.HydraDelegate
  ( HostRaceError
      ( HostRace_CouldNotDecodeReqBody
      , HostRace_RacersParamsNotProvided
      , HostRace_CouldNotResolveRaceOref
      , HostRace_InvalidHeadStatus
      , HostRace_CouldNotQuerySnapshotUtxos
      , HostRace_CollateralUtxoNotAvailableOnL2
      , HostRace_CollateralUtxoNotSpentOnL1
      , HostRace_CommitContractFailed
      )
  , HostRaceRequest(HostRaceRequest)
  , hostRaceErrorCodec
  , hostRaceRequestCodec
  )
import CardanoRaces.Hydra.Lib.Print (printHex)
import Contract.Address (getNetworkId)
import Contract.Log (logError', logInfo')
import Contract.Utxos (getUtxo)
import Control.Error.Util ((!?), (??))
import Control.Monad.Error.Class (catchError, liftEither, throwError)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (encode, printJsonDecodeError) as CA
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (liftAff)
import HTTPure (Response) as HTTPure
import HTTPure (Status, response)
import HTTPure.Status (badRequest, conflict, created, internalServerError) as Status
import HydraSdk.Lib (caDecodeString, txHashCodec)
import HydraSdk.NodeApi (getConfirmedSnapshotUtxos)
import HydraSdk.Types (HydraHeadStatus(HeadStatus_Open), toUtxoMap)

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
        lmap
          (HostRace_CouldNotDecodeReqBody <<< { decodeError: _ } <<< CA.printJsonDecodeError)
          (caDecodeString (hostRaceRequestCodec network) bodyStr)
    racersParams <- reqBody.racersParams ?? HostRace_RacersParamsNotProvided
    do
      headStatus <- lift readHeadStatus
      let headStatusExpected = HeadStatus_Open
      when (headStatus /= headStatusExpected) do
        throwError $ HostRace_InvalidHeadStatus
          { expected: headStatusExpected
          , actual: headStatus
          }
    ExceptT checkCollateral
    raceOut <- liftContract (getUtxo reqBody.raceOref) !? HostRace_CouldNotResolveRaceOref
    let
      commit = lift $ commitRaceUtxoToHydra (reqBody.raceOref /\ raceOut) racersParams
        reqBody.raceParams
    { txHash: depositTxId, raceValidator } <-
      commit `catchError` \err -> do
        logError' $ "commitRaceUtxoToHydra failed with error: "
          <> show err
        throwError HostRace_CommitContractFailed
    logInfo' $ "Successfully commited RaceState utxo: " <> printHex depositTxId
    lift $ initRace depositTxId
      { racersParams
      , raceParams: reqBody.raceParams
      , raceValidator
      }
    pure depositTxId

checkCollateral :: AppM (Either HostRaceError Unit)
checkCollateral =
  runExceptT do
    hydraNodeBaseUrl <- lift getHydraNodeBaseUrl
    snapshotUtxos <-
      liftAff (getConfirmedSnapshotUtxos hydraNodeBaseUrl) >>=
        case _ of
          Right x -> pure x
          Left httpErr -> do
            logError' $ "getConfirmedSnapshotUtxos query failed with error: "
              <> show httpErr
            throwError HostRace_CouldNotQuerySnapshotUtxos
    _ <- findCollateralUtxo (toUtxoMap snapshotUtxos) ??
      HostRace_CollateralUtxoNotAvailableOnL2
    { collateralUtxo } <- lift ask
    case collateralUtxo of
      Nothing ->
        pure unit
      Just (collateralOref /\ _) ->
        lift (liftContract $ getUtxo collateralOref) >>=
          case _ of
            Nothing ->
              pure unit
            Just _ ->
              throwError HostRace_CollateralUtxoNotSpentOnL1

-- Error

errorStatus :: HostRaceError -> Status
errorStatus =
  case _ of
    HostRace_CouldNotDecodeReqBody _ ->
      Status.badRequest
    HostRace_RacersParamsNotProvided ->
      Status.badRequest
    HostRace_CouldNotResolveRaceOref ->
      Status.badRequest
    HostRace_InvalidHeadStatus _ ->
      Status.conflict
    HostRace_CouldNotQuerySnapshotUtxos ->
      Status.internalServerError
    HostRace_CollateralUtxoNotAvailableOnL2 ->
      Status.conflict
    HostRace_CollateralUtxoNotSpentOnL1 ->
      Status.conflict
    HostRace_CommitContractFailed ->
      Status.internalServerError
