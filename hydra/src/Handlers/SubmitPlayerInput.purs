module CardanoRacers.Hydra.Handlers.SubmitPlayerInput where

import Prelude

import Aeson (stringifyAeson)
import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Credential (Credential(PubKeyCredential)) as Plutus
import Cardano.Types (Ed25519KeyHash)
import CardanoRacers.Hydra.Monad (AppM, getAppRunner, readRaceData)
import CardanoRacers.Hydra.RaceSimulator
  ( RaceSimulationError
  , raceSimulationErrorCodec
  , runSimulator
  )
import CardanoRacers.Race.Types (RaceParams(RaceParams))
import Control.Monad.Error.Class (liftEither, liftMaybe, throwError)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Array (find) as Array
import Data.Bifunctor (lmap)
import Data.ByteArray (ByteArray)
import Data.Codec.Argonaut (JsonCodec, encode, object, printJsonDecodeError, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Sum (sumFlat) as CAS
import Data.Either (Either, either)
import Data.Generic.Rep (class Generic)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Effect.Aff (bracket)
import Effect.Aff.AVar (put, tryPut, tryRead, tryTake) as AVar
import Effect.Aff.Class (liftAff)
import HTTPure (Response) as HTTPure
import HTTPure (Status, created, response)
import HTTPure.Status (badRequest, conflict, forbidden, internalServerError) as Status
import HydraSdk.Lib (byteArrayCodec, caDecodeString, ed25519KeyHashCodec)

-- TODO:
-- 1. Parse and validate CSV
-- 2. DONE - Verify that the user is a race participant
-- 3. DONE - Ensure this participant has not already submitted input
-- 4. Verify the signature
-- 5. DONE - Run the simulation
-- 6. Forward player input to peer delegates
-- 7. DONE - Store the simulation result for the player if delegate consensus is achieved
type PlayerInput =
  { csv :: String
  , player :: Ed25519KeyHash
  , signature :: ByteArray
  }

playerInputCodec :: CA.JsonCodec PlayerInput
playerInputCodec =
  CA.object "PlayerInput" $ CAR.record
    { csv: CA.string
    , player: ed25519KeyHashCodec
    , signature: byteArrayCodec
    }

submitPlayerInputHandler :: String -> AppM HTTPure.Response
submitPlayerInputHandler =
  either
    (\e -> response (errorStatus e) (stringifyAeson $ CA.encode submitPlayerInputErrorCodec e))
    (const created)
    <=< submitPlayerInputHandlerReturningErrors

-- Handler

submitPlayerInputHandlerReturningErrors :: String -> AppM (Either SubmitPlayerInputError Unit)
submitPlayerInputHandlerReturningErrors bodyStr =
  runExceptT do
    reqBody <-
      liftEither $
        lmap (CouldNotDecodeReqBody <<< { decodeError: _ } <<< CA.printJsonDecodeError)
          (caDecodeString playerInputCodec bodyStr)
    { raceParams: RaceParams { participants } } <- lift readRaceData
    addr <- liftMaybe MustBeRaceParticipant $ getParticipantAddress reqBody.player participants
    { resultSlots } <- ask
    resultSlots' <-
      liftMaybe PlayerInputSubmittedTooEarly =<<
        liftAff (AVar.tryRead resultSlots)
    slot <- liftMaybe ResultSlotsMisconfigured $ Map.lookup addr resultSlots'
    appRunner <- lift getAppRunner
    ExceptT $ liftAff $ bracket
      (AVar.tryTake slot)
      (void <<< traverse (flip AVar.tryPut slot))
      ( \slotValue ->
          appRunner $ runExceptT do
            case slotValue of
              Nothing ->
                throwError ConcurrentSimulationInProgress
              Just currentResult -> do
                when (isJust currentResult) do
                  throwError SimResultAlreadyExistsForParticipant
                simResult <- ExceptT $ lmap (RaceSimulationFailed <<< { simError: _ }) <$>
                  runSimulator "simulator/input.csv"
                liftAff $ AVar.put (Just simResult) slot
      )
    pure unit

-- Helpers

getParticipantAddress :: Ed25519KeyHash -> Array Plutus.Address -> Maybe Plutus.Address
getParticipantAddress player participants =
  Array.find
    ( \addr ->
        case (unwrap addr).addressCredential of
          Plutus.PubKeyCredential pkh -> player == unwrap pkh
          _ -> false
    )
    participants

-- Errors

data SubmitPlayerInputError
  = CouldNotDecodeReqBody { decodeError :: String }
  | MustBeRaceParticipant
  | PlayerInputSubmittedTooEarly
  | ResultSlotsMisconfigured
  | ConcurrentSimulationInProgress
  | SimResultAlreadyExistsForParticipant
  | RaceSimulationFailed { simError :: RaceSimulationError }

derive instance Generic SubmitPlayerInputError _
derive instance Eq SubmitPlayerInputError

instance Show SubmitPlayerInputError where
  show = genericShow

submitPlayerInputErrorCodec :: CA.JsonCodec SubmitPlayerInputError
submitPlayerInputErrorCodec =
  CAS.sumFlat "SubmitPlayerInputError"
    { "CouldNotDecodeReqBody":
        CAR.record
          { decodeError: CA.string
          }
    , "MustBeRaceParticipant": unit
    , "PlayerInputSubmittedTooEarly": unit
    , "ResultSlotsMisconfigured": unit
    , "ConcurrentSimulationInProgress": unit
    , "SimResultAlreadyExistsForParticipant": unit
    , "RaceSimulationFailed":
        CAR.record
          { simError: raceSimulationErrorCodec
          }
    }

errorStatus :: SubmitPlayerInputError -> Status
errorStatus =
  case _ of
    CouldNotDecodeReqBody _ ->
      Status.badRequest
    MustBeRaceParticipant ->
      Status.forbidden
    PlayerInputSubmittedTooEarly ->
      Status.conflict
    ResultSlotsMisconfigured ->
      Status.internalServerError
    ConcurrentSimulationInProgress ->
      Status.conflict
    SimResultAlreadyExistsForParticipant ->
      Status.conflict
    RaceSimulationFailed _ ->
      Status.badRequest
