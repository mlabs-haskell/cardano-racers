module CardanoRacers.Hydra.Handlers.SubmitPlayerInput where

import Prelude

import Aeson (stringifyAeson)
import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Credential (Credential(PubKeyCredential)) as Plutus
import Cardano.Types (Ed25519KeyHash)
import Cardano.Types.Address (getPaymentCredential)
import Cardano.Types.Credential (asPubKeyHash)
import Cardano.Types.PublicKey (hash, verify) as PublicKey
import CardanoRacers.Hydra.Monad (AppM, findRaceEntryByRaceCs, getAppRunner)
import CardanoRacers.Hydra.RaceSimulator
  ( RaceSimulationError
  , raceSimulationErrorCodec
  , runSimulator
  )
import CardanoRacers.Hydra.Types.RaceStatus (RaceStatus(AcceptingPlayerInputs))
import CardanoRacers.Race.Types (RaceParams(RaceParams))
import CardanoRacers.Services.HydraDelegate (playerInputCodec)
import CardanoRacers.Utils.Cose (mkSigStruct)
import Control.Error.Util ((!?))
import Control.Monad.Error.Class (liftEither, liftMaybe, throwError)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Array (find) as Array
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, encode, printJsonDecodeError, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Sum (sumFlat) as CAS
import Data.Either (Either, either)
import Data.Generic.Rep (class Generic)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Effect.Aff (bracket)
import Effect.Aff.AVar (put, tryPut, tryTake) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (read) as Ref
import HTTPure (Response) as HTTPure
import HTTPure (Status, ok, response)
import HTTPure.Status (badRequest, conflict, forbidden, internalServerError, unauthorized) as Status
import HydraSdk.Lib (caDecodeString)
import Lib.CardanoRacers.Client (mkSigMessage)

submitPlayerInputHandler :: String -> AppM HTTPure.Response
submitPlayerInputHandler =
  either
    (\e -> response (errorStatus e) (stringifyAeson $ CA.encode submitPlayerInputErrorCodec e))
    (const $ ok "null") -- FIXME: use `created`

    <=< submitPlayerInputHandlerReturningErrors

-- Handler

submitPlayerInputHandlerReturningErrors :: String -> AppM (Either SubmitPlayerInputError Unit)
submitPlayerInputHandlerReturningErrors bodyStr =
  runExceptT do
    reqBody <-
      liftEither $
        lmap (CouldNotDecodeReqBody <<< { decodeError: _ } <<< CA.printJsonDecodeError)
          (caDecodeString playerInputCodec bodyStr)
    { raceData, raceStatusRef } <- findRaceEntryByRaceCs reqBody.raceCs !?
      RequestedRaceNotHosted
    resultSlots <-
      liftEffect (Ref.read raceStatusRef) >>=
        case _ of
          AcceptingPlayerInputs slots ->
            pure slots
          _ ->
            throwError PlayerInputSubmitWindowNotActive
    let
      pkh = PublicKey.hash reqBody.auth.vk
      { raceParams: RaceParams { stateCurrencySymbol: raceId, participants } } = raceData
    addr <- liftMaybe MustBeRaceParticipant $ getParticipantAddress pkh participants
    slot <- liftMaybe ResultSlotsMisconfigured $ Map.lookup addr resultSlots
    unless
      (((asPubKeyHash <<< unwrap) =<< getPaymentCredential reqBody.auth.addr) == Just pkh)
      (throwError VkAddressMismatch)
    sigStruct <- liftEffect $ mkSigStruct reqBody.auth.addr $ mkSigMessage reqBody.csv raceId
    unless (PublicKey.verify reqBody.auth.vk (wrap sigStruct) reqBody.auth.signature) $
      throwError InvalidSignature
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
                  runSimulator reqBody.csv
                liftAff $ AVar.put (Just simResult) slot
      )

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
  | RequestedRaceNotHosted
  | MustBeRaceParticipant
  | PlayerInputSubmitWindowNotActive
  | ResultSlotsMisconfigured
  | VkAddressMismatch
  | InvalidSignature
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
    , "RequestedRaceNotHosted": unit
    , "MustBeRaceParticipant": unit
    , "PlayerInputSubmitWindowNotActive": unit
    , "ResultSlotsMisconfigured": unit
    , "VkAddressMismatch": unit
    , "InvalidSignature": unit
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
    RequestedRaceNotHosted ->
      Status.badRequest
    MustBeRaceParticipant ->
      Status.forbidden
    PlayerInputSubmitWindowNotActive ->
      Status.conflict
    ResultSlotsMisconfigured ->
      Status.internalServerError
    VkAddressMismatch ->
      Status.badRequest
    InvalidSignature ->
      Status.unauthorized
    ConcurrentSimulationInProgress ->
      Status.conflict
    SimResultAlreadyExistsForParticipant ->
      Status.conflict
    RaceSimulationFailed _ ->
      Status.badRequest
