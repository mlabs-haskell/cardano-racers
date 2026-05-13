module CardanoRacers.Hydra.Handlers.SubmitPlayerInput where

import Prelude

import Aeson (stringifyAeson)
import Cardano.AsCbor (encodeCbor)
import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Credential (Credential(PubKeyCredential)) as Plutus
import Cardano.Types (Address, Ed25519KeyHash, Ed25519Signature, PublicKey, ScriptHash)
import Cardano.Types.Address (getPaymentCredential)
import Cardano.Types.Credential (asPubKeyHash)
import Cardano.Types.PublicKey (hash, verify) as PublicKey
import CardanoRacers.Hydra.Codec (ed25519SignatureCodec)
import CardanoRacers.Hydra.Lib.Cose (mkSigStruct)
import CardanoRacers.Hydra.Lib.Hash (blake2b256Hash)
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
import HydraSdk.Lib (addressCodec, caDecodeString, publicKeyCodec)

-- 1. TODO - Parse and validate CSV
-- 2. DONE - Verify that the user is a race participant
-- 3. DONE - Ensure this participant has not already submitted input
-- 4. DONE - Verify the signature
-- 5. DONE - Run the simulation
-- 6. NOT PLANNED - Forward player input to peer delegates
-- 7. DONE - Store the simulation result for the player if delegate consensus is achieved
-- 8. DONE - Pass provided CSV to the simulator
type PlayerInput =
  { csv :: String
  , auth ::
      { vk :: PublicKey
      , addr :: Address
      , signature :: Ed25519Signature
      }
  }

playerInputCodec :: CA.JsonCodec PlayerInput
playerInputCodec =
  CA.object "PlayerInput" $ CAR.record
    { csv: CA.string
    , auth:
        CA.object "PlayerInput:auth" $ CAR.record
          { vk: publicKeyCodec
          , addr: addressCodec
          , signature: ed25519SignatureCodec
          }
    }

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
    { raceParams: RaceParams { stateCurrencySymbol: raceId, participants } } <-
      lift readRaceData
    let pkh = PublicKey.hash reqBody.auth.vk
    addr <- liftMaybe MustBeRaceParticipant $ getParticipantAddress pkh participants
    { resultSlots, acceptingPlayerInputs } <- ask
    liftEffect (Ref.read acceptingPlayerInputs) >>= \p ->
      unless p $
        throwError PlayerInputSubmitWindowNotActive
    slot <- do
      slots <- liftEffect $ Ref.read resultSlots
      liftMaybe ResultSlotsMisconfigured $ Map.lookup addr =<< slots
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

mkSigMessage :: String -> ScriptHash -> ByteArray
mkSigMessage userInput raceId = unwrap (encodeCbor raceId) <> blake2b256Hash userInput

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
