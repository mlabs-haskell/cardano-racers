module CardanoRacers.Services.HydraDelegate
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
  , PlayerInput
  , SubmitPlayerInputError
      ( SubmitInput_CouldNotDecodeReqBody
      , SubmitInput_RequestedRaceNotHosted
      , SubmitInput_MustBeRaceParticipant
      , SubmitInput_PlayerInputSubmitWindowNotActive
      , SubmitInput_ResultSlotsMisconfigured
      , SubmitInput_VkAddressMismatch
      , SubmitInput_InvalidSignature
      , SubmitInput_ConcurrentSimulationInProgress
      , SubmitInput_SimResultAlreadyExistsForParticipant
      , SubmitInput_RaceSimulationFailed
      )
  , hostRaceErrorCodec
  , hostRaceRequest
  , hostRaceRequestCodec
  , playerInputCodec
  , submitPlayerInputErrorCodec
  , submitPlayerInputRequest
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Types
  ( Address
  , Ed25519Signature
  , NetworkId
  , PublicKey
  , ScriptHash
  , TransactionHash
  , TransactionInput
  )
import CardanoRacers.Common.Types (RacersParams, racersParamsCodec)
import CardanoRacers.Race.Types (RaceParams, raceParamsCodec)
import CardanoRacers.Utils.Codec (orefCodec)
import CardanoRacers.Utils.HasJson (class HasJson)
import CardanoRacers.Utils.Http (handleResponse, postRequest)
import Ctl.Internal.Helpers ((<</>>))
import Data.Codec.Argonaut
  ( JsonCodec
  , encode
  , null
  , object
  , prismaticCodec
  , string
  ) as CA
import Data.Codec.Argonaut.Record (optional, record) as CAR
import Data.Codec.Argonaut.Sum (sumFlat) as CAS
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just))
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import HydraSdk.Lib
  ( addressCodec
  , cborBytesCodec
  , publicKeyCodec
  , scriptHashCodec
  , txHashCodec
  )
import HydraSdk.Types (HttpError, HydraHeadStatus, headStatusCodec)

type PlayerInput =
  { raceCs :: ScriptHash
  , csv :: String
  , auth ::
      { vk :: PublicKey
      , addr :: Address
      , signature :: Ed25519Signature
      }
  }

playerInputCodec :: CA.JsonCodec PlayerInput
playerInputCodec =
  CA.object "PlayerInput" $ CAR.record
    { raceCs: scriptHashCodec
    , csv: CA.string
    , auth:
        CA.object "PlayerInput:auth" $ CAR.record
          { vk: publicKeyCodec
          , addr: addressCodec
          , signature:
              CA.prismaticCodec "Ed25519Signature" decodeCbor encodeCbor
                cborBytesCodec
          }
    }

data SubmitPlayerInputError
  = SubmitInput_CouldNotDecodeReqBody { decodeError :: String }
  | SubmitInput_RequestedRaceNotHosted
  | SubmitInput_MustBeRaceParticipant
  | SubmitInput_PlayerInputSubmitWindowNotActive
  | SubmitInput_ResultSlotsMisconfigured
  | SubmitInput_VkAddressMismatch
  | SubmitInput_InvalidSignature
  | SubmitInput_ConcurrentSimulationInProgress
  | SubmitInput_SimResultAlreadyExistsForParticipant
  | SubmitInput_RaceSimulationFailed { simError :: String }

derive instance Generic SubmitPlayerInputError _
derive instance Eq SubmitPlayerInputError

instance Show SubmitPlayerInputError where
  show = genericShow

submitPlayerInputErrorCodec :: CA.JsonCodec SubmitPlayerInputError
submitPlayerInputErrorCodec =
  CAS.sumFlat "SubmitPlayerInputError"
    { "SubmitInput_CouldNotDecodeReqBody":
        CAR.record
          { decodeError: CA.string
          }
    , "SubmitInput_RequestedRaceNotHosted": unit
    , "SubmitInput_MustBeRaceParticipant": unit
    , "SubmitInput_PlayerInputSubmitWindowNotActive": unit
    , "SubmitInput_ResultSlotsMisconfigured": unit
    , "SubmitInput_VkAddressMismatch": unit
    , "SubmitInput_InvalidSignature": unit
    , "SubmitInput_ConcurrentSimulationInProgress": unit
    , "SubmitInput_SimResultAlreadyExistsForParticipant": unit
    , "SubmitInput_RaceSimulationFailed":
        CAR.record
          { simError: CA.string
          }
    }

submitPlayerInputRequest
  :: String
  -> PlayerInput
  -> Aff (Either HttpError (Either SubmitPlayerInputError Unit))
submitPlayerInputRequest httpServer playerInput =
  handleResponse
    { resultCodec: CA.null
    , errorCodec: submitPlayerInputErrorCodec
    } <$>
    postRequest
      { url: httpServer <</>> "playerInput"
      , content: Just $ CA.encode playerInputCodec playerInput
      , headers: mempty
      }

-- HostRace

newtype HostRaceRequest = HostRaceRequest
  { raceOref :: TransactionInput
  , raceParams :: RaceParams
  , racersParams :: Maybe RacersParams
  }

derive instance Generic HostRaceRequest _
derive instance Newtype HostRaceRequest _

instance Show HostRaceRequest where
  show = genericShow

instance HasJson HostRaceRequest NetworkId where
  jsonCodec network = const (hostRaceRequestCodec network)

hostRaceRequestCodec :: NetworkId -> CA.JsonCodec HostRaceRequest
hostRaceRequestCodec network =
  wrapIso HostRaceRequest $ CA.object "HostRaceRequest" $ CAR.record
    { raceOref: orefCodec
    , raceParams: raceParamsCodec network
    , racersParams: CAR.optional racersParamsCodec
    }

data HostRaceError
  = HostRace_CouldNotDecodeReqBody { decodeError :: String }
  | HostRace_RacersParamsNotProvided
  | HostRace_CouldNotResolveRaceOref
  | HostRace_InvalidHeadStatus
      { expected :: HydraHeadStatus
      , actual :: HydraHeadStatus
      }
  | HostRace_CouldNotQuerySnapshotUtxos
  | HostRace_CollateralUtxoNotAvailableOnL2
  | HostRace_CollateralUtxoNotSpentOnL1
  | HostRace_CommitContractFailed

derive instance Generic HostRaceError _
derive instance Eq HostRaceError

instance Show HostRaceError where
  show = genericShow

hostRaceErrorCodec :: CA.JsonCodec HostRaceError
hostRaceErrorCodec =
  CAS.sumFlat "HostRaceError"
    { "HostRace_CouldNotDecodeReqBody":
        CAR.record
          { decodeError: CA.string
          }
    , "HostRace_RacersParamsNotProvided": unit
    , "HostRace_CouldNotResolveRaceOref": unit
    , "HostRace_InvalidHeadStatus":
        CAR.record
          { expected: headStatusCodec
          , actual: headStatusCodec
          }
    , "HostRace_CouldNotQuerySnapshotUtxos": unit
    , "HostRace_CollateralUtxoNotAvailableOnL2": unit
    , "HostRace_CollateralUtxoNotSpentOnL1": unit
    , "HostRace_CommitContractFailed": unit
    }

hostRaceRequest
  :: String
  -> NetworkId
  -> HostRaceRequest
  -> Aff (Either HttpError (Either HostRaceError TransactionHash))
hostRaceRequest httpServer network req =
  handleResponse
    { resultCodec: txHashCodec
    , errorCodec: hostRaceErrorCodec
    } <$>
    postRequest
      { url: httpServer <</>> "hostRace"
      , content: Just $ CA.encode (hostRaceRequestCodec network) req
      , headers: mempty
      }
