module CardanoRacers.Services.HydraDelegate
  ( HostRaceRequest(HostRaceRequest)
  , PlayerInput
  , hostRaceRequest
  , hostRaceRequestCodec
  , playerInputCodec
  , submitPlayerInputRequest
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Types
  ( Address
  , Ed25519Signature
  , NetworkId
  , PublicKey
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
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just))
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import HydraSdk.Lib (addressCodec, cborBytesCodec, publicKeyCodec, txHashCodec)
import HydraSdk.Types (HttpError)

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
          , signature:
              CA.prismaticCodec "Ed25519Signature" decodeCbor encodeCbor
                cborBytesCodec
          }
    }

submitPlayerInputRequest :: String -> PlayerInput -> Aff (Either HttpError Unit)
submitPlayerInputRequest httpServer playerInput =
  handleResponse CA.null <$>
    postRequest
      { url: httpServer <</>> "playerInput"
      , content: Just $ CA.encode playerInputCodec playerInput
      , headers: mempty
      }

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

hostRaceRequest
  :: String
  -> NetworkId
  -> HostRaceRequest
  -> Aff (Either HttpError TransactionHash)
hostRaceRequest httpServer network req =
  handleResponse txHashCodec <$>
    postRequest
      { url: httpServer <</>> "hostRace"
      , content: Just $ CA.encode (hostRaceRequestCodec network) req
      , headers: mempty
      }
