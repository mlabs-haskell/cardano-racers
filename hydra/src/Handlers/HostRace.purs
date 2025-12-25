module CardanoRacers.Hydra.Handlers.HostRace
  ( HostRaceError
      ( CouldNotDecodeRequestBody
      , CouldNotResolveRaceOref
      , CouldNotDecodeRaceParams
      , InvalidHeadStatus
      )
  , HostRaceRequest
  , HostRaceResponse
  , HostRaceSuccess
  , hostRaceErrorCodec
  , hostRaceHandler
  , hostRaceHandlerImpl
  , hostRaceRequestCodec
  , hostRaceResponseCodec
  , hostRaceSuccessCodec
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.FromData (fromData)
import Cardano.Types (CborBytes, TransactionHash, TransactionInput)
import CardanoRacers.Hydra.Contracts.Commit (commitRaceUtxoToHydra)
import CardanoRacers.Hydra.Monad (AppM, liftContract, readHeadStatus)
import CardanoRacers.Hydra.Types.ServerResponse
  ( ServerResponse
  , fromEither
  , respCreatedOrBadRequest
  , serverResponseCodec
  )
import CardanoRacers.Race.Types (RaceParams)
import Contract.Utxos (getUtxo)
import Control.Error.Util ((!?), (??))
import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, object, printJsonDecodeError, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Sum (sumFlat) as CAS
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import HTTPure (Response, notFound) as HTTPure
import HydraSdk.Lib (caDecodeString, cborBytesCodec, txHashCodec)
import HydraSdk.Lib (orefCodec) as HydraSdk
import HydraSdk.Types (HydraHeadStatus(HeadStatus_Initializing), headStatusCodec)

hostRaceHandler :: String -> AppM HTTPure.Response
hostRaceHandler bodyStr = do
  resp <- hostRaceHandlerImpl bodyStr
  respCreatedOrBadRequest hostRaceResponseCodec $ fromEither resp

-- TODO: check if another hosting request is currently being processed
hostRaceHandlerImpl :: String -> AppM (Either HostRaceError HostRaceSuccess)
hostRaceHandlerImpl bodyStr =
  runExceptT do
    reqBody <-
      liftEither $
        lmap (CouldNotDecodeRequestBody <<< { decodeErr: _ } <<< CA.printJsonDecodeError)
          (caDecodeString hostRaceRequestCodec bodyStr)
    headStatus <- lift readHeadStatus
    let headStatusExpected = HeadStatus_Initializing
    when (headStatus /= headStatusExpected) do
      throwError $ InvalidHeadStatus
        { expected: headStatusExpected
        , actual: headStatus
        }
    raceOut <- liftContract (getUtxo reqBody.raceOref) !? CouldNotResolveRaceOref
    raceParams <- (fromData =<< decodeCbor reqBody.raceParams) ?? CouldNotDecodeRaceParams
    txHash <- lift $ commitRaceUtxoToHydra (reqBody.raceOref /\ raceOut) raceParams
    pure
      { commitTxHash: txHash
      }

-- Request 

type HostRaceRequest =
  { raceOref :: TransactionInput
  , raceParams :: CborBytes
  }

hostRaceRequestCodec :: CA.JsonCodec HostRaceRequest
hostRaceRequestCodec =
  CA.object "HostRaceRequest" $ CAR.record
    { raceOref: HydraSdk.orefCodec
    , raceParams: cborBytesCodec
    }

-- Response

type HostRaceResponse = ServerResponse HostRaceSuccess HostRaceError

hostRaceResponseCodec :: CA.JsonCodec HostRaceResponse
hostRaceResponseCodec = serverResponseCodec hostRaceSuccessCodec hostRaceErrorCodec

-- Success

type HostRaceSuccess =
  { commitTxHash :: TransactionHash
  }

hostRaceSuccessCodec :: CA.JsonCodec HostRaceSuccess
hostRaceSuccessCodec =
  CA.object "HostRaceSuccess" $ CAR.record
    { commitTxHash: txHashCodec
    }

-- Error

data HostRaceError
  = CouldNotDecodeRequestBody { decodeErr :: String }
  | CouldNotResolveRaceOref
  | CouldNotDecodeRaceParams
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
    , "CouldNotResolveRaceOref": unit
    , "CouldNotDecodeRaceParams": unit
    , "InvalidHeadStatus":
        CAR.record
          { expected: headStatusCodec
          , actual: headStatusCodec
          }
    }
