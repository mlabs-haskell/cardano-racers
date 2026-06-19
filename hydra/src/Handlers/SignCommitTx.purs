module CardanoRacers.Hydra.Handlers.SignCommitTx
  ( SignCommitTxError
      ( CouldNotDecodeReqBody
      , CouldNotDecodeRaceParams
      , CouldNotSignTx
      )
  , SignCommitTxRequestPayload
  , SignCommitTxResponse
  , signCommitTxErrorCodec
  , signCommitTxHandler
  , signCommitTxHandlerImpl
  , signCommitTxRequestPayloadCodec
  , signCommitTxResponseCodec
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.FromData (fromData)
import Cardano.Types (CborBytes, Ed25519KeyHash, Transaction, Vkeywitness)
import Cardano.Types.Transaction (hash) as Transaction
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Hydra.Codec (racersParamsCodec, vkeyWitnessCodec)
import CardanoRacers.Hydra.Lib.Transaction (signTxReturnSignature)
import CardanoRacers.Hydra.Monad (AppM, initRace, liftContract)
import CardanoRacers.Hydra.Types.ServerResponse
  ( ServerResponse
  , fromEither
  , respCreatedOrBadRequest
  , serverResponseCodec
  )
import CardanoRacers.Race.Contract (mkRaceValidator)
import Control.Error.Util ((!?), (??))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Codec.Argonaut (JsonCodec, object, printJsonDecodeError, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Sum (sum, sumFlat) as CAS
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import HTTPure (Response) as HTTPure
import HydraSdk.Lib (caDecodeString, cborBytesCodec, ed25519KeyHashCodec, txCodec)
import Racers (runRacers)

type SignCommitTxRequestPayload =
  { commitTx :: Transaction
  , commitLeader :: Ed25519KeyHash
  , racersParams :: RacersParams
  , raceParams :: CborBytes
  }

signCommitTxRequestPayloadCodec :: CA.JsonCodec SignCommitTxRequestPayload
signCommitTxRequestPayloadCodec =
  CA.object "SignCommitTxRequestPayload" $ CAR.record
    { commitTx: txCodec
    , commitLeader: ed25519KeyHashCodec
    , racersParams: racersParamsCodec
    , raceParams: cborBytesCodec
    }

type SignCommitTxResponse = ServerResponse Vkeywitness SignCommitTxError

signCommitTxResponseCodec :: CA.JsonCodec SignCommitTxResponse
signCommitTxResponseCodec = serverResponseCodec vkeyWitnessCodec signCommitTxErrorCodec

signCommitTxHandler :: String -> AppM HTTPure.Response
signCommitTxHandler =
  (respCreatedOrBadRequest signCommitTxResponseCodec <<< fromEither)
    <=< signCommitTxHandlerImpl

signCommitTxHandlerImpl :: String -> AppM (Either SignCommitTxError Vkeywitness)
signCommitTxHandlerImpl bodyStr =
  runExceptT case caDecodeString signCommitTxRequestPayloadCodec bodyStr of
    Left decodeErr ->
      throwError $ CouldNotDecodeReqBody
        { decodeError: CA.printJsonDecodeError decodeErr
        }
    Right reqBody -> do
      -- TODO: validation
      raceParams <- (fromData =<< decodeCbor reqBody.raceParams) ?? CouldNotDecodeRaceParams
      raceValidator <-
        lift $ liftContract $ runRacers reqBody.racersParams $
          mkRaceValidator raceParams
      sig <- liftContract (signTxReturnSignature reqBody.commitTx) !? CouldNotSignTx
      let depositTxId = Transaction.hash reqBody.commitTx
      lift $ initRace depositTxId
        { racersParams: reqBody.racersParams
        , raceParams
        , raceValidator
        }
      pure sig

----------------------------------------------------------------------
-- SignCommitTxError

data SignCommitTxError
  = CouldNotDecodeReqBody { decodeError :: String }
  | CouldNotDecodeRaceParams
  | CouldNotSignTx

derive instance Generic SignCommitTxError _
derive instance Eq SignCommitTxError

instance Show SignCommitTxError where
  show = genericShow

signCommitTxErrorCodec :: CA.JsonCodec SignCommitTxError
signCommitTxErrorCodec =
  CAS.sumFlat "SignCommitTxError"
    { "CouldNotDecodeReqBody":
        CAR.record
          { decodeError: CA.string
          }
    , "CouldNotDecodeRaceParams": unit
    , "CouldNotSignTx": unit
    }
