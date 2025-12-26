module CardanoRacers.Hydra.Handlers.SignCommitTx
  ( CommitTxSignature
  , SignCommitTxError
      ( CommitTxDecodingFailed
      , CommitTxSigningFailed
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

import Cardano.Types (Ed25519KeyHash, Transaction, Vkeywitness)
import CardanoRacers.Hydra.Codec (vkeyWitnessCodec)
import CardanoRacers.Hydra.Lib.Transaction (txSignatures)
import CardanoRacers.Hydra.Monad (AppM, liftContract)
import CardanoRacers.Hydra.Types.ServerResponse
  ( ServerResponse(ServerResponseError, ServerResponseSuccess)
  , respCreatedOrBadRequest
  , serverResponseCodec
  )
import Contract.Monad (Contract)
import Contract.Transaction (signTransaction)
import Data.Array (difference) as Array
import Data.Codec.Argonaut (JsonCodec, object, printJsonDecodeError, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Sum (sum) as CAS
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Show.Generic (genericShow)
import HTTPure (Response) as HTTPure
import HydraSdk.Lib (caDecodeString, ed25519KeyHashCodec, txCodec)

type SignCommitTxRequestPayload =
  { commitTx :: Transaction
  , commitLeader :: Ed25519KeyHash
  }

signCommitTxRequestPayloadCodec :: CA.JsonCodec SignCommitTxRequestPayload
signCommitTxRequestPayloadCodec =
  CA.object "SignCommitTxRequestPayload" $ CAR.record
    { commitTx: txCodec
    , commitLeader: ed25519KeyHashCodec
    }

type CommitTxSignature = Vkeywitness

type SignCommitTxResponse = ServerResponse CommitTxSignature SignCommitTxError

signCommitTxResponseCodec :: CA.JsonCodec SignCommitTxResponse
signCommitTxResponseCodec = serverResponseCodec vkeyWitnessCodec signCommitTxErrorCodec

signCommitTxHandler :: String -> AppM HTTPure.Response
signCommitTxHandler =
  respCreatedOrBadRequest signCommitTxResponseCodec
    <=< signCommitTxHandlerImpl

signCommitTxHandlerImpl :: String -> AppM SignCommitTxResponse
signCommitTxHandlerImpl bodyStr = do
  case caDecodeString signCommitTxRequestPayloadCodec bodyStr of
    Left decodeErr ->
      pure $ ServerResponseError $ CommitTxDecodingFailed $
        CA.printJsonDecodeError decodeErr
    Right { commitTx } ->
      -- TODO: validation
      maybe (ServerResponseError CommitTxSigningFailed) ServerResponseSuccess <$>
        liftContract (signTxReturnSignature commitTx)

signTxReturnSignature :: Transaction -> Contract (Maybe CommitTxSignature)
signTxReturnSignature tx =
  signTransaction tx <#> \signedTx ->
    case Array.difference (txSignatures signedTx) (txSignatures tx) of
      [ signature ] -> Just signature
      _ -> Nothing

----------------------------------------------------------------------
-- SignCommitTxError

data SignCommitTxError
  = CommitTxDecodingFailed String
  | CommitTxSigningFailed

derive instance Generic SignCommitTxError _
derive instance Eq SignCommitTxError

instance Show SignCommitTxError where
  show = genericShow

signCommitTxErrorCodec :: CA.JsonCodec SignCommitTxError
signCommitTxErrorCodec =
  CAS.sum "SignCommitTxError"
    { "CommitTxDecodingFailed": CA.string
    , "CommitTxSigningFailed": unit
    }
