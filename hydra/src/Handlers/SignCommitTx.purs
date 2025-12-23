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

import Cardano.AsCbor (encodeCbor)
import Cardano.Types
  ( Asset(Asset)
  , AssetName
  , Ed25519KeyHash
  , ScriptHash
  , Transaction
  , TransactionInput
  , TransactionOutput(TransactionOutput)
  , Vkeywitness
  )
import Cardano.Types.AssetName (mkAssetName)
import Cardano.Types.BigNum (one) as BigNum
import Cardano.Types.Transaction (_body)
import Cardano.Types.TransactionBody (_collateral, _inputs)
import CardanoRacers.Hydra.Codec (vkeyWitnessCodec)
import CardanoRacers.Hydra.Lib.Transaction (txSignatures)
import CardanoRacers.Hydra.Monad (AppM, liftContract)
import CardanoRacers.Hydra.Types.ServerResponse
  ( ServerResponse(ServerResponseError, ServerResponseSuccess)
  , respCreatedOrBadRequest
  , serverResponseCodec
  )
import Contract.Monad (Contract, liftedM)
import Contract.Transaction (signTransaction)
import Contract.Utxos (getUtxo)
import Contract.Value (geq, singleton, valueOf) as Value
import Contract.Wallet (ownPaymentPubKeyHash)
import Control.Parallel (parTraverse)
import Data.Array (all, difference, find, fromFoldable, length, partition) as Array
import Data.Codec.Argonaut (JsonCodec, array, object, printJsonDecodeError, string) as CA
import Data.Codec.Argonaut.Generic (nullarySum) as CAG
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Sum (sum) as CAS
import Data.Either (Either(Left, Right))
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Lens ((^.))
import Data.Maybe (Maybe(Just, Nothing), isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple), snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Validation.Semigroup (V, validation)
import Effect.Exception (error)
import Effect.Exception (message) as Error
import HTTPure (Response) as HTTPure
import HydraSdk.Lib (caDecodeString, ed25519KeyHashCodec, txCodec)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(Proxy))

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
    Right { commitTx, commitLeader } ->
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
