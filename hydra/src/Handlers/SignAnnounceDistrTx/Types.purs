module CardanoRacers.Hydra.Handlers.SignAnnounceDistrTx.Types
  ( SignAnnounceDistrTxError(CouldNotDecodeTx, TxValidationFailed, CouldNotSignTx)
  , SignAnnounceDistrTxRequestPayload
  , SignAnnounceDistrTxResponse
  , signAnnounceDistrTxErrorCodec
  , signAnnounceDistrTxRequestPayloadCodec
  , signAnnounceDistrTxResponseCodec
  ) where

import Prelude

import Cardano.Types (Address, Transaction, Vkeywitness)
import CardanoRacers.Hydra.Codec (vkeyWitnessCodec)
import CardanoRacers.Hydra.Types.ServerResponse (ServerResponse, serverResponseCodec)
import Data.Codec.Argonaut (JsonCodec, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Sum (sum) as CAS
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import HydraSdk.Lib (addressCodec, txCodec)

type SignAnnounceDistrTxRequestPayload =
  { tx :: Transaction
  , collateralAddress :: Address
  }

signAnnounceDistrTxRequestPayloadCodec :: CA.JsonCodec SignAnnounceDistrTxRequestPayload
signAnnounceDistrTxRequestPayloadCodec =
  CA.object "SignAnnounceDistrTxRequestPayload" $ CAR.record
    { tx: txCodec
    , collateralAddress: addressCodec
    }

signAnnounceDistrTxResponseCodec :: CA.JsonCodec SignAnnounceDistrTxResponse
signAnnounceDistrTxResponseCodec =
  serverResponseCodec vkeyWitnessCodec signAnnounceDistrTxErrorCodec

type SignAnnounceDistrTxResponse = ServerResponse Vkeywitness SignAnnounceDistrTxError

data SignAnnounceDistrTxError
  = CouldNotDecodeTx String
  | TxValidationFailed
  | CouldNotSignTx

derive instance Generic SignAnnounceDistrTxError _
derive instance Eq SignAnnounceDistrTxError

instance Show SignAnnounceDistrTxError where
  show = genericShow

signAnnounceDistrTxErrorCodec :: CA.JsonCodec SignAnnounceDistrTxError
signAnnounceDistrTxErrorCodec =
  CAS.sum "SignAnnounceDistrTxError"
    { "CouldNotDecodeTx": CA.string
    , "TxValidationFailed": unit
    , "CouldNotSignTx": unit
    }
