module CardanoRacers.Hydra.Handlers.SignAnnounceDistrTx.Types
  ( SignAnnounceDistrTxError
      ( CouldNotDecodeReqBody
      , RequestedRaceNotHosted
      , UnexpectedRaceStatus
      , TxValidationFailed
      , CouldNotSignTx
      )
  , SignAnnounceDistrTxRequestPayload
  , SignAnnounceDistrTxResponse
  , signAnnounceDistrTxErrorCodec
  , signAnnounceDistrTxRequestPayloadCodec
  , signAnnounceDistrTxResponseCodec
  ) where

import Prelude

import Cardano.Types (Address, ScriptHash, Transaction, Vkeywitness)
import CardanoRacers.Hydra.Codec (vkeyWitnessCodec)
import CardanoRacers.Hydra.Types.ServerResponse (ServerResponse, serverResponseCodec)
import Data.Codec.Argonaut (JsonCodec, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Sum (sum, sumFlat) as CAS
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import HydraSdk.Lib (addressCodec, scriptHashCodec, txCodec)

type SignAnnounceDistrTxRequestPayload =
  { raceCs :: ScriptHash
  , tx :: Transaction
  , changeAddress :: Address
  }

signAnnounceDistrTxRequestPayloadCodec :: CA.JsonCodec SignAnnounceDistrTxRequestPayload
signAnnounceDistrTxRequestPayloadCodec =
  CA.object "SignAnnounceDistrTxRequestPayload" $ CAR.record
    { raceCs: scriptHashCodec
    , tx: txCodec
    , changeAddress: addressCodec
    }

signAnnounceDistrTxResponseCodec :: CA.JsonCodec SignAnnounceDistrTxResponse
signAnnounceDistrTxResponseCodec =
  serverResponseCodec vkeyWitnessCodec signAnnounceDistrTxErrorCodec

type SignAnnounceDistrTxResponse = ServerResponse Vkeywitness SignAnnounceDistrTxError

data SignAnnounceDistrTxError
  = CouldNotDecodeReqBody { decodeError :: String }
  | RequestedRaceNotHosted
  | UnexpectedRaceStatus
  | TxValidationFailed
  | CouldNotSignTx

derive instance Generic SignAnnounceDistrTxError _
derive instance Eq SignAnnounceDistrTxError

instance Show SignAnnounceDistrTxError where
  show = genericShow

signAnnounceDistrTxErrorCodec :: CA.JsonCodec SignAnnounceDistrTxError
signAnnounceDistrTxErrorCodec =
  CAS.sumFlat "SignAnnounceDistrTxError"
    { "CouldNotDecodeReqBody":
        CAR.record
          { decodeError: CA.string
          }
    , "RequestedRaceNotHosted": unit
    , "UnexpectedRaceStatus": unit
    , "TxValidationFailed": unit
    , "CouldNotSignTx": unit
    }
