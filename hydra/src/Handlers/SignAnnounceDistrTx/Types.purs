module CardanoRacers.Hydra.Handlers.SignAnnounceDistrTx.Types
  ( SignAnnounceDistrTxError
      ( CouldNotDecodeReqBody
      , RequestedRaceNotHosted
      , UnexpectedRaceStatus
      , TxValidationFailed
      , CouldNotSignTx
      )
  , SignAnnounceDistrTxRequestPayload
  , signAnnounceDistrTxErrorCodec
  , signAnnounceDistrTxRequestPayloadCodec
  ) where

import Prelude

import Cardano.Types (Address, ScriptHash, Transaction)
import Data.Codec.Argonaut (JsonCodec, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Sum (sumFlat) as CAS
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
