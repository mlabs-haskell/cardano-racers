module CardanoRacers.Hydra.Services.HydraPeer
  ( signCommitTxRequest
  ) where

import Prelude

import CardanoRacers.Hydra.Handlers.SignCommitTx
  ( SignCommitTxRequestPayload
  , SignCommitTxResponse
  , signCommitTxRequestPayloadCodec
  , signCommitTxResponseCodec
  )
import CardanoRacers.Hydra.Services.Utils (handleResponse, postRequest)
import Ctl.Internal.Helpers ((<</>>))
import Data.Array (singleton) as Array
import Data.Codec.Argonaut (encode) as CA
import Data.Either (Either)
import Data.Maybe (Maybe(Just))
import Effect.Aff (Aff)
import HydraSdk.Types (HttpError)

signCommitTxRequest
  :: String
  -> SignCommitTxRequestPayload
  -> Aff (Either HttpError SignCommitTxResponse)
signCommitTxRequest httpServer reqBody =
  handleResponse signCommitTxResponseCodec <$>
    postRequest
      { url: httpServer <</>> "signCommitTx"
      , content: Just $ CA.encode signCommitTxRequestPayloadCodec reqBody
      , headers: mempty
      }
