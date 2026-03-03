module CardanoRacers.Hydra.Services.HydraPeer
  ( signAnnounceDistrTxRequest
  , signCommitTxRequest
  ) where

import Prelude

import CardanoRacers.Hydra.Handlers.SignAnnounceDistrTx.Types
  ( SignAnnounceDistrTxRequestPayload
  , SignAnnounceDistrTxResponse
  , signAnnounceDistrTxRequestPayloadCodec
  , signAnnounceDistrTxResponseCodec
  )
import CardanoRacers.Hydra.Handlers.SignCommitTx
  ( SignCommitTxRequestPayload
  , SignCommitTxResponse
  , signCommitTxRequestPayloadCodec
  , signCommitTxResponseCodec
  )
import CardanoRacers.Hydra.Services.Utils (handleResponse, postRequest)
import Ctl.Internal.Helpers ((<</>>))
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

signAnnounceDistrTxRequest
  :: String
  -> SignAnnounceDistrTxRequestPayload
  -> Aff (Either HttpError SignAnnounceDistrTxResponse)
signAnnounceDistrTxRequest httpServer reqBody =
  handleResponse signAnnounceDistrTxResponseCodec <$>
    postRequest
      { url: httpServer <</>> "signAnnounceDistrTx"
      , content: Just $ CA.encode signAnnounceDistrTxRequestPayloadCodec reqBody
      , headers: mempty
      }
