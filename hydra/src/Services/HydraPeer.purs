module CardanoRacers.Hydra.Services.HydraPeer
  ( getRaceResultsRequest
  , signAnnounceDistrTxRequest
  , signCommitTxRequest
  ) where

import Prelude

import Cardano.Types (ScriptHash, Vkeywitness)
import CardanoRacers.Hydra.Codec (vkeyWitnessCodec)
import CardanoRacers.Hydra.Handlers.GetRaceResults
  ( GetRaceResultsError
  , getRaceResultsErrorCodec
  )
import CardanoRacers.Hydra.Handlers.SignAnnounceDistrTx.Types
  ( SignAnnounceDistrTxError
  , SignAnnounceDistrTxRequestPayload
  , signAnnounceDistrTxErrorCodec
  , signAnnounceDistrTxRequestPayloadCodec
  )
import CardanoRacers.Hydra.Handlers.SignCommitTx
  ( SignCommitTxError
  , SignCommitTxRequestPayload
  , signCommitTxErrorCodec
  , signCommitTxRequestPayloadCodec
  )
import CardanoRacers.Hydra.Types.RaceStatus (RaceResults, raceResultsCodec)
import CardanoRacers.Utils.Http (getRequest, handleResponse, postRequest)
import CardanoRaces.Hydra.Lib.Print (printHex)
import Ctl.Internal.Helpers ((<</>>))
import Data.Codec.Argonaut (encode) as CA
import Data.Either (Either)
import Data.Maybe (Maybe(Just))
import Effect.Aff (Aff)
import HydraSdk.Types (HttpError)

signCommitTxRequest
  :: String
  -> SignCommitTxRequestPayload
  -> Aff (Either HttpError (Either SignCommitTxError Vkeywitness))
signCommitTxRequest httpServer reqBody =
  handleResponse
    { resultCodec: vkeyWitnessCodec
    , errorCodec: signCommitTxErrorCodec
    } <$>
    postRequest
      { url: httpServer <</>> "signCommitTx"
      , content: Just $ CA.encode signCommitTxRequestPayloadCodec reqBody
      , headers: mempty
      }

signAnnounceDistrTxRequest
  :: String
  -> SignAnnounceDistrTxRequestPayload
  -> Aff (Either HttpError (Either SignAnnounceDistrTxError Vkeywitness))
signAnnounceDistrTxRequest httpServer reqBody =
  handleResponse
    { resultCodec: vkeyWitnessCodec
    , errorCodec: signAnnounceDistrTxErrorCodec
    } <$>
    postRequest
      { url: httpServer <</>> "signAnnounceDistrTx"
      , content: Just $ CA.encode signAnnounceDistrTxRequestPayloadCodec reqBody
      , headers: mempty
      }

getRaceResultsRequest
  :: String
  -> ScriptHash
  -> Aff (Either HttpError (Either GetRaceResultsError (RaceResults Maybe)))
getRaceResultsRequest httpServer raceCs =
  handleResponse
    { resultCodec: raceResultsCodec
    , errorCodec: getRaceResultsErrorCodec
    } <$>
    getRequest (httpServer <</>> ("raceResults/" <> printHex raceCs))
