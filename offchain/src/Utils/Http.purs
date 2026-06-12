module CardanoRacers.Utils.Http
  ( getRequest
  , handleResponse
  , postRequest
  ) where

import Prelude

import Aeson (Aeson, stringifyAeson)
import Affjax (Error, Response, URL, defaultRequest) as Affjax
import Affjax.RequestBody (RequestBody(String)) as Affjax
import Affjax.RequestHeader (RequestHeader) as Affjax
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode)) as Affjax
import Cardano.Provider (request) as Affjax
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Either (Either(Left, Right))
import Data.HTTP.Method (Method(GET, POST))
import Data.Maybe (Maybe(Just))
import Data.Newtype (wrap)
import Data.Time.Duration (Milliseconds, Seconds(Seconds), fromDuration)
import Effect.Aff (Aff)
import HydraSdk.Lib (caDecodeString)
import HydraSdk.Types
  ( HttpError(DecodeJsonError, HttpRequestError, HttpResponseError)
  )

defaultRequestTimeout :: Milliseconds
defaultRequestTimeout = fromDuration $ Seconds 30.0

getRequest :: Affjax.URL -> Aff (Either Affjax.Error (Affjax.Response String))
getRequest url =
  Affjax.request $ Affjax.defaultRequest
    { method = Left GET
    , url = url
    , responseFormat = Affjax.ResponseFormat.string
    , timeout = Just defaultRequestTimeout
    }

postRequest
  :: { url :: Affjax.URL
     , content :: Maybe Aeson
     , headers :: Array Affjax.RequestHeader
     }
  -> Aff (Either Affjax.Error (Affjax.Response String))
postRequest { url, content, headers } =
  Affjax.request $ Affjax.defaultRequest
    { method = Left POST
    , url = url
    , headers = headers
    , responseFormat = Affjax.ResponseFormat.string
    , content = Affjax.String <<< stringifyAeson <$> content
    , timeout = Just defaultRequestTimeout
    }

handleResponse
  :: forall (a :: Type)
   . CA.JsonCodec a
  -> Either Affjax.Error (Affjax.Response String)
  -> Either HttpError a
handleResponse respCodec = case _ of
  Left affjaxErr ->
    Left $ HttpRequestError $ wrap affjaxErr
  Right { status, body } ->
    case status of
      Affjax.StatusCode statusCode | statusCode >= 200 && statusCode <= 299 ->
        lmap (DecodeJsonError body) $
          caDecodeString respCodec body
      _ ->
        Left $ HttpResponseError status body
