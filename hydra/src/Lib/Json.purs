module CardanoRacers.Hydra.Lib.Json
  ( printJson
  , printJsonUsingCodec
  ) where

import Prelude

import Aeson (Aeson)
import Data.Codec.Argonaut (JsonCodec, encode) as CA

foreign import stringifyAesonWithIndent :: Int -> Aeson -> String

printJson :: Aeson -> String
printJson = stringifyAesonWithIndent 2

printJsonUsingCodec :: forall (a :: Type). CA.JsonCodec a -> a -> String
printJsonUsingCodec codec = printJson <<< CA.encode codec
