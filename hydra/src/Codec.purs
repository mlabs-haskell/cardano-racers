module CardanoRacers.Hydra.Codec
  ( serverConfigCodec
  , uintCodec
  , vkeyWitnessCodec
  ) where

import Prelude

import Cardano.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.Provider (ServerConfig)
import Cardano.Types (Vkeywitness)
import Data.Codec.Argonaut (JsonCodec, boolean, int, object, prismaticCodec, string) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.UInt (UInt)
import Data.UInt (fromInt', toInt) as UInt
import HydraSdk.Lib (cborBytesCodec)

-- TODO: Export asCborCodec from HydraSdk.Lib 
asCborCodec :: forall a. AsCbor a => String -> CA.JsonCodec a
asCborCodec name =
  CA.prismaticCodec name decodeCbor encodeCbor
    cborBytesCodec

serverConfigCodec :: CA.JsonCodec ServerConfig
serverConfigCodec =
  CA.object "ServerConfig" $ CAR.record
    { port: uintCodec
    , host: CA.string
    , secure: CA.boolean
    , path: CA.maybe CA.string
    }

uintCodec :: CA.JsonCodec UInt
uintCodec = CA.prismaticCodec "UInt" UInt.fromInt' UInt.toInt CA.int

vkeyWitnessCodec :: CA.JsonCodec Vkeywitness
vkeyWitnessCodec = asCborCodec "Vkeywitness"
