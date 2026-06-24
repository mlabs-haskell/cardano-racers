module CardanoRacers.Hydra.Codec
  ( ed25519SignatureCodec
  , networkIdCodec
  , plutusAddressCodec
  , racersParamsCodec
  , serverConfigCodec
  , uintCodec
  , vkeyWitnessCodec
  ) where

import Prelude

import Cardano.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Provider (ServerConfig)
import Cardano.Types (Ed25519Signature, NetworkId, Vkeywitness)
import CardanoRacers.Common.Types (RacersParams)
import Data.Codec.Argonaut (JsonCodec, boolean, int, object, prismaticCodec, string) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Generic (nullarySum) as CAG
import Data.Codec.Argonaut.Record (record) as CAR
import Data.UInt (UInt)
import Data.UInt (fromInt', toInt) as UInt
import HydraSdk.Lib (aesonCodec, cborBytesCodec)

-- TODO(low): Export asCborCodec from HydraSdk.Lib 
asCborCodec :: forall a. AsCbor a => String -> CA.JsonCodec a
asCborCodec name =
  CA.prismaticCodec name decodeCbor encodeCbor
    cborBytesCodec

ed25519SignatureCodec :: CA.JsonCodec Ed25519Signature
ed25519SignatureCodec = asCborCodec "Ed25519Signature"

networkIdCodec :: CA.JsonCodec NetworkId
networkIdCodec = CAG.nullarySum "NetworkId"

plutusAddressCodec :: CA.JsonCodec Plutus.Address
plutusAddressCodec = aesonCodec "Plutus.Address"

racersParamsCodec :: CA.JsonCodec RacersParams
racersParamsCodec = aesonCodec "RacersParams"

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
