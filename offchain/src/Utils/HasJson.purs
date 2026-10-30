module CardanoRacers.Utils.HasJson
  ( class HasJson
  , fromJs
  , jsonCodec
  , toJs
  ) where

import Prelude

import Aeson (Aeson, Finite)
import Cardano.Types (Ed25519KeyHash, ScriptHash, TransactionHash)
import Contract.Prim.ByteArray (ByteArray)
import Data.Codec.Argonaut
  ( JsonCodec
  , array
  , boolean
  , decode
  , encode
  , int
  , json
  , number
  , printJsonDecodeError
  , string
  ) as CA
import Data.Codec.Argonaut.Common (tuple) as CACommon
import Data.Codec.Argonaut.Compat (maybe) as CACompat
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import HydraSdk.Lib
  ( byteArrayCodec
  , ed25519KeyHashCodec
  , scriptHashCodec
  , txHashCodec
  )
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(Proxy))

class HasJson a params | a -> params where
  jsonCodec :: params -> Proxy a -> CA.JsonCodec a

toJs :: forall a p. HasJson a p => p -> a -> Aeson
toJs params = CA.encode (jsonCodec params (Proxy :: _ a))

fromJs :: forall a p. HasJson a p => p -> Aeson -> a
fromJs params =
  CA.decode (jsonCodec params Proxy) >>>
    case _ of
      Left decodeErr ->
        unsafeCrashWith $ "fromJs: " <> CA.printJsonDecodeError decodeErr
      Right x ->
        x

instance HasJson Aeson anyParams where
  jsonCodec _ = const CA.json

instance HasJson Boolean anyParams where
  jsonCodec _ = const CA.boolean

instance HasJson String anyParams where
  jsonCodec _ = const CA.string

instance HasJson (Finite Number) anyParams where
  jsonCodec _ = const CA.number

instance HasJson Int anyParams where
  jsonCodec _ = const CA.int

instance HasJson a p => HasJson (Array a) p where
  jsonCodec params = const $ CA.array $ jsonCodec params Proxy

instance (HasJson a p, HasJson b p) => HasJson (a /\ b) p where
  jsonCodec params = const $ CACommon.tuple (jsonCodec params Proxy)
    (jsonCodec params Proxy)

instance HasJson a p => HasJson (Maybe a) p where
  jsonCodec params = const $ CACompat.maybe $ jsonCodec params Proxy

instance HasJson ByteArray anyParams where
  jsonCodec _ = const byteArrayCodec

instance HasJson Ed25519KeyHash anyParams where
  jsonCodec _ = const ed25519KeyHashCodec

instance HasJson TransactionHash anyParams where
  jsonCodec _ = const txHashCodec

instance HasJson ScriptHash anyParams where
  jsonCodec _ = const scriptHashCodec
