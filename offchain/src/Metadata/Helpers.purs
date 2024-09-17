module Racers.Metadata.Helpers
  ( mkKey
  , unsafeMkKey
  , lookupKey
  , lookupMetadata
  , errExpectedObject
  ) where

import Prelude

import Aeson (JsonDecodeError(TypeMismatch))
import Cardano.Types (PlutusData(Map, Bytes))
import Cardano.Types.TransactionMetadatum
  ( TransactionMetadatum(Map, Text)
  ) as TxMetadatum
import Data.ByteArray (byteArrayFromAscii)
import Data.Either (Either(Left))
import Data.Foldable (lookup)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(Nothing), fromJust)

mkKey :: String -> Maybe PlutusData
mkKey str = Bytes <$> byteArrayFromAscii str

unsafeMkKey :: Partial => String -> PlutusData
unsafeMkKey = fromJust <<< mkKey

lookupKey :: String -> PlutusData -> Maybe PlutusData
lookupKey keyStr (Map array) = mkKey keyStr >>= flip lookup array
lookupKey _ _ = Nothing

lookupMetadata
  :: String
  -> TxMetadatum.TransactionMetadatum
  -> Maybe TxMetadatum.TransactionMetadatum
lookupMetadata keyStr (TxMetadatum.Map mp) = Map.lookup
  (TxMetadatum.Text keyStr)
  mp
lookupMetadata _ _ = Nothing

errExpectedObject :: forall (a :: Type). Either JsonDecodeError a
errExpectedObject =
  Left (TypeMismatch "Expected object")
