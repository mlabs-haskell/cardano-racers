module CardanoRacers.Utils.Cose
  ( CoseKey
  , fromBytesCoseKey
  , getCoseKeyHeaderX
  , getCoseSign1Signature
  , mkSigStruct
  ) where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types (Address, CborBytes, RawBytes)
import Ctl.Internal.FfiHelpers (MaybeFfiHelper)
import Data.ByteArray (ByteArray)
import Data.Maybe (Maybe)
import Effect (Effect)

foreign import getCoseSign1Signature :: ByteArray -> Effect ByteArray

foreign import newSigStructure :: ByteArray -> ProtectedHeaderMap -> ByteArray

foreign import data ProtectedHeaderMap :: Type
foreign import newProtectedHeaderMap :: HeaderMap -> ProtectedHeaderMap

foreign import data HeaderMap :: Type
foreign import newHeaderMap :: Effect HeaderMap
foreign import setAlgHeaderToEdDsa :: HeaderMap -> Effect Unit
foreign import setAddressHeader :: CborBytes -> HeaderMap -> Effect Unit

foreign import data CoseKey :: Type
foreign import fromBytesCoseKey :: CborBytes -> Effect CoseKey
foreign import getCoseKeyHeaderX :: MaybeFfiHelper -> CoseKey -> Maybe RawBytes

mkSigStruct :: Address -> ByteArray -> Effect ByteArray
mkSigStruct address payload =
  newSigStructure payload <$> headers
  where
  headers :: Effect ProtectedHeaderMap
  headers = do
    headerMap <- newHeaderMap
    setAlgHeaderToEdDsa headerMap
    setAddressHeader (encodeCbor address) headerMap
    pure $ newProtectedHeaderMap headerMap
