module CardanoRacers.RacePosition.Types where

import Contract.Prelude

import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Value (TokenName, mkTokenName)
import Partial.Unsafe (unsafePartial)

type RaceHash = String -- in hex

slotTokenName :: TokenName
slotTokenName = unsafePartial $ fromJust $ (mkTokenName <=< byteArrayFromAscii)
  "Slot"
