module CardanoRacers.RaceSlot.Types where

import Contract.Prelude

import Contract.Prim.ByteArray (ByteArray, byteArrayFromAscii)
import Contract.Value (TokenName, mkTokenName)
import Partial.Unsafe (unsafePartial)

type RaceHash = ByteArray

slotTokenName :: TokenName
slotTokenName = unsafePartial $ fromJust $ (mkTokenName <=< byteArrayFromAscii)
  "Slot"
