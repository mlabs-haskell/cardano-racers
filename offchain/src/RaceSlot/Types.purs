module CardanoRacers.RaceSlot.Types where

import Contract.Prelude

import Cardano.Plutus.Types.TokenName (TokenName, mkTokenName)
import Contract.Prim.ByteArray (ByteArray, byteArrayFromAscii)
import Partial.Unsafe (unsafePartial)

type RaceHash = ByteArray

slotTokenName :: TokenName
slotTokenName = unsafePartial $ fromJust $ (mkTokenName <=< byteArrayFromAscii)
  "Slot"
