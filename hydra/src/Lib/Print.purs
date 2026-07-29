module CardanoRaces.Hydra.Lib.Print
  ( printHex
  ) where

import Prelude

import Cardano.AsCbor (class AsCbor, encodeCbor)
import Contract.CborBytes (cborBytesToHex)

printHex :: forall (a :: Type). AsCbor a => a -> String
printHex = cborBytesToHex <<< encodeCbor
