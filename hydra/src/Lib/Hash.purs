module CardanoRacers.Hydra.Lib.Hash
  ( blake2b256Hash
  ) where

import Data.ByteArray (ByteArray)

foreign import blake2b256Hash :: String -> ByteArray
