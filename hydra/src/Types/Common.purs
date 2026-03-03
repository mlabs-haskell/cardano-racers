module CardanoRacers.Hydra.Types.Common
  ( Utxo
  ) where

import Cardano.Types (TransactionInput, TransactionOutput)
import Data.Tuple.Nested (type (/\))

type Utxo = TransactionInput /\ TransactionOutput
