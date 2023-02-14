module Utils where

import PlutusTx.Prelude
import Ledger (Address, toPubKeyHash, toValidatorHash)
import Plutus.V2.Ledger.Api (
  TxInfo,
  Value,
 )
import Plutus.V2.Ledger.Contexts (valueLockedBy, valuePaidTo)
import Control.Applicative ((<|>))

{-# INLINABLE valueToAddr #-}
valueToAddr :: TxInfo -> Address -> Maybe Value
valueToAddr info addr =
  (fmap (valuePaidTo info) . toPubKeyHash $ addr)
    <|> (fmap (valueLockedBy info) . toValidatorHash $ addr)


