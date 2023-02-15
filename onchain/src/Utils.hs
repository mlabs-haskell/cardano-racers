module Utils where

import Control.Applicative ((<|>))
import Ledger (Address, toPubKeyHash, toValidatorHash)
import Plutus.V2.Ledger.Api (
  TxInfo,
  Value,
 )
import Plutus.V2.Ledger.Contexts (valueLockedBy, valuePaidTo)
import PlutusTx.Prelude

{-# INLINEABLE valueToAddr #-}
valueToAddr :: TxInfo -> Address -> Maybe Value
valueToAddr info addr =
  (fmap (valuePaidTo info) . toPubKeyHash $ addr)
    <|> (fmap (valueLockedBy info) . toValidatorHash $ addr)
