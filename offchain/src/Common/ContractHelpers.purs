module Common.ContractHelpers (findOwnAuthUtxo) where

import Contract.Prelude

import CardanoRacers.Common.Types (RacersParams)
import Contract.Monad (Contract, liftedM)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Utxos (getWalletUtxos)
import Contract.Value (Value)
import Contract.Value (geq, singleton) as Value
import Control.Apply (lift2)
import Data.Array (find) as Array
import Data.BigInt (fromInt) as BigInt
import Data.Map (toUnfoldable) as Map

findOwnAuthUtxo
  :: RacersParams
  -> Contract (Maybe (TransactionInput /\ TransactionOutputWithRefScript))
findOwnAuthUtxo rp = do
  utxos <- liftedM "could not get wallet utxos" $ getWalletUtxos

  let
    adminValue :: Value
    adminValue = uncurry Value.singleton (unwrap rp).adminToken $ BigInt.fromInt
      1

    botValue :: Value
    botValue = uncurry Value.singleton (unwrap rp).botToken $ BigInt.fromInt 1
    mUtxo =
      Array.find
        ( \(_ /\ txo) -> lift2 (||) (_ `Value.geq` adminValue)
            (_ `Value.geq` botValue)
            (unwrap (unwrap txo).output).amount
        ) $ Map.toUnfoldable utxos
  pure mUtxo
