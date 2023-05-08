module Common.ContractHelpers
  ( findAnyAuthUtxo
  , findAuthInUtxosMap
  , findAdminAuthUtxo
  ) where

import Contract.Prelude

import CardanoRacers.Common.Types (RacersParams)
import Contract.Monad (liftedM)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Utxos (UtxoMap)
import Contract.Value (Value)
import Contract.Value (geq, singleton) as Value
import Contract.Wallet (getWalletUtxos)
import Control.Apply (lift2)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array (find) as Array
import Data.BigInt (fromInt) as BigInt
import Data.Map (toUnfoldable) as Map
import Racers (Racers)

findAnyAuthUtxo
  :: Racers (Maybe (TransactionInput /\ TransactionOutputWithRefScript))
findAnyAuthUtxo = do
  rp <- asks _.params
  utxos <- lift $ liftedM "could not get wallet utxos" $ getWalletUtxos
  pure $ findAuthInUtxosMap rp utxos

findAdminAuthUtxo
  :: Racers (Maybe (TransactionInput /\ TransactionOutputWithRefScript))
findAdminAuthUtxo = do
  utxos <- lift $ liftedM "could not get wallet utxos" $ getWalletUtxos
  asks _.params <#> \rp ->
    let
      adminValue :: Value
      adminValue = uncurry Value.singleton (unwrap rp).adminToken $
        BigInt.fromInt
          1

      mUtxo =
        Array.find
          ( \(_ /\ txo) -> (_ `Value.geq` adminValue)
              (unwrap (unwrap txo).output).amount
          ) $ Map.toUnfoldable utxos
    in
      mUtxo

findAuthInUtxosMap
  :: RacersParams
  -> UtxoMap
  -> Maybe (TransactionInput /\ TransactionOutputWithRefScript)
findAuthInUtxosMap rp utxos =
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
  in
    mUtxo
