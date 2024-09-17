module Common.ContractHelpers
  ( findAnyAuthUtxo
  , findAuthInUtxosMap
  , findAdminAuthUtxo
  , collectDustByThreshold
  ) where

import Contract.Prelude

import Cardano.Plutus.Types.CurrencySymbol (toCardano)
import Cardano.Types (BigInt, TransactionOutput)
import Cardano.Types.Asset (Asset(AdaAsset))
import Cardano.Types.BigNum (BigNum, toBigInt)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Value (Value, valueOf)
import CardanoRacers.Common.Types (RacersParams)
import Contract.Monad (Contract, liftedM, throwContractError)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoMap)
import Contract.Value (geq, singleton) as Value
import Contract.Wallet (getWalletUtxos)
import Control.Apply (lift2)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array (find) as Array
import Data.Map (filter, isEmpty, keys, toUnfoldable) as Map
import Racers (Racers)

findAnyAuthUtxo
  :: Racers (Maybe (TransactionInput /\ TransactionOutput))
findAnyAuthUtxo = do
  rp <- asks _.params
  utxos <- lift $ liftedM "could not get wallet utxos" $ getWalletUtxos
  pure $ findAuthInUtxosMap rp utxos

findAdminAuthUtxo
  :: Racers (Maybe (TransactionInput /\ TransactionOutput))
findAdminAuthUtxo = do
  utxos <- lift $ liftedM "could not get wallet utxos" $ getWalletUtxos
  asks _.params <#> \rp -> do
    adminScriptHash <- toCardano $ fst (unwrap rp).adminToken
    let
      adminValue :: Value
      adminValue =
        Value.singleton
          adminScriptHash
          (unwrap $ snd (unwrap rp).adminToken) $
          BigNum.fromInt
            1

      mUtxo =
        Array.find
          ( \(_ /\ txo) -> (_ `Value.geq` adminValue)
              (unwrap txo).amount
          ) $ Map.toUnfoldable utxos
    mUtxo

findAuthInUtxosMap
  :: RacersParams
  -> UtxoMap
  -> Maybe (TransactionInput /\ TransactionOutput)
findAuthInUtxosMap rp utxos = do
  adminScriptHash <- toCardano $ fst (unwrap rp).adminToken
  botScriptHash <- toCardano $ fst (unwrap rp).botToken
  let

    adminValue :: Value
    adminValue =
      Value.singleton
        adminScriptHash
        (unwrap $ snd (unwrap rp).adminToken) $
        BigNum.fromInt
          1

    botValue :: Value
    botValue =
      Value.singleton
        botScriptHash
        (unwrap $ snd (unwrap rp).botToken) $
        BigNum.fromInt
          1

    mUtxo =
      Array.find
        ( \(_ /\ txo) -> lift2 (||) (_ `Value.geq` adminValue)
            (_ `Value.geq` botValue)
            (unwrap txo).amount
        ) $ Map.toUnfoldable utxos
  mUtxo

collectDustByThreshold :: BigInt -> Contract TransactionHash
collectDustByThreshold threshold = do
  utxos <- liftedM "could not get wallet utxos" $ getWalletUtxos

  let
    dustUtxos = Map.filter
      ( \txo ->
          let
            (adaAmount :: BigNum) = valueOf AdaAsset (unwrap txo).amount
          in
            toBigInt adaAmount <= threshold
      )
      utxos

  when (Map.isEmpty dustUtxos) $ throwContractError "No dust utxos found"

  let

    constraints :: Constraints.TxConstraints
    constraints = foldMap Constraints.mustSpendPubKeyOutput $ Map.keys dustUtxos

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.unspentOutputs dustUtxos

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure txId
