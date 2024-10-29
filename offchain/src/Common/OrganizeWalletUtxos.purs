module Common.OrganizeWalletUtxos (organizeUTXOsByAssetClass) where

import Contract.Prelude

import Cardano.Types (Asset(Asset), PaymentPubKeyHash, ScriptHash, UtxoMap)
import Cardano.Types.Asset (Asset(AdaAsset))
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.PlutusScript (hash)
import Cardano.Types.Value (flatten, unflatten, valueOf)
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types (GameAssetType(DriverType, CarType))
import CardanoRacers.Nitro.Contract (mkNitroPolicy)
import Contract.Monad (liftContractM, liftedM)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Wallet (getWalletUtxos, ownPaymentPubKeyHashes)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.Array (head, partition) as Array
import Data.Map (filter, keys, values) as Map
import Effect.Exception (error)
import Racers (Racers)

organizeUTXOsByAssetClass :: Racers TransactionHash
organizeUTXOsByAssetClass = do
  nitroPolicy <- mkNitroPolicy
  nitroPolicyHash <- lift $ liftContractM "Could not get nitro script hash"
    $ hash
    <$> (head (unwrap nitroPolicy).plutusMintingPolicies)

  driverAssetPolicy <- mkGameAssetPolicy DriverType
  driverAssetPolicyHash <- lift
    $ liftContractM "Could not get driver asset script hash"
    $ hash
    <$> (head (unwrap driverAssetPolicy).plutusMintingPolicies)

  carAssetPolicy <- mkGameAssetPolicy CarType
  carAssetPolicyHash <- lift
    $ liftContractM "Could not get car asset script hash"
    $ hash
    <$> (head (unwrap carAssetPolicy).plutusMintingPolicies)

  pkh <- lift $ liftedM "could not get first wallet pubKeyHash"
    (Array.head <$> ownPaymentPubKeyHashes)

  utxos <- lift $ liftedM "Could not get wallet utxos" getWalletUtxos

  let
    (vals :: Array (Tuple Asset BigNum)) =
      foldMap (\txo -> flatten (unwrap txo).amount) $ Map.values utxos

    (nitroAssets /\ nonNitroAssets) = partitionAssets nitroPolicyHash vals
    (carAssets /\ nonCarAssetPolicyHash) = partitionAssets carAssetPolicyHash
      nonNitroAssets
    (driverAssets /\ _remainingAssets) = partitionAssets driverAssetPolicyHash
      nonCarAssetPolicyHash

  nitroConstraints <- constructConstraints pkh nitroAssets "Nitro assets"
  carConstraints <- constructConstraints pkh carAssets "Car assets"
  driverConstraints <- constructConstraints pkh driverAssets "Driver assets"

  let
    assetsConstraints = nitroConstraints <> carConstraints <> driverConstraints
    (_dustUtxos /\ dustConstraints) = getDustUtxos (BigNum.fromInt 4_000_000)
      utxos

    constraints :: Constraints.TxConstraints
    constraints = dustConstraints <> assetsConstraints

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.unspentOutputs utxos

  txId <- lift $ submitTxFromConstraints lookups constraints
  lift $ awaitTxConfirmed txId
  pure txId

partitionAssets
  :: ScriptHash
  -> Array (Tuple Asset BigNum)
  -> Tuple (Array (Tuple Asset BigNum)) (Array (Tuple Asset BigNum))
partitionAssets policyHash assets =
  let
    pArr = Array.partition
      ( \(asset /\ _) -> case asset of
          AdaAsset -> false
          Asset sh _ -> sh == policyHash
      )
      assets
  in
    (pArr.yes /\ pArr.no)

constructConstraints
  :: PaymentPubKeyHash
  -> Array (Tuple Asset BigNum)
  -> String
  -> Racers Constraints.TxConstraints
constructConstraints pkh assets errMsg =
  if length assets > 0 then do
    vals <- lift $ liftM (error $ "Could not unflatten " <> errMsg) $ unflatten
      assets
    pure $ Constraints.mustPayToPubKey pkh vals
  else pure mempty

getDustUtxos :: BigNum -> UtxoMap -> (Tuple UtxoMap Constraints.TxConstraints)
getDustUtxos threshold utxos = (dustUtxos /\ cstrnts)
  where
  dustUtxos = Map.filter
    (\txo -> (valueOf AdaAsset (unwrap txo).amount) <= threshold)
    utxos
  cstrnts =
    if (length dustUtxos > 0) then
      foldMap Constraints.mustSpendPubKeyOutput $ Map.keys dustUtxos
    else mempty
