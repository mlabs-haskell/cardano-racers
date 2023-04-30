module Test.CardanoRacers.Helpers
  ( initRacersStateWithAdminAndTreasury
  , createRacersParamsHelper
  ) where

import Contract.Prelude

import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Deposit.Contract (mkDepositValidator)
import CardanoRacers.GameAsset.Types (Rarity)
import CardanoRacers.Nitro.Helpers as NitroHelpers
import CardanoRacers.RacersState.Contract (initRacersStateContract) as RacersState
import CardanoRacers.RacersState.Types (RacersState(RacersState))
import Contract.AssocMap (Map) as AssocMap
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Scripts (validatorHash)
import Contract.Test.Plutip (withKeyWallet)
import Contract.Wallet (KeyWallet, getWalletAddresses, getWalletUtxos)
import Control.Monad.Trans.Class (lift)
import Data.Array (head) as Array
import Data.BigInt (BigInt)
import Data.Map (toUnfoldable) as Map
import Racers (Racers, withContract)

createRacersParamsHelper :: Contract RacersParams
createRacersParamsHelper = do
  utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
  (txi /\ _) <- liftContractM "Could not get first utxo" $ Array.head $
    Map.toUnfoldable utxos
  NitroHelpers.createRacersParams txi "NITRO"

initRacersStateWithAdminAndTreasury
  :: (KeyWallet /\ KeyWallet)
  -> BigInt
  -> AssocMap.Map Rarity BigInt
  -> Racers RacersState
initRacersStateWithAdminAndTreasury
  (admin /\ treasury)
  nitroPrice
  assetPrices = do
  treasuryAddr <- lift $ withKeyWallet treasury
    $ liftedM "Could not get address"
    $ Array.head
    <$> getWalletAddresses
  withContract (withKeyWallet admin) do
    ownAddr <- lift $ liftedM "Could not get address" $ Array.head <$>
      getWalletAddresses

    depositScriptHash <- validatorHash <$> mkDepositValidator

    let
      rs = RacersState
        { nitroPrice: nitroPrice
        , treasuryAddress: treasuryAddr
        , operatingAddress: ownAddr
        , assetPrices: assetPrices
        , depositScript: depositScriptHash
        }
    _ <- RacersState.initRacersStateContract rs
    pure rs

