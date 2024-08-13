module Test.CardanoRacers.Helpers
  ( initRacersStateWithAdminAndTreasury
  , createRacersParamsHelper
  ) where

import Contract.Prelude

import Cardano.Plutus.Types.Address as PlutusAddress
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Nitro.Helpers as NitroHelpers
import CardanoRacers.RacersState.Contract (initRacersStateContract) as RacersState
import CardanoRacers.RacersState.Types (AssetPrices, RacersState(RacersState))
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Test.Plutip (withKeyWallet)
import Contract.Wallet (KeyWallet, getWalletAddresses, getWalletUtxos)
import Control.Monad.Trans.Class (lift)
import Data.Array (head) as Array
import Data.BigInt (BigInt, toString)
import Data.Map (toUnfoldable) as Map
import JS.BigInt (fromString) as JSBigInt
import Partial.Unsafe (unsafePartial)
import Racers (Racers, withContract)

createRacersParamsHelper :: Contract RacersParams
createRacersParamsHelper = do
  utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
  (txi /\ _) <- liftContractM "Could not get first utxo" $ Array.head $
    Map.toUnfoldable utxos
  NitroHelpers.createRacersParams txi

initRacersStateWithAdminAndTreasury
  :: (KeyWallet /\ KeyWallet)
  -> BigInt
  -> AssetPrices
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

    treasuryAddrPlutus <- lift
      $ liftContractM "Could not convert treasury address to Plutus"
      $ PlutusAddress.fromCardano treasuryAddr
    ownAddrPlutus <- lift
      $ liftContractM "Could not convert own address to Plutus"
      $ PlutusAddress.fromCardano ownAddr

    let
      rs = RacersState
        { nitroPrice: unsafePartial fromJust $ JSBigInt.fromString $ toString
            nitroPrice
        , treasuryAddress: treasuryAddrPlutus
        , operatingAddress: ownAddrPlutus
        , assetPrices: assetPrices
        }
    _ <- RacersState.initRacersStateContract rs
    pure rs
