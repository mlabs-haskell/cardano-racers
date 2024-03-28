module Test.CardanoRacers.Helpers
  ( initRacersStateWithAdminAndTreasury
  , createRacersParamsHelper
  , fractionOfExUnitsCheck
  ) where

import Contract.Prelude

import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Nitro.Helpers as NitroHelpers
import CardanoRacers.RacersState.Contract (initRacersStateContract) as RacersState
import CardanoRacers.RacersState.Types (AssetPrices, RacersState(RacersState))
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Test.Plutip (withKeyWallet)
import Contract.Test.Assert (ContractCheck, checkExUnitsNotExceed)
import Contract.Wallet (KeyWallet, getWalletAddresses, getWalletUtxos)
import Control.Monad.Trans.Class (lift)
import Data.Array (head) as Array
import Data.BigInt (fromInt, fromNumber, toNumber) as BigInt
import Data.BigInt (BigInt)
import Data.Map (toUnfoldable) as Map
import Racers (Racers, withContract)
import Partial.Unsafe (unsafePartial)

fractionOfExUnitsCheck :: forall (a :: Type). Number -> ContractCheck a
fractionOfExUnitsCheck ratio = 
  let maxExUnits = {mem: BigInt.fromInt 14, steps: BigInt.fromInt 10000}
      mult = BigInt.toNumber $ BigInt.fromInt 1000000
      -- ratio = ratio'
  in checkExUnitsNotExceed 
     { mem: unsafePartial $ fromJust $ BigInt.fromNumber (BigInt.toNumber maxExUnits.mem * ratio * mult)
     , steps: unsafePartial $ fromJust $ BigInt.fromNumber (BigInt.toNumber maxExUnits.steps * ratio * mult)
     }

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

    let
      rs = RacersState
        { nitroPrice: nitroPrice
        , treasuryAddress: treasuryAddr
        , operatingAddress: ownAddr
        , assetPrices: assetPrices
        }
    _ <- RacersState.initRacersStateContract rs
    pure rs
