module Test.CardanoRacers.HydraGroup (suite) where

import Prelude

import Cardano.Types (TransactionHash)
import Cardano.Types.BigNum (fromInt) as BigNum
import CardanoRacers.HydraGroup.Contract
  ( disbandHydraGroup
  , queryHydraGroups
  , registerHydraGroup
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.Test (ContractTest, InitialUTxOs, withKeyWallet, withWallets)
import Contract.Test.Mote (TestPlanM)
import Contract.Transaction (awaitTxConfirmed)
import Contract.Wallet (ownPaymentPubKeyHash)
import Data.Array (head, singleton) as Array
import Data.Newtype (unwrap)
import Mote (group, test)

suite :: TestPlanM ContractTest Unit
suite =
  group "HydraGroup" do
    test "RegisterGroup" do
      withWallets distr \groupManager ->
        withKeyWallet groupManager do
          txHash <- registerTestGroup
          logInfo' $ "Success: " <> show txHash

    test "DisbandGroup" do
      withWallets distr \groupManager ->
        withKeyWallet groupManager do
          registerTxHash <- registerTestGroup
          awaitTxConfirmed registerTxHash
          { oref } <- liftedM "No Hydra groups found" $ Array.head <$>
            queryHydraGroups
          txHash <- disbandHydraGroup oref
          logInfo' $ "Success: " <> show txHash
  where
  distr :: InitialUTxOs
  distr =
    [ BigNum.fromInt 5_000_000
    , BigNum.fromInt 2_000_000_000
    ]

registerTestGroup :: Contract TransactionHash
registerTestGroup = do
  ownPkh <- unwrap <$> liftedM "Could not get own pkh" ownPaymentPubKeyHash
  let
    masterKeys = Array.singleton ownPkh
    httpServers = Array.singleton "httpServer!"
    metadata = "metadata!"
  txHash <- registerHydraGroup masterKeys httpServers metadata
  logInfo' $ "Success: " <> show txHash
  pure txHash
