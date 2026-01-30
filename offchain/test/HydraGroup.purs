module Test.CardanoRacers.HydraGroup (suite) where

import Prelude

import Cardano.Types.BigNum (fromInt) as BigNum
import CardanoRacers.HydraGroup.Contract
  ( RegisterHydraGroupResult
  , disbandHydraGroup
  , findHydraGroupById
  , registerHydraGroup
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.Test (ContractTest, InitialUTxOs, withKeyWallet, withWallets)
import Contract.Test.Mote (TestPlanM)
import Contract.Transaction (awaitTxConfirmed)
import Contract.Wallet (ownPaymentPubKeyHash)
import Data.Array (singleton) as Array
import Data.Newtype (unwrap)
import Mote (group, test)

suite :: TestPlanM ContractTest Unit
suite =
  group "HydraGroup" do
    test "Register new Hydra group" do
      withWallets distr \groupManager ->
        withKeyWallet groupManager do
          { txHash } <- registerTestGroup
          logInfo' $ "Success: " <> show txHash

    test "Find and disband existing Hydra group" do
      withWallets distr \groupManager ->
        withKeyWallet groupManager do
          { txHash: registerTxHash, groupId } <- registerTestGroup
          awaitTxConfirmed registerTxHash
          { oref } <- liftedM "Could not find Hydra Group by ID" $
            findHydraGroupById groupId
          txHash <- disbandHydraGroup oref
          logInfo' $ "Success: " <> show txHash
  where
  distr :: InitialUTxOs
  distr =
    [ BigNum.fromInt 5_000_000
    , BigNum.fromInt 2_000_000_000
    ]

registerTestGroup :: Contract RegisterHydraGroupResult
registerTestGroup = do
  ownPkh <- unwrap <$> liftedM "Could not get own pkh" ownPaymentPubKeyHash
  let
    masterKeys = Array.singleton ownPkh
    httpServers = Array.singleton "httpServer!"
    metadata = "metadata!"
  res <- registerHydraGroup masterKeys httpServers metadata
  logInfo' $ "Success: " <> show res.txHash
  pure res
