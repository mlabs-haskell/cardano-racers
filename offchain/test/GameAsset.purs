module Test.CardanoRacers.GameAsset (suite) where

import Contract.Prelude

import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip (InitialUTxOs, PlutipTest, withWallets)
import Data.BigInt (fromInt) as BigInt
import Mote (group, test)

suite :: TestPlanM PlutipTest Unit
suite = group "GameAsset tests" do
  test "GameAsset" do
    withWallets walletUtxoDistr \_ ->
      pure unit
  where
  walletUtxoDistr :: InitialUTxOs
  walletUtxoDistr =
    [ BigInt.fromInt 5_000_000
    , BigInt.fromInt 2_000_000_000
    ]
