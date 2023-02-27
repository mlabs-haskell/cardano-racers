module Test.CardanoRacers.GameAsset where

import Contract.Prelude

import CardanoRacers.GameAsset.Contract (mintNewDriverNft)
import CardanoRacers.GameAsset.Parameters (Rarity(..))
import Contract.Address (Address, getWalletAddresses)
import Contract.Credential (Credential(PubKeyCredential, ScriptCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), toData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Test.Assert
  ( checkGainAtAddress'
  , checkTokenGainAtAddress'
  , label
  , runChecks
  )
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Contract.Transaction (submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(DatumWitness))
import Contract.TxConstraints as Constraints
import Contract.Utxos (getWalletUtxos)
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value (geq, lovelaceValueOf, scriptCurrencySymbol, singleton) as Value
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Data.Array (head) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, toNumber) as BigInt
import Data.Int (ceil)
import Data.Map (singleton, toUnfoldable) as Map
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

gameAssetSuite :: TestPlanM PlutipTest Unit
gameAssetSuite = do
  test "GameAsset" do
    withWallets walletUtxoDistr \w ->
      withKeyWallet w do
        _ <- mintNewDriverNft Common
        logInfo' $ "hello world"
        pure unit
  where
  walletUtxoDistr :: InitialUTxOs
  walletUtxoDistr =
    [ BigInt.fromInt 5_000_000
    , BigInt.fromInt 2_000_000_000
    ]
