module CardanoRacers.Hydra.Contracts.Collateral
  ( getCollateralUtxo
  ) where

import Contract.Prelude

import Cardano.Types (TransactionOutput(TransactionOutput))
import Cardano.Types.Value (lovelaceValueOf) as Value
import CardanoRacers.Hydra.Const (appConst)
import CardanoRacers.Hydra.Types.Common (Utxo)
import CardanoRacers.Hydra.Types.ContractResult
  ( emptySubmitTxData
  , submitTxReturningContractResult
  )
import Contract.Monad (Contract, liftedM)
import Contract.Transaction (awaitTxConfirmed)
import Contract.TxConstraints (mustPayToPubKey) as Constraints
import Contract.Wallet (getWalletUtxos, ownPaymentPubKeyHash)
import Data.Array (find) as Array
import Data.Map (toUnfoldable) as Map

getCollateralUtxo :: Contract Utxo
getCollateralUtxo =
  queryCollateralUtxo >>=
    maybe
      ( createCollateralUtxo
          *> liftedM "Could not get collateral utxo" queryCollateralUtxo
      )
      pure

queryCollateralUtxo :: Contract (Maybe Utxo)
queryCollateralUtxo =
  getWalletUtxos >>=
    maybe (pure Nothing)
      ( pure <<< Array.find (isCollateralTxOut <<< snd) <<<
          Map.toUnfoldable
      )

isCollateralTxOut :: TransactionOutput -> Boolean
isCollateralTxOut (TransactionOutput txOut) =
  txOut.amount == Value.lovelaceValueOf appConst.collateralLovelace
    && isNothing txOut.datum
    && isNothing txOut.scriptRef

createCollateralUtxo :: Contract Unit
createCollateralUtxo = do
  ownPkh <- liftedM "Could not get own pkh" ownPaymentPubKeyHash
  { txHash } <- submitTxReturningContractResult {} $ emptySubmitTxData
    { constraints =
        Constraints.mustPayToPubKey ownPkh $
          Value.lovelaceValueOf appConst.collateralLovelace
    }
  awaitTxConfirmed txHash
