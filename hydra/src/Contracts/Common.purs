module CardanoRacers.Hydra.Contracts.Common
  ( findCollateralUtxo
  , findRaceStateUtxo
  , fixTx
  ) where

import Prelude

import Cardano.FromData (class FromData, fromData)
import Cardano.Types
  ( Credential(ScriptHashCredential)
  , Language
  , OutputDatum(OutputDatum)
  , PaymentCredential(PaymentCredential)
  , ScriptHash
  , Transaction
  , TransactionOutput
  , UtxoMap
  )
import Cardano.Types.Address (getPaymentCredential)
import CardanoRacers.Hydra.Contracts.Collateral (isCollateralTxOut)
import CardanoRacers.Hydra.Lib.Transaction (reSignTransaction, setAuxDataHash)
import CardanoRacers.Hydra.Types.Common (Utxo)
import CardanoRacers.Race.Types (RaceDatum(RaceState), RewardDistribution)
import Contract.Monad (Contract)
import Contract.ProtocolParameters (getProtocolParameters)
import Ctl.Internal.Transaction (setScriptDataHash)
import Data.Array (elem, find, findMap) as Array
import Data.Map (filterKeys, toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)

-- Recompute script integrity and auxiliary data hashes, and re-sign the
-- transaction. Tx CBOR can change after re-serialization.
fixTx :: Transaction -> Array Language -> Contract Transaction
fixTx tx languages = do
  pparams <- unwrap <$> getProtocolParameters
  let
    costModels =
      -- PlutusV2 for CardanoRacers scripts, PlutusV3 for Hydra scripts
      Map.filterKeys (flip Array.elem languages)
        pparams.costModels
    ws = unwrap (unwrap tx).witnessSet
  fixedTx <- liftEffect $ setScriptDataHash costModels ws.redeemers ws.plutusData $
    setAuxDataHash tx
  signedTx <- reSignTransaction fixedTx
  pure signedTx

findRaceStateUtxo
  :: ScriptHash
  -> UtxoMap
  -> Maybe
       { utxo :: Utxo
       , distr :: Maybe RewardDistribution
       }
findRaceStateUtxo sh utxos =
  Array.findMap
    ( \utxo@(_ /\ txOut) ->
        case getPaymentCredential (unwrap txOut).address of
          Just (PaymentCredential (ScriptHashCredential sh'))
            | sh == sh'
            , Just (RaceState { distribution: distr }) <- decodeInlineDatum txOut ->
                Just
                  { utxo
                  , distr
                  }
          _ ->
            Nothing
    )
    (Map.toUnfoldable utxos)

findCollateralUtxo :: UtxoMap -> Maybe Utxo
findCollateralUtxo =
  Array.find (isCollateralTxOut <<< snd)
    <<< Map.toUnfoldable

decodeInlineDatum :: forall (a :: Type). FromData a => TransactionOutput -> Maybe a
decodeInlineDatum txOut =
  case (unwrap txOut).datum of
    Just (OutputDatum pd) -> fromData pd
    _ -> Nothing

