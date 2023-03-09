module CardanoRacers.Deposit.Contract where

import Contract.Prelude

import CardanoRacers.Common.Types (RacersParams(..))
import CardanoRacers.Deposit.Types (DepositValidatorParams(..))
import CardanoRacers.GameAsset.Types (GameAssetType, Rarity)
import CardanoRacers.RacersState.Contract (queryRacersState)
import CardanoRacers.ScriptsFFI (depositScript)
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (toData)
import Contract.Scripts (MintingPolicy(..), Validator(..), applyArgs)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Data.Profunctor.Choice (left)
import Effect.Exception (error)

requestAssetPurchase :: GameAssetType -> Rarity -> Contract Unit
requestAssetPurchase gt r = do
  (rst /\ _) <- queryRacersState
  pure unit

mkDepositValidator :: RacersParams -> DepositValidatorParams -> Contract Validator
mkDepositValidator rp dp = do
  v2script <- liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope depositScript
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData rp , toData dp]
  pure $ PlutusMintingPolicy $ appliedScript
