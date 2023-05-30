module CardanoRacers.Deposit.Validator (mkDepositValidator) where

import Contract.Prelude

import CardanoRacers.ScriptsFFI (depositScript)
import Contract.Monad (liftContractM)
import Contract.PlutusData (toData)
import Contract.Scripts (Validator(..), applyArgs)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)
import Data.Profunctor.Choice (left)
import Effect.Exception (error)
import Racers (Racers)

mkDepositValidator
  :: Racers Validator
mkDepositValidator = do
  rp <- asks _.params
  v2script <- lift $ liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope depositScript
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData rp ]
  pure $ Validator $ appliedScript
