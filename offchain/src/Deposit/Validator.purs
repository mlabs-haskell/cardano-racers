module CardanoRacers.Deposit.Validator (mkDepositValidator) where

import Contract.Prelude

import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Plutus.Types.Validator (Validator(..))
import CardanoRacers.ScriptsFFI (depositScript)
import Contract.Monad (liftContractM)
import Contract.PlutusData (toData)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
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
    plutusScriptFromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData rp ]
  pure $ Validator $ appliedScript
