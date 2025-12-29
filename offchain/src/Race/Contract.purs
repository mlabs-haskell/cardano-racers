module CardanoRacers.Race.Contract
  ( mkRaceValidator
  , startRace
  ) where

import Contract.Prelude

import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Value (fromCardano) as Plutus.Value
import Cardano.ToData (toData)
import Cardano.Types
  ( AssetName
  , Ed25519KeyHash
  , PlutusScript
  , TransactionHash
  , Value
  )
import Cardano.Types.BigNum (one) as BigNum
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Cardano.Types.Value (singleton) as Value
import CardanoRacers.Helpers (mkPosixTimeUnsafe)
import CardanoRacers.Race.Types
  ( RaceDatum(ValueEscrow, RaceState)
  , RaceParams
  , raceStateTokenName
  , valueEscrowTokenName
  )
import CardanoRacers.RaceSlot.Contract
  ( mintRaceSlotTokenConstraints
  , mkRaceSlotPolicy
  )
import CardanoRacers.RaceSlot.Types (RaceHash)
import CardanoRacers.ScriptsFFI (raceScript)
import Contract.Chain (currentTime)
import Contract.Monad (Contract, liftContractM)
import Contract.ScriptLookups (ScriptLookups)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints (mustPayToScript) as Constraints
import Control.Monad.Trans.Class (lift)
import Data.Array (head) as Array
import Data.Bifunctor (lmap)
import Data.Time.Duration (Days(Days))
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Racers (Racers)

startRace
  :: RaceHash
  -> Value
  -> Array Plutus.Address
  -> Array Ed25519KeyHash
  -> Racers
       { txHash :: TransactionHash
       , raceParams :: RaceParams
       }
startRace raceHash totalRewardValue participants delegates = do
  slotPolicy <- mkRaceSlotPolicy raceHash

  slotPolicyHash <-
    lift $ liftContractM "Could not get race slot token script hash"
      ( PlutusScript.hash <$>
          Array.head (unwrap slotPolicy).plutusMintingPolicies
      )

  (slotConstraints /\ slotLookups) <- mintRaceSlotTokenConstraints
    raceHash
    [ raceStateTokenName /\ one
    , valueEscrowTokenName /\ one
    ]

  nowTime <- lift currentTime

  let
    raceParams :: RaceParams
    raceParams = wrap
      { stateCurrencySymbol: slotPolicyHash
      , totalRewardValue: Plutus.Value.fromCardano totalRewardValue
      , participants
      , delegates
      , escrowTtl: nowTime + mkPosixTimeUnsafe (Days 2.0)
      }

  raceValidatorHash <- lift $ PlutusScript.hash <$> mkRaceValidator raceParams
  let
    mkStateTokenValue :: AssetName -> Value
    mkStateTokenValue tn = Value.singleton slotPolicyHash tn BigNum.one

    raceStateTokenValue :: Value
    raceStateTokenValue = mkStateTokenValue raceStateTokenName

    valueEscrowTokenValue :: Value
    valueEscrowTokenValue = mkStateTokenValue valueEscrowTokenName

    escrowValue :: Value
    escrowValue = unsafePartial $ totalRewardValue <> valueEscrowTokenValue

    -- TODO: ensure totalRewardValue comes from the treasury wallet?
    constraints :: TxConstraints
    constraints = mconcat
      [ slotConstraints

      , Constraints.mustPayToScript raceValidatorHash (toData ValueEscrow)
          DatumInline
          escrowValue

      , Constraints.mustPayToScript raceValidatorHash
          (toData $ RaceState { distribution: Nothing })
          DatumInline
          raceStateTokenValue
      ]

    lookups :: ScriptLookups
    lookups = slotLookups

  lift do
    txHash <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txHash
    pure
      { txHash
      , raceParams
      }

mkRaceValidator :: RaceParams -> Contract PlutusScript
mkRaceValidator params = do
  v2script <- liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope raceScript
    plutusScriptFromEnvelope envelope
  appliedScript <- liftEither $ lmap (error <<< show) $ applyArgs v2script
    $ [ toData params ]
  pure appliedScript
