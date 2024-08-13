module CardanoRacers.AssetRequest.Contract
  ( requestAssetByRarity
  , mkAssetRequestPolicy
  ) where

import Contract.Prelude

import Cardano.FromData (fromData)
import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Plutus.Types.Address as PlutusAddress
import Cardano.Types (Address)
import Cardano.Types.AssetName (mkAssetName)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.PlutusScript (hash)
import CardanoRacers.AssetRequest.Types
  ( AirdropAddressDatum(AirdropAddressDatum)
  , AssetRequestRedeemer(MintRequestToken)
  )
import CardanoRacers.Deposit.Validator (mkDepositValidator)
import CardanoRacers.GameAsset.Types (Rarity)
import CardanoRacers.Helpers (paysToAddrConstraint)
import CardanoRacers.RacersState.Contract
  ( queryRacersRefScriptOutput
  , queryRacersState
  )
import CardanoRacers.RacersState.Types (getAssetPrice)
import CardanoRacers.ScriptsFFI (assetRequestPolicy)
import Contract.Monad (liftContractM, liftedM)
import Contract.PlutusData (toData)
import Contract.ScriptLookups (ScriptLookups, plutusMintingPolicy)
import Contract.ScriptLookups as Lookups
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , InputWithScriptRef(RefInput)
  )
import Contract.TxConstraints as Constraints
import Contract.Value (lovelaceValueOf, singleton) as Value
import Contract.Wallet (getWalletAddresses)
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array (head) as Array
import Data.Int (ceil)
import Data.Map (singleton) as Map
import Data.Profunctor.Choice (left)
import Data.TextEncoder (encodeUtf8)
import Effect.Exception (error)
import JS.BigInt as JSBigInt
import Racers (Racers)

requestAssetByRarity
  :: Rarity
  -> Racers TransactionHash
requestAssetByRarity rarity = do
  assetRequestPolicy <- mkAssetRequestPolicy
  depositScript <- (hash <<< unwrap) <$> mkDepositValidator

  (ownAddr :: Address) <- lift $ liftedM "Could not get first address"
    (Array.head <$> getWalletAddresses)
  plutusAddr <- lift $ liftContractM "Could not convert Address to Plutus"
    (PlutusAddress.fromCardano ownAddr)

  rs /\ stateTxi /\ stateTxo <- queryRacersState

  requestTokenName <- lift $ liftContractM "Could not make required token names"
    $
      (mkAssetName <<< wrap <<< encodeUtf8) (show rarity)

  mAssetRequestPolicyRef <- queryRacersRefScriptOutput depositScript

  red <- lift
    $ liftContractM "Could not get Redeemer data"
    $ fromData
    $ toData
    $ MintRequestToken

  let
    (totalAdaDue :: JSBigInt.BigInt) = getAssetPrice rarity
      (unwrap rs).assetPrices
    (treasuryAmt :: BigNum.BigNum) = BigNum.fromInt <<< ceil
      $ JSBigInt.toNumber totalAdaDue
      * 0.75
    (operatingAmt :: BigNum.BigNum) = BigNum.fromInt <<< ceil
      $ JSBigInt.toNumber totalAdaDue
      * 0.25
    treasuryVal = Value.lovelaceValueOf treasuryAmt
    operatingVal = Value.lovelaceValueOf operatingAmt
    lockedVal = Value.singleton depositScript requestTokenName BigNum.one

    dat = toData $ AirdropAddressDatum { airdropAddress: plutusAddr }
    -- red = toData $ MintRequestToken

    mintRequestTokenConstraints = case mAssetRequestPolicyRef of
      Nothing -> Constraints.mustMintCurrencyWithRedeemer
        depositScript
        red
        requestTokenName
        (Int.fromInt 1)
      Just (refTxi /\ refTxo) ->
        Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
          depositScript
          red
          requestTokenName
          (Int.fromInt 1)
          (RefInput $ wrap { input: refTxi, output: refTxo })

    assetRequestPolicyLookups = maybe
      assetRequestPolicy
      (const mempty)
      mAssetRequestPolicyRef

    constraints :: Constraints.TxConstraints
    constraints =
      Constraints.mustReferenceOutput stateTxi
        <> paysToAddrConstraint (unwrap rs).treasuryAddress treasuryVal
        <> paysToAddrConstraint (unwrap rs).operatingAddress operatingVal
        <> mintRequestTokenConstraints
        <> Constraints.mustPayToScript depositScript dat DatumInline
          lockedVal

    lookups :: Lookups.ScriptLookups
    lookups = assetRequestPolicyLookups
      <> Lookups.unspentOutputs (Map.singleton stateTxi stateTxo)

  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

mkAssetRequestPolicy :: Racers ScriptLookups
mkAssetRequestPolicy = do
  params <- asks _.params
  depositVHash <- (hash <<< unwrap) <$> mkDepositValidator
  v2script <- lift $ liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope assetRequestPolicy
    plutusScriptFromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData params, toData depositVHash ]
  pure $ plutusMintingPolicy $ appliedScript
