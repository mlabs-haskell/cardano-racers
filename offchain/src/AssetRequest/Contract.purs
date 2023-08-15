module CardanoRacers.AssetRequest.Contract
  ( requestAssetByRarity
  , mkAssetRequestPolicy
  ) where

import Contract.Prelude

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
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , applyArgs
  , mintingPolicyHash
  , validatorHash
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , mkTxUnspentOut
  , submitTxFromConstraints
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , InputWithScriptRef(RefInput)
  )
import Contract.TxConstraints as Constraints
import Contract.Value
  ( lovelaceValueOf
  , mkTokenName
  , scriptCurrencySymbol
  , singleton
  ) as Value
import Contract.Wallet (getWalletAddresses)
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array (head) as Array
import Data.BigInt (fromInt, toNumber) as BigInt
import Data.Int (ceil)
import Data.Map (singleton) as Map
import Data.Profunctor.Choice (left)
import Data.TextEncoder (encodeUtf8)
import Effect.Exception (error)
import Racers (Racers)

requestAssetByRarity
  :: Rarity
  -> Racers TransactionHash
requestAssetByRarity rarity = do
  assetRequestPolicy <- mkAssetRequestPolicy
  depositScript <- validatorHash <$> mkDepositValidator
  ownAddr <- lift $ liftedM "could not get first address"
    (Array.head <$> getWalletAddresses)
  rs /\ stateTxi /\ stateTxo <- queryRacersState
  cs <- lift $ liftContractM "Could not get currency symbol"
    $ Value.scriptCurrencySymbol
    $ assetRequestPolicy

  requestTokenName <- lift $ liftContractM "Could not make required token names"
    $
      (Value.mkTokenName <<< wrap <<< encodeUtf8) (show rarity)

  mAssetRequestPolicyRef <- queryRacersRefScriptOutput
    (unwrap $ mintingPolicyHash assetRequestPolicy)

  let
    totalAdaDue = getAssetPrice rarity (unwrap rs).assetPrices
    treasuryAmt = BigInt.fromInt <<< ceil $ BigInt.toNumber totalAdaDue * 0.75
    operatingAmt = BigInt.fromInt <<< ceil $ BigInt.toNumber totalAdaDue * 0.25
    treasuryVal = Value.lovelaceValueOf treasuryAmt
    operatingVal = Value.lovelaceValueOf operatingAmt
    lockedVal = Value.singleton cs requestTokenName $ BigInt.fromInt 1

    dat = Datum $ toData $ AirdropAddressDatum { airdropAddress: ownAddr }
    red = Redeemer $ toData $ MintRequestToken

    mintRequestTokenConstraints = case mAssetRequestPolicyRef of
      Nothing -> Constraints.mustMintCurrencyWithRedeemer
        (mintingPolicyHash assetRequestPolicy)
        red
        requestTokenName
        (BigInt.fromInt 1)
      Just (refTxi /\ refTxo) ->
        Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
          (mintingPolicyHash assetRequestPolicy)
          red
          requestTokenName
          (BigInt.fromInt 1)
          (RefInput $ mkTxUnspentOut refTxi refTxo)

    assetRequestPolicyLookups = maybe (Lookups.mintingPolicy assetRequestPolicy)
      (const mempty)
      mAssetRequestPolicyRef

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustReferenceOutput stateTxi
        <> paysToAddrConstraint (unwrap rs).treasuryAddress treasuryVal
        <> paysToAddrConstraint (unwrap rs).operatingAddress operatingVal
        <> mintRequestTokenConstraints
        <> Constraints.mustPayToScript depositScript dat DatumInline
          lockedVal

    lookups :: Lookups.ScriptLookups Void
    lookups = assetRequestPolicyLookups
      <> Lookups.unspentOutputs (Map.singleton stateTxi stateTxo)

  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

mkAssetRequestPolicy :: Racers MintingPolicy
mkAssetRequestPolicy = do
  params <- asks _.params
  depositVHahs <- validatorHash <$> mkDepositValidator
  v2script <- lift $ liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope assetRequestPolicy
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData params, toData depositVHahs ]
  pure $ PlutusMintingPolicy $ appliedScript
