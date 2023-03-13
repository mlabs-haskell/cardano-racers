module CardanoRacers.AssetRequest.Contract where

import Contract.Prelude

import CardanoRacers.AssetRequest.Types (AirdropAddressDatum(..), AssetRequestRedeemer(..))
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.GameAsset.Types (GameAssetType(..), Rarity)
import CardanoRacers.Helpers (paysToAddrConstraint)
import CardanoRacers.RacersState.Contract (queryRacersState)
import CardanoRacers.ScriptsFFI (assetRequestPolicy, depositScript)
import Contract.Address (getWalletAddresses)
import Contract.AssocMap as AssocMap
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (Datum(..), OutputDatum(..), Redeemer(..), toData, unitDatum, unitRedeemer)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(..), applyArgs)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (TransactionHash, awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Value (lovelaceValueOf, mkTokenName, scriptCurrencySymbol, singleton) as Value
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head, singleton) as Array
import Data.BigInt (fromInt, toNumber) as BigInt
import Data.Int (ceil)
import Data.Map (singleton) as Map
import Data.Profunctor.Choice (left)
import Effect.Exception (error)

requestAssetByRarity :: RacersParams -> Rarity -> Contract TransactionHash
requestAssetByRarity rp rarity = do
  assetRequestPolicy <- mkAssetRequestPolicy rp
  ownAddr <- liftedM "could not get first address"
    (Array.head <$> getWalletAddresses)
  rs /\ stateTxi /\ stateTxo <- queryRacersState rp
  cs <- liftContractM "Could not get currency symbol"
    $ Value.scriptCurrencySymbol
    $ assetRequestPolicy

  -- given that only one asset class per rarity is available at a time, we try
  -- to retrieve it through both  carPrices and driverPrices
  (totalAdaDue /\ assetType) <-
    liftMaybe
      ( error $ show rarity <>
          " is unavailable for purchase. Could not find rarity in state"
      )
      $ (AssocMap.lookup rarity (unwrap rs).carPrices <#> (_ /\ CarType))
      <|>
        (AssocMap.lookup rarity (unwrap rs).driverPrices <#> (_ /\ DriverType))

  let
    tokenNameStr =
      ( show rarity <> ":" <>
          ( case assetType of
              CarType -> "Car"
              DriverType -> "Driver"
          )
      )
  requestTokenName <- liftContractM "Could not make required token names" $
    (Value.mkTokenName <=< byteArrayFromAscii) tokenNameStr

  logInfo' tokenNameStr

  let
    treasuryAmt = BigInt.fromInt <<< ceil $ BigInt.toNumber totalAdaDue * 0.75
    operatingAmt = BigInt.fromInt <<< ceil $ BigInt.toNumber totalAdaDue * 0.25
    treasuryVal = Value.lovelaceValueOf treasuryAmt
    operatingVal = Value.lovelaceValueOf operatingAmt
    lockedVal = Value.singleton cs requestTokenName $ BigInt.fromInt 1

    dat = Datum $ toData $ AirdropAddressDatum { airdropAddress: ownAddr }

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustReferenceOutput stateTxi
        <> paysToAddrConstraint (unwrap rs).treasuryAddress treasuryVal
        <> paysToAddrConstraint (unwrap rs).operatingAddress operatingVal
        <> Constraints.mustMintValueWithRedeemer unitRedeemer lockedVal
        <> Constraints.mustPayToScript (unwrap rs).depositScript dat DatumInline
          lockedVal

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy assetRequestPolicy
      <> Lookups.unspentOutputs (Map.singleton stateTxi stateTxo)

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure txId

mkAssetRequestPolicy :: RacersParams -> Contract MintingPolicy
mkAssetRequestPolicy params = do
  v2script <- liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope assetRequestPolicy
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ Array.singleton
    $ toData params
  pure $ PlutusMintingPolicy $ appliedScript
