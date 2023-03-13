module CardanoRacers.Deposit.Contract
  ( queryRequestsWithAirdropAddress
  , mkDepositValidator
  ) where

import Contract.Prelude

import CardanoRacers.AssetRequest.Contract (mkAssetRequestPolicy)
import CardanoRacers.AssetRequest.Types (AirdropAddressDatum(..))
import CardanoRacers.Common.Types (RacersParams(..))
import CardanoRacers.Deposit.Types (DepositValidatorParams(..))
import CardanoRacers.GameAsset.Types (GameAssetType, Rarity(..))
import CardanoRacers.RacersState.Contract (queryRacersState)
import CardanoRacers.ScriptsFFI (depositScript)
import Contract.Address (Address, addressToBech32, scriptHashAddress)
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (OutputDatum(..), fromData, toData)
import Contract.Prim.ByteArray (byteArrayToHex, byteArrayToIntArray)
import Contract.Scripts (MintingPolicy(..), Validator(..), applyArgs)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Utxos (utxosAt)
import Contract.Value (TokenName, flattenValue, getTokenName)
import Contract.Value (flattenValue, scriptCurrencySymbol) as Value
import Ctl.Internal.Serialization.Types (TransactionOutput)
import Data.Array (catMaybes, index)
import Data.Array (elem, filter, head) as Array
import Data.BigInt (BigInt)
import Data.Char (fromCharCode)
import Data.Map (Map)
import Data.Map (fromFoldable, toUnfoldable) as Map
import Data.Profunctor.Choice (left)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (fromCharArray)
import Effect.Exception (error)

queryRequestsWithAirdropAddress
  :: RacersParams -> Contract (Map Address (Array (Rarity /\ BigInt)))
queryRequestsWithAirdropAddress rp = do
  (st /\ _) <- queryRacersState rp

  assetRequestPolicy <- mkAssetRequestPolicy rp
  assetRequestSymmol <- liftContractM "Could not get currency symbol"
    $ Value.scriptCurrencySymbol
    $ assetRequestPolicy

  utxosAtDeposit <- utxosAt $ scriptHashAddress (unwrap st).depositScript
    Nothing

  let 
    requestTxos =
      Array.filter
        ( Array.elem assetRequestSymmol <<< map fst <<< flattenValue
            <<< _.amount
            <<< unwrap
        )
        $ map (\(_ /\ txOutRS) -> (unwrap txOutRS).output)
        $ Map.toUnfoldable utxosAtDeposit
    pendingRequests = catMaybes $ requestTxos <#> \requestUtxo -> do
      let
        tokenNamesAmts = catMaybes
          $ map (\(_ /\ tk /\ a) -> Tuple <$> parseRequestToken tk <*> pure a)
          $ flattenValue (unwrap requestUtxo).amount
      airdropAddress <- case (unwrap requestUtxo).datum of
        OutputDatum d -> (_.airdropAddress <<< unwrap) <$> (fromData (unwrap d) :: Maybe AirdropAddressDatum)
        _ -> Nothing
      pure $ airdropAddress /\ tokenNamesAmts

  pure $ Map.fromFoldable pendingRequests
  where 
    parseRequestToken :: TokenName -> Maybe Rarity
    parseRequestToken tk = do
      let
        tkBytes = getTokenName tk
        ia = byteArrayToIntArray tkBytes
      tkStr <- fromCharArray <$> traverse fromCharCode ia
      let splitted = split (Pattern ":") tkStr
      rarityStr <- Array.head splitted
      case rarityStr of
        "Common" -> pure Common
        "Rare" -> pure Rare
        "Epic" -> pure Epic
        _ -> Nothing



mkDepositValidator
  :: RacersParams -> DepositValidatorParams -> Contract Validator
mkDepositValidator rp dp = do
  v2script <- liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope depositScript
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData rp, toData dp ]
  pure $ Validator $ appliedScript
