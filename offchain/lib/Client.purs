module Lib.CardanoRacers.Client where

import Contract.Prelude

import CardanoRacers.AssetRequest.Contract (requestAssetByRarity)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types (GameAssetType(..), Rarity(..))
import CardanoRacers.Nitro.Contract (buyNitroContract, mkNitroPolicy)
import CardanoRacers.RaceRegistry.Contract
  ( confirmAssetSelection
  , registerPositionInRace
  )
import CardanoRacers.RaceSlot.Contract (mkRaceSlotPolicy)
import CardanoRacers.RaceSlot.Types (slotTokenName)
import Contract.Config (testnetConfig)
import Contract.Monad (liftContractM, liftedM, runContract, throwContractError)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Scripts (mintingPolicyHash)
import Contract.Transaction (TransactionHash)
import Contract.Value (mkTokenName, scriptCurrencySymbol)
import Contract.Wallet (getWalletAddresses)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.Contract.Wallet (ownPubKeyHashes)
import Data.Array (head) as Array
import Lib.CardanoRacers.Common (CredentialProvider, Nitro, Race, toWalletSpec)
import Lib.CardanoRacers.Queries (Queries, mkQueries)
import Racers (Racers, runRacers, withContract)
import Record (merge)
import Type.Row (type (+))

type Client r =
  ( buyNitro :: Nitro -> Aff TransactionHash
  , requestAsset :: String -> Aff TransactionHash
  , registerInRace :: Race -> Aff TransactionHash
  , joinRace :: Race -> String -> String -> Aff TransactionHash
  | r
  )

mkClient
  :: CredentialProvider -> RacersParams -> Aff (Record (Client + Queries + ()))
mkClient cp rp = do
  queries <- mkQueries cp rp
  let
    walletSpec = toWalletSpec cp
    cfg = testnetConfig { walletSpec = Just walletSpec }

    runC :: Racers ~> Aff
    runC = runContract cfg <<< runRacers rp
  pure $
    { buyNitro: \amount -> runC (buyNitro amount)
    , requestAsset: \rarity -> runC (requestAsset rarity)
    , registerInRace: \race -> runC (registerInRace race)
    , joinRace: \race car driver -> runC (joinRace race car driver)
    } `merge` queries

buyNitro :: Nitro -> Racers TransactionHash
buyNitro = buyNitroContract

requestAsset :: String -> Racers TransactionHash
requestAsset rarityStr = do
  rarity <- lift $ case rarityStr of
    "common" -> pure Common
    "rare" -> pure Rare
    "epic" -> pure Epic
    x -> throwContractError ("Invalid rarity" <> x)
  requestAssetByRarity rarity

registerInRace :: Race -> Racers TransactionHash
registerInRace race = do
  nitroPolicyHash <- mintingPolicyHash <$> mkNitroPolicy
  driverAssetPolicyHash <- mintingPolicyHash <$> mkGameAssetPolicy DriverType
  carAssetPolicyHash <- mintingPolicyHash <$> mkGameAssetPolicy CarType
  slotSymbol <-
    withContract (liftedM "could not get currency symbol from policy")
      $ scriptCurrencySymbol
      <$> mkRaceSlotPolicy (wrap race.raceId)
  let
    rgp = wrap
      { slotAssetClass: slotSymbol /\ slotTokenName
      , nitroPolicyHash
      , driverAssetPolicyHash
      , carAssetPolicyHash
      , nitroFee: race.nitroFee
      }

  firstPkh <- lift $ liftedM "Could not get first own public key hash"
    $ ownPubKeyHashes
    <#> Array.head

  registerPositionInRace rgp firstPkh

joinRace :: Race -> String -> String -> Racers TransactionHash
joinRace race carTokenStr driverTokenStr = do
  nitroPolicyHash <- mintingPolicyHash <$> mkNitroPolicy
  driverAssetPolicyHash <- mintingPolicyHash <$> mkGameAssetPolicy DriverType
  carAssetPolicyHash <- mintingPolicyHash <$> mkGameAssetPolicy CarType
  slotSymbol <-
    withContract (liftedM "could not get currency symbol from policy")
      $ scriptCurrencySymbol
      <$> mkRaceSlotPolicy (wrap race.raceId)
  let
    rgp = wrap
      { slotAssetClass: slotSymbol /\ slotTokenName
      , nitroPolicyHash
      , driverAssetPolicyHash
      , carAssetPolicyHash
      , nitroFee: race.nitroFee
      }

  carToken <- lift
    $ liftContractM ("Could not create token name from" <> carTokenStr)
    $ (mkTokenName <=< byteArrayFromAscii) carTokenStr
  driverToken <- lift
    $ liftContractM ("Could not create token name from" <> driverTokenStr)
    $ (mkTokenName <=< byteArrayFromAscii) driverTokenStr

  firstPkh <- lift $ liftedM "Could not get first own public key hash"
    $ ownPubKeyHashes
    <#> Array.head

  firstAddr <- lift $ liftedM "Could not get first own address"
    $ getWalletAddresses
    <#> Array.head

  let
    participant = wrap
      { car: carToken
      , driver: driverToken
      , payoutAddress: firstAddr
      }

  confirmAssetSelection rgp firstPkh participant
