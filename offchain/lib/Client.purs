module Lib.CardanoRacers.Client where

import Contract.Prelude

import CardanoRacers.AssetRequest.Contract (requestAssetByRarity)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.GameAsset.Types (Rarity(Common, Rare, Epic))
import CardanoRacers.Nitro.Contract (buyNitroContract)
import CardanoRacers.RaceRegistry.Contract
  ( confirmAssetSelection
  , registerPositionInRace
  )
import Contract.Config (ContractParams, WalletSpec)
import Contract.Monad (liftContractM, liftedM, runContract, throwContractError)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Transaction (TransactionHash)
import Contract.Value (mkTokenName)
import Contract.Wallet (getWalletAddresses)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, fromAff)
import Ctl.Internal.Contract.Wallet (ownPubKeyHashes)
import Data.Array (head) as Array
import Effect.Aff.Compat (EffectFn1, EffectFn3, mkEffectFn1, mkEffectFn3)
import Lib.CardanoRacers.Common
  ( Nitro
  , Race
  , createRegistryParams
  , fromJsBigInt
  )
import Lib.CardanoRacers.Queries (Queries, mkQueries)
import Racers (Racers, runRacers)
import Record (merge)
import Type.Row (type (+))

type Client r =
  ( buyNitro :: EffectFn1 Nitro (Promise TransactionHash)
  , requestAsset :: EffectFn1 String (Promise TransactionHash)
  , registerInRace :: EffectFn1 Race (Promise TransactionHash)
  , joinRace :: EffectFn3 Race String String (Promise TransactionHash)
  | r
  )

mkClient
  :: ContractParams
  -> WalletSpec
  -> RacersParams
  -> Record (Client + Queries + ())
mkClient cp walletSpec rp =
  let
    queries = mkQueries cp walletSpec rp
    cfg = cp { walletSpec = Just walletSpec }

    runC :: Racers ~> Aff
    runC = runContract cfg <<< runRacers rp
  in
    { buyNitro: mkEffectFn1 $ fromAff <<< runC <<< buyNitro
    , requestAsset: mkEffectFn1 $ fromAff <<< runC <<< requestAsset
    , registerInRace: mkEffectFn1 $ fromAff <<< runC <<< registerInRace
    , joinRace: mkEffectFn3 $ \race car driver -> fromAff $ runC $ joinRace race
        car
        driver
    } `merge` queries

buyNitro :: Nitro -> Racers TransactionHash
buyNitro = buyNitroContract <<< fromJsBigInt

requestAsset :: String -> Racers TransactionHash
requestAsset rarityStr = do
  rarity <- lift $ case rarityStr of
    "common" -> pure Common
    "rare" -> pure Rare
    "epic" -> pure Epic
    x -> throwContractError ("Invalid rarity: " <> x)
  requestAssetByRarity rarity

registerInRace :: Race -> Racers TransactionHash
registerInRace race = do
  rgp <- createRegistryParams race
  firstPkh <- lift $ liftedM "Could not get first own public key hash"
    $ ownPubKeyHashes
    <#> Array.head

  registerPositionInRace rgp firstPkh

joinRace :: Race -> String -> String -> Racers TransactionHash
joinRace race carTokenStr driverTokenStr = do
  rgp <- createRegistryParams race
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
