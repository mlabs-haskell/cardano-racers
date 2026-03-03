module Lib.CardanoRacers.Client where

import Contract.Prelude

import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Data.Lite (toBytes)
import Cardano.Plutus.Types.Address as PlutusAddress
import Cardano.Types.AssetName (mkAssetName)
import CardanoRacers.AssetRequest.Contract (requestAssetByRarity)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.GameAsset.Types (Rarity(Common, Rare, Epic))
import CardanoRacers.Nitro.Contract (buyNitroContract)
import CardanoRacers.RaceRegistry.Contract
  ( confirmAssetSelection
  , registerPositionInRace
  )
import Contract.CborBytes (cborBytesToHex, hexToCborBytes)
import Contract.Config (ContractParams, WalletSpec)
import Contract.Monad (liftContractM, liftedM, runContract, throwContractError)
import Contract.Prim.ByteArray (byteArrayFromAscii, byteArrayToHex)
import Contract.Transaction (TransactionInput(..))
import Contract.Wallet (getWalletAddresses)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, fromAff)
import Ctl.Internal.Contract.Wallet (ownPubKeyHashes)
import Data.Array (head) as Array
import Data.Int (fromString) as Int
import Data.String (Pattern(..))
import Data.String as String
import Data.UInt (fromInt) as UInt
import Effect.Aff.Compat
  ( EffectFn1
  , EffectFn2
  , EffectFn3
  , mkEffectFn1
  , mkEffectFn2
  , mkEffectFn3
  )
import Lib.CardanoRacers.Common
  ( Nitro
  , Race
  , TransactionHashFFI
  , createRegistryParams
  , fromJsBigInt
  )
import Lib.CardanoRacers.Queries (Queries, mkQueries)
import Racers (Racers, runRacers)
import Record (merge)
import Type.Row (type (+))

type Client r =
  ( buyNitro :: EffectFn1 Nitro (Promise TransactionHashFFI)
  , requestAsset :: EffectFn1 String (Promise TransactionHashFFI)
  , registerInRace :: EffectFn2 Race String (Promise TransactionHashFFI)
  , joinRace :: EffectFn3 Race String String (Promise TransactionHashFFI)
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
    , registerInRace: mkEffectFn2 $ \race txInJson -> fromAff $ runC $
        registerInRace race txInJson
    , joinRace: mkEffectFn3 $ \race car driver -> fromAff $ runC $ joinRace race
        car
        driver
    } `merge` queries

buyNitro :: Nitro -> Racers TransactionHashFFI
buyNitro = map (cborBytesToHex <<< encodeCbor) <<< buyNitroContract <<<
  fromJsBigInt

requestAsset :: String -> Racers TransactionHashFFI
requestAsset rarityStr = do
  rarity <- lift $ case rarityStr of
    "common" -> pure Common
    "rare" -> pure Rare
    "epic" -> pure Epic
    x -> throwContractError ("Invalid rarity: " <> x)
  txh <- requestAssetByRarity rarity
  pure $ byteArrayToHex $ toBytes (unwrap txh)

registerInRace :: Race -> String -> Racers TransactionHashFFI
registerInRace race txInStr = do
  rgp <- createRegistryParams race

  slotTxIn <- lift $ case String.split (Pattern "#") txInStr of
    [ txHashStr, txIndexStr ] -> do
      txHashBytes <- liftContractM "Could not convert txHash hex to Cbor bytes "
        $ hexToCborBytes txHashStr

      txHash <- liftContractM "Could not decode txHash Cbor" $ decodeCbor
        txHashBytes

      txIndex <-
        maybe (throwContractError "Could not parse transaction index") pure
          $ Int.fromString txIndexStr
          <#> UInt.fromInt
      pure $ TransactionInput { index: txIndex, transactionId: txHash }
    _ -> throwContractError "Invalid transaction input"

  firstPkh <- lift $ liftedM "Could not get first own public key hash"
    $ ownPubKeyHashes
    <#> Array.head

  txh <- registerPositionInRace rgp (wrap firstPkh) slotTxIn
  pure $ cborBytesToHex $ encodeCbor txh

joinRace :: Race -> String -> String -> Racers TransactionHashFFI
joinRace race carTokenStr driverTokenStr = do
  rgp <- createRegistryParams race
  carToken <- lift
    $ liftContractM ("Could not create token name from" <> carTokenStr)
    $ (mkAssetName <=< byteArrayFromAscii) carTokenStr
  driverToken <- lift
    $ liftContractM ("Could not create token name from" <> driverTokenStr)
    $ (mkAssetName <=< byteArrayFromAscii) driverTokenStr

  firstPkh <- lift $ liftedM "Could not get first own public key hash"
    $ ownPubKeyHashes
    <#> Array.head

  firstAddr <- lift $ liftedM "Could not get first own address"
    $ getWalletAddresses
    <#> Array.head

  firstAddrPlutus <- lift
    $ liftContractM "Could not convert Plutus address to Cardano"
    $ PlutusAddress.fromCardano firstAddr

  let
    participant = wrap
      { car: carToken
      , driver: driverToken
      , payoutAddress: firstAddrPlutus
      }

  txh <- confirmAssetSelection rgp (wrap firstPkh) participant
  pure $ cborBytesToHex $ encodeCbor txh
