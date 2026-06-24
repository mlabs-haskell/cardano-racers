module Lib.CardanoRacers.Client where

import Contract.Prelude

import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Data.Lite (toBytes)
import Cardano.Plutus.Types.Address as PlutusAddress
import Cardano.Types (ScriptHash)
import Cardano.Types.AssetName (mkAssetName)
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Cardano.Types.PublicKey (fromRawBytes) as PublicKey
import CardanoRacers.AssetRequest.Contract (requestAssetByRarity)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.GameAsset.Types (Rarity(Common, Rare, Epic))
import CardanoRacers.Nitro.Contract (buyNitroContract)
import CardanoRacers.RaceRegistry.Contract
  ( confirmAssetSelection
  , registerPositionInRace
  )
import CardanoRacers.RaceSlot.Contract (mkRaceSlotPolicy)
import CardanoRacers.Services.HydraDelegate
  ( SubmitPlayerInputError
  , submitPlayerInputRequest
  )
import CardanoRacers.Utils.Cose
  ( fromBytesCoseKey
  , getCoseKeyHeaderX
  , getCoseSign1Signature
  )
import CardanoRacers.Utils.Hash (blake2b256Hash)
import Contract.CborBytes (cborBytesToHex, hexToCborBytes)
import Contract.Config (ContractParams, WalletSpec)
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedM
  , runContract
  , throwContractError
  )
import Contract.Prim.ByteArray (byteArrayFromAscii, byteArrayToHex)
import Contract.Transaction (TransactionInput(TransactionInput))
import Contract.Wallet (getWalletAddress, getWalletAddresses, signData)
import Control.Monad.Error.Class (liftMaybe, throwError)
import Control.Monad.Except (except, runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, fromAff)
import Ctl.Internal.Contract.Wallet (ownPubKeyHashes)
import Ctl.Internal.FfiHelpers (maybeFfiHelper)
import Data.Array (head) as Array
import Data.ByteArray (ByteArray)
import Data.Int (fromString) as Int
import Data.String (Pattern(Pattern))
import Data.String as String
import Data.Traversable (traverse_)
import Data.UInt (fromInt) as UInt
import Effect.Aff.Compat
  ( EffectFn1
  , EffectFn2
  , EffectFn3
  , mkEffectFn1
  , mkEffectFn2
  , mkEffectFn3
  )
import Effect.Exception (error)
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
  , completeRace :: EffectFn3 Race (Array String) String (Promise Unit)
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
    , completeRace: mkEffectFn3 $ \race hydraGroupHttpServers csvInput ->
        fromAff $ runC $ completeRace race hydraGroupHttpServers
          csvInput
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

completeRace :: Race -> Array String -> String -> Racers Unit
completeRace race hydraGroupHttpServers csvInput = do
  raceCs <- do
    slotPolicy <- mkRaceSlotPolicy $ wrap race.raceId
    lift $ liftContractM "Could not get race slot token script hash"
      ( PlutusScript.hash <$>
          Array.head (unwrap slotPolicy).plutusMintingPolicies
      )
  res <- lift $ submitPlayerInputToDelegates raceCs hydraGroupHttpServers
    csvInput
  case res of
    Left domainErr ->
      throwError $ error $ "submitPlayerInput endpoint returned error: "
        <> show domainErr
    Right _ ->
      pure unit

submitPlayerInputToDelegates
  :: ScriptHash
  -> Array String
  -> String
  -> Contract (Either SubmitPlayerInputError Unit)
submitPlayerInputToDelegates raceCs hydraGroupHttpServers csv = do
  addr <- liftedM "Could not get wallet address" getWalletAddress
  { signature: coseSign1, key } <- signData addr $ wrap $ mkSigMessage csv
    raceCs
  sigBytes <- liftEffect $ getCoseSign1Signature $ unwrap coseSign1
  signature <- liftMaybe (error "Could not decode signature") $
    decodeCbor (wrap sigBytes)
  coseKey <- liftEffect $ fromBytesCoseKey key
  vk <-
    liftMaybe (error "Could not get verification key")
      (PublicKey.fromRawBytes =<< getCoseKeyHeaderX maybeFfiHelper coseKey)
  runExceptT $ traverse_
    ( \httpServer -> do
        res <- liftAff $ submitPlayerInputRequest httpServer
          { raceCs
          , csv
          , auth:
              { vk
              , addr
              , signature
              }
          }
        case res of
          Left httpError ->
            lift $ throwError $ error
              $ "submitPlayerInput request failed with error: "
              <> show httpError
              <> ", delegate server: "
              <> httpServer
          Right x ->
            except x
    )
    hydraGroupHttpServers

mkSigMessage :: String -> ScriptHash -> ByteArray
mkSigMessage userInput raceCs = unwrap (encodeCbor raceCs) <> blake2b256Hash
  userInput
