module CardanoRacers.Hydra.Demo.StartRace
  ( main
  ) where

import Prelude

import Aeson (stringifyAeson)
import Affjax (defaultRequest) as Affjax
import Affjax.RequestBody (RequestBody(String)) as Affjax.RequestBody
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Cardano.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.Plutus.Types.Address (fromCardano) as Plutus.Address
import Cardano.Provider (request)
import Cardano.ToData (toData)
import Cardano.Types (Ed25519KeyHash, ScriptHash, TransactionHash, Value)
import Cardano.Types.BigNum (fromInt) as BigNum
import Cardano.Types.PrivateKey (toPublicKey) as PrivateKey
import Cardano.Types.Value (lovelaceValueOf)
import Cardano.Wallet.Key (getPrivatePaymentKey)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Hydra.Handlers.HostRace
  ( HostRaceRequest
  , HostRaceResponse
  , hostRaceRequestCodec
  , hostRaceResponseCodec
  )
import CardanoRacers.Hydra.Handlers.SubmitPlayerInput (mkSigMessage, playerInputCodec)
import CardanoRacers.Hydra.Lib.Cose (getCoseSign1Signature)
import CardanoRacers.Hydra.Lib.Retry (retryOnAnyError, retryOnFalse)
import CardanoRacers.Hydra.Monad (initContractEnv)
import CardanoRacers.Hydra.Services.Utils (handleResponse, postRequest)
import CardanoRacers.Hydra.Types.ServerResponse
  ( ServerResponse(ServerResponseError, ServerResponseSuccess)
  )
import CardanoRacers.HydraGroup.Contract (findHydraGroupById)
import CardanoRacers.HydraGroup.Contract (registerHydraGroup) as HydraGroup
import CardanoRacers.HydraGroup.Types (HydraGroupInfo)
import CardanoRacers.Nitro.Helpers (createRacersParams)
import CardanoRacers.Race.Contract (distributeRewards, startRace)
import CardanoRacers.Race.Types (RaceParams)
import CardanoRacers.RaceSlot.Types (RaceHash)
import Contract.CborBytes (cborBytesToHex, hexToCborBytes)
import Contract.Log (logError')
import Contract.Monad (Contract, liftContractM, liftedM, runContractInEnv)
import Contract.Wallet
  ( Wallet(KeyWallet)
  , getWalletAddress
  , getWalletUtxos
  , ownPaymentPubKeyHash
  , signData
  )
import Control.Monad.Error.Class (liftMaybe, throwError)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Reader (ask)
import Ctl.Internal.Contract.AwaitTxConfirmed (awaitTxConfirmed)
import Ctl.Internal.Helpers ((<</>>))
import Data.Array (head, singleton) as Array
import Data.ByteArray (byteArrayFromAscii)
import Data.Codec.Argonaut (JsonCodec, encode, null, object, printJsonDecodeError, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Either (Either(Left, Right))
import Data.HTTP.Method (Method(POST))
import Data.Log.Level (LogLevel(Trace))
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Minutes(Minutes), Seconds(Seconds), convertDuration, fromDuration)
import Data.Traversable (traverse_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, throw)
import HydraSdk.Lib (caDecodeFile)
import HydraSdk.Types (HttpError)
import JS.BigInt (fromInt) as BigInt
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)
import Node.Process (argv)
import Partial.Unsafe (unsafePartial)
import Racers (runRacers)

type Config =
  { blockfrostApiKeyFile :: FilePath
  , signingKeyFile :: FilePath
  }

configCodec :: CA.JsonCodec Config
configCodec =
  CA.object "Config" $ CAR.record
    { blockfrostApiKeyFile: CA.string
    , signingKeyFile: CA.string
    }

main :: Effect Unit
main = do
  args <- argv
  case args of
    [ _, _, configPath ] ->
      caDecodeFile configCodec configPath >>=
        case _ of
          Right cfg ->
            launchAff_ do
              contractEnv <- initContractEnv cfg.blockfrostApiKeyFile cfg.signingKeyFile Trace
              runContractInEnv contractEnv do
                -- Register Hydra group
                groupId <- registerHydraGroup
                logAndDelay $ "Registered new Hydra group with ID: " <> toHex groupId

                -- Create RacersParams
                utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
                (nonceOref /\ _) <-
                  liftContractM "Could not get first utxo" $ Array.head $
                    Map.toUnfoldable utxos
                racersParams <- createRacersParams nonceOref

                -- Start race
                addr <- liftedM "Could not get wallet address" getWalletAddress
                plutusAddr <- liftMaybe (error "Could not convert wallet address") $
                  Plutus.Address.fromCardano addr
                let
                  participants = Array.singleton plutusAddr -- one participant
                  rewardWeights = [ 50_000, 30_000, 20_000 ] <#> wrap <<< { numerator: _ } <<<
                    BigInt.fromInt
                { txHash: startRaceTxHash, raceParams } <-
                  runRacers racersParams $
                    startRace Nothing raceHashFixture totalRewardValueFixture rewardWeights
                      participants
                      delegatesFixture
                logAndDelay $ "startRace success: "
                  <> toHex startRaceTxHash

                -- Discover Hydra group
                _groupEntry@{ groupInfo } <- liftedM "Could not find Hydra group by ID" $
                  findHydraGroupById groupId
                logAndDelay $ "Found valid Hydra group with ID: "
                  <> toHex groupId
                  <> ", group info: "
                  <> show groupInfo

                -- Host L2 race
                resp <- hostRace groupInfo startRaceTxHash racersParams raceParams
                liftEffect case resp of
                  Left httpError ->
                    throw $ "host request failed: " <> show httpError
                  Right (ServerResponseError hostRaceError) ->
                    throw $ "could not host race: " <> show hostRaceError
                  Right (ServerResponseSuccess { commitTxHash }) ->
                    log $ "hostRace success: " <> toHex commitTxHash

                -- Submit player input
                do
                  submitted <-
                    retryOnFalse { timeout: Minutes 10.0, delay: Seconds 30.0 }
                      ( submitPlayerInput groupInfo raceParams "simulator/input.csv" >>=
                          case _ of
                            Left httpError -> do
                              logError' $ "submitPlayerInput request failed with error: "
                                <> show httpError
                              pure false
                            Right _ ->
                              pure true
                      )
                  unless submitted $ throwError $ error
                    "Failed to submit player input after multiple attempts"

                -- Distribute rewards
                do
                  txHash <-
                    retryOnAnyError
                      "distributeRewards"
                      { timeout: Minutes 15.0, delay: Seconds 30.0 }
                      (runRacers racersParams $ distributeRewards raceParams)
                  logAndDelay $ "distributeRewards success: "
                    <> toHex txHash

                -- Disband Hydra group
                -- disbandTxHash <- disbandHydraGroup groupEntry.oref
                -- logAndDelay $ "Successfully disbanded Hydra group with ID: "
                -- <> toHex groupId
                -- <> ", TX hash: "
                -- <> toHex disbandTxHash
                pure unit
          Left decodeErr ->
            throw $ "could not decode config: " <>
              CA.printJsonDecodeError decodeErr
    _ ->
      throw "invalid command-line arguments"

registerHydraGroup :: Contract ScriptHash
registerHydraGroup = do
  ownPkh <- unwrap <$> liftedM "Could not get own pkh" ownPaymentPubKeyHash
  let
    masterKeys = Array.singleton ownPkh
    httpServers = [ "http://127.0.0.1:7010", "http://127.0.0.1:7012" ]
    metadata = "Demo group"
  { txHash, groupId } <- HydraGroup.registerHydraGroup masterKeys httpServers metadata
  awaitTxConfirmed txHash
  pure groupId

hostRace
  :: HydraGroupInfo
  -> TransactionHash
  -> RacersParams
  -> RaceParams
  -> Contract (Either HttpError HostRaceResponse)
hostRace groupInfo startRaceTxHash racersParams raceParams = do
  httpServer <- liftMaybe (error "Could not get httpServer") $ Array.head
    (unwrap groupInfo).hydraGroupHttpServers
  liftAff $ handleResponse hostRaceResponseCodec <$>
    request
      ( Affjax.defaultRequest
          { method = Left POST
          , url = httpServer <</>> "hostRace"
          , content =
              Just $ Affjax.RequestBody.String $ stringifyAeson $ CA.encode
                hostRaceRequestCodec
                reqBody
          , responseFormat = Affjax.ResponseFormat.string
          , timeout = Just $ fromDuration $ Seconds 30.0
          }
      )
  where
  reqBody :: HostRaceRequest
  reqBody =
    { raceOref: wrap { transactionId: startRaceTxHash, index: zero }
    , racersParams
    , raceParams: encodeCbor $ toData raceParams
    }

submitPlayerInput
  :: HydraGroupInfo
  -> RaceParams
  -> FilePath
  -> Contract (Either HttpError Unit)
submitPlayerInput groupInfo raceParams inputCsvPath = do
  addr <- liftedM "Could not get wallet address" getWalletAddress
  csv <- liftAff $ readTextFile UTF8 inputCsvPath
  { signature: coseSign1 } <- signData addr $ wrap $ mkSigMessage csv
    (unwrap raceParams).stateCurrencySymbol
  sigBytes <- liftEffect $ getCoseSign1Signature $ unwrap coseSign1
  signature <- liftMaybe (error "Could not decode signature") $
    decodeCbor (wrap sigBytes)
  let httpServers = (unwrap groupInfo).hydraGroupHttpServers
  { wallet } <- ask
  -- FIXME: use key from DataSignature instead 
  -- https://github.com/mlabs-haskell/hydra-auction-offchain/blob/bead07c8bd06eaa8198de6582585bd111dd9d1e6/src/Wallet.purs#L105
  vk <-
    case wallet of
      Just (KeyWallet kw) -> do
        sk <- liftAff $ unwrap <$> getPrivatePaymentKey kw
        pure $ PrivateKey.toPublicKey sk
      _ -> throwError $ error "Could not get verification key"
  runExceptT $
    traverse_
      ( \httpServer ->
          ExceptT $ liftAff $ handleResponse CA.null <$>
            postRequest
              { url: httpServer <</>> "playerInput"
              , content: Just $ CA.encode playerInputCodec
                  { csv
                  , auth:
                      { vk
                      , addr
                      , signature
                      }
                  }
              , headers: mempty
              }
      )
      httpServers

raceHashFixture :: RaceHash
raceHashFixture = unsafePartial fromJust $ byteArrayFromAscii "TestRaceHash"

totalRewardValueFixture :: Value
totalRewardValueFixture = lovelaceValueOf $ BigNum.fromInt 7_000_000

delegatesFixture :: Array Ed25519KeyHash
delegatesFixture =
  keyHashFromHex <$>
    [ "0e0607203c2ab6f2f729e2317502277191c681283637d5ee6b2a9933"
    , "35c92e61b4f915ce7615ea8b8ece661843e6bc5f591fe99036d388a9"
    ]

keyHashFromHex :: String -> Ed25519KeyHash
keyHashFromHex str = unsafePartial fromJust $ decodeCbor =<< hexToCborBytes str

logAndDelay :: forall (m :: Type -> Type). MonadAff m => String -> m Unit
logAndDelay str = do
  liftEffect $ log str
  liftAff $ delay $ convertDuration $ Seconds 2.0

toHex :: forall (a :: Type). AsCbor a => a -> String
toHex = cborBytesToHex <<< encodeCbor
