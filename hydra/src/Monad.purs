module CardanoRacers.Hydra.Monad
  ( AppLogger
  , AppM(AppM)
  , AppState
  , RaceData
  , appLogger
  , cleanupApp
  , getAppLauncher
  , getAppRunner
  , getHydraUtxos
  , initApp
  , initContractEnv
  , launchApp
  , liftContract
  , liftContractNullCosts
  , readHeadStatus
  , readHydraSnapshot
  , readRaceData
  , runApp
  , setHeadStatus
  , setHydraSnapshot
  , setRaceData
  ) where

import Prelude

import Cardano.Types (NetworkId(MainnetId, TestnetId), PlutusScript, UtxoMap)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Hydra.Config (AppConfig)
import CardanoRacers.Hydra.Contracts.Collateral (getCollateralUtxo)
import CardanoRacers.Hydra.Lib.AVar (readNow) as AVar
import CardanoRacers.Hydra.Lib.Contract (runContractNullCosts)
import CardanoRacers.Hydra.Types.Common (Utxo)
import CardanoRacers.Race.Types (RaceParams)
import Contract.Config
  ( ContractParams
  , PrivatePaymentKeySource(PrivatePaymentKeyFile)
  , ProviderBackendParams
  , WalletSpec(UseKeys)
  , blockfrostPublicMainnetServerConfig
  , blockfrostPublicPreprodServerConfig
  , blockfrostPublicPreviewServerConfig
  , defaultConfirmTxDelay
  , defaultTimeParams
  , disabledSynchronizationParams
  , emptyHooks
  , mkBlockfrostBackendParams
  )
import Contract.Monad (Contract, ContractEnv, mkContractEnv, runContractInEnv, stopContractEnv)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, liftMaybe, throwError)
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Logger.Trans (LoggerT(LoggerT), runLoggerT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, asks, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Either (either)
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (take, trim) as String
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff, runAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (new) as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, error)
import Effect.Exception (message) as Error
import HydraSdk.Lib (modify) as AVar
import HydraSdk.Types
  ( HydraHeadStatus(HeadStatus_Unknown)
  , HydraSnapshot
  , QueryLayer(Blockfrost, CardanoNode)
  , emptySnapshot
  , toUtxoMap
  )
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)

newtype AppM (a :: Type) = AppM (LoggerT (ReaderT AppState Aff) a)

derive instance Newtype (AppM a) _
derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadAsk AppState AppM
derive newtype instance MonadReader AppState AppM
derive newtype instance MonadThrow Error AppM
derive newtype instance MonadError Error AppM
derive newtype instance MonadLogger AppM
derive newtype instance MonadRec AppM

type AppLogger = Message -> ReaderT AppState Aff Unit

type AppState =
  { config :: AppConfig
  , contractEnv :: ContractEnv
  , collateralUtxo :: Utxo
  , headStatus :: AVar HydraHeadStatus
  , race :: AVar (Maybe RaceData)
  , snapshot :: AVar HydraSnapshot
  }

type RaceData =
  { racersParams :: RacersParams
  , raceParams :: RaceParams
  , raceValidator :: PlutusScript
  }

runApp :: forall (a :: Type). AppState -> AppLogger -> AppM a -> Aff a
runApp state logger =
  flip runReaderT state
    <<< flip runLoggerT logger
    <<< unwrap

launchApp :: forall (a :: Type). AppState -> AppLogger -> AppM a -> Effect Unit
launchApp state logger =
  void
    <<< launchAff
    <<< runApp state logger

getAppRunner :: AppM (forall (a :: Type). AppM a -> Aff a)
getAppRunner =
  wrap $
    LoggerT \logger ->
      ask <#> \state ->
        runApp state logger

getAppLauncher :: AppM (forall (a :: Type). AppM a -> Effect Unit)
getAppLauncher =
  wrap $
    LoggerT \logger ->
      ask <#> \state ->
        launchApp state logger

cleanupApp :: AppState -> Effect Unit
cleanupApp state = do
  log "Finalizing CTL Contract environment."
  runAff_
    ( either
        (log <<< append "cleanupApp failed with error: " <<< Error.message)
        (const (log "Successfully cleaned up CTL Contract environment."))
    )
    (stopContractEnv state.contractEnv)

liftContract :: forall (a :: Type). Contract a -> AppM a
liftContract contract = do
  { contractEnv } <- ask
  liftAff $ runContractInEnv contractEnv contract

liftContractNullCosts :: forall (a :: Type). Contract a -> AppM a
liftContractNullCosts contract = do
  { contractEnv } <- ask
  liftAff $ runContractNullCosts contractEnv contract

appLogger :: AppLogger
appLogger message = do
  { config: { logLevel } } <- ask
  when (message.level >= logLevel) do
    messageFormatted <- prettyFormatter message
    liftEffect $ log messageFormatted

readHeadStatus :: AppM HydraHeadStatus
readHeadStatus =
  AVar.readNow (error "readHeadStatus: empty avar")
    =<< asks _.headStatus

setHeadStatus :: HydraHeadStatus -> AppM Unit
setHeadStatus status =
  (void <<< AVar.modify (const (pure status)))
    =<< asks _.headStatus

readRaceData :: AppM RaceData
readRaceData =
  liftMaybe (error "readRaceData: Nothing found")
    =<< AVar.readNow (error "readRaceData: empty avar")
    =<< asks _.race

setRaceData :: RaceData -> AppM Unit
setRaceData rd =
  (void <<< AVar.modify (const (pure $ Just rd)))
    =<< asks _.race

readHydraSnapshot :: AppM HydraSnapshot
readHydraSnapshot =
  AVar.readNow (error "readHydraSnapshot: empty avar")
    =<< asks _.snapshot

getHydraUtxos :: AppM UtxoMap
getHydraUtxos = do
  snapshot <- readHydraSnapshot
  pure $ toUtxoMap (unwrap snapshot).utxo

setHydraSnapshot :: HydraSnapshot -> AppM Unit
setHydraSnapshot snapshot = (void <<< AVar.modify (const (pure snapshot))) =<< asks _.snapshot

initApp :: AppConfig -> Aff AppState
initApp config@{ hydraNodeStartupParams } = do
  blockfrostApiKeyFile <-
    case hydraNodeStartupParams.queryLayer of
      Blockfrost { apiKeyFile } ->
        pure apiKeyFile
      CardanoNode _ ->
        throwError $ error $
          "initApp: Could not get Blockfrost API key. Unexpected query layer configuration: "
            <> show hydraNodeStartupParams.queryLayer
  contractEnv <-
    initContractEnv blockfrostApiKeyFile hydraNodeStartupParams.cardanoSigningKey
      config.logLevel
  collateralUtxo <- runContractInEnv contractEnv getCollateralUtxo
  headStatus <- AVar.new HeadStatus_Unknown
  race <- AVar.new Nothing
  snapshot <- AVar.new emptySnapshot
  pure
    { config
    , contractEnv
    , collateralUtxo
    , headStatus
    , race
    , snapshot
    }

initContractEnv :: FilePath -> FilePath -> LogLevel -> Aff ContractEnv
initContractEnv blockfrostApiKeyFile signingKey logLevel = do
  blockfrostApiKey <- String.trim <$> readTextFile UTF8 blockfrostApiKeyFile
  network /\ backendParams <-
    liftMaybe
      (error "initContractEnv: Could not build ProviderBackendParams. Unknown network prefix.")
      (mkBackendParams blockfrostApiKey)
  let contractParams = mkContractParams backendParams network logLevel signingKey
  mkContractEnv contractParams

mkBackendParams :: String -> Maybe (NetworkId /\ ProviderBackendParams)
mkBackendParams blockfrostApiKey = do
  let networkPrefix = String.take 7 blockfrostApiKey
  networkId /\ blockfrostConfig <-
    case networkPrefix of
      "mainnet" ->
        Just $ MainnetId /\ blockfrostPublicMainnetServerConfig
      "preprod" ->
        Just $ TestnetId /\ blockfrostPublicPreprodServerConfig
      "preview" ->
        Just $ TestnetId /\ blockfrostPublicPreviewServerConfig
      _ ->
        Nothing
  pure $ networkId /\ mkBlockfrostBackendParams
    { blockfrostConfig
    , blockfrostApiKey: Just blockfrostApiKey
    , confirmTxDelay: defaultConfirmTxDelay
    }

mkContractParams
  :: ProviderBackendParams
  -> NetworkId
  -> LogLevel
  -> FilePath
  -> ContractParams
mkContractParams backendParams networkId logLevel cardanoSk =
  { backendParams
  , networkId
  , logLevel
  , walletSpec: Just $ UseKeys (PrivatePaymentKeyFile cardanoSk) Nothing Nothing
  , customLogger: Nothing
  , suppressLogs: false
  , hooks: emptyHooks
  , timeParams: defaultTimeParams
  , synchronizationParams: disabledSynchronizationParams
  }
