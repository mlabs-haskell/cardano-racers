module CardanoRacers.Hydra.Monad
  ( AppLogger
  , AppM(AppM)
  , AppState
  , RaceData
  , RaceEntry
  , RaceResultSlots
  , appLogger
  , cleanupApp
  , findRaceEntryByDepositTxId
  , findRaceEntryByRaceCs
  , getAppLauncher
  , getAppRunner
  , initApp
  , initContractEnv
  , initRace
  , launchApp
  , liftContract
  , liftContractNullCosts
  , printRaceId
  , readHeadStatus
  , readHydraSnapshot
  , removeRaceEntry
  , runApp
  , setHeadStatus
  , setHydraSnapshot
  ) where

import Prelude

import Aeson (Finite)
import Cardano.AsCbor (encodeCbor)
import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Types
  ( NetworkId(MainnetId, TestnetId)
  , PlutusScript
  , ScriptHash
  , TransactionHash
  , UtxoMap
  )
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Hydra.Config (AppConfig, AppQueryBackend(Blockfrost, Kupmios))
import CardanoRacers.Hydra.Contracts.Collateral (getCollateralUtxo)
import CardanoRacers.Hydra.Lib.Contract (runContractNullCosts)
import CardanoRacers.Hydra.Types.Common (Utxo)
import CardanoRacers.Hydra.Types.RaceStatus (RaceStatus(Initializing))
import CardanoRacers.Race.Types (RaceParams)
import CardanoRaces.Hydra.Lib.Print (printHex)
import Contract.CborBytes (cborBytesToHex)
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
  , mkCtlBackendParams
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
import Data.Map (Map)
import Data.Map (delete, empty, insert, lookup, pop, union) as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (take, trim) as String
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff, runAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (new, read, tryRead) as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, error, throw)
import Effect.Exception (message) as Error
import Effect.Ref (Ref)
import Effect.Ref (new, read, write) as Ref
import HydraSdk.Lib (modify) as AVar
import HydraSdk.Types
  ( HydraHeadStatus(HeadStatus_Unknown)
  , HydraSnapshot
  , emptySnapshot
  , toUtxoMap
  )
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
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

-- TODO: some AVars here could probably just be Refs
type AppState =
  { config :: AppConfig
  , contractEnv :: ContractEnv
  , collateralUtxo :: Maybe Utxo
  , headStatus :: AVar HydraHeadStatus
  , snapshot :: AVar HydraSnapshot
  , races ::
      AVar
        { entries :: Map ScriptHash RaceEntry
        , deposits :: Map TransactionHash ScriptHash
        }
  }

type RaceEntry =
  { depositTxId :: TransactionHash
  , raceData :: RaceData
  , raceStatusRef :: Ref RaceStatus
  }

type RaceResultSlots = Map Plutus.Address (AVar (Maybe (Finite Number)))

type RaceData =
  { racersParams :: RacersParams
  , raceParams :: RaceParams
  , raceValidator :: PlutusScript
  }

printRaceId :: RaceData -> String
printRaceId rd = printHex (unwrap rd.raceParams).stateCurrencySymbol

initRace :: TransactionHash -> RaceData -> AppM Unit
initRace depositTxId raceData = do
  { races } <- ask
  void $ AVar.modify
    ( \{ entries, deposits } -> do
        raceStatusRef <- liftEffect $ Ref.new Initializing
        let raceCs = (unwrap raceData.raceParams).stateCurrencySymbol
        pure
          { entries: Map.insert raceCs { depositTxId, raceData, raceStatusRef } entries
          , deposits: Map.insert depositTxId raceCs deposits
          }
    )
    races

findRaceEntryByDepositTxId :: TransactionHash -> AppM (Maybe RaceEntry)
findRaceEntryByDepositTxId depositTxId = do
  { races } <- ask
  { entries, deposits } <- liftAff $ AVar.read races
  pure $ flip Map.lookup entries =<< Map.lookup depositTxId deposits

findRaceEntryByRaceCs :: ScriptHash -> AppM (Maybe RaceEntry)
findRaceEntryByRaceCs raceCs = do
  { races } <- ask
  { entries } <- liftAff $ AVar.read races
  pure $ Map.lookup raceCs entries

removeRaceEntry :: TransactionHash -> AppM Unit
removeRaceEntry depositTxId = do
  { races } <- ask
  void $ AVar.modify
    ( \{ entries, deposits } ->
        pure case Map.pop depositTxId deposits of
          Just (raceCs /\ depositsUpdated) ->
            { entries: Map.delete raceCs entries
            , deposits: depositsUpdated
            }
          Nothing ->
            { entries
            , deposits
            }
    )
    races

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
readHeadStatus = liftAff <<< AVar.read =<< asks _.headStatus

setHeadStatus :: HydraHeadStatus -> AppM Unit
setHeadStatus status =
  (void <<< AVar.modify (const (pure status)))
    =<< asks _.headStatus

readHydraSnapshot :: AppM HydraSnapshot
readHydraSnapshot = liftAff <<< AVar.read =<< asks _.snapshot

setHydraSnapshot :: HydraSnapshot -> AppM Unit
setHydraSnapshot snapshot = (void <<< AVar.modify (const (pure snapshot))) =<< asks _.snapshot

initApp :: AppConfig -> Aff AppState
initApp config@{ hydraNodeStartupParams, isHeadLeader } = do
  contractEnv <-
    initContractEnv config.queryBackend hydraNodeStartupParams.cardanoSigningKey
      config.logLevel
  collateralUtxo <-
    if isHeadLeader then Just <$> runContractInEnv contractEnv getCollateralUtxo
    else pure Nothing
  headStatus <- AVar.new HeadStatus_Unknown
  snapshot <- AVar.new emptySnapshot
  races <- AVar.new { entries: Map.empty, deposits: Map.empty }
  pure
    { config
    , contractEnv
    , collateralUtxo
    , headStatus
    , snapshot
    , races
    }

initContractEnv :: AppQueryBackend -> FilePath -> LogLevel -> Aff ContractEnv
initContractEnv queryBackend signingKey logLevel = do
  network /\ backendParams <- liftEffect $ mkBackendParams queryBackend
  let contractParams = mkContractParams backendParams network logLevel signingKey
  mkContractEnv contractParams

mkBackendParams :: AppQueryBackend -> Effect (NetworkId /\ ProviderBackendParams)
mkBackendParams queryBackend =
  case queryBackend of
    Blockfrost { apiKeyFile } -> do
      blockfrostApiKey <- String.trim <$> readTextFile UTF8 apiKeyFile
      let networkPrefix = String.take 7 blockfrostApiKey
      networkId /\ blockfrostConfig <-
        case networkPrefix of
          "mainnet" ->
            pure $ MainnetId /\ blockfrostPublicMainnetServerConfig
          "preprod" ->
            pure $ TestnetId /\ blockfrostPublicPreprodServerConfig
          "preview" ->
            pure $ TestnetId /\ blockfrostPublicPreviewServerConfig
          _ ->
            throw $ "mkBackendParams: unsupported network. Blockfrost API key prefix: "
              <> networkPrefix
      pure $ networkId /\ mkBlockfrostBackendParams
        { blockfrostConfig
        , blockfrostApiKey: Just blockfrostApiKey
        , confirmTxDelay: defaultConfirmTxDelay
        }
    Kupmios { network, kupoConfig, ogmiosConfig } ->
      pure $ network /\ mkCtlBackendParams
        { ogmiosConfig
        , kupoConfig
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
