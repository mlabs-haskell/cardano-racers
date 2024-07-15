module Lib.CardanoRacers.Common where

import Contract.Prelude

import Aeson (decodeJsonString)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types (GameAssetType(DriverType, CarType))
import CardanoRacers.Nitro.Contract (mkNitroPolicy)
import CardanoRacers.RaceRegistry.Types (RegistryParams)
import CardanoRacers.RaceSlot.Contract (mkRaceSlotPolicy)
import CardanoRacers.RaceSlot.Types (slotTokenName)
import CardanoRacers.RacersState.Contract (modifyRacersStateContract)
import Contract.Config
  ( ContractParams
  , NetworkId(TestnetId, MainnetId)
  , PrivatePaymentKeySource(PrivatePaymentKeyValue)
  , PrivateStakeKeySource(PrivateStakeKeyValue)
  , QueryBackendParams
  , ServerConfig
  , StakeKeyPresence(WithStakeKey, WithoutStakeKey)
  , WalletSpec
      ( UseKeys
      , ConnectToNami
      , ConnectToGero
      , ConnectToFlint
      , ConnectToEternl
      , ConnectToLode
      , ConnectToLace
      , ConnectToNuFi
      , ConnectToVespr
      )
  , mkBlockfrostBackendParams
  , mkCtlBackendParams
  , privateKeyFromBytes
  , testnetConfig
  )
import Contract.Monad (liftedM)
import Contract.Prim.ByteArray
  ( ByteArray
  , RawBytes(RawBytes)
  , byteArrayToIntArray
  , hexToByteArray
  , byteArrayToHex
  )
import Contract.Scripts (mintingPolicyHash)
import Contract.Value (TokenName, getTokenName, scriptCurrencySymbol)
import Contract.Wallet
  ( WalletExtension
      ( NamiWallet
      , EternlWallet
      , NuFiWallet
      , LodeWallet
      , GeroWallet
      , FlintWallet
      , LaceWallet
      , VesprWallet
      )
  )
import Contract.Wallet.Key
  ( keyWalletPrivatePaymentKey
  , keyWalletPrivateStakeKey
  , mkKeyWalletFromMnemonic
  )
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Except.Trans (ExceptT, mapExceptT, runExceptT)
import Cardano.Types (PrivateKey)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.Char (fromCharCode)
import Data.List.NonEmpty (singleton) as NonEmpty
import Data.Profunctor.Choice (left)
import Data.String (Pattern(Pattern), stripPrefix)
import Data.String.CodeUnits (fromCharArray)
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt (fromInt) as UInt
import Effect.Aff.Compat (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2)
import Effect.Exception (error)
import Effect.Uncurried (EffectFn4, mkEffectFn4)
import Foreign
  ( Foreign
  , ForeignError(ForeignError)
  , MultipleErrors
  , readBoolean
  , readInt
  , readNumber
  , readString
  , renderForeignError
  )
import Foreign.Index (readProp)
import Foreign.Object (Object)
import Foreign.Object (fromFoldable) as Object
import Partial.Unsafe (unsafePartial)
import Racers (Racers, withContract)

foreign import fromJsBigInt :: JSBigInt -> BigInt
foreign import toJsBigInt :: BigInt -> JSBigInt

foreign import data JSBigInt :: Type

type Lovelace = JSBigInt
type Nitro = JSBigInt

type AssetPricesFFI =
  { common :: Lovelace
  , rare :: Lovelace
  , epic :: Lovelace
  }

type CredentialProvider = WalletSpec

type TransactionHashFFI = String

type Race = { raceId :: Uint8Array, nitroFee :: Nitro }

contractParams
  :: { fromCtlBackend :: EffectFn2 Foreign Foreign ContractParams
     , fromBlockfrostBackend :: EffectFn2 Foreign Foreign ContractParams
     }
contractParams =
  { fromCtlBackend: mkEffectFn2 $ \f opts -> mkCtlBackend f >>= mkContractParams
      opts
  , fromBlockfrostBackend: mkEffectFn2 $ \f opts -> mkBlockfrostBackend f >>=
      mkContractParams opts
  }
  where
  mkContractParams :: Foreign -> QueryBackendParams -> Effect ContractParams
  mkContractParams opts backend = do
    mNeworkId <- liftExcept $ readNetworkId opts
    mLogLevel <- liftExcept $ readLogLevel opts
    pure $ testnetConfig
      { backendParams = backend
      , networkId = fromMaybe testnetConfig.networkId mNeworkId
      , logLevel = fromMaybe Error mLogLevel
      }

  -- Returns nothing if networkId can not be parsed as a string, otherwise
  -- returns the parsed NetworkId or an error if the string is not a valid
  readNetworkId :: Foreign -> ExceptT MultipleErrors Effect (Maybe NetworkId)
  readNetworkId f = flip mapExceptT (readProp "networkId" f >>= readString)
    $ map
    $
      either (const $ pure Nothing)
        ( case _ of
            "mainnet" -> pure $ Just MainnetId
            "testnet" -> pure $ Just TestnetId
            _ -> Left $ NonEmpty.singleton $ ForeignError
              "Invalid 'networkId'. Expected 'testnet' or 'mainnet'"
        )

  -- Returns nothing if logLevel can not be parsed as a string, otherwise
  -- returns the parsed LogLevel or an error if the string is not a valid
  readLogLevel :: Foreign -> ExceptT MultipleErrors Effect (Maybe LogLevel)
  readLogLevel f = flip mapExceptT (readProp "logLevel" f >>= readString) $ map
    $
      either (const $ Right Nothing)
        ( case _ of
            "trace" -> Right $ Just Trace
            "debug" -> Right $ Just Debug
            "info" -> Right $ Just Info
            "warn" -> Right $ Just Warn
            "error" -> Right $ Just Error
            _ -> Left $ NonEmpty.singleton $ ForeignError
              "Invalid 'logLevel'. Expected 'traec', 'debug', 'info', 'warn' or 'error'"
        )

  mkBlockfrostBackend :: Foreign -> Effect QueryBackendParams
  mkBlockfrostBackend f = do
    blockfrostConfig <- liftExcept (readProp "blockfrostConfig" f) >>=
      parseServerConfig
    mBlockfrostApiKey <-
      runExceptT (readProp "blockfrostApiKey" f >>= readString) <#> hush
    (mConfirmTxDelay :: Maybe Seconds) <-
      runExceptT (readProp "confirmTxDelay" f >>= readNumber <#> Seconds) <#>
        hush
    pure $ mkBlockfrostBackendParams
      { blockfrostConfig
      , blockfrostApiKey: mBlockfrostApiKey
      , confirmTxDelay: mConfirmTxDelay
      }

  mkCtlBackend :: Foreign -> Effect QueryBackendParams
  mkCtlBackend f = do
    ogmiosConfig <- liftExcept (readProp "ogmiosConfig" f) >>= parseServerConfig
    kupoConfig <- liftExcept (readProp "kupoConfig" f) >>= parseServerConfig
    pure $ mkCtlBackendParams { ogmiosConfig, kupoConfig }

  liftExcept :: forall a. ExceptT MultipleErrors Effect a -> Effect a
  liftExcept =
    flip bind (liftEither <<< left (error <<< show <<< map renderForeignError))
      <<< runExceptT

  parseServerConfig :: Foreign -> Effect ServerConfig
  parseServerConfig f = do
    (port /\ host /\ secure) <- liftEither <<< left (error <<< show) =<<
      runExceptT do
        port <- readProp "port" f >>= readInt <#> UInt.fromInt
        host <- readProp "host" f >>= readString
        secure <- readProp "secure" f >>= readBoolean
        pure $ port /\ host /\ secure
    mPath <- runExceptT (readProp "path" f >>= readString) <#> hush
    pure
      { port
      , host
      , secure
      , path: mPath
      }

walletSpec
  :: { walletFromMnemonic :: EffectFn4 String Int Int Boolean CredentialProvider
     , walletFromPrivateKey :: EffectFn1 String CredentialProvider
     , walletFromPrivateKeyAndStakeKey ::
         EffectFn2 String String CredentialProvider
     , browserWallet :: Object (EffectFn1 Unit CredentialProvider)
     }
walletSpec =
  { walletFromMnemonic
  , walletFromPrivateKey
  , walletFromPrivateKeyAndStakeKey
  , browserWallet
  }
  where
  walletFromMnemonic = mkEffectFn4 $
    \mnemonic accountIndex addressIndex hasStake -> do
      kw <- liftEither $ left error $ mkKeyWalletFromMnemonic mnemonic
        { accountIndex: UInt.fromInt accountIndex
        , addressIndex: UInt.fromInt addressIndex
        }
        (if hasStake then WithStakeKey else WithoutStakeKey)
      pure $ UseKeys (PrivatePaymentKeyValue $ keyWalletPrivatePaymentKey kw)
        (PrivateStakeKeyValue <$> keyWalletPrivateStakeKey kw)

  walletFromPrivateKey = mkEffectFn1 $ \privateKeyStr -> do
    privateKey <- liftMaybe (error "Could not deserialise private key") $
      mkPrivateKey
        privateKeyStr
    pure $ UseKeys (PrivatePaymentKeyValue $ wrap privateKey) Nothing

  walletFromPrivateKeyAndStakeKey = mkEffectFn2 $ \privateKeyStr stakeKeyStr ->
    do
      privateKey <- liftMaybe (error "Could not deserialise private key") $
        mkPrivateKey
          privateKeyStr
      stakeKey <- liftMaybe (error "Could not deserialise stake key") $
        mkPrivateKey
          stakeKeyStr
      pure $ UseKeys (PrivatePaymentKeyValue $ wrap privateKey)
        (Just $ PrivateStakeKeyValue $ wrap stakeKey)

  browserWallet = Object.fromFoldable
    $ map
        ( \(name /\ spec) -> ("connectTo" <> name) /\ mkEffectFn1
            (const $ pure spec)
        )
    $
      [ "Nami" /\ ConnectToNami
      , "GeroWallet" /\ ConnectToGero
      , "Flint" /\ ConnectToFlint
      , "Eternl" /\ ConnectToEternl
      , "LodeWallet" /\ ConnectToLode
      , "Lace" /\ ConnectToLace
      , "Vespr" /\ ConnectToVespr
      , "NuFi" /\ ConnectToNuFi
      ]

mkPrivateKey :: String -> Maybe PrivateKey
mkPrivateKey str =
  mkPrivateKey' str <|> (stripPrefix (Pattern "5820") str >>= mkPrivateKey)
  where
  mkPrivateKey' :: String -> Maybe PrivateKey
  mkPrivateKey' str' = hexToByteArray str' >>= RawBytes >>> privateKeyFromBytes

mkRacersParams :: EffectFn1 String RacersParams
mkRacersParams = mkEffectFn1 $ \rpStr -> liftEither $ lmap (error <<< show) $
  decodeJsonString rpStr

walletExtensionFromString :: String -> Maybe WalletExtension
walletExtensionFromString name = case name of
  "nami" -> Just NamiWallet
  "gerowallet" -> Just GeroWallet
  "flint" -> Just FlintWallet
  "eternl" -> Just EternlWallet
  "LodeWallet" -> Just LodeWallet
  "nufi" -> Just NuFiWallet
  "lace" -> Just LaceWallet
  "vespr" -> Just VesprWallet
  _ -> Nothing

assetTypeFromString :: String -> Maybe GameAssetType
assetTypeFromString "driver" = pure DriverType
assetTypeFromString "car" = pure CarType
assetTypeFromString _ = Nothing

assetTypeToString :: GameAssetType -> String
assetTypeToString DriverType = "driver"
assetTypeToString CarType = "car"

tokenNameToString :: TokenName -> String
tokenNameToString tk =
  if null intArray then "Lovelace" else toAscii $ getTokenName tk
  where
  intArray = byteArrayToIntArray $ getTokenName tk

  toAscii :: ByteArray -> String
  toAscii ba = fromCharArray
    $ map (\x -> unsafePartial $ fromJust $ fromCharCode x)
    $ byteArrayToIntArray ba

createRegistryParams :: Race -> Racers RegistryParams
createRegistryParams race = do
  nitroPolicyHash <- mintingPolicyHash <$> mkNitroPolicy
  driverAssetPolicyHash <- mintingPolicyHash <$> mkGameAssetPolicy DriverType
  carAssetPolicyHash <- mintingPolicyHash <$> mkGameAssetPolicy CarType
  slotSymbol <-
    withContract (liftedM "could not get currency symbol from policy")
      $ scriptCurrencySymbol
      <$> mkRaceSlotPolicy (wrap race.raceId)
  pure $ wrap
    { slotAssetClass: slotSymbol /\ slotTokenName
    , nitroPolicyHash
    , driverAssetPolicyHash
    , carAssetPolicyHash
    , nitroFee: fromJsBigInt race.nitroFee
    }

setAssetPrices :: AssetPricesFFI -> Racers TransactionHashFFI
setAssetPrices assetPricesFFI = do
  let
    assetPrices = wrap $
      { common: fromJsBigInt assetPricesFFI.common
      , rare: fromJsBigInt assetPricesFFI.rare
      , epic: fromJsBigInt assetPricesFFI.epic
      }
  txh <- modifyRacersStateContract
    (\cur -> wrap $ (unwrap cur) { assetPrices = assetPrices })
  pure $ byteArrayToHex (unwrap txh)
