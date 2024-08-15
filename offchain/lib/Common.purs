module Lib.CardanoRacers.Common where

import Contract.Prelude

import Aeson (decodeJsonString)
import Cardano.Plutus.Types.MintingPolicyHash
  ( MintingPolicyHash(MintingPolicyHash)
  )
import Cardano.Serialization.Lib (toBytes)
import Cardano.Types (NetworkId(MainnetId, TestnetId), PrivateKey, Value(Value))
import Cardano.Types.AssetName (unAssetName)
import Cardano.Types.BigInt as JSBigInt
import Cardano.Types.BigNum as BigNum
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.PlutusScript as PlutusScript
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
  , QueryBackendParams
  , ServerConfig
  , mkBlockfrostBackendParams
  , mkCtlBackendParams
  , testnetConfig
  )
import Contract.Keys (privateKeyFromBytes)
import Contract.Monad (liftContractM)
import Contract.Prim.ByteArray
  ( ByteArray
  , RawBytes(RawBytes)
  , byteArrayToHex
  , byteArrayToIntArray
  , hexToByteArray
  )
import Contract.ScriptLookups (ScriptLookups)
import Contract.Value (TokenName)
import Contract.Wallet
  ( WalletExtension
      ( GenericCip30Wallet
      , LaceWallet
      , NuFiWallet
      , LodeWallet
      , EternlWallet
      , FlintWallet
      , GeroWallet
      , NamiWallet
      )
  , WalletSpec
  )
import Control.Alt ((<|>))
import Control.Monad.Except.Trans (ExceptT, mapExceptT, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as DataBigInt
import Data.Char (fromCharCode)
import Data.List.NonEmpty (singleton) as NonEmpty
import Data.Profunctor.Choice (left)
import Data.String (Pattern(Pattern), stripPrefix)
import Data.String.CodeUnits (fromCharArray)
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt (fromInt) as UInt
import Effect.Aff.Compat (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2)
import Effect.Exception (error)
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
import Partial.Unsafe (unsafePartial)
import Racers (Racers)

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

-- walletSpec
--   :: { walletFromMnemonic :: EffectFn4 String Int Int Boolean CredentialProvider
--      , walletFromPrivateKey :: EffectFn1 String CredentialProvider
--      , walletFromPrivateKeyAndStakeKey ::
--          EffectFn2 String String CredentialProvider
--      , browserWallet :: Object (EffectFn1 Unit CredentialProvider)
--      }
-- walletSpec =
--   { walletFromMnemonic
--   , walletFromPrivateKey
--   , walletFromPrivateKeyAndStakeKey
--   , browserWallet
--   }
--   where
--   walletFromMnemonic = mkEffectFn4 $
--     \mnemonic accountIndex addressIndex hasStake -> do
--       (kw :: KeyWallet) <- liftEither $ left error $ mkKeyWalletFromMnemonic mnemonic
--         { accountIndex: UInt.fromInt accountIndex
--         , addressIndex: UInt.fromInt addressIndex
--         }
--         (if hasStake then WithStakeKey else WithoutStakeKey)
--       privPaymentKeyF <- lift $  launchAff (unwrap kw).paymentKey
--       privPaymentKey <- joinFiber privPaymentKeyF
--       privStakeKey <- (unwrap kw).stakeKey
--       pure $ UseKeys (PrivatePaymentKeyValue privPaymentKey)
--         (PrivateStakeKeyValue <$> privStakeKey)
--
--   walletFromPrivateKey = mkEffectFn1 $ \privateKeyStr -> do
--     privateKey <- liftMaybe (error "Could not deserialise private key") $
--       mkPrivateKey
--         privateKeyStr
--     pure $ UseKeys (PrivatePaymentKeyValue $ wrap privateKey) Nothing
--
--   walletFromPrivateKeyAndStakeKey = mkEffectFn2 $ \privateKeyStr stakeKeyStr ->
--     do
--       privateKey <- liftMaybe (error "Could not deserialise private key") $
--         mkPrivateKey
--           privateKeyStr
--       stakeKey <- liftMaybe (error "Could not deserialise stake key") $
--         mkPrivateKey
--           stakeKeyStr
--       pure $ UseKeys (PrivatePaymentKeyValue $ wrap privateKey)
--         (Just $ PrivateStakeKeyValue $ wrap stakeKey)
--
--   browserWallet = Object.fromFoldable
--     $ map
--         ( \(name /\ spec) -> ("connectTo" <> name) /\ mkEffectFn1
--             (const $ pure spec)
--         )
--     $
--       [ "Nami" /\ ConnectToNami
--       , "GeroWallet" /\ ConnectToGero
--       , "Flint" /\ ConnectToFlint
--       , "Eternl" /\ ConnectToEternl
--       , "LodeWallet" /\ ConnectToLode
--       , "Lace" /\ ConnectToLace
--       , "Vespr" /\ ConnectToGenericCip30 "vespr"
--       , "NuFi" /\ ConnectToNuFi
--       ]

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
  "vespr" -> Just (GenericCip30Wallet "vespr")
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
  if null intArray then "Lovelace" else toAscii tkBytes
  where
  tkBytes = unAssetName tk
  intArray = byteArrayToIntArray tkBytes

  toAscii :: ByteArray -> String
  toAscii ba = fromCharArray
    $ map (\x -> unsafePartial $ fromJust $ fromCharCode x)
    $ byteArrayToIntArray ba

createRegistryParams :: Race -> Racers RegistryParams
createRegistryParams race = do
  nitroPolicy <- mkNitroPolicy
  nitroPolicyHash <- lift
    $ liftContractM "Could not get script hash of nitro policy"
    $ mintingPolicyHash nitroPolicy

  driverAssetPolicy <- mkGameAssetPolicy DriverType
  driverAssetPolicyHash <- lift
    $ liftContractM "Could not get script hash of driver asset policy"
    $ mintingPolicyHash driverAssetPolicy

  carAssetPolicy <- mkGameAssetPolicy CarType
  carAssetPolicyHash <- lift
    $ liftContractM "Could not get script hash of car asset policy"
    $ mintingPolicyHash carAssetPolicy

  slotPolicy <- mkRaceSlotPolicy (wrap race.raceId)
  slotSymbol <- lift
    $ liftContractM "Could not get script hash of races slot policy"
    $ mintingPolicyHash slotPolicy

  pure $ wrap
    { slotAssetClass: (unwrap slotSymbol) /\ (unwrap slotTokenName)
    , nitroPolicyHash
    , driverAssetPolicyHash
    , carAssetPolicyHash
    , nitroFee: toBI $ fromJsBigInt race.nitroFee
    }

setAssetPrices :: AssetPricesFFI -> Racers TransactionHashFFI
setAssetPrices assetPricesFFI = do
  let
    assetPrices = wrap $
      { common: toBI $ fromJsBigInt assetPricesFFI.common
      , rare: toBI $ fromJsBigInt assetPricesFFI.rare
      , epic: toBI $ fromJsBigInt assetPricesFFI.epic
      }
  txh <- modifyRacersStateContract
    (\cur -> wrap $ (unwrap cur) { assetPrices = assetPrices })
  pure $ byteArrayToHex (toBytes $ unwrap txh)

toBI :: DataBigInt.BigInt -> JSBigInt.BigInt
toBI = unsafePartial fromJust <<< JSBigInt.fromString <<< DataBigInt.toString

mintingPolicyHash :: ScriptLookups -> Maybe MintingPolicyHash
mintingPolicyHash sl = case head (unwrap sl).plutusMintingPolicies of
  Just pmp -> Just $ MintingPolicyHash $ PlutusScript.hash pmp
  Nothing -> do
    (MintingPolicyHash <<< NativeScript.hash) <$> head
      (unwrap sl).nativeMintingPolicies

negation :: Value -> Value
negation (Value c ma) =
  Value c $ wrap $ map
    ( map
        ( \b ->
            unsafePartial fromJust $ BigNum.fromInt (-1) `BigNum.mul` b
        )
    )
    (unwrap ma)
