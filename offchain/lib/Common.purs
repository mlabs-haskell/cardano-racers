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
import Contract.Config
  ( PrivatePaymentKeySource(..)
  , PrivateStakeKeySource(..)
  , StakeKeyPresence(..)
  , WalletSpec(..)
  , defaultKupoServerConfig
  , defaultOgmiosWsConfig
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
      )
  )
import Contract.Wallet.Key
  ( keyWalletPrivatePaymentKey
  , keyWalletPrivateStakeKey
  , mkKeyWalletFromMnemonic
  )
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Serialization.Types (PrivateKey)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.Char (fromCharCode)
import Data.Profunctor.Choice (left)
import Data.String (Pattern(Pattern), stripPrefix)
import Data.String.CodeUnits (fromCharArray)
import Data.UInt (fromInt) as UInt
import Effect.Aff.Compat (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2)
import Effect.Exception (error)
import Effect.Uncurried (EffectFn4, mkEffectFn4)
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

type Race = { raceId :: Uint8Array, nitroFee :: Nitro }

customCfg walletSpec = testnetConfig
  { walletSpec = Just walletSpec
  , backendParams = mkCtlBackendParams
      { kupoConfig: defaultKupoServerConfig
          { port = UInt.fromInt 1442, path = Nothing }
      , ogmiosConfig: defaultOgmiosWsConfig
      }
  }

mkWalletSpec
  :: { walletFromMnemonic :: EffectFn4 String Int Int Boolean CredentialProvider
     , walletFromPrivateKey :: EffectFn1 String CredentialProvider
     , walletFromPrivateKeyAndStakeKey ::
         EffectFn2 String String CredentialProvider
     , browserWallet :: Object (EffectFn1 Unit CredentialProvider)
     }
mkWalletSpec =
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
