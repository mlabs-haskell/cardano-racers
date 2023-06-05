module Lib.CardanoRacers.Common where

import Contract.Prelude

import Aeson (decodeJsonString)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types (GameAssetType(..))
import CardanoRacers.Nitro.Contract (mkNitroPolicy)
import CardanoRacers.RaceRegistry.Types (RegistryParams)
import CardanoRacers.RaceSlot.Contract (mkRaceSlotPolicy)
import CardanoRacers.RaceSlot.Types (slotTokenName)
import Contract.Config
  ( PrivatePaymentKeySource(..)
  , PrivateStakeKeySource(..)
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
  , RawBytes(..)
  , byteArrayToIntArray
  , hexToByteArray
  )
import Contract.Scripts (mintingPolicyHash)
import Contract.Value (TokenName, getTokenName, scriptCurrencySymbol)
import Contract.Wallet (WalletExtension(..))
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Serialization.Types (PrivateKey)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt (fromString) as BigInt
import Data.Char (fromCharCode)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.String (Pattern(..), stripPrefix)
import Data.String.CodeUnits (fromCharArray)
import Data.UInt (fromInt) as UInt
import Effect.Aff.Compat (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Racers (Racers, withContract)

type Lovelace = BigInt
type Nitro = BigInt

data CredentialProvider
  = Wallet WalletExtension
  | Keys PrivatePaymentKeySource (Maybe PrivateStakeKeySource)

type Race = { raceId :: Uint8Array, nitroFee :: Nitro }

customCfg walletSpec = testnetConfig
  { walletSpec = Just walletSpec
  , backendParams = mkCtlBackendParams
      { kupoConfig: defaultKupoServerConfig
          { port = UInt.fromInt 1442, path = Nothing }
      , ogmiosConfig: defaultOgmiosWsConfig
      }
  }

bg :: String -> Effect BigInt
bg str = liftMaybe (error $ "Bad amount: " <> str) $ BigInt.fromString str

mkCredentialProviderFFI
  :: { mkKeys ::
         EffectFn2 String (Fn1 MaybeFfiHelper (Maybe String)) CredentialProvider
     , mkWalletExtension :: EffectFn1 String CredentialProvider
     }
mkCredentialProviderFFI = { mkKeys, mkWalletExtension }
  where
  mkKeys = mkEffectFn2 $ \pkStr mskStrF -> do
    let mskStr = runFn1 mskStrF maybeFfiHelper
    mSk <- for mskStr $ liftMaybe (error "Could not deserialise secret key") <<<
      mkPrivateKey
    pk <- liftMaybe (error "Could not deserialise private key") $ mkPrivateKey
      pkStr
    pure $ Keys (PrivatePaymentKeyValue $ wrap pk)
      (PrivateStakeKeyValue <<< wrap <$> mSk)

  mkWalletExtension = mkEffectFn1 $ \weStr -> do
    we <- liftMaybe (error "Could not deserialise wallet extension") $
      walletExtensionFromString weStr
    pure $ Wallet we

mkPrivateKey :: String -> Maybe PrivateKey
mkPrivateKey str =
  mkPrivateKey' str <|> (stripPrefix (Pattern "5820") str >>= mkPrivateKey)
  where
  mkPrivateKey' :: String -> Maybe PrivateKey
  mkPrivateKey' str' = hexToByteArray str' >>= RawBytes >>> privateKeyFromBytes

mkRacersParamsFFI :: EffectFn1 String RacersParams
mkRacersParamsFFI = mkEffectFn1 $ \rpStr -> liftEither $ lmap (error <<< show) $
  decodeJsonString rpStr

toWalletSpec :: CredentialProvider -> WalletSpec
toWalletSpec (Wallet NamiWallet) = ConnectToNami
toWalletSpec (Wallet GeroWallet) = ConnectToGero
toWalletSpec (Wallet FlintWallet) = ConnectToFlint
toWalletSpec (Wallet EternlWallet) = ConnectToEternl
toWalletSpec (Wallet LodeWallet) = ConnectToLode
toWalletSpec (Wallet LaceWallet) = ConnectToLace
toWalletSpec (Wallet NuFiWallet) = ConnectToNuFi
toWalletSpec (Keys pk msk) = UseKeys pk msk

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
    , nitroFee: race.nitroFee
    }
