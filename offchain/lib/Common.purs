module Lib.CardanoRacers.Common where

import Contract.Prelude

import Aeson (decodeJsonString)
import CardanoRacers.Common.Types (RacersParams(..))
import CardanoRacers.Helpers (decodeAesonString)
import Contract.Config (PrivatePaymentKeySource(..), PrivateStakeKeySource(..), WalletSpec(..), privateKeyFromBytes)
import Contract.Prim.ByteArray (RawBytes(..), hexToByteArray)
import Contract.Wallet (WalletExtension(..))
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Deserialization.Keys (privateKeyFromBech32)
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Serialization.Types (PrivateKey)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.String (Pattern(..), stripPrefix)
import Effect.Aff.Compat (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2)
import Effect.Exception (error)

type Lovelace = BigInt
type Nitro = BigInt

data CredentialProvider
  = Wallet WalletExtension
  | Keys PrivatePaymentKeySource (Maybe PrivateStakeKeySource)

type Race = { raceId :: Uint8Array, nitroFee :: Nitro }

mkCredentialProviderFFI
  :: { mkKeys ::
         EffectFn2 String (Fn1 MaybeFfiHelper (Maybe String)) CredentialProvider
     , mkWalletExtension :: EffectFn1 String CredentialProvider
     }
mkCredentialProviderFFI = { mkKeys, mkWalletExtension }
  where
  mkKeys = mkEffectFn2 $ \pkStr mskStrF -> do
    let mskStr = runFn1 mskStrF maybeFfiHelper
    mSk <- for mskStr $ liftMaybe (error "Could not deserialise secret key") <<< mkPrivateKey
    pk <- liftMaybe (error "Could not deserialise private key") $ mkPrivateKey pkStr
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
mkRacersParamsFFI = mkEffectFn1 $ \rpStr -> liftEither $ lmap (error <<< show) $ decodeJsonString rpStr


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

