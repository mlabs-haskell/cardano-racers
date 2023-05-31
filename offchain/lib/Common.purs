module Lib.CardanoRacers.Common where

import Contract.Prelude

import Contract.Config
  ( PrivatePaymentKeySource
  , PrivateStakeKeySource
  , WalletSpec(..)
  )
import Contract.Wallet (WalletExtension(..))
import Data.ArrayBuffer.Types (Uint8Array)
import Data.BigInt (BigInt)

type Lovelace = BigInt
type Nitro = BigInt

data CredentialProvider
  = Wallet WalletExtension
  | Keys PrivatePaymentKeySource (Maybe PrivateStakeKeySource)

type Race = { raceId :: Uint8Array, nitroFee :: Nitro }

toWalletSpec :: CredentialProvider -> WalletSpec
toWalletSpec (Wallet NamiWallet) = ConnectToNami
toWalletSpec (Wallet GeroWallet) = ConnectToGero
toWalletSpec (Wallet FlintWallet) = ConnectToFlint
toWalletSpec (Wallet EternlWallet) = ConnectToEternl
toWalletSpec (Wallet LodeWallet) = ConnectToLode
toWalletSpec (Wallet LaceWallet) = ConnectToLace
toWalletSpec (Wallet NuFiWallet) = ConnectToNuFi
toWalletSpec (Keys pk msk) = UseKeys pk msk

