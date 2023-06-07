module Lib.CardanoRacers.AdminFFI
  ( module X
  , mkAdminFFI
  , mkAdminFFITest
  , initRacersFFI
  ) where

import Contract.Prelude

import Aeson (decodeJsonString, encodeAeson)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Helpers (paysToAddrConstraint)
import Contract.Address (addressFromBech32)
import Contract.Config (PrivatePaymentKey(..), PrivatePaymentKeySource(..))
import Contract.Monad (Contract, runContract)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (lovelaceValueOf)
import Control.Monad.Error.Class (liftMaybe)
import Control.Promise (Promise, fromAff)
import Ctl.Internal.Types.RedeemerTag (RedeemerTag(..))
import Data.BigInt (BigInt)
import Data.BigInt (fromString) as BigInt
import Data.Function.Uncurried (Fn2, mkFn2)
import Effect.Aff.Compat (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2)
import Effect.Exception (error)
import Lib.CardanoRacers.Admin (Admin, InitialStateFFI, initRacers, mkAdmin)
import Lib.CardanoRacers.Bot (Bot)
import Lib.CardanoRacers.Client (Client, mkClient)
import Lib.CardanoRacers.Common
  ( CredentialProvider(..)
  , customCfg
  , mkPrivateKey
  , toWalletSpec
  )
import Lib.CardanoRacers.Common (mkCredentialProviderFFI, mkRacersParamsFFI) as X
import Lib.CardanoRacers.Queries (Queries)
import Partial.Unsafe (unsafePartial)
import Type.Row (type (+))

initRacersFFI :: EffectFn2 CredentialProvider InitialStateFFI (Promise String)
initRacersFFI = mkEffectFn2 \cp is -> fromAff $ initRacers cp is <#>
  (encodeAeson >>> show)

mkAdminFFI
  :: Fn2 CredentialProvider RacersParams (Record (Admin + Bot + Queries + ()))
mkAdminFFI = mkFn2 mkAdmin

mkAdminFFITest :: Record (Admin + Bot + Queries + ())
mkAdminFFITest = mkAdmin (Keys (PrivatePaymentKeyValue privateKey) Nothing) rp
  where
  rp = unsafePartial $ fromJust $ hush $ decodeJsonString
    "{\"RacersParams\":{\"stateToken\":[{\"unCurrencySymbol\":\"bab9b7cd0932176c73a1290a712330e429a12c012c4edb04d3e31835\"},{\"unTokenName\":\"RacersStateNFT\"}],\"botToken\":[{\"unCurrencySymbol\":\"121d0af155c2e0bc5da5e14701cecdedf05798baadf5d3ab12122c8c\"},{\"unTokenName\":\"RacersBotNFT\"}],\"adminToken\":[{\"unCurrencySymbol\":\"23d5ae79ddf758596aaa32908225b3dce9e0d57650e0d17f5a716309\"},{\"unTokenName\":\"RacersAdminNFT\"}]}}"
  privateKey = PrivatePaymentKey $ unsafePartial $ fromJust $ mkPrivateKey
    "582043a451628918e1a04e35fc638850d05885bc4d13dd72692194ba82545d7e57ab"

-- credentialFromPrivateKeyString :: String -> CredentialProvider
-- credentialFromPrivateKeyString s = Keys (PrivatePaymentKeyValue privateKey)
--   Nothing
--   where
--   privateKey = PrivatePaymentKey $ unsafePartial $ fromJust $ mkPrivateKey s

-- payAda :: EffectFn2 BigInt String (Promise Unit)
-- payAda = mkEffectFn2 $ \amount addrStr -> fromAff $ runContract cfg do
--   addr <- addressFromBech32 addrStr
--   let
--     constraints :: Constraints.TxConstraints Void Void
--     constraints = paysToAddrConstraint addr $ lovelaceValueOf amount
-- 
--   txId <- submitTxFromConstraints (mempty :: Lookups.ScriptLookups Void)
--     constraints
--   awaitTxConfirmed txId
--   pure unit
--   where
--     walletSpec = toWalletSpec $ credentialFromPrivateKeyString
--       "5820a57db0c6cc5c10e066f6b6cde609a25c81461a49431d6e11d01e408ea5f6135e"
--     cfg = customCfg walletSpec
