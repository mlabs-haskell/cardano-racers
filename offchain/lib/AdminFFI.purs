module Lib.CardanoRacers.AdminFFI
  ( module X
  , mkAdmin
  , mkAdminTest
  , initRacers
  ) where

import Contract.Prelude

import Aeson (decodeJsonString, encodeAeson)
import CardanoRacers.Common.Types (RacersParams)
import Contract.Config
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivatePaymentKeySource(PrivatePaymentKeyValue)
  )
import Control.Promise (Promise, fromAff)
import Data.Function.Uncurried (Fn2, mkFn2)
import Effect.Aff.Compat (EffectFn2, mkEffectFn2)
import Lib.CardanoRacers.Admin (Admin, InitialStateFFI)
import Lib.CardanoRacers.Admin (initRacers, mkAdmin) as Admin
import Lib.CardanoRacers.Bot (Bot)
import Lib.CardanoRacers.Common (CredentialProvider(Keys), mkPrivateKey)
import Lib.CardanoRacers.Common (mkCredentialProvider, mkRacersParams) as X
import Lib.CardanoRacers.Queries (Queries)
import Partial.Unsafe (unsafePartial)
import Type.Row (type (+))

initRacers :: EffectFn2 CredentialProvider InitialStateFFI (Promise String)
initRacers = mkEffectFn2 \cp is -> fromAff $ Admin.initRacers cp is <#>
  (encodeAeson >>> show)

mkAdmin
  :: Fn2 CredentialProvider RacersParams (Record (Admin + Bot + Queries + ()))
mkAdmin = mkFn2 Admin.mkAdmin

mkAdminTest :: Record (Admin + Bot + Queries + ())
mkAdminTest = Admin.mkAdmin (Keys (PrivatePaymentKeyValue privateKey) Nothing)
  rp
  where
  rp = unsafePartial $ fromJust $ hush $ decodeJsonString
    "{\"RacersParams\":{\"stateToken\":[{\"unCurrencySymbol\":\"552dd2d9684c178b6e013d2b901dd3e3fddabe93809827748bb2ed4b\"},{\"unTokenName\":\"RacersStateNFT\"}],\"botToken\":[{\"unCurrencySymbol\":\"e9500fdc9605f55ea61857c555b01bc80005c79edb9cd81032bdbba2\"},{\"unTokenName\":\"RacersBotNFT\"}],\"adminToken\":[{\"unCurrencySymbol\":\"8acf85912dd60bed6acf6211ad620f29981aa96eb754fd7fcc871a00\"},{\"unTokenName\":\"RacersAdminNFT\"}]}}"
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
