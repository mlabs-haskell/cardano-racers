module Lib.CardanoRacers.AdminFFI (module X, mkAdminFFI, mkAdminFFITest, bg) where

import Contract.Prelude

import Aeson (decodeJsonString)
import CardanoRacers.Common.Types (RacersParams)
import Contract.Config (PrivatePaymentKey(..), PrivatePaymentKeySource(..))
import Control.Monad.Error.Class (liftMaybe)
import Data.BigInt (BigInt)
import Data.BigInt (fromString) as BigInt
import Data.Function.Uncurried (Fn2, mkFn2)
import Effect.Aff.Compat (EffectFn1, mkEffectFn1)
import Effect.Exception (error)
import Lib.CardanoRacers.Admin (Admin, mkAdmin)
import Lib.CardanoRacers.Bot (Bot)
import Lib.CardanoRacers.Common (CredentialProvider(..), mkPrivateKey)
import Lib.CardanoRacers.Common (mkCredentialProviderFFI, mkRacersParamsFFI) as X
import Lib.CardanoRacers.Queries (Queries)
import Partial.Unsafe (unsafePartial)
import Type.Row (type (+))

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

bg :: EffectFn1 String BigInt
bg = mkEffectFn1 $ \str -> liftMaybe (error $ "Bad amount: " <> str) $
  BigInt.fromString str
