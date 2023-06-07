module Lib.CardanoRacers.BotFFI (module X, mkBotFFI, mkBotFFITest) where

import Contract.Prelude

import Aeson (decodeJsonString)
import CardanoRacers.Common.Types (RacersParams)
import Contract.Config (PrivatePaymentKey(..), PrivatePaymentKeySource(..))
import Data.Function.Uncurried (Fn2, mkFn2)
import Lib.CardanoRacers.Bot (Bot, mkBot)
import Lib.CardanoRacers.Common (CredentialProvider(..), mkPrivateKey)
import Lib.CardanoRacers.Common (mkCredentialProviderFFI, mkRacersParamsFFI) as X
import Lib.CardanoRacers.Queries (Queries)
import Partial.Unsafe (unsafePartial)
import Type.Row (type (+))

mkBotFFI
  :: Fn2 CredentialProvider RacersParams (Record (Bot + Queries + ()))
mkBotFFI = mkFn2 mkBot

mkBotFFITest :: Record (Bot + Queries + ())
mkBotFFITest = mkBot (Keys (PrivatePaymentKeyValue privateKey) Nothing) rp
  where
  rp = unsafePartial $ fromJust $ hush $ decodeJsonString
    "{\"RacersParams\":{\"stateToken\":[{\"unCurrencySymbol\":\"bab9b7cd0932176c73a1290a712330e429a12c012c4edb04d3e31835\"},{\"unTokenName\":\"RacersStateNFT\"}],\"botToken\":[{\"unCurrencySymbol\":\"121d0af155c2e0bc5da5e14701cecdedf05798baadf5d3ab12122c8c\"},{\"unTokenName\":\"RacersBotNFT\"}],\"adminToken\":[{\"unCurrencySymbol\":\"23d5ae79ddf758596aaa32908225b3dce9e0d57650e0d17f5a716309\"},{\"unTokenName\":\"RacersAdminNFT\"}]}}"
  privateKey = PrivatePaymentKey $ unsafePartial $ fromJust $ mkPrivateKey
    "582050389c06908083d9d9a559c0ca8d74e364a4016b2174710215e18c2cfe6eeca6"
