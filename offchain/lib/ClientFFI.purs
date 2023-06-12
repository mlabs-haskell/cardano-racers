module Lib.CardanoRacers.ClientFFI (module X, mkClient, mkClientTest) where

import Contract.Prelude

import Aeson (decodeJsonString)
import CardanoRacers.Common.Types (RacersParams)
import Contract.Config
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivatePaymentKeySource(PrivatePaymentKeyValue)
  , WalletSpec(UseKeys)
  )
import Data.Function.Uncurried (Fn2, mkFn2)
import Lib.CardanoRacers.Client (Client)
import Lib.CardanoRacers.Client (mkClient) as Client
import Lib.CardanoRacers.Common (mkPrivateKey)
import Lib.CardanoRacers.Common (mkRacersParams, mkWalletSpec) as X
import Lib.CardanoRacers.Queries (Queries)
import Partial.Unsafe (unsafePartial)
import Type.Row (type (+))

mkClient
  :: Fn2 WalletSpec RacersParams (Record (Client + Queries + ()))
mkClient = mkFn2 Client.mkClient

mkClientTest :: Record (Client + Queries + ())
mkClientTest = Client.mkClient
  (UseKeys (PrivatePaymentKeyValue privateKey) Nothing)
  rp
  where
  rp = unsafePartial $ fromJust $ hush $ decodeJsonString
    "{\"RacersParams\":{\"stateToken\":[{\"unCurrencySymbol\":\"bab9b7cd0932176c73a1290a712330e429a12c012c4edb04d3e31835\"},{\"unTokenName\":\"RacersStateNFT\"}],\"botToken\":[{\"unCurrencySymbol\":\"121d0af155c2e0bc5da5e14701cecdedf05798baadf5d3ab12122c8c\"},{\"unTokenName\":\"RacersBotNFT\"}],\"adminToken\":[{\"unCurrencySymbol\":\"23d5ae79ddf758596aaa32908225b3dce9e0d57650e0d17f5a716309\"},{\"unTokenName\":\"RacersAdminNFT\"}]}}"
  privateKey = PrivatePaymentKey $ unsafePartial $ fromJust $ mkPrivateKey
    "582050389c06908083d9d9a559c0ca8d74e364a4016b2174710215e18c2cfe6eeca6"
