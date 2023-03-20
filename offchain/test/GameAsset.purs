module Test.CardanoRacers.GameAsset (suite) where

import Contract.Prelude

-- import CardanoRacers.GameAsset.Contract (mintNewDriverNft)
import CardanoRacers.GameAsset.Types (Rarity(Common))
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Data.BigInt (fromInt) as BigInt
import Mote (group, test)

suite :: TestPlanM PlutipTest Unit
suite = group "GameAsset tests" do
  test "GameAsset" do
    withWallets walletUtxoDistr \w ->
      withKeyWallet w do
        -- _ <- mintNewDriverNft
        --   { tokenNameStr: "RacersDriver_Schumacher"
        --   , nameStr: "Schumacher"
        --   , image:
        --       "ipfs://k2cwuee3arxg398hwxx6c0iferxitu126xntuzg8t765oo020h5y6npn"
        --   , mediaType: Just "image/png"
        --   , description: Just "Good old Michael Schumacher"
        --   , rarity: Common
        --   }
        pure unit
  where
  walletUtxoDistr :: InitialUTxOs
  walletUtxoDistr =
    [ BigInt.fromInt 5_000_000
    , BigInt.fromInt 2_000_000_000
    ]
