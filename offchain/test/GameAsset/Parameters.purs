module Test.CardanoRacers.GameAsset.Parameters (suite) where

import Contract.Prelude

import CardanoRacers.GameAsset.Parameters (generateUniformParameters)
import CardanoRacers.GameAsset.Types (Rarity(Common, Rare, Epic))
import Contract.Test.Mote (TestPlanM)
import Control.Apply (lift2)
import Mote (group, test)
import Random.LCG (lcgM)
import Test.QuickCheck (mkSeed, quickCheckGen')
import Test.QuickCheck.Gen (Gen, chooseInt)

paramsByRarityGen :: Rarity -> Gen (Array Int)
paramsByRarityGen rarity = chooseInt 1 lcgM <#> \i -> generateUniformParameters
  (mkSeed i)
  rarity

paramsGen :: Gen (Array Int)
paramsGen = chooseInt 1 3 >>=
  case _ of
    1 -> paramsByRarityGen Common
    2 -> paramsByRarityGen Rare
    3 -> paramsByRarityGen Epic
    _ -> pure []

suite :: TestPlanM (Aff Unit) Unit
suite = group "Parameters" do
  test "Params generated have fixed length of 4" do
    liftEffect $ quickCheckGen' 10000 $ ((_ == 4) <<< length) <$> paramsGen
  test "All params have value greater than 0"
    $ liftEffect
    $ quickCheckGen' 10000
    $ all (_ > 0)
    <$> paramsGen
  test "All params have value less than 10000"
    $ liftEffect
    $ quickCheckGen' 10000
    $ all (_ <= 10000)
    <$> paramsGen
  test "Common params have combined score of >= 4" do
    liftEffect $ quickCheckGen' 10000 $ sum
      >>> lift2 (&&)
        (_ >= 4)
        (_ <= 40000)
      <$> paramsByRarityGen Common
  test "Rare params have combined score of >= 10000" do
    liftEffect $ quickCheckGen' 10000 $ sum
      >>> lift2 (&&)
        (_ >= 10000)
        (_ <= 40000)
      <$> paramsByRarityGen Rare
  test "Epic params have combined score of >= 20000" do
    liftEffect $ quickCheckGen' 10000 $ sum
      >>> lift2 (&&)
        (_ >= 20000)
        (_ <= 40000)
      <$> paramsByRarityGen Epic
