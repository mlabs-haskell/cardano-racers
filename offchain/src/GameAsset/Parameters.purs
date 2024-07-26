module CardanoRacers.GameAsset.Parameters
  ( generateUniformParameters
  , generateGaussianParameters
  ) where

import Contract.Prelude hiding (choose)

import CardanoRacers.GameAsset.Types (Rarity(Common, Rare, Epic))
import Data.Array (zip)
import Data.Array as Array
import Data.Int (floor)
import Data.List.Lazy (replicateM)
import Data.Number (abs, cos, log, pi, sqrt) as Math
import Random.LCG (Seed)
import Test.QuickCheck.Gen (Gen, choose, chooseInt, evalGen)

maxParameterScore :: Int
maxParameterScore = 10000

parameterCount :: Int
parameterCount = 4

rarityMinRequirement :: Rarity -> Int
rarityMinRequirement = case _ of
  Common -> 4
  Rare -> 10000
  Epic -> 20000

-- | Generate a random number in [x,y)
chooseUpperExclusive :: Number -> Number -> Gen Number
chooseUpperExclusive x y = choose x y >>= \n ->
  if n == y then chooseUpperExclusive x y else pure n

generateUniformParameters :: Seed -> Rarity -> Array Int
generateUniformParameters seed r = flip evalGen { newSeed: seed, size: 1 } $ do
  let
    maxTotalScore = parameterCount * maxParameterScore
    initialParams = [ 1, 1, 1, 1 ]
    adjustedMaxTotalScore = maxTotalScore - sum initialParams

    singleDistrPass :: Int -> Array Int -> Gen (Int /\ Array Int)
    singleDistrPass 0 params = pure (0 /\ params)
    singleDistrPass rem params = case Array.uncons params of
      Nothing -> pure (rem /\ [])
      Just { head: p, tail: ps } ->
        let
          room = maxParameterScore - p
        in
          if room == 0 then do
            (rem /\ ps') <- singleDistrPass rem ps
            pure (rem /\ Array.cons p ps')
          else do
            scoreAdded <- chooseInt 1 (min room rem)
            (rem /\ ps') <- singleDistrPass (rem - scoreAdded) ps
            pure (rem /\ Array.cons (scoreAdded + p) ps')

    distribute :: Int -> Array Int -> Gen (Array Int)
    distribute 0 params = pure params
    distribute rem params = do
      (rem /\ params') <- singleDistrPass rem params
      distribute rem params'

  total <- chooseInt (rarityMinRequirement r) adjustedMaxTotalScore
  distribute total initialParams

generateGaussianParameters :: Seed -> Rarity -> (Array Int)
generateGaussianParameters seed rarity = flip evalGen { newSeed: seed, size: 1 }
  do
    r <- map Array.fromFoldable $ replicateM parameterCount
      (gaussianRandomGen mean deviation)
    let
      base = baseByRarity
      rr = zip (map floor r) base
    pure $ map (capAt maxParameterScore) $ map (uncurry (+)) rr
  where
  baseByRarity = case rarity of
    Common -> Array.replicate parameterCount 0
    Rare -> Array.replicate parameterCount (floor $ 10000.0 `div` 4.0)
    Epic -> Array.replicate parameterCount (floor $ 20000.0 `div` 4.0)

  capAt :: forall x. Ord x => x -> x -> x
  capAt n x = if x >= n then n else x
  mean = 0.0
  deviation = 1400.0

-- | Generate a random number from a gaussian distribution using the Box-Muller transform.
gaussianRandomGen :: Number -> Number -> Gen Number
gaussianRandomGen mean std = do
  (u /\ v) <- (/\) <$> map (1.0 - _) randomUnitInterval <*> randomUnitInterval
  let z = Math.sqrt (-2.0 * Math.log u) * Math.cos (2.0 * Math.pi * v)
  pure $ Math.abs $ mean + std * z
  where
  randomUnitInterval = chooseUpperExclusive 0.0 1.0

