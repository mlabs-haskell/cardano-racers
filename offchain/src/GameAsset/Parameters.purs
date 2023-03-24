module CardanoRacers.GameAsset.Parameters
  ( generateUniformParameters
  , generateGaussianParameters
  ) where

import Contract.Prelude hiding (choose)

import CardanoRacers.GameAsset.Types (Rarity(Common, Rare, Epic))
import Control.Apply (lift2)
import Data.Array (zip)
import Data.Array as Array
import Data.Int (floor, pow)
import Data.List.Lazy (replicateM)
import Math (abs, cos, log, pi, sqrt) as Math
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
generateUniformParameters seed r = flip evalGen { newSeed: seed, size: 1 }
  $ chooseInt (rarityMinRequirement r) maxTotalScore
  >>= splitXTimes 2
  where
  maxTotalScore = parameterCount * maxParameterScore

  splitXTimes :: Int -> Int -> Gen (Array Int)
  splitXTimes 0 n = pure [ n ]
  splitXTimes level n = do
    -- to ensure that every attribute has a minimum value of 1, random pivot
    -- ranges from 2 ^ (level - 1) to n - 2 ^ (level - 1)
    let
      splitRandom :: Gen (Int /\ Int)
      splitRandom = chooseInt (2 `pow` (level - 1)) (n - 2 `pow` (level - 1))
        >>= \pivot -> pure $ pivot /\ (n - pivot)
    (pivot /\ rest) <- splitRandom
    lift2 (<>) (splitXTimes (level - 1) pivot) (splitXTimes (level - 1) rest)

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
