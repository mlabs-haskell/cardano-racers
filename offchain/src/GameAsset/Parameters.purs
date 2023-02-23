module CardanoRacers.GameAsset.Parameters
  ( generateUniformParameters
  , generateGaussianParameters
  , Rarity(Common, Epic, Rare)
  ) where

import Contract.Prelude

import Data.Array (zip)
import Data.Array as Array
import Data.Int (floor)
import Data.List.Lazy (replicateM)
import Effect.Random (randomInt, randomRange)
import Math (abs, cos, log, pi, sqrt) as Math

data Rarity = Common | Rare | Epic

maxParameterScore :: Int
maxParameterScore = 10000

parameterCount :: Int
parameterCount = 4

rarityMinRequirement :: Rarity -> Int
rarityMinRequirement = case _ of
  Common -> 0
  Rare -> 10000
  Epic -> 20000

generateUniformParameters :: Rarity -> Effect (Array Int)
generateUniformParameters r = randomInt (rarityMinRequirement r) maxTotalScore
  >>= splitXTimes 2
  where
  maxTotalScore = parameterCount * maxParameterScore

  splitXTimes :: Int -> Int -> Effect (Array Int)
  splitXTimes 0 n = pure $ [ n ]
  splitXTimes level n = do
    (pivot /\ rest) <- splitRandom n
    (splitXTimes (level - 1) pivot) <> (splitXTimes (level - 1) rest)

  splitRandom :: Int -> Effect (Int /\ Int)
  splitRandom n = randomInt 0 n >>= \pivot -> pure $ pivot /\ (n - pivot)

generateGaussianParameters :: Rarity -> Effect (Array Int)
generateGaussianParameters rarity = do
  r <- map Array.fromFoldable $ replicateM parameterCount
    (gaussianRandom mean deviation)
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
gaussianRandom :: Number -> Number -> Effect Number
gaussianRandom mean std = do
  u <- (1.0 - _) <$> randomRange 0.0 1.0
  v <- randomRange 0.0 1.0
  let z = Math.sqrt (-2.0 * Math.log u) * Math.cos (2.0 * Math.pi * v)
  pure $ Math.abs $ mean + std * z
