module CardanoRacers.GameAsset.Parameters (main, generateParameters, Rarity(Common,Epic,Rare)) where

import Contract.Prelude

import Data.Array (zip)
import Data.Array as Array
import Data.Int (floor)
import Data.List.Lazy (replicateM)
import Effect.Random (randomInt, randomRange)
import Math (abs, cos, log, pi, sqrt) as Math

main :: Effect Unit
main = do
    log "Hello, world!"
    log "Uniform:"
    traverse_ (presentParams <=< generateParameters) [Common, Rare, Epic]
    log "Gaussian:"
    traverse_ (presentParams <=< generateGaussianParameters) [Common, Rare, Epic]
    x <- traverse generateGaussianParameters $ Array.replicate 100000 Epic
    log $ "Max: " <> show (maximum $ map sum x)
  where presentParams = \x -> log (show x <> "  -> "<>  show (sum x))

data Rarity = Common | Rare | Epic

generateGaussianParameters :: Rarity -> Effect (Array Int)
generateGaussianParameters rarity = do
    r <- map Array.fromFoldable $ replicateM 4 (gaussianRandom 0.0 1400.0)
    -- base <- splitXTimes 2 minScore
    let base = baseByRarity
        rr = zip (map floor r) base
    pure $ map (capAt 10000) $ map (uncurry (+)) rr
  where minScore = case rarity of
            Common -> 0
            Rare -> 10000
            Epic -> 20000
        baseByRarity = case rarity of
            Common -> [0, 0, 0, 0]
            Rare -> [2500, 2500, 2500, 2500]
            Epic -> [5000, 5000, 5000, 5000]
        capAt :: forall x. Ord x => x -> x -> x
        capAt n x = if x >= n then n else x

gaussianRandom :: Number -> Number -> Effect Number
gaussianRandom mean std = do
    u <- (1.0 - _) <$> randomRange 0.0 1.0
    v <- randomRange 0.0 1.0
    -- Box-Muller transform
    let z = Math.sqrt (-2.0 * Math.log u) * Math.cos (2.0 * Math.pi * v)
    pure $ Math.abs  $ mean + std * z

generateParameters :: Rarity -> Effect (Array Int)
generateParameters = case _ of
    Common -> randomInt 0 max >>= splitXTimes splitLevel
    Rare -> randomInt 10000 max >>= splitXTimes splitLevel
    Epic -> randomInt 20000 max >>= splitXTimes splitLevel
  where max = 40000
        splitLevel = 2

splitXTimes :: Int -> Int -> Effect (Array Int)
splitXTimes 0 n = pure $ [n]
splitXTimes level n = do
    (pivot /\ rest) <- splitRandom n
    (splitXTimes (level - 1) pivot) <> (splitXTimes (level - 1) rest)

splitRandom :: Int -> Effect (Int /\ Int)
splitRandom n = randomInt 0 n >>= \pivot -> pure $ pivot /\ (n - pivot)


