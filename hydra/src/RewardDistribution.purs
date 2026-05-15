module CardanoRacers.Hydra.RewardDistribution
  ( distributeRewards
  ) where

import Prelude

import Cardano.Plutus.Types.Map (fromCardano) as Plutus.Map
import Cardano.Plutus.Types.Value (Value) as Plutus
import Cardano.Plutus.Types.Value (flattenValue, singleton) as Plutus.Value
import CardanoRacers.Hydra.Types.RaceStatus (RaceResults)
import CardanoRacers.Race.Types (RaceParams(RaceParams), RewardDistribution)
import CardanoRacers.Types.FixedDecimal (FixedDecimal, N5, emul, fromFixed5, toFixedZero)
import Data.Array (foldMap, length, replicate, splitAt, zip, zipWith) as Array
import Data.Foldable (sum)
import Data.Identity (Identity)
import Data.Map (fromFoldable) as Map
import Data.Maybe (fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import JS.BigInt (BigInt)
import JS.BigInt (fromInt, toInt) as BigInt
import Partial.Unsafe (unsafePartial)

distributeRewards
  :: RaceResults Identity -- final sorted results (fastest first)
  -> RaceParams
  -> RewardDistribution
distributeRewards results (RaceParams { totalRewardValue, rewardWeights }) =
  Plutus.Map.fromCardano $ Map.fromFoldable $
    Array.zip
      (_.addr <$> results)
      ( fixedRewardWeights <#> \ratio ->
          modifyValueAmounts totalRewardValue $ \amount ->
            fromFixed5 $ emul ratio $ toFixedZero
              amount
      )
  where
  resultCount :: Int
  resultCount = Array.length results

  -- Ensures excess reward weights are redistributed deterministically by
  -- splitting the discarded total evenly across retained results (integer
  -- quotient per item) and distributing any remainder one-by-one to the
  -- earliest entries.
  fixedRewardWeights :: Array (FixedDecimal N5)
  fixedRewardWeights
    | Array.length rewardWeights <= resultCount = rewardWeights
    | otherwise =
        let
          { before, after } = Array.splitAt resultCount rewardWeights
          quot /\ rem = (unwrap $ sum after).numerator `biQuotRem` BigInt.fromInt resultCount
          remList = Array.replicate (unsafePartial fromJust $ BigInt.toInt rem) one
        in
          Array.zipWith
            (\r ratio -> wrap { numerator: quot + r } + ratio)
            (remList <> Array.replicate (Array.length before - Array.length remList) zero)
            before

biQuotRem :: BigInt -> BigInt -> Tuple BigInt BigInt
biQuotRem x y = div x y /\ mod x y

modifyValueAmounts :: Plutus.Value -> (BigInt -> BigInt) -> Plutus.Value
modifyValueAmounts val f =
  Array.foldMap
    (\(cs /\ tn /\ amnt) -> Plutus.Value.singleton cs tn amnt)
    (Plutus.Value.flattenValue val <#> (\(cs /\ tn /\ amnt) -> cs /\ tn /\ f amnt))
