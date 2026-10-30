module Test.CardanoRacers.Race (suite) where

import Prelude

import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Address (pubKeyHashAddress) as Plutus.Address
import Cardano.Plutus.Types.Map (singleton) as Plutus.Map
import Cardano.Plutus.Types.Value (coinToValue) as Plutus.Value
import Cardano.Types (Value)
import Cardano.Types.BigInt (fromInt) as JSBigInt
import Cardano.Types.BigNum (fromInt) as BigNum
import Cardano.Types.Value (lovelaceValueOf)
import Cardano.Wallet.Key (getPrivatePaymentKey, privateKeyToPkh)
import CardanoRacers.Race.Contract (distributeRewards, startRace)
import CardanoRacers.Race.Types (RewardDistribution)
import CardanoRacers.RaceSlot.Types (RaceHash)
import CardanoRacers.RacersState.Types (AssetPrices(AssetPrices))
import Contract.Log (logInfo')
import Contract.Test (ContractTest, InitialUTxOs, withKeyWallet, withWallets)
import Contract.Test.Mote (TestPlanM)
import Data.Array (replicate)
import Data.BigInt (fromInt) as BigInt
import Data.ByteArray (byteArrayFromAscii)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (wrap)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (liftAff)
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Racers (runRacers)
import Test.CardanoRacers.Helpers
  ( createRacersParamsHelper
  , initRacersStateWithAdminAndTreasury
  )

suite :: TestPlanM ContractTest Unit
suite =
  group "Race" do
    test "DistributeRewards" do
      withWallets (distr /\ distr /\ distr /\ replicate numParticipants distr)
        \(admin /\ treasury /\ anyone /\ participants) -> do
          rp <- withKeyWallet admin createRacersParamsHelper
          runRacers rp do
            void $ initRacersStateWithAdminAndTreasury (admin /\ treasury)
              (BigInt.fromInt 1_000_000)
              assetPrices
          pkhs <- liftAff $ traverse
            (map privateKeyToPkh <<< getPrivatePaymentKey)
            participants
          let
            addrs =
              flip Plutus.Address.pubKeyHashAddress Nothing <<< wrap <<< wrap
                <$> pkhs
          { raceParams } <-
            withKeyWallet admin $ runRacers rp $
              startRace (Just $ mkRewardDistribution addrs) raceHash
                totalRewardValue
                ( [ 50_000, 30_000, 20_000 ] <#> wrap <<< { numerator: _ } <<<
                    JSBigInt.fromInt
                )
                addrs
                mempty

          txHash <- withKeyWallet anyone $ runRacers rp $
            distributeRewards raceParams
          logInfo' $ "Success: " <> show txHash
  where
  distr :: InitialUTxOs
  distr =
    [ BigNum.fromInt 5_000_000
    , BigNum.fromInt 2_000_000_000
    ]

  assetPrices :: AssetPrices
  assetPrices = AssetPrices
    { common: JSBigInt.fromInt 5_000_000
    , rare: JSBigInt.fromInt 10_000_000
    , epic: JSBigInt.fromInt 20_000_000
    }

raceHash :: RaceHash
raceHash = unsafePartial fromJust $ byteArrayFromAscii "TestRaceHash"

totalRewardValue :: Value
totalRewardValue =
  lovelaceValueOf $ BigNum.fromInt $ singleRewardLovelace * numParticipants

singleRewardLovelace :: Int
singleRewardLovelace = 1_000_000

numParticipants :: Int
numParticipants = 5

mkRewardDistribution :: Array Plutus.Address -> RewardDistribution
mkRewardDistribution =
  foldMap \addr ->
    Plutus.Map.singleton addr $ Plutus.Value.coinToValue $ wrap $
      JSBigInt.fromInt singleRewardLovelace
