module Lib.CardanoRacers.Queries where

import Contract.Prelude

import Aeson (Aeson, encodeAeson)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types (GameAssetType(..))
import CardanoRacers.Nitro.Contract (mkNitroPolicy)
import CardanoRacers.RaceRegistry.Contract (queryRegistryUtxos)
import CardanoRacers.RaceSlot.Contract (mkRaceSlotPolicy)
import CardanoRacers.RaceSlot.Types (slotTokenName)
import CardanoRacers.RacersState.Contract (queryRacersState)
import CardanoRacers.RacersState.Types (AssetPrices(..))
import Contract.Address (addressToBech32)
import Contract.Config (WalletSpec(..), testnetConfig)
import Contract.Monad (liftedM, runContract)
import Contract.Scripts (mintingPolicyHash)
import Contract.Value (scriptCurrencySymbol)
import Contract.Wallet (WalletExtension(..))
import Control.Monad.Trans.Class (lift)
import Data.Array (concat) as Array
import Data.Map (toUnfoldable) as Map
import Foreign.Object (Object)
import Foreign.Object (fromFoldable) as Object
import Lib.CardanoRacers.Common
  ( CredentialProvider(..)
  , Lovelace
  , Race
  , toWalletSpec
  )
import Racers (Racers, runRacers, withContract)

type Queries :: forall k. k -> Row Type
type Queries r =
  ( getNitroPrice :: Aff Lovelace
  , getAssetPrices :: Aff (Object Lovelace) -- Asset prices as object
  , getTreasuryAddress :: Aff String -- Address as bech32
  , getOperatingAddress :: Aff String -- Address as bech32
  , queryRaceRegistry :: Race -> Aff (Array Aeson)
  )

mkQueries :: CredentialProvider -> RacersParams -> Aff (Record (Queries ()))
mkQueries cp rp = do
  let
    walletSpec = toWalletSpec cp
    cfg = testnetConfig { walletSpec = Just walletSpec }

    runQ :: Racers ~> Aff
    runQ = runContract cfg <<< runRacers rp
  pure
    { getNitroPrice: runQ getNitroPrice
    , getAssetPrices: runQ getAssetPrices
    , getTreasuryAddress: runQ getTreasuryAddress
    , getOperatingAddress: runQ getOperatingAddress
    , queryRaceRegistry: \race -> runQ (queryRaceRegistry race)
    }

getNitroPrice :: Racers Lovelace
getNitroPrice = queryRacersState <#> fst >>> unwrap >>> _.nitroPrice

getAssetPrices :: Racers (Object Lovelace)
getAssetPrices = queryRacersState <#> fst >>> unwrap >>> _.assetPrices >>>
  assetPricesToObject
  where
  assetPricesToObject (AssetPrices x) = Object.fromFoldable
    [ "common" /\ x.common
    , "rare" /\ x.rare
    , "epic" /\ x.epic
    ]

getTreasuryAddress :: Racers String
getTreasuryAddress = queryRacersState <#> (fst >>> unwrap >>> _.treasuryAddress)
  >>= lift
  <<< addressToBech32

getOperatingAddress :: Racers String
getOperatingAddress = queryRacersState
  <#> (fst >>> unwrap >>> _.operatingAddress)
  >>= lift
  <<< addressToBech32

queryRaceRegistry :: Race -> Racers (Array Aeson)
queryRaceRegistry race = do
  nitroPolicyHash <- mintingPolicyHash <$> mkNitroPolicy
  driverAssetPolicyHash <- mintingPolicyHash <$> mkGameAssetPolicy DriverType
  carAssetPolicyHash <- mintingPolicyHash <$> mkGameAssetPolicy CarType
  slotSymbol <-
    withContract (liftedM "could not get currency symbol from policy")
      $ scriptCurrencySymbol
      <$> mkRaceSlotPolicy (wrap race.raceId)
  let
    rgp = wrap
      { slotAssetClass: slotSymbol /\ slotTokenName
      , nitroPolicyHash
      , driverAssetPolicyHash
      , carAssetPolicyHash
      , nitroFee: race.nitroFee
      }
  queryRegistryUtxos rgp <#> Map.toUnfoldable >>> map (snd >>> snd)
    >>> Array.concat
    >>> map encodeAeson
