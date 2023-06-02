module Lib.CardanoRacers.Queries where

import Contract.Prelude

import Aeson (Aeson, encodeAeson)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.RaceRegistry.Contract (queryRegistryUtxos)
import CardanoRacers.RacersState.Contract (queryRacersState)
import CardanoRacers.RacersState.Types (AssetPrices(..))
import Contract.Address (addressToBech32)
import Contract.Config
  ( defaultKupoServerConfig
  , defaultOgmiosWsConfig
  , mkCtlBackendParams
  , testnetConfig
  )
import Contract.Monad (runContract)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, fromAff)
import Data.Array (concat) as Array
import Data.Map (toUnfoldable) as Map
import Data.UInt as UInt
import Effect.Aff.Compat (EffectFn1, mkEffectFn1)
import Foreign.Object (Object)
import Foreign.Object (fromFoldable) as Object
import Lib.CardanoRacers.Common
  ( CredentialProvider
  , Lovelace
  , Race
  , createRegistryParams
  , toWalletSpec
  )
import Racers (Racers, runRacers)

type Queries r =
  ( getNitroPrice :: EffectFn1 Unit (Promise Lovelace)
  , getAssetPrices ::
      EffectFn1 Unit (Promise (Object Lovelace)) -- Asset prices as object
  , getTreasuryAddress :: EffectFn1 Unit (Promise String) -- Address as bech32
  , getOperatingAddress :: EffectFn1 Unit (Promise String) -- Address as bech32
  , queryRaceRegistry :: EffectFn1 Race (Promise (Array Aeson))
  | r
  )

mkQueries :: CredentialProvider -> RacersParams -> Record (Queries ())
mkQueries cp rp =
  let
    walletSpec = toWalletSpec cp
    cfg = testnetConfig
      { walletSpec = Just walletSpec
      , backendParams = mkCtlBackendParams
          { kupoConfig: defaultKupoServerConfig
              { port = UInt.fromInt 1442, path = Nothing }
          , ogmiosConfig: defaultOgmiosWsConfig
          }
      }

    runQ :: Racers ~> Aff
    runQ = runContract cfg <<< runRacers rp
  in
    { getNitroPrice: mkEffectFn1 $ const $ fromAff $ runQ getNitroPrice
    , getAssetPrices: mkEffectFn1 $ const $ fromAff $ runQ getAssetPrices
    , getTreasuryAddress: mkEffectFn1 $ const $ fromAff $ runQ
        getTreasuryAddress
    , getOperatingAddress: mkEffectFn1 $ const $ fromAff $ runQ
        getOperatingAddress
    , queryRaceRegistry: mkEffectFn1 $ fromAff <<< runQ <<< queryRaceRegistry
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
  rgp <- createRegistryParams race
  queryRegistryUtxos rgp <#> Map.toUnfoldable >>> map (snd >>> snd)
    >>> Array.concat
    >>> map encodeAeson
