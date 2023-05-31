module Lib.CardanoRacers.Admin where

import Contract.Prelude

import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.RaceRegistry.Contract (initRace)
import CardanoRacers.RacersState.Contract (modifyRacersStateContract)
import Contract.Address (addressFromBech32)
import Contract.Config (testnetConfig)
import Contract.Monad (liftContractM, runContract)
import Contract.Transaction (TransactionHash)
import Control.Monad.Trans.Class (lift)
import Data.BigInt (BigInt)
import Foreign.Object (Object)
import Foreign.Object (lookup) as Object
import Lib.CardanoRacers.Common
  ( CredentialProvider
  , Lovelace
  , Race
  , toWalletSpec
  )
import Lib.CardanoRacers.Queries (Queries, mkQueries)
import Racers (Racers, runRacers)
import Record (merge)
import Type.Row (type (+))

type Admin r =
  ( setNitroPrice :: Lovelace -> Aff TransactionHash
  , setAssetPrices :: Object Lovelace -> Aff TransactionHash
  , setTreasuryAddress :: String -> Aff TransactionHash
  , setOperatingAddress :: String -> Aff TransactionHash
  , createRace :: Race -> BigInt -> Aff Unit
  | r
  )

mkAdmin
  :: CredentialProvider -> RacersParams -> Aff (Record (Admin + Queries + ()))
mkAdmin cp rp = do
  queries <- mkQueries cp rp
  let
    walletSpec = toWalletSpec cp
    cfg = testnetConfig { walletSpec = Just walletSpec }

    runA :: Racers ~> Aff
    runA = runContract cfg <<< runRacers rp
  pure $
    { setNitroPrice: \price -> runA (setNitroPrice price)
    , setAssetPrices: \ap -> runA (setAssetPrices ap)
    , setTreasuryAddress: \ta -> runA (setTreasuryAddress ta)
    , setOperatingAddress: \oa -> runA (setOperatingAddress oa)
    , createRace: \race slots -> runA (createRace race slots)
    } `merge` queries

setNitroPrice :: Lovelace -> Racers TransactionHash
setNitroPrice nitroPrice = modifyRacersStateContract
  (\cur -> wrap $ (unwrap cur) { nitroPrice = nitroPrice })

setAssetPrices :: Object Lovelace -> Racers TransactionHash
setAssetPrices assetPricesObj = do
  commonPrice <- lift $ liftContractM "Could not get 'common' price" $
    Object.lookup "common" assetPricesObj
  rarePrice <- lift $ liftContractM "Could not get 'rare' price" $ Object.lookup
    "rare"
    assetPricesObj
  epicPrice <- lift $ liftContractM "Could not get 'epic' price" $ Object.lookup
    "epic"
    assetPricesObj
  let
    assetPrices = wrap $
      { common: commonPrice, rare: rarePrice, epic: epicPrice }
  modifyRacersStateContract
    (\cur -> wrap $ (unwrap cur) { assetPrices = assetPrices })

setTreasuryAddress :: String -> Racers TransactionHash
setTreasuryAddress addrStr = do
  treasuryAddr <- lift $ addressFromBech32 addrStr
  modifyRacersStateContract
    (\cur -> wrap $ (unwrap cur) { treasuryAddress = treasuryAddr })

setOperatingAddress :: String -> Racers TransactionHash
setOperatingAddress addrStr = do
  operatingAddr <- lift $ addressFromBech32 addrStr
  modifyRacersStateContract
    (\cur -> wrap $ (unwrap cur) { operatingAddress = operatingAddr })

createRace :: Race -> BigInt -> Racers Unit
createRace race slotCount = void $ initRace (wrap race.raceId) race.nitroFee
  slotCount
