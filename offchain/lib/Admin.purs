module Lib.CardanoRacers.Admin where

import Contract.Prelude

import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.RacersState.Contract (modifyRacersStateContract)
import Contract.Address (addressFromBech32)
import Contract.Config (testnetConfig)
import Contract.Monad (liftContractM, runContract)
import Contract.Transaction (TransactionHash)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, fromAff)
import Effect.Aff.Compat (EffectFn1, mkEffectFn1)
import Foreign.Object (Object)
import Foreign.Object (lookup) as Object
import Lib.CardanoRacers.Bot (Bot, mkBot)
import Lib.CardanoRacers.Common (CredentialProvider, Lovelace, toWalletSpec)
import Lib.CardanoRacers.Queries (Queries, mkQueries)
import Racers (Racers, runRacers)
import Record (merge)
import Type.Row (type (+))

type Admin r =
  ( setNitroPrice :: EffectFn1 Lovelace (Promise TransactionHash)
  , setAssetPrices :: EffectFn1 (Object Lovelace) (Promise TransactionHash)
  , setTreasuryAddress :: EffectFn1 String (Promise TransactionHash)
  , setOperatingAddress :: EffectFn1 String (Promise TransactionHash)
  | r
  )

mkAdmin
  :: CredentialProvider -> RacersParams -> Record (Admin + Bot + Queries + ())
mkAdmin cp rp =
  let
    queries = mkQueries cp rp
    bot = mkBot cp rp
    walletSpec = toWalletSpec cp
    cfg = testnetConfig { walletSpec = Just walletSpec }

    runA :: Racers ~> Aff
    runA = runContract cfg <<< runRacers rp
  in
    { setNitroPrice: mkEffectFn1 $ fromAff <<< runA <<< setNitroPrice
    , setAssetPrices: mkEffectFn1 $ fromAff <<< runA <<< setAssetPrices
    , setTreasuryAddress: mkEffectFn1 $ fromAff <<< runA <<< setTreasuryAddress
    , setOperatingAddress: mkEffectFn1 $ fromAff <<< runA <<<
        setOperatingAddress
    } `merge` queries `merge` bot

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
