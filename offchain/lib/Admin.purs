module Lib.CardanoRacers.Admin where

import Contract.Prelude

import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Nitro.Helpers (createRacersParams) as NitroHelpers
import CardanoRacers.RacersState.Contract
  ( initRacersStateContract
  , modifyRacersStateContract
  )
import CardanoRacers.RacersState.Types
  ( AssetPrices(AssetPrices)
  , RacersState(RacersState)
  )
import Contract.Address (addressFromBech32)
import Contract.Monad (liftContractM, liftedM, runContract)
import Contract.Transaction (TransactionHash)
import Contract.Wallet (getWalletUtxos)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, fromAff)
import Data.Array (head) as Array
import Data.Map (toUnfoldable) as Map
import Effect.Aff.Compat (EffectFn1, mkEffectFn1)
import Lib.CardanoRacers.Bot (Bot, mkBot)
import Lib.CardanoRacers.Common
  ( AssetPricesFFI
  , CredentialProvider
  , Lovelace
  , customCfg
  , fromJsBigInt
  , toWalletSpec
  )
import Lib.CardanoRacers.Queries (Queries, mkQueries)
import Racers (Racers, runRacers)
import Record (merge)
import Type.Row (type (+))

type Admin r =
  ( setNitroPrice :: EffectFn1 Lovelace (Promise TransactionHash)
  , setAssetPrices :: EffectFn1 AssetPricesFFI (Promise TransactionHash)
  , setTreasuryAddress :: EffectFn1 String (Promise TransactionHash)
  , setOperatingAddress :: EffectFn1 String (Promise TransactionHash)
  | r
  )

type InitialStateFFI =
  { treasuryAddress :: String
  , operatingAddress :: String
  , assetPrices :: AssetPricesFFI
  , nitroPrice :: Lovelace
  }

initRacers :: CredentialProvider -> InitialStateFFI -> Aff RacersParams
initRacers cp initialState =
  let
    walletSpec = toWalletSpec cp
    cfg = customCfg walletSpec
  in
    runContract cfg do
      utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
      (txi /\ _) <- liftContractM "Could not get first utxo" $ Array.head $
        Map.toUnfoldable utxos
      rp <- NitroHelpers.createRacersParams txi
      treasuryAddress <- addressFromBech32 initialState.treasuryAddress
      operatingAddress <- addressFromBech32 initialState.operatingAddress
      let
        rs = RacersState
          { treasuryAddress: treasuryAddress
          , operatingAddress: operatingAddress
          , nitroPrice: fromJsBigInt initialState.nitroPrice
          , assetPrices: AssetPrices
              { common: fromJsBigInt initialState.assetPrices.common
              , rare: fromJsBigInt initialState.assetPrices.rare
              , epic: fromJsBigInt initialState.assetPrices.epic
              }
          }
      _ <- runRacers rp do
        initRacersStateContract rs
      pure rp

mkAdmin
  :: CredentialProvider -> RacersParams -> Record (Admin + Bot + Queries + ())
mkAdmin cp rp =
  let
    queries = mkQueries cp rp
    bot = mkBot cp rp
    walletSpec = toWalletSpec cp
    cfg = customCfg walletSpec

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
  (\cur -> wrap $ (unwrap cur) { nitroPrice = fromJsBigInt nitroPrice })

setAssetPrices :: AssetPricesFFI -> Racers TransactionHash
setAssetPrices assetPricesFFI = do
  let
    assetPrices = wrap $
      { common: fromJsBigInt assetPricesFFI.common
      , rare: fromJsBigInt assetPricesFFI.rare
      , epic: fromJsBigInt assetPricesFFI.epic
      }
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
