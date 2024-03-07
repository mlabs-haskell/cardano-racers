module Lib.CardanoRacers.Admin where

import Contract.Prelude

import CardanoRacers.AssetRequest.Contract (mkAssetRequestPolicy)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Deposit.Validator (mkDepositValidator)
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types (GameAssetType(DriverType, CarType))
import CardanoRacers.Nitro.Contract (mkNitroPolicy)
import CardanoRacers.Nitro.Helpers (createRacersParams) as NitroHelpers
import CardanoRacers.RacersState.Contract
  ( createRacersRefScriptOutputs
  , initRacersStateContract
  , modifyRacersStateContract
  )
import CardanoRacers.RacersState.Types
  ( AssetPrices(AssetPrices)
  , RacersState(RacersState)
  )
import Contract.Address (addressFromBech32, addressToBech32)
import Contract.Config (ContractParams, WalletSpec)
import Contract.Log (logInfo')
import Contract.Monad (liftContractM, liftedM, runContract, throwContractError)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
import Contract.Wallet (getWalletAddresses, getWalletUtxos)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, fromAff)
import Data.Array (head) as Array
import Data.Map (toUnfoldable) as Map
import Effect.Aff.Compat (EffectFn1, mkEffectFn1)
import Lib.CardanoRacers.Bot (Bot, mkBot)
import Lib.CardanoRacers.Common
  ( AssetPricesFFI
  , Lovelace
  , TransactionHashFFI
  , fromJsBigInt
  )
import Lib.CardanoRacers.Queries (Queries, mkQueries)
import Racers (Racers, runRacers)
import Record (merge)
import Type.Row (type (+))

type Admin r =
  ( setNitroPrice :: EffectFn1 Lovelace (Promise TransactionHashFFI)
  , setTreasuryAddress :: EffectFn1 String (Promise TransactionHashFFI)
  , setOperatingAddress :: EffectFn1 String (Promise TransactionHashFFI)
  | r
  )

type InitialStateFFI =
  { treasuryAddress :: String
  , operatingAddress :: String
  , assetPrices :: AssetPricesFFI
  , nitroPrice :: Lovelace
  }

initRacers
  :: ContractParams -> WalletSpec -> InitialStateFFI -> Aff RacersParams
initRacers cp walletSpec initialState =
  let
    cfg = cp { walletSpec = Just walletSpec }
  in
    runContract cfg do
      addrs <- getWalletAddresses
      traverse_ (logInfo' <=< addressToBech32) addrs
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
        assetRequestPolicy <- mkAssetRequestPolicy
        driverAssetPolicy <- mkGameAssetPolicy DriverType
        carAssetPolicy <- mkGameAssetPolicy CarType
        nitroPolicy <- mkNitroPolicy

        nitroScriptRef <- lift $ case nitroPolicy of
          PlutusMintingPolicy s -> pure s
          _ -> throwContractError "Not plutus script"
        assetRequestScriptRef <- lift $ case assetRequestPolicy of
          PlutusMintingPolicy s -> pure s
          _ -> throwContractError "Not plutus script"
        driverPolicyRef <- lift $ case driverAssetPolicy of
          PlutusMintingPolicy s -> pure s
          _ -> throwContractError "Not plutus script"
        carPolicyRef <- lift $ case carAssetPolicy of
          PlutusMintingPolicy s -> pure s
          _ -> throwContractError "Not plutus script"

        depositAssetScriptRef <- unwrap <$> mkDepositValidator

        traverse_ createRacersRefScriptOutputs
          [ [ nitroScriptRef
            , assetRequestScriptRef
            ]
          , [ driverPolicyRef
            , carPolicyRef
            , depositAssetScriptRef
            ]
          ]

        initRacersStateContract rs
      pure rp

mkAdmin
  :: ContractParams
  -> WalletSpec
  -> RacersParams
  -> Record (Admin + Bot + Queries + ())
mkAdmin cp walletSpec rp =
  let
    queries = mkQueries cp walletSpec rp
    bot = mkBot cp walletSpec rp
    cfg = cp { walletSpec = Just walletSpec }

    runA :: Racers ~> Aff
    runA = runContract cfg <<< runRacers rp
  in
    { setNitroPrice: mkEffectFn1 $ fromAff <<< runA <<< setNitroPrice
    , setTreasuryAddress: mkEffectFn1 $ fromAff <<< runA <<< setTreasuryAddress
    , setOperatingAddress: mkEffectFn1 $ fromAff <<< runA <<<
        setOperatingAddress
    } `merge` queries `merge` bot

setNitroPrice :: Lovelace -> Racers TransactionHashFFI
setNitroPrice nitroPrice = (byteArrayToHex <<< unwrap) <$>
  modifyRacersStateContract
    (\cur -> wrap $ (unwrap cur) { nitroPrice = fromJsBigInt nitroPrice })

setTreasuryAddress :: String -> Racers TransactionHashFFI
setTreasuryAddress addrStr = do
  treasuryAddr <- lift $ addressFromBech32 addrStr
  txh <- modifyRacersStateContract
    (\cur -> wrap $ (unwrap cur) { treasuryAddress = treasuryAddr })
  pure $ byteArrayToHex (unwrap txh)

setOperatingAddress :: String -> Racers TransactionHashFFI
setOperatingAddress addrStr = do
  operatingAddr <- lift $ addressFromBech32 addrStr
  txh <- modifyRacersStateContract
    (\cur -> wrap $ (unwrap cur) { operatingAddress = operatingAddr })
  pure $ byteArrayToHex (unwrap txh)
