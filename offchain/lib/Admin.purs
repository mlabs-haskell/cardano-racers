module Lib.CardanoRacers.Admin where

import Contract.Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Plutus.Types.Address as PlutusAddress
import Cardano.Types.Address as Address
import CardanoRacers.AssetRequest.Contract (mkAssetRequestPolicy)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Deposit.Validator (mkDepositValidator)
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types (GameAssetType(DriverType, CarType))
import CardanoRacers.Helpers (fromBIToJSBI)
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
import Contract.Address (addressFromBech32)
import Contract.CborBytes (cborBytesToHex)
import Contract.Config (ContractParams, WalletSpec)
import Contract.Log (logInfo')
import Contract.Monad (liftContractM, liftedM, runContract, throwContractError)
import Contract.Wallet (getWalletAddresses, getWalletUtxos)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, fromAff)
import Data.Array (head)
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
      traverse_ (logInfo' <<< Address.toBech32) addrs
      utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
      (txi /\ _) <- liftContractM "Could not get first utxo" $ Array.head $
        Map.toUnfoldable utxos
      rp <- NitroHelpers.createRacersParams txi

      treasuryAddress <- addressFromBech32 initialState.treasuryAddress
      treasuryAddrPlutus <-
        liftContractM "Could not convert treasury address to Plutus"
          $ PlutusAddress.fromCardano treasuryAddress

      operatingAddress <- addressFromBech32 initialState.operatingAddress
      operatingAddrPlutus <-
        liftContractM "Could not convert treasury address to Plutus"
          $ PlutusAddress.fromCardano operatingAddress

      let
        rs = RacersState
          { treasuryAddress: treasuryAddrPlutus
          , operatingAddress: operatingAddrPlutus
          , nitroPrice: fromBIToJSBI $ fromJsBigInt initialState.nitroPrice
          , assetPrices: AssetPrices
              { common: fromBIToJSBI $ fromJsBigInt
                  initialState.assetPrices.common
              , rare: fromBIToJSBI $ fromJsBigInt initialState.assetPrices.rare
              , epic: fromBIToJSBI $ fromJsBigInt initialState.assetPrices.epic
              }
          }
      _ <- runRacers rp do
        assetRequestPolicy <- mkAssetRequestPolicy
        driverAssetPolicy <- mkGameAssetPolicy DriverType
        carAssetPolicy <- mkGameAssetPolicy CarType
        nitroPolicy <- mkNitroPolicy

        nitroScriptRef <- lift $
          case head (unwrap nitroPolicy).plutusMintingPolicies of
            Just s -> pure s
            Nothing -> throwContractError "Not plutus script"

        assetRequestScriptRef <- lift $
          case head (unwrap assetRequestPolicy).plutusMintingPolicies of
            Just s -> pure s
            Nothing -> throwContractError "Not plutus script"

        driverPolicyRef <- lift $
          case head (unwrap driverAssetPolicy).plutusMintingPolicies of
            Just s -> pure s
            Nothing -> throwContractError "Not plutus script"

        carPolicyRef <- lift $
          case head (unwrap carAssetPolicy).plutusMintingPolicies of
            Just s -> pure s
            Nothing -> throwContractError "Not plutus script"

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
setNitroPrice nitroPrice = (cborBytesToHex <<< encodeCbor) <$>
  modifyRacersStateContract
    ( \cur -> wrap $ (unwrap cur)
        { nitroPrice = fromBIToJSBI $ fromJsBigInt nitroPrice }
    )

setTreasuryAddress :: String -> Racers TransactionHashFFI
setTreasuryAddress addrStr = do
  treasuryAddr <- lift $ addressFromBech32 addrStr
  treasuryAddrPlutus <- lift
    $ liftContractM "Could not convert treasury address to Plutus"
    $ PlutusAddress.fromCardano treasuryAddr
  txh <- modifyRacersStateContract
    (\cur -> wrap $ (unwrap cur) { treasuryAddress = treasuryAddrPlutus })
  pure $ cborBytesToHex (encodeCbor txh)

setOperatingAddress :: String -> Racers TransactionHashFFI
setOperatingAddress addrStr = do
  operatingAddr <- lift $ addressFromBech32 addrStr
  operatingAddrPlutus <- lift
    $ liftContractM "Could not convert operating address to Plutus"
    $ PlutusAddress.fromCardano operatingAddr
  txh <- modifyRacersStateContract
    (\cur -> wrap $ (unwrap cur) { operatingAddress = operatingAddrPlutus })
  pure $ cborBytesToHex (encodeCbor txh)
