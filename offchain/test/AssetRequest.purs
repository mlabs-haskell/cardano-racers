module Test.CardanoRacers.AssetRequest (suite) where

import Contract.Prelude

import Cardano.Plutus.Types.Address (scriptHashAddress)
import Cardano.Plutus.Types.Address as PlutusAddress
import Cardano.Plutus.Types.TokenName (mkTokenName) as Value
import Cardano.Types.BigInt as JSBigInt
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.PlutusScript as PlutusScript
import CardanoRacers.AssetRequest.Contract
  ( mkAssetRequestPolicy
  , requestAssetByRarity
  )
import CardanoRacers.AssetRequest.Types
  ( AirdropAddressDatum(AirdropAddressDatum)
  , AssetRequestRedeemer(MintRequestToken)
  )
import CardanoRacers.Deposit.Validator (mkDepositValidator)
import CardanoRacers.GameAsset.Types (Rarity(Rare, Common))
import CardanoRacers.Helpers
  ( fromBIToBigNum
  , fromBIToJSBI
  , fromJSBIToBI
  , paysToAddrConstraint
  )
import CardanoRacers.RacersState.Contract (queryRacersState)
import CardanoRacers.RacersState.Types (AssetPrices(AssetPrices), getAssetPrice)
import Contract.Address (getNetworkId)
import Contract.Monad (liftContractM, liftedM)
import Contract.PlutusData (RedeemerDatum(RedeemerDatum), toData)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Test.Assert
  ( checkGainAtAddress'
  , checkTokenGainAtAddress'
  , label
  , runChecks
  )
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Testnet
  ( ContractTest
  , InitialUTxOs
  , withKeyWallet
  , withWallets
  )
import Contract.Transaction (submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(DatumInline))
import Contract.TxConstraints as Constraints
import Contract.Value (lovelaceValueOf, singleton) as Value
import Contract.Wallet (getWalletAddresses)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.Array (head) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, toNumber) as BigInt
import Data.Int (ceil)
import Data.Map (singleton) as Map
import Mote (group, test)
import Racers (runRacers, withContract)
import Test.CardanoRacers.Helpers
  ( createRacersParamsHelper
  , initRacersStateWithAdminAndTreasury
  )
import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM ContractTest Unit
suite = group "AssetRequest" do
  test "User requests asset by rarity" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(admin /\ treasury /\ user) -> do

        treasuryAddr <- withKeyWallet treasury
          $ liftedM "Could not get treasury address"
          $ Array.head
          <$> getWalletAddresses

        operatingAddress <- withKeyWallet admin
          $ liftedM "Could not get admin address"
          $ Array.head
          <$> getWalletAddresses

        rp <- withKeyWallet admin createRacersParamsHelper
        runRacers rp do
          rs <- initRacersStateWithAdminAndTreasury (admin /\ treasury)
            (BigInt.fromInt 1_000_000)
            defaultAssetPrices

          assetRequestMP <- mkAssetRequestPolicy
          assetRequestCs <- lift
            $ liftContractM "Could not get script hash of asset request policy"
            $ head
            $ map PlutusScript.hash
            $ (unwrap assetRequestMP).plutusMintingPolicies

          let rarities = [ Common ] -- , Rare, Epic ]

          depositScript <- (PlutusScript.hash <<< unwrap) <$> mkDepositValidator

          let
            depositAddressPlutus = scriptHashAddress (wrap depositScript)
              Nothing

          networkId <- lift $ getNetworkId
          depositAddress <- lift
            $ liftContractM
                "Could not convert deposit Plutus address to Cardano"
            $ PlutusAddress.toCardano networkId depositAddressPlutus

          for_ rarities $ \rarity -> do
            withContract (withKeyWallet user) do
              assetRequestTokenName <- lift
                $ liftContractM "Could not make required token names"
                $
                  (Value.mkTokenName <=< byteArrayFromAscii) (show rarity)

              let
                (assetPrice :: BigInt) = fromJSBIToBI $ getAssetPrice rarity
                  (unwrap rs).assetPrices
                amountToTreasury = BigInt.fromInt <<< ceil
                  $ BigInt.toNumber assetPrice
                  * 0.75
                amountToOperating = BigInt.fromInt <<< ceil
                  $ BigInt.toNumber assetPrice
                  * 0.25
                assertions =
                  [ checkGainAtAddress' (label treasuryAddr "Treasury")
                      (fromBIToJSBI amountToTreasury)
                  , checkGainAtAddress' (label operatingAddress "Operating")
                      (fromBIToJSBI amountToOperating)
                  , checkTokenGainAtAddress' (label depositAddress "Deposit")
                      ( assetRequestCs /\ unwrap assetRequestTokenName /\
                          one
                      )
                  ]

              withContract (runChecks assertions <<< lift) $
                requestAssetByRarity rarity
  test
    "User fails to request asset by rarity with incorrect amount paid to operating/treasury"
    do
      withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
        \(admin /\ treasury /\ alice) -> do
          let rarity = Rare
          rp <- withKeyWallet admin createRacersParamsHelper
          runRacers rp do
            rs <- initRacersStateWithAdminAndTreasury (admin /\ treasury)
              (BigInt.fromInt 1_000_000)
              defaultAssetPrices

            assetRequestPolicy <- mkAssetRequestPolicy
            assetRequestCs <- lift
              $ liftContractM
                  "Could not get script hash of asset request policy"
              $ head
              $ map PlutusScript.hash
              $ (unwrap assetRequestPolicy).plutusMintingPolicies

            withContract (withKeyWallet alice) do
              ownAddr <- lift $ liftedM "Could not get own address"
                $ Array.head
                <$> getWalletAddresses
              ownAddrPlutus <- lift
                $ liftContractM
                    "Could not convert address from Cardano to Plutus"
                $ PlutusAddress.fromCardano ownAddr

              assetRequestTokenName <- lift
                $ liftContractM "Could not make required token names"
                $ (Value.mkTokenName <=< byteArrayFromAscii) (show rarity)

              depositScript <- (validatorHash <<< unwrap) <$> mkDepositValidator

              let
                assetPrice = getAssetPrice rarity (unwrap rs).assetPrices
                (incorrectPayments :: Array (Number /\ Number)) =
                  [ (0.74 /\ 0.25)
                  , (0.75 /\ 0.24)
                  , (0.76 /\ 0.24)
                  , (0.74 /\ 0.26)
                  ]

                dat = toData $ AirdropAddressDatum
                  { airdropAddress: ownAddrPlutus }
                red = RedeemerDatum $ toData $ MintRequestToken

                testIncorrectPayment
                  :: (Number /\ Number) -> _
                testIncorrectPayment (treasuryRatio /\ operatingRatio) = do
                  (_ /\ stateTxi /\ stateTxo) <- queryRacersState
                  let
                    (amountToTreasury :: BigNum) =
                      fromBIToBigNum $ BigInt.fromInt $ ceil
                        $ JSBigInt.toNumber
                            assetPrice
                        * treasuryRatio
                    (amountToOperating :: BigNum) =
                      fromBIToBigNum $ BigInt.fromInt $ ceil
                        $ JSBigInt.toNumber
                            assetPrice
                        * operatingRatio
                    treasuryVal = Value.lovelaceValueOf amountToTreasury
                    operatingVal = Value.lovelaceValueOf amountToOperating

                    lockedVal =
                      Value.singleton assetRequestCs
                        (unwrap assetRequestTokenName)
                        BigNum.one

                    constraints :: Constraints.TxConstraints
                    constraints = Constraints.mustReferenceOutput stateTxi
                      <> paysToAddrConstraint (unwrap rs).treasuryAddress
                        treasuryVal
                      <> paysToAddrConstraint (unwrap rs).operatingAddress
                        operatingVal
                      <> Constraints.mustMintValueWithRedeemer red
                        ( Mint.singleton assetRequestCs
                            (unwrap assetRequestTokenName)
                            Int.one
                        )
                      <> Constraints.mustPayToScript depositScript
                        dat
                        DatumInline
                        lockedVal

                    lookups :: Lookups.ScriptLookups
                    lookups = assetRequestPolicy
                      <> Lookups.unspentOutputs
                        (Map.singleton stateTxi stateTxo)

                  resE <- try $ lift $ submitTxFromConstraints lookups
                    constraints
                  resE `shouldSatisfy` isLeft

              traverse_ testIncorrectPayment incorrectPayments

  where
  walletUtxoDistr :: InitialUTxOs
  walletUtxoDistr =
    [ BigNum.fromInt 5_000_000
    , BigNum.fromInt 2_000_000_000
    , BigNum.fromInt 2_000_000_000
    ]

  defaultAssetPrices :: AssetPrices
  defaultAssetPrices = AssetPrices
    { common: JSBigInt.fromInt 5_000_000
    , rare: JSBigInt.fromInt 10_000_000
    , epic: JSBigInt.fromInt 20_000_000
    }
