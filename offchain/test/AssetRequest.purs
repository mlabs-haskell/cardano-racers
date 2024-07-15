module Test.CardanoRacers.AssetRequest (suite) where

import Contract.Prelude

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
import CardanoRacers.Helpers (paysToAddrConstraint)
import CardanoRacers.RacersState.Contract (queryRacersState)
import CardanoRacers.RacersState.Types (AssetPrices(AssetPrices), getAssetPrice)
import Contract.Address (scriptHashAddress)
import Contract.Monad (liftContractM, liftedM)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), toData)
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
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Contract.Transaction (submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(DatumInline))
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Contract.Wallet (getWalletAddresses)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Data.Array (head) as Array
import Data.BigInt (fromInt, toNumber) as BigInt
import Data.Int (ceil)
import Data.Map (singleton) as Map
import Mote (group, test)
import Racers (runRacers, withContract)
import Test.CardanoRacers.Helpers
  ( createRacersParamsHelper
  , initRacersStateWithAdminAndTreasury
  , fractionOfExUnitsCheck
  )
import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM PlutipTest Unit
suite = group "AssetRequest" do
  test "User requests asset by rarity" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(admin /\ treasury /\ user) -> do
        treasuryAddr <- withKeyWallet treasury
          $ liftedM "Could not get treasury address"
          $ Array.head
          <$> getWalletAddresses
        operatingAddress <- withKeyWallet admin
          $ liftedM "Could not get treasury address"
          $ Array.head
          <$> getWalletAddresses
        rp <- withKeyWallet admin createRacersParamsHelper
        runRacers rp do
          rs <- initRacersStateWithAdminAndTreasury (admin /\ treasury)
            (BigInt.fromInt 1_000_000)
            defaultAssetPrices
          assetRequestCs <-
            withContract (liftedM "Could not get currency symbol")
              $ Value.scriptCurrencySymbol
              <$> mkAssetRequestPolicy

          let rarities = [ Common ] -- , Rare, Epic ]

          depositScript <- validatorHash <$> mkDepositValidator

          for_ rarities $ \rarity -> do
            withContract (withKeyWallet user) do
              assetRequestTokenName <- lift
                $ liftContractM "Could not make required token names"
                $
                  (Value.mkTokenName <=< byteArrayFromAscii) (show rarity)

              let
                assetPrice = getAssetPrice rarity (unwrap rs).assetPrices
                depositAddress = scriptHashAddress depositScript
                  Nothing
                amountToTreasury = BigInt.fromInt <<< ceil
                  $ BigInt.toNumber assetPrice
                  * 0.75
                amountToOperating = BigInt.fromInt <<< ceil
                  $ BigInt.toNumber assetPrice
                  * 0.25
                assertions =
                  [ checkGainAtAddress' (label treasuryAddr "Treasury")
                      amountToTreasury
                  , checkGainAtAddress' (label operatingAddress "Operating")
                      amountToOperating
                  , checkTokenGainAtAddress' (label depositAddress "Deposit")
                      (assetRequestCs /\ assetRequestTokenName /\ one)
                  , fractionOfExUnitsCheck 0.75
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
            assetRequestCs <-
              withContract (liftedM "Could not get currency symbol")
                $ Value.scriptCurrencySymbol
                <$> mkAssetRequestPolicy

            withContract (withKeyWallet alice) do
              ownAddr <- lift $ liftedM "Could not get own address"
                $ Array.head
                <$> getWalletAddresses
              assetRequestTokenName <- lift
                $ liftContractM "Could not make required token names"
                $
                  (Value.mkTokenName <=< byteArrayFromAscii) (show rarity)
              depositScript <- validatorHash <$> mkDepositValidator

              let
                assetPrice = getAssetPrice rarity (unwrap rs).assetPrices
                incorrectPayments =
                  [ (0.74 /\ 0.25)
                  , (0.75 /\ 0.24)
                  , (0.76 /\ 0.24)
                  , (0.74 /\ 0.26)
                  ]

                dat = Datum $ toData $ AirdropAddressDatum
                  { airdropAddress: ownAddr }
                red = Redeemer $ toData $ MintRequestToken

                testIncorrectPayment (treasuryRatio /\ operatingRatio) = do
                  (_ /\ stateTxi /\ stateTxo) <- queryRacersState
                  let
                    amountToTreasury = BigInt.fromInt <<< ceil
                      $ BigInt.toNumber assetPrice
                      * treasuryRatio
                    amountToOperating = BigInt.fromInt <<< ceil
                      $ BigInt.toNumber assetPrice
                      * operatingRatio
                    treasuryVal = Value.lovelaceValueOf amountToTreasury
                    operatingVal = Value.lovelaceValueOf amountToOperating

                    lockedVal =
                      Value.singleton assetRequestCs assetRequestTokenName one

                    constraints :: Constraints.TxConstraints Void Void
                    constraints = Constraints.mustReferenceOutput stateTxi
                      <> paysToAddrConstraint (unwrap rs).treasuryAddress
                        treasuryVal
                      <> paysToAddrConstraint (unwrap rs).operatingAddress
                        operatingVal
                      <> Constraints.mustMintValueWithRedeemer red
                        ( Value.singleton assetRequestCs assetRequestTokenName
                            one
                        )
                      <> Constraints.mustPayToScript depositScript
                        dat
                        DatumInline
                        lockedVal

                    lookups :: Lookups.ScriptLookups Void
                    lookups = Lookups.mintingPolicy assetRequestPolicy
                      <> Lookups.unspentOutputs
                        (Map.singleton stateTxi stateTxo)

                  resE <- try $ lift $ submitTxFromConstraints lookups
                    constraints
                  resE `shouldSatisfy` isLeft

              traverse_ testIncorrectPayment incorrectPayments

  where
  walletUtxoDistr :: InitialUTxOs
  walletUtxoDistr =
    [ BigInt.fromInt 5_000_000
    , BigInt.fromInt 2_000_000_000
    , BigInt.fromInt 2_000_000_000
    ]

  defaultAssetPrices :: AssetPrices
  defaultAssetPrices = AssetPrices
    { common: BigInt.fromInt 5_000_000
    , rare: BigInt.fromInt 10_000_000
    , epic: BigInt.fromInt 20_000_000
    }
