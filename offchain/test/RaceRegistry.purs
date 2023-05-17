module Test.CardanoRacers.RaceRegistry (suite) where

import Contract.Prelude

import CardanoRacers.AssetRequest.Contract
  ( mkAssetRequestPolicy
  , requestAssetByRarity
  )
import CardanoRacers.Deposit.Contract
  ( consumeAndRedeemRequests
  , mkDepositValidator
  )
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types
  ( AssetOption
  , GameAssetObject
  , GameAssetType(CarType, DriverType)
  , Rarity(Common, Rare, Epic)
  )
import CardanoRacers.Helpers (counterNonce)
import CardanoRacers.Nitro.Contract
  ( adminMintsNitroContract
  , buyNitroContract
  , mkNitroPolicy
  )
import CardanoRacers.RaceSlot.Contract (mkRaceSlotPolicy)
import CardanoRacers.RaceSlot.Types (RaceHash, slotTokenName)
import CardanoRacers.RaceRegistry.Contract
  ( confirmParticipatingAssets
  , initRace
  , mkRaceRegistryScript
  , queryRegistryUtxos
  , registerPositionInRace
  , supplyRegistrySlots
  )
import CardanoRacers.RaceRegistry.Types
  ( RegistryEntry(AssetSelection, PendingSelection)
  , RegistryParams
  )
import CardanoRacers.RacersState.Contract (createRacersRefScriptOutput)
import CardanoRacers.RacersState.Types (AssetPrices(AssetPrices), RacersState)
import Contract.Address (scriptHashAddress)
import Contract.Log (logInfo')
import Contract.Metadata (mkCip25String)
import Contract.Monad (liftContractM, liftedM, throwContractError)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , mintingPolicyHash
  , validatorHash
  )
import Contract.Test.Assert (checkTokenGainAtAddress', label, runChecks)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Contract.Value (mkTokenName, scriptCurrencySymbol)
import Contract.Wallet (KeyWallet, getWalletAddresses)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.Contract.Wallet (ownPubKeyHashes)
import Data.Array (concat, head) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Map (Map, fromFoldable, toUnfoldable) as Map
import Effect.Ref as Ref
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Racers (Racers, runRacers, withContract)
import Test.CardanoRacers.Helpers
  ( createRacersParamsHelper
  , initRacersStateWithAdminAndTreasury
  )
import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM PlutipTest Unit
suite = group "Race Registry" do
  test "Initializes Race" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ treasuryKey /\ userKey) -> do
        rp <- withKeyWallet adminKey do
          rp <- createRacersParamsHelper
          _ <- runRacers rp $ adminMintsNitroContract (BigInt.fromInt 1_000_000)
          pure rp

        runRacers rp do
          st <- initRacersStateWithAdminAndTreasury (adminKey /\ treasuryKey)
            (BigInt.fromInt 1_000_000)
            assetPrices

          raceHash <- lift
            $ liftContractM "could not convert hex string to bytearray"
            $ byteArrayFromAscii "TestRaceHash"

          -- Registry Params 
          slotSymbol <-
            withContract (liftedM "could not get currency symbol from policy")
              $ scriptCurrencySymbol
              <$> mkRaceSlotPolicy raceHash
          nitroPolicyHash <- mintingPolicyHash <$> mkNitroPolicy
          driverAssetPolicyHash <- mintingPolicyHash <$> mkGameAssetPolicy
            DriverType
          carAssetPolicyHash <- mintingPolicyHash <$> mkGameAssetPolicy CarType

          let
            nitroFee = BigInt.fromInt 100
            slots = BigInt.fromInt 10
            rgp = wrap
              { slotAssetClass: slotSymbol /\ slotTokenName
              , nitroPolicyHash
              , driverAssetPolicyHash
              , carAssetPolicyHash
              , nitroFee
              }

          registryScriptAddress <- flip scriptHashAddress Nothing
            <<< validatorHash
            <$> mkRaceRegistryScript rgp

          let
            assertions = checkTokenGainAtAddress'
              (label registryScriptAddress "RaceRegistry Address")
              (slotSymbol /\ slotTokenName /\ slots)

          _ <-
            withContract
              (runChecks [ assertions ] <<< lift <<< withKeyWallet adminKey) $
              initRace raceHash nitroFee slots

          pure unit
  test "Valid user race registration" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ treasuryKey /\ userKey) -> do
        rp <- withKeyWallet adminKey do
          rp <- createRacersParamsHelper
          _ <- runRacers rp $ adminMintsNitroContract (BigInt.fromInt 1_000_000)
          pure rp

        runRacers rp do
          st <- initRacersStateWithAdminAndTreasury (adminKey /\ treasuryKey)
            (BigInt.fromInt 1_000_000)
            assetPrices

          raceHash <- lift
            $ liftContractM "could not convert hex string to bytearray"
            $ byteArrayFromAscii "TestRaceHash"

          (rgp /\ _) <- setupRegistryAndAssets adminKey userKey st raceHash
            (BigInt.fromInt 2)

          withContract (withKeyWallet userKey) do
            _ <- buyNitroContract $ BigInt.fromInt 100

            firstPkh <- lift $ liftedM "Could not get first own public key hash"
              $ ownPubKeyHashes
              <#> Array.head

            -- Register in the race
            _ <- registerPositionInRace rgp firstPkh

            -- Check if the user is registered with the correct assets
            us <- queryRegistryUtxos rgp
            let
              registeredEntries = Array.concat $ (snd <<< snd) <$>
                (Map.toUnfoldable us :: Array _)
            registeredEntries `shouldSatisfy` (elem $ PendingSelection firstPkh)
  test
    "Registration contract throws when not enough slots available at registry"
    do
      withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
        \(adminKey /\ treasuryKey /\ userKey) -> do
          rp <- withKeyWallet adminKey do
            rp <- createRacersParamsHelper
            _ <- runRacers rp $ adminMintsNitroContract
              (BigInt.fromInt 1_000_000)
            pure rp

          runRacers rp do
            st <- initRacersStateWithAdminAndTreasury (adminKey /\ treasuryKey)
              (BigInt.fromInt 1_000_000)
              assetPrices

            raceHash <- lift
              $ liftContractM "could not convert hex string to bytearray"
              $ byteArrayFromAscii "TestRaceHash"

            (rgp /\ _) <- setupRegistryAndAssets adminKey userKey st raceHash
              (BigInt.fromInt 1) -- only 1 slot

            withContract (withKeyWallet userKey) do
              _ <- buyNitroContract $ BigInt.fromInt 100

              firstPkh <- lift
                $ liftedM "Could not get first own public key hash"
                $ ownPubKeyHashes
                <#> Array.head

              -- Register in the race
              _ <- registerPositionInRace rgp firstPkh -- first registration passes
              failure <- try $ registerPositionInRace rgp firstPkh

              failure `shouldSatisfy` isLeft
  test "Valid registration and confirmation of assets" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ treasuryKey /\ userKey) -> do
        rp <- withKeyWallet adminKey do
          rp <- createRacersParamsHelper
          _ <- runRacers rp $ adminMintsNitroContract (BigInt.fromInt 1_000_000)
          pure rp

        runRacers rp do
          st <- initRacersStateWithAdminAndTreasury (adminKey /\ treasuryKey)
            (BigInt.fromInt 1_000_000)
            assetPrices

          raceHash <- lift
            $ liftContractM "could not convert hex string to bytearray"
            $ byteArrayFromAscii "TestRaceHash"

          (rgp /\ mintedAssets) <- setupRegistryAndAssets adminKey userKey st
            raceHash
            (BigInt.fromInt 2)

          withContract (withKeyWallet userKey) do
            _ <- buyNitroContract $ BigInt.fromInt 100

            firstPkh <- lift $ liftedM "Could not get first own public key hash"
              $ ownPubKeyHashes
              <#> Array.head
            firstAddr <- lift $ liftedM "Could not get first address"
              $ getWalletAddresses
              <#> Array.head

            (carTk /\ driverTk) <- lift $ liftContractM
              "Could not get driver and car"
              do
                d <- _.tokenName <$> find ((_ == DriverType) <<< _.assetType)
                  mintedAssets
                c <- _.tokenName <$> find ((_ == CarType) <<< _.assetType)
                  mintedAssets
                pure $ c /\ d

            -- Register in the race
            _ <- registerPositionInRace rgp firstPkh

            let
              raceParticipant = wrap
                { car: carTk, driver: driverTk, payoutAddress: firstAddr }
            -- Confirm participating assets
            _ <- confirmParticipatingAssets rgp firstPkh raceParticipant

            -- Check if the user is registered with the correct assets
            us <- queryRegistryUtxos rgp
            let
              registeredEntries = Array.concat $ (snd <<< snd) <$>
                (Map.toUnfoldable us :: Array _)
            registeredEntries `shouldSatisfy`
              (elem $ AssetSelection raceParticipant)

            pure unit
  test "Asset selection fails when wallet does not contain assets" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ treasuryKey /\ userKey) -> do
        rp <- withKeyWallet adminKey do
          rp <- createRacersParamsHelper
          _ <- runRacers rp $ adminMintsNitroContract (BigInt.fromInt 1_000_000)
          pure rp

        runRacers rp do
          st <- initRacersStateWithAdminAndTreasury (adminKey /\ treasuryKey)
            (BigInt.fromInt 1_000_000)
            assetPrices

          raceHash <- lift
            $ liftContractM "could not convert hex string to bytearray"
            $ byteArrayFromAscii "TestRaceHash"

          (rgp /\ _) <- setupRegistryAndAssets adminKey userKey st raceHash
            (BigInt.fromInt 2)

          withContract (withKeyWallet userKey) do
            _ <- buyNitroContract $ BigInt.fromInt 100

            firstPkh <- lift $ liftedM "Could not get first own public key hash"
              $ ownPubKeyHashes
              <#> Array.head
            firstAddr <- lift $ liftedM "Could not get first address"
              $ getWalletAddresses
              <#> Array.head

            (carTk /\ driverTk) <- lift $ liftContractM
              "Could not get driver and car token names"
              do
                d <- mkTokenName <=< byteArrayFromAscii $ "BadDriverAsset"
                c <- mkTokenName <=< byteArrayFromAscii $ "BadCarAsset"
                pure $ c /\ d

            -- Register in the race
            _ <- registerPositionInRace rgp firstPkh

            let
              raceParticipant = wrap
                { car: carTk, driver: driverTk, payoutAddress: firstAddr }
            -- Confirm participating assets
            failure <- try $ confirmParticipatingAssets rgp firstPkh
              raceParticipant

            failure `shouldSatisfy` isLeft
  test "Bad registry contract interacition" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ treasuryKey /\ userKey) -> do
        rp <- withKeyWallet adminKey do
          rp <- createRacersParamsHelper
          _ <- runRacers rp $ adminMintsNitroContract (BigInt.fromInt 1_000_000)
          _ <- throwContractError "Not implemented"
          pure rp
        {- Contract test cases:
          - User attempts to modify (tamper with slots tokens) value at registry
          - User alters existing registry entries in datum
          - User does not burn enough Nitro
          - User exceeds max number of entries in registry in a utxo
          - User does not sign when confirming asset selection
          - Asset selection Tx does not contain all required inputs
        -}
        pure unit

  test "playground" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ treasuryKey /\ userKey) -> do
        rp <- withKeyWallet adminKey do
          rp <- createRacersParamsHelper
          _ <- runRacers rp $ adminMintsNitroContract (BigInt.fromInt 1_000_000)
          pure rp

        runRacers rp do
          st <- initRacersStateWithAdminAndTreasury (adminKey /\ treasuryKey)
            (BigInt.fromInt 1_000_000)
            assetPrices

          raceHash <- lift
            $ liftContractM "could not convert hex string to bytearray"
            $ byteArrayFromAscii "TestRaceHash"

          (rgp /\ mintedAssets) <- setupRegistryAndAssets adminKey userKey st
            raceHash
            (BigInt.fromInt 2)

          -- logInfo' "initialized race"

          -- _ <- withContract (withKeyWallet adminKey)
          --   $ lift (liftedM "asdf" $ ownPubKeyHashes <#> Array.head)
          --   >>= registerPositionInRace rgp

          -- logInfo' $ "registreing admin"

          withContract (withKeyWallet userKey) do
            _ <- buyNitroContract $ BigInt.fromInt 100

            firstPkh <- lift $ liftedM "Could not get first own public key hash"
              $ ownPubKeyHashes
              <#> Array.head
            firstAddr <- lift $ liftedM "Could not get first address"
              $ getWalletAddresses
              <#> Array.head

            testTk <- lift $ liftContractM "not token name" $
              (mkTokenName <=< byteArrayFromAscii) "test"

            (carTk /\ driverTk) <- lift $ liftContractM
              "Could not get driver and car"
              do
                d <- _.tokenName <$> find ((_ == DriverType) <<< _.assetType)
                  mintedAssets
                c <- _.tokenName <$> find ((_ == CarType) <<< _.assetType)
                  mintedAssets
                pure $ c /\ d

            _ <- registerPositionInRace rgp firstPkh
            _ <- registerPositionInRace rgp firstPkh
            r <- try $ registerPositionInRace rgp firstPkh

            when (not $ isLeft r) $
              logInfo' "expected error, registration passed"

            logInfo' $ "registering user"

            _ <- confirmParticipatingAssets rgp firstPkh
              ( wrap
                  { car: carTk
                  , driver: driverTk
                  , payoutAddress: firstAddr
                  }
              )

            us <- queryRegistryUtxos rgp
            logInfo' $ show $ (snd <<< snd) <$> (Map.toUnfoldable us :: Array _)

            withContract (withKeyWallet adminKey) do
              -- _ <- collectRegistryScriptLeftovers raceHash rgp
              -- us' <- queryRegistryUtxos rgp
              -- logInfo' $ show $ (snd <<< snd) <$> (Map.toUnfoldable us' :: Array _)

              _ <- supplyRegistrySlots raceHash rgp $ BigInt.fromInt 10
              pure unit

            _ <- registerPositionInRace rgp firstPkh

            us <- queryRegistryUtxos rgp
            logInfo' $ show $ (snd <<< snd) <$> (Map.toUnfoldable us :: Array _)

            pure unit

  where
  walletUtxoDistr :: InitialUTxOs
  walletUtxoDistr =
    [ BigInt.fromInt 5_000_000
    , BigInt.fromInt 2_000_000_000
    ]

  assetPrices :: AssetPrices
  assetPrices = AssetPrices
    { common: BigInt.fromInt 5000000
    , rare: BigInt.fromInt 1000000
    , epic: BigInt.fromInt 20000000
    }

  availableAssets :: Map.Map Rarity AssetOption
  availableAssets = Map.fromFoldable
    [ Common /\
        { name: unsafePartial $ fromJust $ mkCip25String "CommonCar"
        , assetType: CarType
        , imageUrl:
            "https://cdn.pixabay.com/photo/31/19/17/comic-2026591_1280.png"
        , description: "Cool car with lots of experience"
        , nitroAmount: BigInt.fromInt 100
        }
    , Rare /\
        { name: unsafePartial $ fromJust $ mkCip25String "RareDriver"
        , assetType: DriverType
        , imageUrl:
            "https://cdn.pixabay.com/photo/31/19/17/comic-2026591_1280.png"
        , description: "Cool car with lots of experience"
        , nitroAmount: BigInt.fromInt 200
        }
    , Epic /\
        { name: unsafePartial $ fromJust $ mkCip25String "EpicCar"
        , assetType: CarType
        , imageUrl:
            "https://cdn.pixabay.com/photo/31/19/17/comic-2026591_1280.png"
        , description: "Cool car with lots of experience"
        , nitroAmount: BigInt.fromInt 300
        }
    ]

  -- initRacersAndMintAssetsHelper
  --   :: KeyWallet
  --   -> KeyWallet
  --   -> BigInt
  --   -> AssetPrices
  --   -> Racers (RegistryParams /\ Array GameAssetObject)

  setupRegistryAndAssets
    :: KeyWallet
    -> KeyWallet
    -> RacersState
    -> RaceHash
    -> BigInt
    -> Racers (RegistryParams /\ Array GameAssetObject)
  setupRegistryAndAssets adminKey userKey st raceHash slots = do
    withContract (withKeyWallet adminKey)
      do
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

        _ <- createRacersRefScriptOutput nitroScriptRef
        _ <- createRacersRefScriptOutput assetRequestScriptRef
        _ <- createRacersRefScriptOutput driverPolicyRef
        _ <- createRacersRefScriptOutput carPolicyRef
        _ <- createRacersRefScriptOutput depositAssetScriptRef

        pure unit

    let
      requests =
        [ Common
        , Rare
        , Epic
        ]

    counterRef <- liftEffect $ Ref.new 0

    _ <- withContract (withKeyWallet userKey) do
      traverse_ requestAssetByRarity requests

    assets <- withContract (withKeyWallet adminKey) $
      consumeAndRedeemRequests
        5
        availableAssets
        (counterNonce counterRef)
        st

    (rgp /\ _) <- withContract (withKeyWallet adminKey) $ initRace
      raceHash
      (BigInt.fromInt 10)
      slots

    pure (rgp /\ assets)
