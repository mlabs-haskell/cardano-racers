module Test.CardanoRacers.RaceRegistry (suite) where

import Contract.Prelude

import CardanoRacers.AssetRequest.Contract
  ( mkAssetRequestPolicy
  , requestAssetByRarity
  )
import CardanoRacers.Deposit.Contract (consumeAndRedeemRequests)
import CardanoRacers.Deposit.Validator (mkDepositValidator)
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
  , burnNitroConstraints
  , buyNitroContract
  , mintNitroAndPayToAddressContract
  , mkNitroPolicy
  )
import CardanoRacers.RaceRegistry.Contract
  ( confirmAssetSelection
  , findUtxoWithAvailableSlotToken
  , getRegistryEntriesFromOutput
  , initRace
  , mkRaceRegistryScript
  , queryRegistryUtxos
  , registerPositionInRace
  )
import CardanoRacers.RaceRegistry.Types
  ( RaceParticipant
  , RegistryDatum
  , RegistryEntry(AssetSelection, PendingSelection)
  , RegistryParams
  , RegistryRedeemer(Enroll, SelectAssets)
  )
import CardanoRacers.RaceSlot.Contract (mkRaceSlotPolicy)
import CardanoRacers.RaceSlot.Types (RaceHash, slotTokenName)
import CardanoRacers.RacersState.Contract (createRacersRefScriptOutputs)
import CardanoRacers.RacersState.Types (AssetPrices(AssetPrices))
import Contract.Address (PubKeyHash, scriptHashAddress)
import Contract.Metadata (mkCip25String)
import Contract.Monad (liftContractM, liftedM, throwContractError)
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , mintingPolicyHash
  , validatorHash
  )
import Contract.Test.Assert (ContractCheck, checkTokenGainAtAddress', label, runChecks)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints (DatumPresence(DatumInline))
import Contract.TxConstraints as Constraints
import Contract.Value (Value, geq, mkTokenName, negation, scriptCurrencySymbol)
import Contract.Value as Value
import Contract.Wallet (KeyWallet, getWalletAddresses, getWalletUtxos)
import Control.Apply (lift2)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.Contract.Wallet (ownPubKeyHashes)
import Data.Array (concat, drop, filter, head, null, take) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.FoldableWithIndex (findWithIndex)
import Data.Map
  ( Map
  , fromFoldable
  , keys
  , lookup
  , singleton
  , toUnfoldable
  , union
  , unions
  ) as Map
import Effect.Ref as Ref
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Racers (Racers, runRacers, withContract)
import Test.CardanoRacers.Helpers
  ( createRacersParamsHelper
  , initRacersStateWithAdminAndTreasury
  , fractionOfExUnitsCheck
  )
import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM PlutipTest Unit
suite = group "Race Registry" do
  test "Initializes Race" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ treasuryKey) -> do
        rp <- withKeyWallet adminKey do
          rp <- createRacersParamsHelper
          _ <- runRacers rp $ adminMintsNitroContract (BigInt.fromInt 1_000_000)
          pure rp

        runRacers rp do
          _ <- initRacersStateWithAdminAndTreasury (adminKey /\ treasuryKey)
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
            slots = 10
            utxoCount = 3
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
            assertions = [ checkTokenGainAtAddress' 
                            (label registryScriptAddress "RaceRegistry Address")
                            (slotSymbol /\ slotTokenName /\ (BigInt.fromInt slots))
                         , fractionOfExUnitsCheck 0.85
                         ]

          _ <-
            withContract
              (runChecks assertions <<< lift <<< withKeyWallet adminKey) $
              initRace raceHash nitroFee slots utxoCount

          pure unit
  test "Valid user race registration" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ treasuryKey /\ userKey) -> do
        rp <- withKeyWallet adminKey do
          rp <- createRacersParamsHelper
          _ <- runRacers rp $ adminMintsNitroContract (BigInt.fromInt 1_000_000)
          pure rp

        runRacers rp do
          _ <- initRacersStateWithAdminAndTreasury (adminKey /\ treasuryKey)
            (BigInt.fromInt 1_000_000)
            assetPrices

          raceHash <- lift
            $ liftContractM "could not convert hex string to bytearray"
            $ byteArrayFromAscii "TestRaceHash"

          (rgp /\ _) <- setupRegistryAndAssets adminKey userKey raceHash
            (BigInt.fromInt 20)
            2

          withContract (withKeyWallet userKey) do
            _ <- buyNitroContract $ BigInt.fromInt 100

            firstPkh <- lift $ liftedM "Could not get first own public key hash"
              $ ownPubKeyHashes
              <#> Array.head

            let assertions = [ fractionOfExUnitsCheck 0.50 
                             ]

            _ <- withContract (runChecks assertions <<< lift) $ 
                   registerInRaceWithFirstAvailableSlot rgp firstPkh

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
            _ <- initRacersStateWithAdminAndTreasury (adminKey /\ treasuryKey)
              (BigInt.fromInt 1_000_000)
              assetPrices

            raceHash <- lift
              $ liftContractM "could not convert hex string to bytearray"
              $ byteArrayFromAscii "TestRaceHash"

            (rgp /\ _) <- setupRegistryAndAssets adminKey userKey raceHash
              (BigInt.fromInt 20)
              1 -- only 1 slot

            withContract (withKeyWallet userKey) do
              _ <- buyNitroContract $ BigInt.fromInt 100

              firstPkh <- lift
                $ liftedM "Could not get first own public key hash"
                $ ownPubKeyHashes
                <#> Array.head

              _ <- registerInRaceWithFirstAvailableSlot rgp firstPkh
              failure <- try $ registerInRaceWithFirstAvailableSlot rgp firstPkh

              failure `shouldSatisfy` isLeft

  test "Valid registration and confirmation of assets" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ treasuryKey /\ userKey) -> do
        rp <- withKeyWallet adminKey do
          rp <- createRacersParamsHelper
          _ <- runRacers rp $ adminMintsNitroContract (BigInt.fromInt 1_000_000)
          pure rp

        runRacers rp do
          _ <- initRacersStateWithAdminAndTreasury (adminKey /\ treasuryKey)
            (BigInt.fromInt 1_000_000)
            assetPrices

          raceHash <- lift
            $ liftContractM "could not convert hex string to bytearray"
            $ byteArrayFromAscii "TestRaceHash"

          (rgp /\ mintedAssets) <- setupRegistryAndAssets adminKey userKey
            raceHash
            (BigInt.fromInt 20)
            2

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
            
            let assertions = [ fractionOfExUnitsCheck 0.75 
                             ]

            (slotTxi /\ _) <-
              withContract
                (liftedM "Could not find any valid UTxOs with free slot tokens")
                $
                  findUtxoWithAvailableSlotToken rgp
            -- Register in the race
            _ <- withContract (runChecks assertions <<< lift) $ registerPositionInRace rgp firstPkh slotTxi

            let
              raceParticipant = wrap
                { car: carTk, driver: driverTk, payoutAddress: firstAddr }
            -- Confirm participating assets
            _ <- withContract (runChecks assertions  <<<  lift) $ confirmAssetSelection rgp firstPkh raceParticipant

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
          _ <- initRacersStateWithAdminAndTreasury (adminKey /\ treasuryKey)
            (BigInt.fromInt 1_000_000)
            assetPrices

          raceHash <- lift
            $ liftContractM "could not convert hex string to bytearray"
            $ byteArrayFromAscii "TestRaceHash"

          (rgp /\ _) <- setupRegistryAndAssets adminKey userKey raceHash
            (BigInt.fromInt 20)
            2

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

            _ <- registerInRaceWithFirstAvailableSlot rgp firstPkh

            let
              raceParticipant = wrap
                { car: carTk, driver: driverTk, payoutAddress: firstAddr }
            -- Confirm participating assets
            failure <- try $ confirmAssetSelection rgp firstPkh raceParticipant

            failure `shouldSatisfy` isLeft
  test "Altering existing entries fails" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ treasuryKey /\ userKey) -> do
        rp <- withKeyWallet adminKey do
          rp <- createRacersParamsHelper
          pure rp

        userAddress <- withKeyWallet userKey
          $ liftedM "Could not get user address"
          $ Array.head
          <$> getWalletAddresses

        treasuryAddress <- withKeyWallet treasuryKey
          $ liftedM "Could not get user address"
          $ Array.head
          <$> getWalletAddresses

        withKeyWallet adminKey $ runRacers rp $ do
          _ <- adminMintsNitroContract $ BigInt.fromInt 1000
          _ <- mintNitroAndPayToAddressContract (BigInt.fromInt 100) userAddress
          _ <- mintNitroAndPayToAddressContract (BigInt.fromInt 100)
            treasuryAddress
          pure unit

        runRacers rp do
          _ <- initRacersStateWithAdminAndTreasury (adminKey /\ treasuryKey)
            (BigInt.fromInt 1_000_000)
            assetPrices

          raceHash <- lift
            $ liftContractM "could not convert hex string to bytearray"
            $ byteArrayFromAscii "TestRaceHash"

          (rgp /\ _) <- setupRegistryAndAssets adminKey userKey
            raceHash
            (BigInt.fromInt 20)
            2

          withContract (withKeyWallet userKey) do
            firstPkh <- lift $ liftedM "Could not get first own public key hash"
              $ ownPubKeyHashes
              <#> Array.head

            _ <- registerInRaceWithFirstAvailableSlot rgp firstPkh
            pure unit

          res <- try $ withContract (withKeyWallet treasuryKey) $
            enrollsAlteringExistingEntries rgp

          res `shouldSatisfy` isLeft
          pure unit
  test "User can split registry datum" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ attackerKey /\ userKey) -> do
        rp <- withKeyWallet adminKey do
          rp <- createRacersParamsHelper
          pure rp

        userAddress <- withKeyWallet userKey
          $ liftedM "Could not get user address"
          $ Array.head
          <$> getWalletAddresses

        attackerAddress <- withKeyWallet attackerKey
          $ liftedM "Could not get user address"
          $ Array.head
          <$> getWalletAddresses

        withKeyWallet adminKey $ runRacers rp $ do
          _ <- adminMintsNitroContract $ BigInt.fromInt 1000
          _ <- mintNitroAndPayToAddressContract (BigInt.fromInt 100) userAddress
          _ <- mintNitroAndPayToAddressContract (BigInt.fromInt 100)
            attackerAddress
          pure unit

        runRacers rp do
          _ <- initRacersStateWithAdminAndTreasury (adminKey /\ adminKey)
            (BigInt.fromInt 1_000_000)
            assetPrices

          raceHash <- lift
            $ liftContractM "could not convert hex string to bytearray"
            $ byteArrayFromAscii "TestRaceHash"

          (rgp /\ _) <- setupRegistryAndAssets adminKey userKey
            raceHash
            (BigInt.fromInt 20)
            3

          withContract (withKeyWallet userKey) do
            firstPkh <- lift $ liftedM "Could not get first own public key hash"
              $ ownPubKeyHashes
              <#> Array.head
            _ <- registerInRaceWithFirstAvailableSlot rgp firstPkh
            _ <- registerInRaceWithFirstAvailableSlot rgp firstPkh
            pure unit

          _ <- withContract (withKeyWallet attackerKey) $
            userSplitsRegistryDatum rgp

          withContract (withKeyWallet adminKey) do
            entries <- Array.concat <<< map (\(_ /\ _ /\ re) -> re)
              <<< Map.toUnfoldable
              <$> queryRegistryUtxos rgp
            entries `shouldSatisfy` ((==) 3 <<< length)
  test "User spending slot tokens fails" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ attackerKey /\ userKey) -> do
        rp <- withKeyWallet adminKey do
          rp <- createRacersParamsHelper
          pure rp

        userAddress <- withKeyWallet userKey
          $ liftedM "Could not get user address"
          $ Array.head
          <$> getWalletAddresses

        attackerAddress <- withKeyWallet attackerKey
          $ liftedM "Could not get user address"
          $ Array.head
          <$> getWalletAddresses

        withKeyWallet adminKey $ runRacers rp $ do
          _ <- adminMintsNitroContract $ BigInt.fromInt 1000
          _ <- mintNitroAndPayToAddressContract (BigInt.fromInt 100) userAddress
          _ <- mintNitroAndPayToAddressContract (BigInt.fromInt 100)
            attackerAddress
          pure unit

        runRacers rp do
          _ <- initRacersStateWithAdminAndTreasury (adminKey /\ adminKey)
            (BigInt.fromInt 1_000_000)
            assetPrices

          raceHash <- lift
            $ liftContractM "could not convert hex string to bytearray"
            $ byteArrayFromAscii "TestRaceHash"

          (rgp /\ _) <- setupRegistryAndAssets adminKey userKey
            raceHash
            (BigInt.fromInt 20)
            2

          res <- try $ withContract (withKeyWallet attackerKey) $
            userSpendsSlotTokens rgp
          res `shouldSatisfy` isLeft

          pure unit
  test "Asset selection fails if user does not sign" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(adminKey /\ attackerKey /\ userKey) -> do
        rp <- withKeyWallet adminKey do
          rp <- createRacersParamsHelper
          pure rp

        userAddress <- withKeyWallet userKey
          $ liftedM "Could not get user address"
          $ Array.head
          <$> getWalletAddresses

        attackerAddress <- withKeyWallet attackerKey
          $ liftedM "Could not get user address"
          $ Array.head
          <$> getWalletAddresses

        withKeyWallet adminKey $ runRacers rp $ do
          _ <- adminMintsNitroContract $ BigInt.fromInt 1000
          _ <- mintNitroAndPayToAddressContract (BigInt.fromInt 100) userAddress
          _ <- mintNitroAndPayToAddressContract (BigInt.fromInt 100)
            attackerAddress
          pure unit

        runRacers rp do
          _ <- initRacersStateWithAdminAndTreasury (adminKey /\ attackerKey)
            (BigInt.fromInt 1_000_000)
            assetPrices

          raceHash <- lift
            $ liftContractM "could not convert hex string to bytearray"
            $ byteArrayFromAscii "TestRaceHash"

          (rgp /\ mintedAssets) <- setupRegistryAndAssets adminKey attackerKey
            raceHash
            (BigInt.fromInt 20)
            2

          _ <- withContract (withKeyWallet attackerKey) do
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

            _ <- registerInRaceWithFirstAvailableSlot rgp firstPkh

            res <- try $ doesNotSignAssetSelection rgp
              ( wrap
                  { car: carTk
                  , driver: driverTk
                  , payoutAddress: firstAddr
                  }
              )

            res `shouldSatisfy` isLeft

            pure unit

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
        { name: unsafePartial $ fromJust $ mkCip25String
            "CommonCarRareDriverLoooNaaa"
        , assetType: CarType
        , imageUrl:
            "https://cdn.pixabay.com/photo/31/19/17/comic-2026591_1280.png"
        , description: "Cool car with lots of experience"
        , nitroAmount: BigInt.fromInt 100
        }
    , Rare /\
        { name: unsafePartial $ fromJust $ mkCip25String
            "RareDriverLooooooongNaaae"
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

  setupRegistryAndAssets
    :: KeyWallet
    -> KeyWallet
    -> RaceHash
    -> BigInt
    -> Int
    -> Racers (RegistryParams /\ Array GameAssetObject)
  setupRegistryAndAssets adminKey userKey raceHash nitroFee slots = do
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

        traverse_ createRacersRefScriptOutputs
          [ [ nitroScriptRef
            , assetRequestScriptRef
            ]
          , [ driverPolicyRef
            , carPolicyRef
            , depositAssetScriptRef
            ]
          ]

        pure unit

    let
      requests =
        [ Common
        , Rare
        , Epic
        ]

    counterRef <- liftEffect $ Ref.new 0

    let assertions :: forall a. Array (ContractCheck a)
        assertions = [ fractionOfExUnitsCheck 0.85
                     ]

    _ <- withContract (runChecks assertions <<< lift <<< withKeyWallet userKey) do
      traverse_ requestAssetByRarity requests

    assets <- withContract (runChecks assertions <<< lift <<< withKeyWallet adminKey) $
      consumeAndRedeemRequests
        5
        Nothing
        availableAssets
        (const $ liftEffect $ counterNonce counterRef)

    (rgp /\ _) <- withContract (withKeyWallet adminKey) $ initRace
      raceHash
      nitroFee
      slots
      1

    pure (rgp /\ assets)

enrollsAlteringExistingEntries :: RegistryParams -> Racers TransactionHash
enrollsAlteringExistingEntries rgp = do
  registryScript <- mkRaceRegistryScript rgp
  (slotTxi /\ slotTxo) <-
    withContract
      (liftedM "Could not find any valid UTxOs with free slot tokens") $
      findUtxoWithAvailableSlotToken rgp

  firstPkh <- lift $ liftedM "Could not get first own public key hash"
    $ ownPubKeyHashes
    <#> Array.head

  let
    newRegistry :: Array RegistryEntry
    newRegistry = map PendingSelection [ firstPkh ]

    previousValueAtRegistry :: Value
    previousValueAtRegistry = (unwrap (unwrap slotTxo).output).amount

    registryDatum = wrap $ toData (wrap newRegistry :: RegistryDatum)
    enrollRedeemer = wrap $ toData $ Enroll [ firstPkh ]

    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustSpendScriptOutput slotTxi enrollRedeemer
      <> Constraints.mustPayToScript (validatorHash registryScript)
        registryDatum
        DatumInline
        previousValueAtRegistry

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.unspentOutputs (Map.singleton slotTxi slotTxo)
      <> Lookups.validator registryScript

  (nitroConstraints /\ nitroLookups) <- burnNitroConstraints
    $ (unwrap rgp).nitroFee

  lift do
    txId <- submitTxFromConstraints (nitroLookups <> lookups)
      (constraints <> nitroConstraints)
    awaitTxConfirmed txId
    pure txId

registerInRaceWithFirstAvailableSlot
  :: RegistryParams -> PubKeyHash -> Racers TransactionHash
registerInRaceWithFirstAvailableSlot rgp firstPkh =
  do
    withContract
      (liftedM "Could not find any valid UTxOs with free slot tokens")
      (findUtxoWithAvailableSlotToken rgp)
    >>= registerPositionInRace rgp firstPkh
    <<< fst

userSpendsSlotTokens :: RegistryParams -> Racers TransactionHash
userSpendsSlotTokens rgp = do
  registryScript <- mkRaceRegistryScript rgp
  (slotTxi /\ slotTxo) <-
    withContract
      (liftedM "Could not find any valid UTxOs with free slot tokens") $
      findUtxoWithAvailableSlotToken rgp
  registryEntries <- lift
    $ liftContractM "Couldn't decode utxo datum registry entries"
    $ getRegistryEntriesFromOutput slotTxo
  firstPkh <- lift $ liftedM "Could not get first own public key hash"
    $ ownPubKeyHashes
    <#> Array.head

  let
    newRegistry :: Array RegistryEntry
    newRegistry = map PendingSelection [ firstPkh ] <> registryEntries

    previousValueAtRegistry :: Value
    previousValueAtRegistry = uncurry Value.singleton
      (unwrap rgp).slotAssetClass
      (BigInt.fromInt 1)

    registryDatum = wrap $ toData (wrap newRegistry :: RegistryDatum)
    enrollRedeemer = wrap $ toData $ Enroll [ firstPkh ]

    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustSpendScriptOutput slotTxi enrollRedeemer
      <> Constraints.mustPayToScript (validatorHash registryScript)
        registryDatum
        DatumInline
        previousValueAtRegistry

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.unspentOutputs (Map.singleton slotTxi slotTxo)
      <> Lookups.validator registryScript

  (nitroConstraints /\ nitroLookups) <- burnNitroConstraints
    $ (unwrap rgp).nitroFee

  lift do
    txId <- submitTxFromConstraints (nitroLookups <> lookups)
      (constraints <> nitroConstraints)
    awaitTxConfirmed txId
    pure txId

userSplitsRegistryDatum :: RegistryParams -> Racers TransactionHash
userSplitsRegistryDatum rgp = do
  registryScript <- mkRaceRegistryScript rgp
  (slotTxi /\ slotTxo) <-
    withContract
      (liftedM "Could not find any valid UTxOs with free slot tokens") $
      findUtxoWithAvailableSlotToken rgp
  registryEntries <- lift
    $ liftContractM "Couldn't decode utxo datum registry entries"
    $ getRegistryEntriesFromOutput slotTxo
  firstPkh <- lift $ liftedM "Could not get first own public key hash"
    $ ownPubKeyHashes
    <#> Array.head

  let
    newRegistry :: Array RegistryEntry
    newRegistry = map PendingSelection [ firstPkh ]

    singleSlotValue :: Value
    singleSlotValue = uncurry Value.singleton (unwrap rgp).slotAssetClass
      (BigInt.fromInt 1)

    existingEntriesSlotValue :: Value
    existingEntriesSlotValue = (unwrap (unwrap slotTxo).output).amount <>
      (negation singleSlotValue)

    existingRegistryDatum = wrap $ toData
      (wrap registryEntries :: RegistryDatum)
    singleRegistryDatum = wrap $ toData (wrap newRegistry :: RegistryDatum)

    enrollRedeemer = wrap $ toData $ Enroll [ firstPkh ]

    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustSpendScriptOutput slotTxi enrollRedeemer
      <> Constraints.mustPayToScript (validatorHash registryScript)
        existingRegistryDatum
        DatumInline
        existingEntriesSlotValue
      <> Constraints.mustPayToScript (validatorHash registryScript)
        singleRegistryDatum
        DatumInline
        singleSlotValue

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.unspentOutputs (Map.singleton slotTxi slotTxo)
      <> Lookups.validator registryScript

  (nitroConstraints /\ nitroLookups) <- burnNitroConstraints
    $ (unwrap rgp).nitroFee

  lift do
    txId <- submitTxFromConstraints (nitroLookups <> lookups)
      (constraints <> nitroConstraints)
    awaitTxConfirmed txId
    pure txId

doesNotSignAssetSelection
  :: RegistryParams -> RaceParticipant -> Racers TransactionHash
doesNotSignAssetSelection rgp participant = do
  let selections = [ participant ]
  utxos <- lift $ liftedM "Could not get wallet utxos" getWalletUtxos
  registryVal <- mkRaceRegistryScript rgp
  driverAssetSymbol <-
    withContract (liftedM "Could not get driver asset symbol")
      $ scriptCurrencySymbol
      <$> mkGameAssetPolicy DriverType
  carAssetSymbol <- withContract (liftedM "Could not get car asset symbol")
    $ scriptCurrencySymbol
    <$> mkGameAssetPolicy CarType
  registryUtxos <- queryRegistryUtxos rgp

  firstPkh <- lift $ liftedM "Could not get first own public key hash"
    $ ownPubKeyHashes
    <#> Array.head

  let
    txiEntries = map (\(txi /\ _ /\ entries) -> txi /\ entries) $
      Map.toUnfoldable registryUtxos

    -- | Allocate slots for the given race participants.
    -- Returns Nothing if there aren't enough slots for all participants.
    allocateSlots
      :: Array RaceParticipant
      -> Array (TransactionInput /\ Array RegistryEntry)
      -> Maybe (Array (TransactionInput /\ Array RegistryEntry))
    allocateSlots sels txisWithSlots =
      foldl folder (sels /\ []) txisWithSlots #
        ( \(selsLeft /\ finalTxis) ->
            if Array.null selsLeft then Just finalTxis else Nothing
        )
      where
      folder
        :: ( Array RaceParticipant /\ Array
               (TransactionInput /\ Array RegistryEntry)
           )
        -> (TransactionInput /\ Array RegistryEntry)
        -> ( Array RaceParticipant /\ Array
               (TransactionInput /\ Array RegistryEntry)
           )
      folder (remainingSels /\ updatedTxis) (txi /\ entries) =
        let
          slotsWithPkh = Array.filter (_ == (PendingSelection firstPkh)) entries
          otherSlots = Array.filter (_ /= (PendingSelection firstPkh)) entries
          allocatedSlots = map AssetSelection $ Array.take (length slotsWithPkh)
            remainingSels
          newRemainingSels = Array.drop (length slotsWithPkh) remainingSels
          remainingSlots = Array.drop (length allocatedSlots) slotsWithPkh
        in
          if Array.null allocatedSlots then newRemainingSels /\ updatedTxis -- No slots allocated; accumulator remains unchanged.
          else newRemainingSels /\
            ( updatedTxis <>
                [ (txi /\ (allocatedSlots <> otherSlots <> remainingSlots)) ]
            ) -- Update the accumulator with the allocated slots and the remaining slots.

  allocatedTxiWithEntries <- lift
    $ liftContractM
        "Could not allocate selections, not enought slots with given pkh"
    $ allocateSlots selections txiEntries
  allocationsWithOutputs <- lift $ liftContractM "Not possible" $ traverse
    ( \(txi /\ entries) -> Map.lookup txi registryUtxos <#>
        (\(txo /\ _) -> (txi /\ txo /\ entries))
    )
    allocatedTxiWithEntries

  assetUtxoMap <- do
    let
      findAssetForSelection sel =
        traverse findUtxoWithAsset [ (unwrap sel).driver, (unwrap sel).car ]

      findUtxoWithAsset tk =
        lift
          $ liftContractM
              ("Could not find asset: " <> (show tk) <> " in wallet utxos")
          $ map (lift2 Map.singleton _.index _.value)
          $ findWithIndex hasAsset utxos
        where
        hasAsset _ txo =
          let
            amount = (unwrap (unwrap txo).output).amount
            driverVal = Value.singleton driverAssetSymbol tk one
            carVal = Value.singleton carAssetSymbol tk one
          in
            amount `geq` driverVal || amount `geq` carVal

    Map.unions
      <<< Array.concat
      <$> traverse findAssetForSelection selections

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      foldMap Constraints.mustSpendPubKeyOutput (Map.keys assetUtxoMap)
        <> foldMap
          ( \(txi /\ txo /\ entries) ->
              Constraints.mustSpendScriptOutput txi
                (wrap $ toData $ SelectAssets)
                <> Constraints.mustPayToScript
                  (validatorHash registryVal)
                  (wrap $ toData (wrap entries :: RegistryDatum))
                  DatumInline
                  (unwrap (unwrap txo).output).amount
          )
          allocationsWithOutputs

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.unspentOutputs
        ( assetUtxoMap `Map.union` Map.unions
            ( map (\(txi /\ txo /\ _) -> Map.singleton txi txo)
                allocationsWithOutputs
            )
        )
        <> Lookups.validator registryVal

  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId
