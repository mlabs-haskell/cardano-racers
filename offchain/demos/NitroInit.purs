module CardanoRacers.NitroInit where

import Contract.Prelude

import Aeson (JsonDecodeError, decodeJsonString, encodeAeson)
import CardanoRacers.AssetRequest.Contract
  ( mkAssetRequestPolicy
  , requestAssetByRarity
  )
import CardanoRacers.Common.Types (RacersParams(..))
import CardanoRacers.Deposit.Contract
  ( consumeAndRedeemRequests
  , mkDepositValidator
  , queryRequestsWithAirdropAddress
  )
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types
  ( AssetOption
  , GameAssetAttributes(..)
  , GameAssetObject
  , GameAssetType(..)
  , Rarity(Common, Rare, Epic)
  )
import CardanoRacers.Helpers (counterNonce, getTxoWithRefScrpt)
import CardanoRacers.Nitro.Contract
  ( adminMintsNitroContract
  , botMintsNitroContract
  , buyNitroContract
  , mkNitroPolicy
  )
import CardanoRacers.Nitro.Helpers (createRacersParams)
import CardanoRacers.RacePosition.Contract (mkRacePositionPolicy)
import CardanoRacers.RacePosition.Types (slotTokenName)
import CardanoRacers.RaceRegistry.Contract
  ( collectRegistryScriptLeftovers
  , confirmParticipatingAssets
  , initRace
  , queryRegistryUtxos
  , registerPositionInRace
  )
import CardanoRacers.RaceRegistry.Types
  ( RaceParticipant(..)
  , RegistryEntry(..)
  , RegistryParams(..)
  )
import CardanoRacers.RacersState.Contract
  ( createRacersRefScriptOutput
  , initRacersStateContract
  , modifyRacersStateContract
  , queryRacersState
  )
import CardanoRacers.RacersState.Types (RacersState(..))
import Contract.Address
  ( Address
  , addressFromBech32
  , addressToBech32
  , scriptHashAddress
  )
import Contract.AssocMap (empty, insert) as AssocMap
import Contract.Config
  ( NetworkId(..)
  , PrivatePaymentKeySource(..)
  , WalletSpec(..)
  , testnetConfig
  , testnetEternlConfig
  )
import Contract.Credential (Credential(PubKeyCredential, ScriptCredential))
import Contract.Hashing (publicKeyHash)
import Contract.Log (logError', logInfo')
import Contract.Metadata (mkCip25String, unCip25String)
import Contract.Monad
  ( Contract
  , liftContractE
  , liftContractM
  , liftedM
  , runContract
  , throwContractError
  )
import Contract.PlutusData (unitDatum)
import Contract.Prim.ByteArray
  ( ByteArray(..)
  , byteArrayFromAscii
  , byteArrayToIntArray
  , hexToByteArray
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(..)
  , ValidatorHash
  , mintingPolicyHash
  , validatorHash
  )
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value
  ( TokenName
  , Value
  , adaSymbol
  , flattenNonAdaAssets
  , flattenValue
  , getTokenName
  , lovelaceValueOf
  , mkTokenName
  , mpsSymbol
  , scriptCurrencySymbol
  )
import Contract.Value as Value
import Contract.Wallet (PrivatePaymentKey(..), privateKeyFromBytes)
import Contract.Wallet
  ( getWalletAddress
  , getWalletAddresses
  , getWalletBalance
  , getWalletUtxos
  )
import Contract.Wallet.Key (publicKeyFromPrivateKey)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (catchError, liftMaybe, throwError)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse)
import Control.Promise (Promise, fromAff, toAffE)
import Ctl.Internal.Contract.Wallet (ownPubKeyHashes)
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Plutus.Conversion (toPlutusAddress)
import Ctl.Internal.Serialization.Address
  ( enterpriseAddress
  , enterpriseAddressToAddress
  , keyHashCredential
  )
import Ctl.Internal.Serialization.Types (PrivateKey)
import Ctl.Internal.Types.RawBytes (RawBytes(RawBytes))
import Data.Array (concat, filter, head, mapMaybe) as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Char (fromCharCode)
import Data.FoldableWithIndex (foldWithIndexM, foldrWithIndex)
import Data.Int (round, toNumber)
import Data.Map (Map)
import Data.Map (empty, fromFoldable, insert, lookup, toUnfoldable) as Map
import Data.String (Pattern(..), split, stripPrefix)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Pattern (Pattern(Pattern))
import Effect.Aff (error)
import Effect.Aff.Compat (EffectFn1, EffectFn2, mkEffectFn2)
import Effect.Ref as Ref
import Foreign.Object (Object)
import Foreign.Object (empty, insert, lookup) as Object
import Partial.Unsafe (unsafePartial)
import Racers (Racers, runRacers, withContract)

foreign import setupListeners :: Listeners -> Effect Unit
foreign import _getParams :: Effect String
foreign import promptFor :: String -> Effect String
foreign import _getSelectedActor :: MaybeFfiHelper -> Effect (Maybe String)

type NitroWallet =
  { name :: String, address :: String, balance :: Array (Array String) }

type WalletStates =
  { wallets :: Array NitroWallet
  , depositScript :: Array NitroWallet
  -- Array since its simpler to work with on js side
  }

type Listeners =
  { refreshWallet :: Effect (Promise WalletStates)
  , refreshState :: Effect (Promise String)
  , refreshRequests :: Effect (Promise String)
  , refreshRace :: Effect (Promise String)

  -- general
  , initRacersState :: Effect (Promise String)
  , modifyRacersState :: Effect (Promise TransactionHash)
  , resetTokens :: Effect (Promise (Array TransactionHash))

  --nitro
  , mintNitro :: Effect (Promise TransactionHash)
  , userBuyNitro :: Effect (Promise TransactionHash)

  -- assets
  , makeAssetRequest :: Effect (Promise TransactionHash)
  , redeemRequests :: Effect (Promise (Array GameAssetObject))
  , getAvailableAssets ::
      Effect
        ( Promise
            ( Object
                { name :: String
                , assetType :: String
                , description :: String
                , imageUrl :: String
                }
            )
        )
  , setAssetOption ::
      EffectFn2 String
        { name :: String
        , assetType :: String
        , description :: String
        , imageUrl :: String
        , nitroAmount :: String
        }
        Unit

  --  races
  , createRace :: Effect (Promise TransactionHash)
  , registerInRace :: Effect (Promise TransactionHash)
  , raceWithAssets :: Effect (Promise String)
  , closeRace :: Effect (Promise TransactionHash)
  , closeRaceManual :: Effect (Promise TransactionHash)
  }

keys :: Array (Tuple String String)
keys =
  [ "Admin" /\
      "582043a451628918e1a04e35fc638850d05885bc4d13dd72692194ba82545d7e57ab"
  , "Bot" /\
      "58201590a76582c9fb63fbf18df5399043290077083babc6d65e7f68633a682d62d3"
  , "Treasury" /\
      "5820a57db0c6cc5c10e066f6b6cde609a25c81461a49431d6e11d01e408ea5f6135e"
  , "User" /\
      "582050389c06908083d9d9a559c0ca8d74e364a4016b2174710215e18c2cfe6eeca6"
  ]

garbageAddressStr :: String
garbageAddressStr =
  "addr_test1qzzlcml07a2jsj6dmvpkgnrzr46jf9xkzysz432qrpksycrug3qthh2pspp2cnx244zqt6e4nnxva3nzgclw2pymkfesf07qg0"

main :: Effect Unit
main = do
  cRef <- Ref.new 0
  assetRef <- Ref.new initialAvailableAssets
  mintedAssetsRef <- Ref.new Map.empty
  setupListeners
    { refreshWallet: refreshWallets
    , initRacersState
    , refreshState
    , refreshRequests
    , mintNitro
    , modifyRacersState
    , userBuyNitro
    , resetTokens
    , makeAssetRequest
    , redeemRequests: redeemRequests cRef assetRef mintedAssetsRef
    , getAvailableAssets: getAvailableAssets assetRef
    , setAssetOption: mkEffectFn2 $ setAssetOption assetRef
    , createRace
    , registerInRace
    , refreshRace
    , raceWithAssets: raceWithAssets mintedAssetsRef
    , closeRaceManual
    , closeRace: closeRace mintedAssetsRef
    }
  pure unit

getSelectedActor :: Effect String
getSelectedActor = liftMaybe (error "actor not selected") =<< _getSelectedActor
  maybeFfiHelper

getAvailableAssets
  :: Ref.Ref (Map Rarity AssetOption)
  -> Effect
       ( Promise
           ( Object
               { name :: String
               , assetType :: String
               , description :: String
               , imageUrl :: String
               }
           )
       )
getAvailableAssets r = fromAff do
  assets <- liftEffect $ Ref.read r
  pure $ foldrWithIndex
    ( \rarity option obj -> Object.insert (show rarity)
        { name: unCip25String option.name
        , assetType: show option.assetType
        , description: option.description
        , imageUrl: option.imageUrl
        }
        obj
    )
    Object.empty
    assets

getParams :: Effect RacersParams
getParams = do
  paramsStr <- _getParams
  either (throwError <<< error <<< show) pure $ decodeJsonString paramsStr

setAssetOption
  :: Ref.Ref (Map Rarity AssetOption)
  -> String
  -> { name :: String
     , assetType :: String
     , description :: String
     , imageUrl :: String
     , nitroAmount :: String
     }
  -> Effect Unit
setAssetOption r rarityStr option = do
  availableAssets <- Ref.read r
  rarity <- liftMaybe (error "invalid rarity") $ rarityFromString rarityStr
  assetType <- liftMaybe (error "invalid asset type") $ case option.assetType of
    "CarType" -> Just CarType
    "DriverType" -> Just DriverType
    _ -> Nothing
  cip25Name <- liftMaybe (error "could not create cip25 string") $ mkCip25String
    option.name
  nitroAmount <- liftMaybe (error "could not convert nitro amount to bigint") $
    BigInt.fromString option.nitroAmount
  let
    assetOption =
      { name: cip25Name
      , assetType
      , description: option.description
      , imageUrl: option.imageUrl
      , nitroAmount: nitroAmount
      }
  Ref.write (Map.insert rarity assetOption availableAssets) r

refreshWallets :: Effect (Promise WalletStates)
refreshWallets = fromAff do
  -- rp <- liftEffect $ getParams
  wallets <- parTraverse refreshWallet keys
  depositBalance <- toAffE $ withActor "Admin" $
    ( flip catchError (\e -> logError' ("Deposit script: " <> show e) $> []) do
        rp <- liftEffect getParams
        runRacers rp do
          (rs /\ _) <- queryRacersState
          let depAddr = scriptHashAddress (unwrap rs).depositScript Nothing
          depAddrString <- lift $ addressToBech32 depAddr
          utxos <- lift $ utxosAt depAddr
          let
            totalValue = foldMap (_.amount <<< unwrap <<< _.output <<< unwrap)
              utxos
          pure $
            [ { name: ""
              , address: depAddrString
              , balance: prettifyBalance totalValue
              }
            ]
    )
  pure
    { wallets
    , depositScript: depositBalance
    }

createRefScripts :: Racers Unit
createRefScripts = do
  assetRequestScriptRef <- mkAssetRequestPolicy >>= case _ of
    PlutusMintingPolicy s -> pure s
    _ -> lift $ throwContractError "Not plutus script"
  gameAssetScriptRef <- mkGameAssetPolicy >>= case _ of
    PlutusMintingPolicy s -> pure s
    _ -> lift $ throwContractError "Not plutus script"
  nitroPolicyScriptRef <- mkNitroPolicy >>= case _ of
    PlutusMintingPolicy s -> pure s
    _ -> lift $ throwContractError "Not plutus script"

  depositAssetScriptRef <- unwrap <$> mkDepositValidator

  _ <- createRacersRefScriptOutput assetRequestScriptRef
  _ <- createRacersRefScriptOutput gameAssetScriptRef
  _ <- createRacersRefScriptOutput nitroPolicyScriptRef
  _ <- createRacersRefScriptOutput depositAssetScriptRef
  pure unit

initRacersState :: Effect (Promise String)
initRacersState = do
  nitroPriceStr <- promptFor "Enter nitro price in lovelace"
  withActor "Admin" do
    nitroPrice <- liftContractM "couldn't convert to bigint" $ BigInt.fromString
      nitroPriceStr
    utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
    (txi /\ _) <- liftContractM "Could not get first utxo" $ Array.head $
      Map.toUnfoldable utxos
    rp <- createRacersParams txi
    ownAddr <- liftedM "Could not get wallet address" getWalletAddress
    treasuryAddr <- liftContractM "could not get address" $ actorAddress
      "Treasury"
    botAddr <- liftContractM "could not get address" $ actorAddress "Bot"
    depositScriptHash <- depositScriptHashHelper rp
    let
      assetPrices = wrap $
        { common: BigInt.fromInt 5_000_000
        , rare: BigInt.fromInt 10_000_000
        , epic: BigInt.fromInt 20_000_000

        }
      nitroState = RacersState
        { nitroPrice: nitroPrice
        , treasuryAddress: treasuryAddr
        , operatingAddress: ownAddr
        , depositScript: depositScriptHash
        , assetPrices
        }
    runRacers rp do
      void $ initRacersStateContract nitroState
      createRefScripts
    sendToBot rp botAddr
    pure $ show $ encodeAeson rp
  where
  sendToBot :: RacersParams -> Address -> Contract Unit
  sendToBot rp botAddr = do
    let
      constraints :: Constraints.TxConstraints Void Void
      constraints = paysToAddrConstraint botAddr
        (uncurry Value.singleton (unwrap rp).botToken $ BigInt.fromInt 1)

      lookups :: Lookups.ScriptLookups Void
      lookups = mempty

    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure unit

mintNitro :: Effect (Promise TransactionHash)
mintNitro = do
  rp <- getParams
  amo <- promptFor "Enter NITRO amount"
  actor <- getSelectedActor
  a <- liftMaybe (error "couldn't convert amount") $ BigInt.fromString amo
  case actor of
    "Admin" -> withActor "Admin" $ runRacers rp $ adminMintsNitroContract a
    "Bot" -> withActor "Bot" $ runRacers rp $ botMintsNitroContract a
    _ -> throwError $ error $ "Actor is not appropriate admin or bot" <> actor

makeAssetRequest :: Effect (Promise TransactionHash)
makeAssetRequest = do
  rp <- getParams
  rarityStr <- promptFor "Enter requested rarity class"
  let
    contract = runRacers rp do
      rarity <- lift $ liftContractM "Unrecognized rarity class" $
        rarityFromString
          rarityStr
      requestAssetByRarity rarity
  actor <- getSelectedActor
  if actor == "User" then
    withActor "User" contract
  else fromAff $ runContract testnetEternlConfig contract

redeemRequests
  :: Ref.Ref Int
  -> Ref.Ref (Map Rarity AssetOption)
  -> Ref.Ref (Map TokenName GameAssetObject)
  -> Effect (Promise (Array GameAssetObject))
redeemRequests cRef assetRef mintedAssetRef = do
  rp <- getParams
  availableAssets <- Ref.read assetRef
  withActor "Bot" $ runRacers rp do
    (rs /\ _) <- queryRacersState
    logInfo' "Querying racers state"
    -- depRefScriptTxi <- queryOrCreateDepositReferenceScript rp
    -- depRefScriptTxo <- getTxoWithRefScrpt depRefScriptTxi
    logInfo' "Queried deposit reference script"
    assetObjs <- consumeAndRedeemRequests 3 availableAssets (counterNonce cRef)
      rs -- Nothing
    _ <- liftEffect $ traverse
      (\ao -> Ref.modify (Map.insert ao.tokenName ao) mintedAssetRef)
      assetObjs
    pure $ assetObjs

createRace :: Effect (Promise TransactionHash)
createRace = do
  rp <- getParams
  withActor "Bot" $ runRacers rp do
    (raceHash /\ nitroFee) <- promptRaceParams
    raceSlotsStr <- liftEffect $ promptFor "Enter number of participants:"
    raceSlots <- lift $ liftContractM "couldn't convert to bigint" $
      BigInt.fromString raceSlotsStr
    snd <$> initRace raceHash nitroFee raceSlots

registerInRace :: Effect (Promise TransactionHash)
registerInRace = do
  rp <- getParams
  let
    contract = runRacers rp do
      (raceHash /\ nitroFee) <- promptRaceParams
      rgp <- createRaceRegistryParams raceHash nitroFee
      firstPkh <- lift $ liftedM "Could not get first own public key hash"
        $ ownPubKeyHashes
        <#> Array.head
      fst <$> registerPositionInRace rgp firstPkh

  actor <- getSelectedActor
  if actor == "User" then
    withActor "User" contract
  else fromAff $ runContract testnetEternlConfig contract

raceWithAssets
  :: Ref.Ref (Map TokenName GameAssetObject) -> Effect (Promise String)
raceWithAssets mintedAssetsRef = do
  rp <- getParams
  let
    contract = runRacers rp do
      (raceHash /\ nitroFee) <- promptRaceParams
      driverStr <- liftEffect $ promptFor "Enter driver tokenname: "
      carStr <- liftEffect $ promptFor "Enter car tokenname: "
      driverTk <- lift $ liftContractM "invalid driver name" $ mkTokenName
        <=< byteArrayFromAscii
        $ driverStr
      carTk <- lift $ liftContractM "invalid car name" $ mkTokenName
        <=< byteArrayFromAscii
        $ carStr
      rgp <- createRaceRegistryParams raceHash nitroFee

      firstPkh <- lift $ liftedM "Could not get first own public key hash"
        $ ownPubKeyHashes
        <#> Array.head
      firstAddr <- lift $ liftedM "Could not get first address"
        $ getWalletAddresses
        <#> Array.head

      _ <- confirmParticipatingAssets rgp firstPkh $ wrap
        { driver: driverTk, car: carTk, payoutAddress: firstAddr }

      ma <- liftEffect $ Ref.read mintedAssetsRef
      res <- lift $ liftContractM "could not get lap time" $ do
        da <- _.attributes <$> Map.lookup driverTk ma
        ca <- _.attributes <$> Map.lookup carTk ma
        daSum <- case da of
          DriverAttrs das -> pure
            $
              ( \daa -> daa.aggression + daa.experience + daa.luck +
                  daa.reflexes
              )
            $ unwrap das
          _ -> Nothing
        caSum <- case ca of
          CarAttrs cas -> pure
            $
              ( \caa -> caa.acceleration + caa.cornering + caa.aerodynamics +
                  caa.topSpeed
              )
            $ unwrap cas
          _ -> Nothing
        pure $ caSum + daSum

      pure $ "Lap time: " <> show res

  actor <- getSelectedActor
  if actor == "User" then
    withActor "User" contract
  else fromAff $ runContract testnetEternlConfig contract

sumAttrs :: GameAssetObject -> GameAssetObject -> Maybe BigInt
sumAttrs d c = (+) <$> daSum <*> caSum
  where
  daSum = case d.attributes of
    DriverAttrs das -> pure
      $
        ( \daa -> daa.aggression + daa.experience + daa.luck +
            daa.reflexes
        )
      $ unwrap das
    _ -> Nothing
  caSum = case c.attributes of
    CarAttrs cas -> pure
      $
        ( \caa -> caa.acceleration + caa.cornering + caa.aerodynamics +
            caa.topSpeed
        )
      $ unwrap cas
    _ -> Nothing

closeRace
  :: Ref.Ref (Map TokenName GameAssetObject) -> Effect (Promise TransactionHash)
closeRace mar = do
  rp <- getParams
  ma <- Ref.read mar
  withActor "Bot" $ runRacers rp do
    (raceHash /\ nitroFee) <- promptRaceParams
    rgp <- createRaceRegistryParams raceHash nitroFee
    rewardAmount <- withContract (liftedM "Could not convert reward to BigInt")
      $ BigInt.fromString
      <$> liftEffect (promptFor "Enter race rewards")
    entries <- Array.concat <<< map (snd <<< snd) <<< Map.toUnfoldable <$>
      queryRegistryUtxos rgp

    winner <- lift $ liftContractM "could not get winner"
      $ maximumBy
          ( comparing
              ( fromMaybe (BigInt.fromInt 0) <<<
                  (uncurry sumAttrs <=< getAssetObjects ma)
              )
          )
      $ Array.mapMaybe castParticipant entries

    _ <- collectRegistryScriptLeftovers rgp

    lift (sendPayout rewardAmount (unwrap winner).payoutAddress)
  where
  castParticipant (AssetSelection p) = Just p
  castParticipant _ = Nothing

  getAssetObjects
    :: Map TokenName GameAssetObject
    -> RaceParticipant
    -> Maybe (GameAssetObject /\ GameAssetObject)
  getAssetObjects ma p = do
    d <- Map.lookup (unwrap p).driver ma
    c <- Map.lookup (unwrap p).car ma
    pure $ d /\ c

sendPayout :: BigInt -> Address -> Contract TransactionHash
sendPayout rewardAmount addr = do
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = paysToAddrConstraint addr
      (lovelaceValueOf rewardAmount)

    lookups :: Lookups.ScriptLookups Void
    lookups = mempty

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure txId

closeRaceManual :: Effect (Promise TransactionHash)
closeRaceManual = do
  rp <- getParams
  withActor "Bot" $ runRacers rp do
    rewardAmount <- withContract (liftedM "Could not convert reward to BigInt")
      $ BigInt.fromString
      <$> liftEffect (promptFor "Enter race rewards")
    (raceHash /\ nitroFee) <- promptRaceParams
    rgp <- createRaceRegistryParams raceHash nitroFee

    winningPairStr <- liftEffect $ promptFor
      "Select winning driver car pair (e.g. 'CommonDriver:0,RareCar:1')"
    (d /\ c) <- lift $ (split (Pattern ",") winningPairStr) #
      ( \a -> case a of
          [ d, c ] -> (/\)
            <$>
              ( liftContractM "could not create driver tk name" $ mkTokenName
                  <=< byteArrayFromAscii
                  $ d
              )
            <*>
              ( liftContractM "could not create driver tk name" $ mkTokenName
                  <=< byteArrayFromAscii
                  $ c
              )
          _ -> throwContractError "Invalid input"
      )
    entries <- Array.concat <<< map (snd <<< snd) <<< Map.toUnfoldable <$>
      queryRegistryUtxos rgp

    winner <- lift
      $ liftContractM "could not find given driver and car in participants"
      $ castParticipant
      =<< find (isWinner d c) entries

    _ <- collectRegistryScriptLeftovers rgp

    lift (sendPayout rewardAmount (unwrap winner).payoutAddress)
  where
  castParticipant (AssetSelection p) = Just p
  castParticipant _ = Nothing
  isWinner d c = case _ of
    AssetSelection p -> (unwrap p).driver == d && (unwrap p).car == c
    _ -> false

refreshRace :: Effect (Promise String)
refreshRace = do
  rp <- getParams
  withActor "Admin" $ runRacers rp do
    (raceHash /\ nitroFee) <- promptRaceParams
    rgp <- createRaceRegistryParams raceHash nitroFee
    entries <- Array.concat <<< map (snd <<< snd) <<< Map.toUnfoldable <$>
      queryRegistryUtxos rgp
    let
      prettifyEntry (PendingSelection pkh) = "Enrolled: " <> show pkh
      prettifyEntry (AssetSelection ass) = "Raced with: "
        <> tokenNameToString (unwrap ass).driver
        <> " driving "
        <> tokenNameToString (unwrap ass).car
    pure $ show $ encodeAeson $ map prettifyEntry entries

promptRaceParams :: Racers (String /\ BigInt)
promptRaceParams = do
  raceHash <- liftEffect $ promptFor "Enter Race Name:"
  nitroFeeStr <- liftEffect $ promptFor "Enter NITRO registration fee:"
  nitroFee <- lift $ liftContractM "couldn't convert to bigint" $
    BigInt.fromString nitroFeeStr
  pure (raceHash /\ nitroFee)

createRaceRegistryParams :: String -> BigInt -> Racers RegistryParams
createRaceRegistryParams raceHash nitroFee = do
  slotSymbol <- withContract (liftedM "could not get symbol") $ mpsSymbol
    <<< mintingPolicyHash
    <$> mkRacePositionPolicy raceHash
  nitroPolicyHash <- mintingPolicyHash <$> mkNitroPolicy
  gameAssetPolicyHash <- mintingPolicyHash <$> mkGameAssetPolicy

  pure $ wrap
    { slotAssetClass: (slotSymbol /\ slotTokenName)
    , nitroPolicyHash: nitroPolicyHash
    , gameAssetPolicyHash: gameAssetPolicyHash
    , nitroFee
    }

userBuyNitro :: Effect (Promise TransactionHash)
userBuyNitro = do
  rp <- getParams
  amo <- promptFor "Enter NITRO amount"
  let
    contract = runRacers rp do
      a <- lift $ liftContractM "couldn't convert amount" $ BigInt.fromString
        amo
      buyNitroContract a
  actor <- getSelectedActor
  if actor == "User" then
    withActor "User" contract
  else fromAff $ runContract testnetEternlConfig contract

withActor :: forall a. String -> Contract a -> Effect (Promise a)
withActor actor contract = case lookup actor keys of
  Just k -> fromAff $ runKeyWalletContract k contract
  Nothing -> throwError $ error $ "Could not find actor " <> actor

withSelectedActor :: forall a. Contract a -> Effect (Promise a)
withSelectedActor contract = do
  actor <- getSelectedActor
  withActor actor contract

refreshWallet :: String /\ String -> Aff NitroWallet
refreshWallet (name /\ phex) = runKeyWalletContract phex do
  addr <- liftedM "could not get wallet address" $ getWalletAddress
  bech32addr <- addressToBech32 addr
  bal <- liftedM "could not get wallet balance" $ getWalletBalance
  pure { name, address: bech32addr, balance: prettifyBalance bal }

prettifyBalance :: Value -> Array (Array String)
prettifyBalance bal =
  map
    ( \(cs /\ tk /\ am) ->
        [ tokenNameToString tk
        , if eq cs adaSymbol then
            BigInt.toString am <> "  ("
              <>
                ( show $
                    toNumber
                      ( round
                          ( BigInt.toNumber am
                              `div` 1000.0
                          )
                      ) `div` 1000.0
                )
              <> " Ada)"
          else BigInt.toString am
        ]
    ) $
    flattenValue bal

resetTokens :: Effect (Promise (Array TransactionHash))
resetTokens = fromAff $ parTraverse resetWallet $ map snd keys
  where
  resetWallet phex = runKeyWalletContract phex do
    garbageAddress <- addressFromBech32 garbageAddressStr
    bal <- liftedM "could not get wallet balance" $ getWalletBalance

    let
      nonAdaValue =
        foldl (\acc (cs /\ tk /\ i) -> Value.singleton cs tk i <> acc)
          (lovelaceValueOf $ BigInt.fromInt 0) $ flattenNonAdaAssets bal

      constraints :: Constraints.TxConstraints Void Void
      constraints = paysToAddrConstraint garbageAddress nonAdaValue

      lookups :: Lookups.ScriptLookups Void
      lookups = mempty

    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

refreshState :: Effect (Promise String)
refreshState = do
  rp <- getParams
  withActor "Admin" $ runRacers rp do
    (ns /\ _) <- queryRacersState
    prettyState <- lift $ stateToSimpleJson ns
    pure prettyState

refreshRequests :: Effect (Promise String)
refreshRequests = do
  rp <- getParams
  withActor "Admin" $ runRacers rp do
    (rs /\ _) <- queryRacersState
    pendingReqs <- queryRequestsWithAirdropAddress rs
    processedReqs <- for (Map.toUnfoldable pendingReqs :: Array _)
      \(_ /\ pendingReq) -> do
        reqAddr <- lift $ addressToBech32 $ pendingReq.airdropAddress
        pure
          { airdropAddress: reqAddr
          , requestedAssets: map (\(r /\ i) -> show r /\ i)
              pendingReq.requestedAssets
          }
    pure $ show $ encodeAeson processedReqs

stateToSimpleJson :: RacersState -> Contract String
stateToSimpleJson rs = do
  let uRs = unwrap rs
  treasuryAddr <- addressToBech32 uRs.treasuryAddress
  operatingAddr <- addressToBech32 uRs.operatingAddress
  pure $ show $ encodeAeson
    { nitroPrice: uRs.nitroPrice
    , treasuryAddress: treasuryAddr
    , operatingAddress: operatingAddr
    , assetPrices: show uRs.assetPrices
    , depositScript: uRs.depositScript
    }

modifyRacersState :: Effect (Promise TransactionHash)
modifyRacersState = do
  rp <- getParams
  newStateStr <- promptFor "Enter Racers State JSON:"
  withActor "Admin" $ runRacers rp do
    newState <- lift $ stateFromSimpleJson newStateStr
    (ns /\ _) <- queryRacersState
    txId <- modifyRacersStateContract newState
    logInfo' $ "Modified nitro state: " <> show (encodeAeson ns)
    pure txId

stateFromSimpleJson :: String -> Contract RacersState
stateFromSimpleJson json = do
  obj <- liftContractE decodedJson
  -- assetPrices <-
  --   foldWithIndexM
  --     ( \rarityStr priceMap price -> do
  --         rarity <- liftContractM "could not parse rarity" $ rarityFromString
  --           rarityStr
  --         pure $ AssocMap.insert rarity price priceMap
  --     )
  --     AssocMap.empty
  --     (obj.assetPrices :: Object BigInt)
  assetPrices <- liftContractM "Could not decode asset prices" do
    cmn <- Object.lookup "Common" obj.assetPrices
    rr <- Object.lookup "Rare" obj.assetPrices
    epc <- Object.lookup "Epic" obj.assetPrices
    pure $ wrap { common: cmn, rare: rr, epic: epc }
  -- foldrWithIndex (\rarity price obj -> Object.insert (show rarity) price obj) Object.empty obj.assetPrices
  -- foldMapWithIndex (\rarity price -> Object.singleton (show rarity) price) obj.assetPrices
  treasuryAddress <- addressFromBech32 obj.treasuryAddress
  operatingAddress <- addressFromBech32 obj.operatingAddress
  pure $ wrap
    { nitroPrice: (obj.nitroPrice :: BigInt)
    , treasuryAddress
    , operatingAddress
    , assetPrices
    , depositScript: (obj.depositScript :: ValidatorHash)
    }
  where
  decodedJson
    :: Either JsonDecodeError
         { nitroPrice :: _
         , treasuryAddress :: _
         , operatingAddress :: _
         , assetPrices :: _
         , depositScript :: _
         }
  decodedJson = decodeJsonString json

actorAddress :: String -> Maybe Address
actorAddress actor = do
  skstr <- lookup actor keys
  sk <- mkPrivateKey skstr
  let addr = myPrivateKeysToAddress (PrivatePaymentKey sk) TestnetId
  toPlutusAddress addr
  where
  myPrivateKeysToAddress payKey network =
    let
      pubPayKey = publicKeyFromPrivateKey (unwrap payKey)
    in
      pubPayKey # publicKeyHash
        >>> unwrap
        >>> keyHashCredential
        >>> { network, paymentCred: _ }
        >>> enterpriseAddress
        >>> enterpriseAddressToAddress

runKeyWalletContract :: forall a. String -> Contract a -> Aff a
runKeyWalletContract phex c = runContract cfg c
  where
  cfg = testnetConfig { walletSpec = mkWalletSpec phex }

tokenNameToString :: TokenName -> String
tokenNameToString tk =
  if null intArray then "Lovelace" else toAscii $ getTokenName tk
  where
  intArray = byteArrayToIntArray $ getTokenName tk

  toAscii :: ByteArray -> String
  toAscii ba = fromCharArray
    $ map (\x -> unsafePartial $ fromJust $ fromCharCode x)
    $ byteArrayToIntArray ba

rarityFromString :: String -> Maybe Rarity
rarityFromString str = case str of
  "Common" -> Just Common
  "Rare" -> Just Rare
  "Epic" -> Just Epic
  _ -> Nothing

mkWalletSpec :: String -> Maybe WalletSpec
mkWalletSpec phex = Just $ UseKeys (PrivatePaymentKeyValue $ wrap pkey) Nothing
  where
  pkey = unsafePartial $ fromJust $ mkPrivateKey phex

depositScriptHashHelper :: RacersParams -> Contract ValidatorHash
depositScriptHashHelper rp = runRacers rp do
  depositVal <- mkDepositValidator
  pure $ validatorHash depositVal

mkPrivateKey :: String -> Maybe PrivateKey
mkPrivateKey str =
  mkPrivateKey' str <|> (stripPrefix (Pattern "5820") str >>= mkPrivateKey)
  where
  mkPrivateKey' :: String -> Maybe PrivateKey
  mkPrivateKey' str = hexToByteArray str >>= RawBytes >>> privateKeyFromBytes

paysToAddrConstraint
  :: Address -> Value -> Constraints.TxConstraints Void Void
paysToAddrConstraint a v = case (unwrap a).addressCredential of
  PubKeyCredential pkh ->
    Constraints.mustPayToPubKey (wrap pkh) v
  ScriptCredential vh ->
    Constraints.mustPayToScript vh unitDatum DatumWitness v

initialAvailableAssets :: Map Rarity AssetOption
initialAvailableAssets = Map.fromFoldable
  [ Common /\
      { name: unsafePartial $ fromJust $ mkCip25String "Subaru"
      , assetType: CarType
      , imageUrl:
          "ipfs://k2cwuee3arxg398hwxx6c0iferxitu126xntuzg8t765oo020h5y6npn"
      , description: "Common car nothing too special"
      , nitroAmount: BigInt.fromInt 100
      }
  , Rare /\
      { name: unsafePartial $ fromJust $ mkCip25String "The Driver"
      , assetType: DriverType
      , imageUrl:
          "ipfs://k2cwuee3arxg398hwxx6c0iferxitu126xntuzg8t765oo020h5y6npn"
      , description: "Rare driver with lots of experience"
      , nitroAmount: BigInt.fromInt 200
      }
  , Epic /\
      { name: unsafePartial $ fromJust $ mkCip25String "Mustang"
      , assetType: CarType
      , imageUrl:
          "ipfs://k2cwuee3arxg398hwxx6c0iferxitu126xntuzg8t765oo020h5y6npn"
      , description: "Exceptional vehicle!"
      , nitroAmount: BigInt.fromInt 300
      }
  ]
