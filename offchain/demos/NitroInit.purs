module CardanoRacers.NitroInit where

import Contract.Prelude

import Aeson (JsonDecodeError, decodeJsonString, encodeAeson)
import CardanoRacers.AssetRequest.Contract
  ( mkAssetRequestPolicy
  , requestAssetByRarity
  )
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Deposit.Contract
  ( consumeAndRedeemRequests
  , createDepositReferenceScriptOutput
  , mkDepositValidator
  , queryOrCreateDepositReferenceScript
  , queryRequestsWithAirdropAddress
  )
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types
  ( AssetOption
  , GameAssetType(..)
  , Rarity(Common, Rare, Epic)
  , rarityFromString
  )
import CardanoRacers.Helpers (counterNonce)
import CardanoRacers.Nitro.Contract
  ( adminMintsNitroContract
  , botMintsNitroContract
  , buyNitroContract
  )
import CardanoRacers.Nitro.Helpers (createRacersParams)
import CardanoRacers.RacersState.Contract
  ( initRacersStateContract
  , modifyRacersStateContract
  , queryRacersState
  )
import CardanoRacers.RacersState.Types (RacersState(..))
import Contract.Address
  ( Address
  , ByteArray
  , addressFromBech32
  , addressToBech32
  , getWalletAddress
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
  )
import Contract.PlutusData (unitDatum)
import Contract.Prim.ByteArray (byteArrayToIntArray, hexToByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (ValidatorHash, validatorHash)
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Utxos (getWalletBalance, getWalletUtxos, utxosAt)
import Contract.Value
  ( TokenName
  , Value
  , adaSymbol
  , flattenNonAdaAssets
  , flattenValue
  , getTokenName
  , lovelaceValueOf
  , scriptCurrencySymbol
  )
import Contract.Value as Value
import Contract.Wallet (PrivatePaymentKey(..), privateKeyFromBytes)
import Contract.Wallet.Key (publicKeyFromPrivateKey)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (catchError, liftMaybe, throwError)
import Control.Parallel (parTraverse)
import Control.Promise (Promise, fromAff, toAffE)
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Plutus.Conversion (toPlutusAddress)
import Ctl.Internal.Serialization.Address
  ( enterpriseAddress
  , enterpriseAddressToAddress
  , keyHashCredential
  )
import Ctl.Internal.Serialization.Types (PrivateKey)
import Ctl.Internal.Types.RawBytes (RawBytes(RawBytes))
import Data.Array (head) as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Char (fromCharCode)
import Data.FoldableWithIndex (foldWithIndexM, foldrWithIndex)
import Data.Int (round, toNumber)
import Data.Map (Map)
import Data.Map (fromFoldable, insert, toUnfoldable) as Map
import Data.String (stripPrefix)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Pattern (Pattern(Pattern))
import Effect.Aff (error)
import Effect.Aff.Compat (EffectFn1, EffectFn2, mkEffectFn2)
import Effect.Ref as Ref
import Foreign.Object (Object)
import Foreign.Object (empty, insert) as Object
import Partial.Unsafe (unsafePartial)

foreign import setupListeners :: Listeners -> Effect Unit
foreign import getParams :: Effect String
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
  , initRacersState :: Effect (Promise String)
  , mintNitro :: Effect (Promise TransactionHash)
  , modifyRacersState :: Effect (Promise TransactionHash)
  , userBuyNitro :: Effect (Promise TransactionHash)
  , resetTokens :: Effect (Promise (Array TransactionHash))
  , makeAssetRequest :: Effect (Promise TransactionHash)
  , redeemRequests :: Effect (Promise (Array TransactionHash))
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
        }
        Unit
  --   , mintDriver :: Effect (Promise TransactionHash)
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
    , redeemRequests: redeemRequests cRef assetRef
    , getAvailableAssets: getAvailableAssets assetRef
    , setAssetOption: mkEffectFn2 $ setAssetOption assetRef
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

setAssetOption
  :: Ref.Ref (Map Rarity AssetOption)
  -> String
  -> { name :: String
     , assetType :: String
     , description :: String
     , imageUrl :: String
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
  let
    assetOption =
      { name: cip25Name
      , assetType
      , description: option.description
      , imageUrl: option.imageUrl
      }
  Ref.write (Map.insert rarity assetOption availableAssets) r

refreshWallets :: Effect (Promise WalletStates)
refreshWallets = fromAff do
  wallets <- parTraverse refreshWallet keys
  pjson <- liftEffect $ getParams
  depositBalance <- toAffE $ withActor "Admin"
    ( flip catchError (\e -> logError' ("Deposit script: " <> show e) $> []) do
        rp <- liftContractE $ decodeJsonString pjson
        (rs /\ _) <- queryRacersState rp
        let depAddr = scriptHashAddress (unwrap rs).depositScript Nothing
        depAddrString <- addressToBech32 depAddr
        utxos <- utxosAt depAddr
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

initRacersState :: Effect (Promise String)
initRacersState = do
  nitroPriceStr <- promptFor "Enter nitro price in lovelace"
  withActor "Admin" do
    utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
    (txi /\ _) <- liftContractM "Could not get first utxo" $ Array.head $
      Map.toUnfoldable utxos
    rp <- createRacersParams txi "NITRO"

    ownAddr <- liftedM "Could not get wallet address" getWalletAddress
    treasuryAddr <- liftContractM "could not get address" $ actorAddress
      "Treasury"
    botAddr <- liftContractM "could not get address" $ actorAddress "Bot"
    nitroPrice <- liftContractM "couldn't convert to bigint" $ BigInt.fromString
      nitroPriceStr
    depositScriptHash <- depositScriptHashHelper rp
    let
      assetPrices = foldl (flip $ uncurry AssocMap.insert) AssocMap.empty
        [ (Common /\ BigInt.fromInt 5_000_000)
        , (Rare /\ BigInt.fromInt 10_000_000)
        , (Epic /\ BigInt.fromInt 20_000_000)
        ]
      nitroState = RacersState
        { nitroPrice: nitroPrice
        , treasuryAddress: treasuryAddr
        , operatingAddress: ownAddr
        , depositScript: depositScriptHash
        , assetPrices
        }
    void $ initRacersStateContract rp nitroState
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
  pjson <- getParams
  amo <- promptFor "Enter NITRO amount"
  actor <- getSelectedActor
  nsp <- liftEither $ lmap (error <<< show) $ decodeJsonString pjson
  a <- liftMaybe (error "couldn't convert amount") $ BigInt.fromString amo
  case actor of
    "Admin" -> withActor "Admin" $ adminMintsNitroContract nsp a
    "Bot" -> withActor "Bot" $ botMintsNitroContract nsp a
    _ -> throwError $ error $ "Actor is not appropriate admin or bot" <> actor

makeAssetRequest :: Effect (Promise TransactionHash)
makeAssetRequest = do
  pjson <- getParams
  rarityStr <- promptFor "Enter requested rarity class"
  let
    contract = do
      rp <- liftContractE $ decodeJsonString pjson
      rarity <- liftContractM "Unrecognized rarity class" $ rarityFromString
        rarityStr
      requestAssetByRarity rp rarity
  actor <- getSelectedActor
  if actor == "User" then
    withActor "User" contract
  else fromAff $ runContract testnetEternlConfig contract

redeemRequests
  :: Ref.Ref Int
  -> Ref.Ref (Map Rarity AssetOption)
  -> Effect (Promise (Array TransactionHash))
redeemRequests cRef assetRef = do
  pjson <- getParams
  availableAssets <- Ref.read assetRef
  withActor "Bot" do
    rp <- liftContractE $ decodeJsonString pjson
    (rs /\ _) <- queryRacersState rp
    depRefOref <- queryOrCreateDepositReferenceScript rp
    consumeAndRedeemRequests rp availableAssets (counterNonce cRef) rs
      (Just depRefOref)

userBuyNitro :: Effect (Promise TransactionHash)
userBuyNitro = do
  pjson <- getParams
  amo <- promptFor "Enter NITRO amount"
  let
    contract = do
      nsp <- liftContractE $ decodeJsonString pjson
      a <- liftContractM "couldn't convert amount" $ BigInt.fromString amo
      buyNitroContract nsp a
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
  pjson <- getParams
  withActor "Admin" do
    nsp <- liftContractE $ decodeJsonString pjson
    (ns /\ _) <- queryRacersState nsp
    prettyState <- stateToSimpleJson ns
    pure prettyState

refreshRequests :: Effect (Promise String)
refreshRequests = do
  pjson <- getParams
  withActor "Admin" do
    rp <- liftContractE $ decodeJsonString pjson
    (rs /\ _) <- queryRacersState rp
    pendingReqs <- queryRequestsWithAirdropAddress rp rs
    processedReqs <- for (Map.toUnfoldable pendingReqs :: Array _)
      \(_ /\ pendingReq) -> do
        reqAddr <- addressToBech32 $ pendingReq.airdropAddress
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
  let
    assetPrices =
      foldrWithIndex
        (\rarity price obj -> Object.insert (show rarity) price obj)
        Object.empty
        uRs.assetPrices
  -- foldMapWithIndex (\rarity price -> Object.singleton (show rarity) price) uRs.assetPrices
  pure $ show $ encodeAeson
    { nitroPrice: uRs.nitroPrice
    , treasuryAddress: treasuryAddr
    , operatingAddress: operatingAddr
    , assetPrices: assetPrices
    , depositScript: uRs.depositScript
    }

modifyRacersState :: Effect (Promise TransactionHash)
modifyRacersState = do
  pjson <- getParams
  newStateStr <- promptFor "Enter Racers State JSON:"
  withActor "Admin" do
    nsp <- liftContractE $ decodeJsonString pjson
    newState <- stateFromSimpleJson newStateStr
    (ns /\ _) <- queryRacersState nsp
    txId <- modifyRacersStateContract nsp newState
    logInfo' $ "Modified nitro state: " <> show (encodeAeson ns)
    pure txId

stateFromSimpleJson :: String -> Contract RacersState
stateFromSimpleJson json = do
  obj <- liftContractE decodedJson
  assetPrices <-
    foldWithIndexM
      ( \rarityStr priceMap price -> do
          rarity <- liftContractM "could not parse rarity" $ rarityFromString
            rarityStr
          pure $ AssocMap.insert rarity price priceMap
      )
      AssocMap.empty
      (obj.assetPrices :: Object BigInt)
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

mkWalletSpec :: String -> Maybe WalletSpec
mkWalletSpec phex = Just $ UseKeys (PrivatePaymentKeyValue $ wrap pkey) Nothing
  where
  pkey = unsafePartial $ fromJust $ mkPrivateKey phex

depositScriptHashHelper :: RacersParams -> Contract ValidatorHash
depositScriptHashHelper rp = do
  assetRequestPolicySymbol <- liftedM "could not get asset request symbol"
    $ mkAssetRequestPolicy rp
    <#> scriptCurrencySymbol
  assetPolicySymbol <- liftedM "could not get game asset symbol"
    $ mkGameAssetPolicy rp
    <#> scriptCurrencySymbol
  depositVal <- mkDepositValidator rp $
    wrap
      { assetPolicySymbol
      , assetRequestPolicySymbol
      }
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
      }
  , Rare /\
      { name: unsafePartial $ fromJust $ mkCip25String "Dan The Driver Man"
      , assetType: DriverType
      , imageUrl:
          "ipfs://k2cwuee3arxg398hwxx6c0iferxitu126xntuzg8t765oo020h5y6npn"
      , description: "Rare driver with lots of experience"
      }
  , Epic /\
      { name: unsafePartial $ fromJust $ mkCip25String "Mustang"
      , assetType: CarType
      , imageUrl:
          "ipfs://k2cwuee3arxg398hwxx6c0iferxitu126xntuzg8t765oo020h5y6npn"
      , description: "Exceptional vehicle!"
      }
  ]
