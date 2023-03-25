module CardanoRacers.NitroInit where

import Contract.Prelude

import Aeson (decodeJsonString, encodeAeson)
import CardanoRacers.AssetRequest.Contract (mkAssetRequestPolicy)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Deposit.Contract (mkDepositValidator)
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types (Rarity(Common, Rare, Epic))
import CardanoRacers.Nitro.Contract (adminMintsNitroContract, botMintsNitroContract, buyNitroContract)
import CardanoRacers.Nitro.Helpers (createRacersParams)
import CardanoRacers.RacersState.Contract (initRacersStateContract, modifyRacersStateContract, queryRacersState)
import CardanoRacers.RacersState.Types (RacersState(..))
import Contract.Address (Address, ByteArray, addressFromBech32, addressToBech32, getWalletAddress)
import Contract.AssocMap (empty, insert) as AssocMap
import Contract.Config (NetworkId(..), PrivatePaymentKeySource(..), WalletSpec(..), testnetConfig)
import Contract.Credential (Credential(PubKeyCredential, ScriptCredential))
import Contract.Hashing (publicKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractE, liftContractM, liftedM, runContract)
import Contract.PlutusData (unitDatum)
import Contract.Prim.ByteArray (byteArrayToIntArray, hexToByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (ValidatorHash, validatorHash)
import Contract.Transaction (TransactionHash, awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Utxos (getWalletBalance, getWalletUtxos)
import Contract.Value (TokenName, Value, adaSymbol, flattenNonAdaAssets, flattenValue, getTokenName, lovelaceValueOf, scriptCurrencySymbol)
import Contract.Value as Value
import Contract.Wallet (PrivatePaymentKey(..), privateKeyFromBytes)
import Contract.Wallet.Key (publicKeyFromPrivateKey)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Parallel (parTraverse)
import Control.Promise (Promise, fromAff)
import Ctl.Internal.Plutus.Conversion (toPlutusAddress)
import Ctl.Internal.Serialization.Address (enterpriseAddress, enterpriseAddressToAddress, keyHashCredential)
import Ctl.Internal.Serialization.Types (PrivateKey)
import Ctl.Internal.Types.RawBytes (RawBytes(RawBytes))
import Data.Array (head) as Array
import Data.BigInt as BigInt
import Data.Char (fromCharCode)
import Data.Int (round, toNumber)
import Data.Map (toUnfoldable) as Map
import Data.String (stripPrefix)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Pattern (Pattern(Pattern))
import Effect.Aff (error)
import Partial.Unsafe (unsafePartial)

foreign import setupListeners :: Listeners -> Effect Unit

foreign import promptFor :: String -> Effect String

type NitroWallet =
  { name :: String, address :: String, balance :: Array (Array String) }

type Listeners =
  { refreshWallet :: Effect (Promise (Array NitroWallet))
  , refreshState :: Effect (Promise String)
  , initNitro :: Effect (Promise String)
  , adminMintNitro :: Effect (Promise TransactionHash)
  , botMintNitro :: Effect (Promise TransactionHash)
  , modifyNitroState :: Effect (Promise TransactionHash)
  , userBuyNitro :: Effect (Promise TransactionHash)
  , resetTokens :: Effect (Promise (Array TransactionHash))
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
  "addr_test1vzm7qtyntnr2axplvcn982whyq5ehgnqwxvj38cuhujjhqqrckn69"

main :: Effect Unit
main = do
  setupListeners
    { refreshWallet: fromAff $ parTraverse refreshWallet keys
    , initNitro: initNitro
    , refreshState: refreshState
    , adminMintNitro
    , botMintNitro
    , modifyNitroState
    , userBuyNitro
    , resetTokens
--     , mintDriver
    }
  pure unit

-- mintDriver :: Effect (Promise TransactionHash)
-- mintDriver = withActor "Admin" do
--   txid <- mintNewDriverNft Common
--   pure txid

initNitro :: Effect (Promise String)
initNitro = do
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

adminMintNitro :: Effect (Promise TransactionHash)
adminMintNitro = do
  pjson <- promptFor "Enter NitroScriptParams:"
  amo <- promptFor "Enter NITRO amount"
  withActor "Admin" do
    nsp <- liftContractE $ decodeJsonString pjson
    a <- liftContractM "couldn't convert amount" $ BigInt.fromString amo
    adminMintsNitroContract nsp a

botMintNitro :: Effect (Promise TransactionHash)
botMintNitro = do
  pjson <- promptFor "Enter NitroScriptParams:"
  amo <- promptFor "Enter NITRO amount"
  withActor "Bot" do
    nsp <- liftContractE $ decodeJsonString pjson
    a <- liftContractM "couldn't convert amount" $ BigInt.fromString amo
    botMintsNitroContract nsp a

modifyNitroState :: Effect (Promise TransactionHash)
modifyNitroState = do
  pjson <- promptFor "Enter NitroScriptParams:"
  amo <- promptFor "Enter NITRO price"
  treasuryAddrStr <- promptFor "Enter treasury address"
  operatingAddrStr <- promptFor "Enter operating address"
  withActor "Admin" do
    nsp <- liftContractE $ decodeJsonString pjson
    a <- liftContractM "couldn't convert amount" $ BigInt.fromString amo
    (ns /\ _) <- queryRacersState nsp
    treasuryAddr <- addressFromBech32 treasuryAddrStr <|> pure
      (unwrap ns).treasuryAddress
    operatingAddr <- addressFromBech32 operatingAddrStr <|> pure
      (unwrap ns).operatingAddress
    txId <- modifyRacersStateContract nsp
      ( wrap $ (unwrap ns)
          { nitroPrice = a
          , treasuryAddress = treasuryAddr
          , operatingAddress = operatingAddr
          }
      )
    logInfo' $ "Modified nitro state: " <> show (encodeAeson ns)
    pure txId

userBuyNitro :: Effect (Promise TransactionHash)
userBuyNitro = do
  pjson <- promptFor "Enter NitroScriptParams:"
  amo <- promptFor "Enter NITRO amount"
  withActor "User" do
    nsp <- liftContractE $ decodeJsonString pjson
    a <- liftContractM "couldn't convert amount" $ BigInt.fromString amo
    buyNitroContract nsp a

withActor :: forall a. String -> Contract a -> Effect (Promise a)
withActor actor contract = case lookup actor keys of
  Just k -> fromAff $ runKeyWalletContract k contract
  Nothing -> throwError $ error $ "Could not find actor " <> actor

refreshWallet :: String /\ String -> Aff NitroWallet
refreshWallet (name /\ phex) = runKeyWalletContract phex do
  addr <- liftedM "could not get wallet address" $ getWalletAddress
  bech32addr <- addressToBech32 addr
  bal <- liftedM "could not get wallet balance" $ getWalletBalance
  let
    parsedBal =
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
  pure { name, address: bech32addr, balance: parsedBal }

resetTokens :: Effect (Promise (Array TransactionHash))
resetTokens = fromAff $ parTraverse resetWallet $ map snd keys
  where
  resetWallet phex = runKeyWalletContract phex do
    utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
    (txi /\ _) <- liftContractM "Could not get first utxo" $ Array.head $
      Map.toUnfoldable utxos
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
  pjson <- promptFor "Enter NitroScriptParams:"
  withActor "Admin" do
    nsp <- liftContractE $ decodeJsonString pjson
    (ns /\ _) <- queryRacersState nsp
    treasuryAddr <- addressToBech32 (unwrap ns).treasuryAddress
    operatingAddr <- addressToBech32 (unwrap ns).operatingAddress
    pure $ show $ encodeAeson
      { treasuryAddress: treasuryAddr
      , operatingAddress: operatingAddr
      , nitroPrice: BigInt.toString (unwrap ns).nitroPrice
      }

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
