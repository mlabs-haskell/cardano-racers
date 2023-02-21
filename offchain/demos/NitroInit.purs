module CardanoRacers.NitroInit where

import Contract.Prelude

import Aeson (decodeJsonString, encodeAeson)
import CardanoRacers.Nitro.Contract (adminMintsNitroContract,
botMintsNitroContract, initNitroStateContract, modifyNitroStateContract, queryNitroState, buyNitroContract)
import CardanoRacers.Nitro.Helpers (createNitroScriptParams, mintAdminNft)
import CardanoRacers.Nitro.Types (NitroScriptParams(..), NitroState(..))
import Contract.Address (Address, ByteArray, addressFromBech32, addressToBech32, getWalletAddress, getWalletAddresses)
import Contract.Config (PrivatePaymentKeySource(..), WalletSpec(..), testnetConfig)
import Contract.Credential (Credential(PubKeyCredential, ScriptCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftContractE, liftContractM, liftedM, runContract)
import Contract.PlutusData (unitDatum)
import Contract.Prim.ByteArray (byteArrayFromAscii, byteArrayToHex, byteArrayToIntArray, hexToByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (TransactionHash(..), awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Utxos (getWalletBalance, getWalletUtxos)
import Contract.Value (TokenName, Value, flattenValue, getTokenName)
import Contract.Value as Value
import Contract.Wallet (PrivatePaymentKey(PrivatePaymentKey), PrivateStakeKey(PrivateStakeKey), privateKeyFromBytes)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe, throwError)
import Control.Promise (Promise, fromAff)
import Ctl.Internal.Serialization.Types (PrivateKey)
import Ctl.Internal.Types.ByteArray (byteArrayToUTF16le)
import Ctl.Internal.Types.RawBytes (RawBytes(RawBytes))
import Data.Array (head) as Array
import Data.BigInt as BigInt
import Data.Char (fromCharCode)
import Data.Map (singleton, toUnfoldable) as Map
import Data.String (joinWith, stripPrefix)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Pattern (Pattern(Pattern))
import Effect.Aff (error)
import Partial.Unsafe (unsafePartial)

foreign import setupListeners :: Listeners -> Effect Unit

foreign import promptFor :: String -> Effect String

type NitroWallet = { name :: String, address :: String, balance :: Array (Array String) }

type Listeners = { refreshWallet :: Effect (Promise (Array NitroWallet))
                 , refreshState :: Effect (Promise String)
                 , initNitro :: Effect (Promise String)
                 , adminMintNitro :: Effect (Promise TransactionHash)
                 , botMintNitro :: Effect (Promise TransactionHash)
                 , modifyNitroState :: Effect (Promise TransactionHash)
                 , userBuyNitro :: Effect (Promise TransactionHash)
                 }

keys = [ "Admin" /\  "582043a451628918e1a04e35fc638850d05885bc4d13dd72692194ba82545d7e57ab"
       , "Bot" /\  "58201590a76582c9fb63fbf18df5399043290077083babc6d65e7f68633a682d62d3"
       , "Treasury" /\  "5820a57db0c6cc5c10e066f6b6cde609a25c81461a49431d6e11d01e408ea5f6135e"
       , "User" /\  "582050389c06908083d9d9a559c0ca8d74e364a4016b2174710215e18c2cfe6eeca6"
       ]


main :: Effect Unit
main = do
    log "Hello, world!"
    -- _ <- adminMintNitro
    setupListeners { refreshWallet: fromAff $ traverse refreshWallet keys
                   , initNitro: initNitro "Admin" 
                   , refreshState: refreshState
                   , adminMintNitro
                   , botMintNitro
                   , modifyNitroState
                   , userBuyNitro
                   }
    pure unit

initNitro :: String -> Effect (Promise String)
initNitro actor = do
  nitroPriceStr <- promptFor "Enter nitro price in lovelace"
  withActor actor do
    utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
    (txi /\ _) <- liftContractM "Could not get first utxo" $ Array.head $
      Map.toUnfoldable utxos
    nsp <- createNitroScriptParams txi "NITRO"
    ownAddr <- liftedM "Could not get wallet address" getWalletAddress
    treasuryAddr <- addressFromBech32 "addr_test1vzc3uzp3d4a5vq8ffmef8q753zv3ahsu0fa2f2z9tczpl0glyy4hc"
    botAddr <- addressFromBech32 "addr_test1vzz9hzrtxy5djng7xf43hvz8r8w9rfa6gaal99a20kxakdcjfq8w3"
    nitroPrice <- liftContractM "couldn't convert to bigint" $ BigInt.fromString nitroPriceStr
    let nitroState = NitroState
            { nitroPrice: nitroPrice
            , treasuryAddress: treasuryAddr
            , operatingAddress: ownAddr
            }
    _ <- initNitroStateContract nsp nitroState

    let 
        paysToAddrConstraint
          :: Address -> Value -> Constraints.TxConstraints Void Void
        paysToAddrConstraint a v = case (unwrap a).addressCredential of
          PubKeyCredential pkh ->
            Constraints.mustPayToPubKey (wrap pkh) v
          ScriptCredential vh ->
            Constraints.mustPayToScript vh unitDatum DatumWitness v

        constraints :: Constraints.TxConstraints Void Void
        constraints = paysToAddrConstraint botAddr (uncurry Value.singleton (unwrap nsp).botToken $ BigInt.fromInt 1)

        lookups :: Lookups.ScriptLookups Void
        lookups = mempty

    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure $ show $ encodeAeson nsp

adminMintNitro :: Effect (Promise TransactionHash)
adminMintNitro  = do 
  pjson <- promptFor "Enter NitroScriptParams:"
  amo <- promptFor "Enter NITRO amount"
  withActor "Admin" do
     nsp <- liftContractE $ decodeJsonString pjson
     a <- liftContractM "couldn't convert amount" $ BigInt.fromString amo
     adminMintsNitroContract nsp a

botMintNitro :: Effect (Promise TransactionHash)
botMintNitro  = do 
  pjson <- promptFor "Enter NitroScriptParams:"
  amo <- promptFor "Enter NITRO amount"
  withActor "Bot" do
     nsp <- liftContractE $ decodeJsonString pjson
     a <- liftContractM "couldn't convert amount" $ BigInt.fromString amo
     botMintsNitroContract nsp a


modifyNitroState :: Effect (Promise TransactionHash)
modifyNitroState =  do
  pjson <- promptFor "Enter NitroScriptParams:"
  amo <- promptFor "Enter NITRO price"
  withActor "Admin" do
     nsp <- liftContractE $ decodeJsonString pjson
     a <- liftContractM "couldn't convert amount" $ BigInt.fromString amo
     (ns /\ _) <- queryNitroState nsp
     modifyNitroStateContract nsp (wrap $ (unwrap ns) { nitroPrice = a })

userBuyNitro :: Effect (Promise TransactionHash)
userBuyNitro = do
  pjson <- promptFor "Enter NitroScriptParams:"
  amo <- promptFor "Enter NITRO amount"
  withActor "User" do
     nsp <- liftContractE $ decodeJsonString pjson
     a <- liftContractM "couldn't convert amount" $ BigInt.fromString amo
     buyNitroContract nsp a


withActor :: forall a. String -> Contract () a -> Effect (Promise a)
withActor actor contract = case lookup actor keys of
  Just k -> fromAff $ runKeyWalletContract k contract
  Nothing -> throwError $ error $ "Could not find actor " <> actor

refreshWallet :: String /\ String -> Aff NitroWallet
refreshWallet (name /\ phex) = runKeyWalletContract phex do
  addr <- liftedM "could not get wallet address" $ getWalletAddress
  bech32addr <- addressToBech32 addr
  bal <- liftedM "could not get wallet balance" $ getWalletBalance
  let parsedBal = map (\(_ /\ tk /\ am) -> [tokenNameToString tk, BigInt.toString am]) $ flattenValue bal
  pure { name, address: bech32addr, balance:  parsedBal }

refreshState :: Effect (Promise String)
refreshState = do
  pjson <- promptFor "Enter NitroScriptParams:"
  withActor "Admin" do
    nsp <- liftContractE $ decodeJsonString pjson
    (ns /\ _) <- queryNitroState nsp
    pure $ show $ encodeAeson ns

runKeyWalletContract :: forall a. String -> Contract () a -> Aff a
runKeyWalletContract phex c = runContract cfg c
  where
    cfg = testnetConfig { walletSpec = mkWalletSpec phex }

tokenNameToString :: TokenName -> String
tokenNameToString tk = if null intArray then "Lovelace" else toAscii $ getTokenName tk
  where
    intArray = byteArrayToIntArray $ getTokenName tk
    toAscii :: ByteArray -> String
    toAscii ba = fromCharArray $ map (\x -> unsafePartial $ fromJust $ fromCharCode x) $ byteArrayToIntArray ba


mkWalletSpec :: String -> Maybe WalletSpec
mkWalletSpec phex = Just  $ UseKeys (PrivatePaymentKeyValue $ wrap pkey) Nothing
  where
    pkey = unsafePartial $ fromJust $ mkPrivateKey phex

    mkPrivateKey' :: String -> Maybe PrivateKey
    mkPrivateKey' str = hexToByteArray str >>= RawBytes >>> privateKeyFromBytes

    mkPrivateKey :: String -> Maybe PrivateKey
    mkPrivateKey str =
      mkPrivateKey' str <|> (stripPrefix (Pattern "5820") str >>= mkPrivateKey)
