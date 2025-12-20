module CardanoRacers.Helpers
  ( wrapEncodeAeson
  , counterNonce
  , decodeWrappedAeson
  , paysToAddrConstraint
  , decodeAesonString
  , fromBIToJSBI
  , fromJSBIToBI
  , fromBIToBigNum
  , fromBIToInt
  , fromBIToDataBI
  , fromBigNumToBI
  , fromJSBIToInt
  , fromJSBIToBigNum
  , mkMint
  , mkPosixTimeUnsafe
  ) where

import Contract.Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , decodeAeson
  , encodeAeson
  , getField
  )
import Cardano.Plutus.Types.Address (Address)
import Cardano.Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  )
import Cardano.Plutus.Types.CurrencySymbol as CurrencySymbol
import Cardano.Plutus.Types.Value as PlutusValue
import Cardano.Types (Mint)
import Cardano.Types.BigInt as CTBigInt
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as CT
import Cardano.Types.Int as CTInt
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.PlutusData (unit) as PlutusData
import Contract.Time (POSIXTime(..))
import Contract.TxConstraints (DatumPresence(DatumWitness))
import Contract.TxConstraints as Constraints
import Contract.Value (Value)
import Data.BigInt as Data
import Data.BigInt as DataBigInt
import Data.Time.Duration (class Duration, fromDuration)
import Effect.Ref (Ref)
import Effect.Ref (read, write) as Ref
import Foreign.Object (singleton)
import JS.BigInt as JSBigInt
import Partial.Unsafe (unsafePartial)

wrapEncodeAeson :: forall (a :: Type). EncodeAeson a => String -> a -> Aeson
wrapEncodeAeson constr = encodeAeson <<< singleton constr <<< encodeAeson

counterNonce :: Ref Int -> Effect String
counterNonce ref = do
  n <- Ref.read ref
  Ref.write (n + 1) ref
  pure $ show n

decodeWrappedAeson
  :: forall (a ∷ Type) (r :: Type)
   . (DecodeAeson a)
  => String
  -> (a -> Either JsonDecodeError r)
  -> Aeson
  -> Either JsonDecodeError r
decodeWrappedAeson constr k aes = caseAesonObject
  (Left $ TypeMismatch $ "expected object got " <> show aes)
  (k <=< flip getField constr)
  aes

decodeAesonString
  :: forall (r :: Type)
   . String
  -> (String -> r)
  -> Aeson
  -> Either JsonDecodeError r
decodeAesonString str f aes = decodeAeson aes >>=
  ( \str' ->
      if str == str' then
        Right $ f str'
      else Left $ TypeMismatch ("expected string: " <> str <> " got " <> str')
  )

paysToAddrConstraint
  :: Address -> Value -> Constraints.TxConstraints
paysToAddrConstraint a v = case (unwrap a).addressCredential of
  PubKeyCredential pkh ->
    Constraints.mustPayToPubKey (wrap $ unwrap pkh) v
  ScriptCredential vh ->
    Constraints.mustPayToScript (unwrap vh) PlutusData.unit DatumWitness v

fromBIToJSBI :: Data.BigInt -> JSBigInt.BigInt
fromBIToJSBI = unsafePartial fromJust <<< JSBigInt.fromString <<<
  DataBigInt.toString

fromBigNumToBI :: BigNum -> Data.BigInt
fromBigNumToBI = unsafePartial fromJust <<< DataBigInt.fromString <<<
  BigNum.toString

fromJSBIToBI :: JSBigInt.BigInt -> Data.BigInt
fromJSBIToBI = unsafePartial fromJust <<< Data.fromString <<<
  JSBigInt.toString

fromBIToBigNum :: Data.BigInt -> BigNum
fromBIToBigNum = unsafePartial fromJust <<< BigNum.fromString <<<
  DataBigInt.toString

fromJSBIToBigNum :: JSBigInt.BigInt -> BigNum
fromJSBIToBigNum = unsafePartial fromJust <<< BigNum.fromString <<<
  JSBigInt.toString

fromJSBIToInt :: JSBigInt.BigInt -> Int
fromJSBIToInt = unsafePartial fromJust <<< JSBigInt.toInt

fromBIToInt :: Data.BigInt -> CT.Int
fromBIToInt = unsafePartial fromJust <<< CTInt.fromString <<<
  DataBigInt.toString

mkMint :: PlutusValue.Value -> Mint
mkMint v = unsafePartial $ fromJust
  $ Mint.unflatten
  $ map
      ( \(cs /\ tk /\ amt) ->
          unsafePartial (fromJust $ CurrencySymbol.toCardano cs) /\ unwrap tk /\
            mkInt amt
      )
  $ PlutusValue.flattenValue v

fromBIToDataBI :: CTBigInt.BigInt -> Data.BigInt
fromBIToDataBI = unsafePartial fromJust <<< DataBigInt.fromString <<<
  CTBigInt.toString

mkInt :: JSBigInt.BigInt -> Int.Int
mkInt a = unsafePartial $ fromJust $ Int.fromBigInt a

mkPosixTimeUnsafe :: forall (a :: Type). Duration a => a -> POSIXTime
mkPosixTimeUnsafe =
  unsafePartial fromJust
    <<< map wrap
    <<< CTBigInt.fromNumber
    <<< unwrap
    <<< fromDuration
