module CardanoRacers.Helpers
  ( wrapEncodeAeson
  , counterNonce
  , decodeWrappedAeson
  , paysToAddrConstraint
  , decodeAesonString
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
import Contract.Address (Address)
import Contract.Credential (Credential(PubKeyCredential, ScriptCredential))
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData (unitDatum)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.TxConstraints (DatumPresence(DatumWitness))
import Contract.TxConstraints as Constraints
import Contract.Value (Value)
import Ctl.Internal.Contract.Monad (getQueryHandle)
import Ctl.Internal.Plutus.Conversion (toPlutusTxOutputWithRefScript)
import Effect.Ref (Ref)
import Effect.Ref (read, write) as Ref
import Foreign.Object (singleton)

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
  :: Address -> Value -> Constraints.TxConstraints Void Void
paysToAddrConstraint a v = case (unwrap a).addressCredential of
  PubKeyCredential pkh ->
    Constraints.mustPayToPubKey (wrap pkh) v
  ScriptCredential vh ->
    Constraints.mustPayToScript vh unitDatum DatumWitness v
