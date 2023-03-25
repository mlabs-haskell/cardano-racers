module CardanoRacers.Helpers where

import Contract.Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , encodeAeson
  , getField
  )
import Contract.Address (Address)
import Contract.Credential (Credential(PubKeyCredential, ScriptCredential))
import Contract.PlutusData (unitDatum)
import Contract.TxConstraints (DatumPresence(DatumWitness))
import Contract.TxConstraints as Constraints
import Contract.Value (Value)
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

paysToAddrConstraint
  :: Address -> Value -> Constraints.TxConstraints Void Void
paysToAddrConstraint a v = case (unwrap a).addressCredential of
  PubKeyCredential pkh ->
    Constraints.mustPayToPubKey (wrap pkh) v
  ScriptCredential vh ->
    Constraints.mustPayToScript vh unitDatum DatumWitness v
