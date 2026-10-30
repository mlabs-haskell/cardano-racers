module CardanoRacers.Types.FixedDecimal
  ( FixedDecimal(FixedDecimal)
  , N0
  , N5
  , class AddNat
  , convertExp
  , emul
  , fixedDecimalCodec
  , fromFixed5
  , fromFixedZero
  , toFixed5
  , toFixedZero
  ) where

import Prelude

import Cardano.Plutus.DataSchema (class KnownNat, Nat, S, Z)
import Cardano.Plutus.DataSchema.Nat (natVal)
import Contract.PlutusData (class FromData, class ToData, fromData, toData)
import Data.Codec.Argonaut (JsonCodec, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Ord (abs)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import HydraSdk.Lib (bigIntCodec)
import JS.BigInt (BigInt)
import JS.BigInt (fromInt, pow) as BigInt
import Type.Proxy (Proxy(Proxy))

newtype FixedDecimal :: Nat -> Type
newtype FixedDecimal exp = FixedDecimal { numerator :: BigInt }

derive instance Generic (FixedDecimal exp) _
derive instance Newtype (FixedDecimal exp) _
derive newtype instance Eq (FixedDecimal exp)
derive newtype instance Semiring (FixedDecimal exp)
derive newtype instance Ring (FixedDecimal exp)

instance Show (FixedDecimal exp) where
  show = genericShow

instance ToData (FixedDecimal exp) where
  toData = toData <<< _.numerator <<< unwrap

instance FromData (FixedDecimal exp) where
  fromData = map (wrap <<< { numerator: _ }) <<< fromData

fixedDecimalCodec :: forall (exp :: Nat). CA.JsonCodec (FixedDecimal exp)
fixedDecimalCodec =
  wrapIso FixedDecimal $ CA.object "FixedDecimal" $ CAR.record
    { numerator: bigIntCodec
    }

class AddNat :: Nat -> Nat -> Nat -> Constraint
class AddNat a b res | a b -> res, a res -> b, b res -> a

instance AddNat a b res => AddNat (S a) b (S res)
else instance AddNat a b res => AddNat a (S b) (S res)
else instance AddNat Z Z Z

-- Functions

emul
  :: forall (expA :: Nat) (expB :: Nat) (expC :: Nat)
   . AddNat expA expB expC
  => FixedDecimal expA
  -> FixedDecimal expB
  -> FixedDecimal expC
emul (FixedDecimal a) (FixedDecimal b) = FixedDecimal
  { numerator: a.numerator * b.numerator }

convertExp
  :: forall (expA :: Nat) (expB :: Nat)
   . KnownNat expA
  => KnownNat expB
  => FixedDecimal expA
  -> FixedDecimal expB
convertExp (FixedDecimal { numerator: a }) =
  let
    ediff = BigInt.fromInt $ natVal (Proxy :: Proxy expB) - natVal
      (Proxy :: Proxy expA)
    ten = BigInt.fromInt 10
  in
    FixedDecimal
      { numerator:
          if ediff >= zero then a * (ten `BigInt.pow` ediff)
          else a `div` (ten `BigInt.pow` (abs ediff))
      }

-- Helpers

type N5 = S (S (S (S (S Z))))
type N0 = Z

toFixedZero :: BigInt -> FixedDecimal N0
toFixedZero a = wrap { numerator: a }

fromFixedZero :: FixedDecimal N0 -> BigInt
fromFixedZero (FixedDecimal n) = n.numerator

toFixed5 :: BigInt -> FixedDecimal N5
toFixed5 = (convertExp :: FixedDecimal N0 -> FixedDecimal N5) <<< toFixedZero

fromFixed5 :: FixedDecimal N5 -> BigInt
fromFixed5 = fromFixedZero <<<
  (convertExp :: FixedDecimal N5 -> FixedDecimal N0)
