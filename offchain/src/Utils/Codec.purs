module CardanoRacers.Utils.Codec
  ( addressBech32Codec
  , assetNameCodec
  , bigNumStringCodec
  , orefCodec
  , plutusAddressBech32Codec
  , plutusValueCodec
  , posixTimeCodec
  , uintStringCodec
  , valueCodec
  ) where

import Prelude

import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Address (fromCardano, toCardano) as Plutus.Address
import Cardano.Plutus.Types.Value (Value) as Plutus
import Cardano.Plutus.Types.Value (fromCardano, toCardano) as Plutus.Value
import Cardano.Types
  ( Address
  , AssetName
  , BigNum
  , Coin(Coin)
  , MultiAsset
  , NetworkId
  , ScriptHash
  , TransactionInput(TransactionInput)
  , Value(Value)
  )
import Cardano.Types.Address (fromBech32, toBech32) as Address
import Cardano.Types.AssetName (mkAssetName, unAssetName)
import Cardano.Types.BigNum (fromString, toString) as BigNum
import Cardano.Types.MultiAsset (add, empty, flatten, singleton) as MultiAsset
import Contract.Time (POSIXTime(POSIXTime))
import Data.Array (foldRecM) as Array
import Data.Codec.Argonaut (JsonCodec, array, object, prismaticCodec, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Maybe (Maybe, fromJust)
import Data.Profunctor (dimap, wrapIso)
import Data.Tuple.Nested ((/\))
import Data.UInt (UInt)
import Data.UInt (fromString, toString) as UInt
import HydraSdk.Lib (bigIntCodec, byteArrayCodec, scriptHashCodec, txHashCodec)
import Partial.Unsafe (unsafePartial)

addressBech32Codec :: CA.JsonCodec Address
addressBech32Codec =
  CA.prismaticCodec "Address" Address.fromBech32 Address.toBech32
    CA.string

plutusAddressBech32Codec :: NetworkId -> CA.JsonCodec Plutus.Address
plutusAddressBech32Codec network =
  CA.prismaticCodec "Plutus.Address"
    Plutus.Address.fromCardano
    (unsafePartial fromJust <<< Plutus.Address.toCardano network) -- FIXME: unsafe
    addressBech32Codec

assetNameCodec :: CA.JsonCodec AssetName
assetNameCodec =
  CA.prismaticCodec "AssetName" mkAssetName unAssetName
    byteArrayCodec

bigNumStringCodec :: CA.JsonCodec BigNum
bigNumStringCodec =
  CA.prismaticCodec "BigNum" BigNum.fromString BigNum.toString
    CA.string

orefCodec :: CA.JsonCodec TransactionInput
orefCodec =
  wrapIso TransactionInput $ CA.object "TransactionInput" $ CAR.record
    { transactionId: txHashCodec
    , index: uintStringCodec
    }

posixTimeCodec :: CA.JsonCodec POSIXTime
posixTimeCodec = wrapIso POSIXTime bigIntCodec

uintStringCodec :: CA.JsonCodec UInt
uintStringCodec =
  CA.prismaticCodec "UInt" UInt.fromString UInt.toString
    CA.string

-- Value codec

coinCodec :: CA.JsonCodec Coin
coinCodec = wrapIso Coin bigNumStringCodec

type MultiAssetEntry =
  { policy :: ScriptHash
  , name :: AssetName
  , quantity :: BigNum
  }

multiAssetCodec :: CA.JsonCodec MultiAsset
multiAssetCodec =
  CA.prismaticCodec "MultiAsset" toMultiAsset fromMultiAsset $
    CA.array multiAssetEntryCodec
  where
  fromMultiAsset :: MultiAsset -> Array MultiAssetEntry
  fromMultiAsset =
    map (\(policy /\ name /\ quantity) -> { policy, name, quantity })
      <<< MultiAsset.flatten

  toMultiAsset :: Array MultiAssetEntry -> Maybe MultiAsset
  toMultiAsset =
    Array.foldRecM
      ( \acc rec -> MultiAsset.add acc $ MultiAsset.singleton rec.policy
          rec.name
          rec.quantity
      )
      MultiAsset.empty

  multiAssetEntryCodec :: CA.JsonCodec MultiAssetEntry
  multiAssetEntryCodec =
    CA.object "MultiAssetEntry" $ CAR.record
      { policy: scriptHashCodec
      , name: assetNameCodec
      , quantity: bigNumStringCodec
      }

type ValueJsonRepr =
  { lovelace :: Coin
  , tokens :: MultiAsset
  }

valueCodec :: CA.JsonCodec Value
valueCodec =
  dimap fromValue toValue $ CA.object "ValueJsonRepr" $ CAR.record
    { lovelace: coinCodec
    , tokens: multiAssetCodec
    }
  where
  fromValue :: Value -> ValueJsonRepr
  fromValue (Value lovelace tokens) = { lovelace, tokens }

  toValue :: ValueJsonRepr -> Value
  toValue { lovelace, tokens } = Value lovelace tokens

plutusValueCodec :: CA.JsonCodec Plutus.Value
plutusValueCodec =
  -- FIXME: unsafe
  dimap (unsafePartial fromJust <<< Plutus.Value.toCardano)
    Plutus.Value.fromCardano
    valueCodec
