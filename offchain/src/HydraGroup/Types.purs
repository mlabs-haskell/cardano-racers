module CardanoRacers.HydraGroup.Types
  ( HydraGroupInfo(HydraGroupInfo)
  , HydraGroupRegistryRedeemer(DisbandGroup)
  , hydraGroupTokenName
  ) where

import Prelude

import Cardano.Plutus.DataSchema (Z)
import Cardano.Types (AssetName, Ed25519KeyHash, ScriptHash, TransactionInput)
import CardanoRacers.Helpers (assetNameFromAsciiUnsafe)
import Contract.PlutusData
  ( class FromData
  , class HasPlutusSchema
  , class ToData
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
  , genericFromData
  , genericToData
  )
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

-- HydraGroupInfo

newtype HydraGroupInfo = HydraGroupInfo
  { hydraGroupUniqueId :: ScriptHash
  , hydraGroupNonceOref :: TransactionInput
  , hydraGroupMasterKeys :: Array Ed25519KeyHash
  , hydraGroupHttpServers :: Array String
  , hydraGroupMetadata :: String
  }

derive instance Generic HydraGroupInfo _
derive instance Newtype HydraGroupInfo _
derive instance Eq HydraGroupInfo

instance Show HydraGroupInfo where
  show = genericShow

instance
  HasPlutusSchema HydraGroupInfo
    ( "HydraGroupInfo"
        :=
          ( "hydraGroupUniqueId"
              := I ScriptHash
              :+ "hydraGroupNonceOref"
              := I TransactionInput
              :+ "hydraGroupMasterKeys"
              := I (Array Ed25519KeyHash)
              :+ "hydraGroupHttpServers"
              := I (Array String)
              :+ "hydraGroupMetadata"
              := I String
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData HydraGroupInfo where
  toData = genericToData

instance FromData HydraGroupInfo where
  fromData = genericFromData

-- HydraGroupRegistryRedeemer

data HydraGroupRegistryRedeemer = DisbandGroup

derive instance Generic HydraGroupRegistryRedeemer _
derive instance Eq HydraGroupRegistryRedeemer

instance Show HydraGroupRegistryRedeemer where
  show = genericShow

instance
  HasPlutusSchema HydraGroupRegistryRedeemer
    ( "DisbandGroup"
        := PNil
        @@ Z
        :+ PNil
    )

instance ToData HydraGroupRegistryRedeemer where
  toData = genericToData

instance FromData HydraGroupRegistryRedeemer where
  fromData = genericFromData

hydraGroupTokenName :: AssetName
hydraGroupTokenName = assetNameFromAsciiUnsafe "HYDRA_GROUP"
