{-# LANGUAGE TemplateHaskell #-}

module CommonTypes where

-- import Data.Function (on)
import GHC.Generics
import GHC.Show (Show)
import Ledger (Address, AssetClass)
import PlutusTx qualified (unstableMakeIsData)
import PlutusTx.Prelude

data Rarity = Common | Rare | Epic
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''Rarity

instance Eq Rarity where
  Common == Common = True
  Rare == Rare = True
  Epic == Epic = True
  _ == _ = False

rarityToBuiltinByteString :: Rarity -> BuiltinByteString
rarityToBuiltinByteString Common = "Common"
rarityToBuiltinByteString Rare = "Rare"
rarityToBuiltinByteString Epic = "Epic"

data AssetPrices = AssetPrices
  { common :: Integer
  , rare :: Integer
  , epic :: Integer
  }
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''AssetPrices

getPrice :: Rarity -> AssetPrices -> Integer
getPrice Common = common
getPrice Rare = rare
getPrice Epic = epic

data RacersParams = RacersParams
  { adminToken :: AssetClass
  -- ^ Admin NFT AssetClass that allows free minting and state modification
  , botToken :: AssetClass
  -- ^ Bot NFT AssetClass that allows bot to mint Nitro tokens only
  , stateToken :: AssetClass
  -- ^ State NFT AssetClass that reprensents the current RacersState
  -- | see https://github.com/Plutonomicon/plutonomicon/blob/main/statethread.md
  }
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''RacersParams

data RacersState = RacersState
  { nitroPrice :: Integer -- Nitro price in Lovelace
  , treasuryAddress :: Address
  , operatingAddress :: Address
  , assetPrices :: AssetPrices
  }
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''RacersState

data GameAsset
  = Driver
  | Car
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''GameAsset

instance Eq GameAsset where
  Driver == Driver = True
  Car == Car = True
  _ == _ = False

gameAssetToBuiltinByteString :: GameAsset -> BuiltinByteString
gameAssetToBuiltinByteString Driver = "Driver"
gameAssetToBuiltinByteString Car = "Car"

newtype AirdropAddressDatum = AirdropAddressDatum
  {airdropAddress :: Address}
PlutusTx.unstableMakeIsData ''AirdropAddressDatum
