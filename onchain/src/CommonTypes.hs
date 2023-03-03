{-# LANGUAGE TemplateHaskell #-}

module CommonTypes where

import Data.Function (on)
import GHC.Generics
import GHC.Show (Show)
import Ledger (Address, AssetClass)
import Plutus.V2.Ledger.Api (
  Map,
  TokenName,
 )
import PlutusTx qualified (unstableMakeIsData)
import PlutusTx.Prelude

data Rarity = Common | Rare | Epic deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''Rarity

instance Eq Rarity where
  Common == Common = True
  Rare == Rare = True
  Epic == Epic = True
  _ == _ = False

instance Ord Rarity where
  compare = compare `on` toInt
    where
      toInt :: Rarity -> Integer
      toInt Common = 0
      toInt Rare = 1
      toInt Epic = 2

data RacersParams = RacersParams
  { adminToken :: AssetClass
  -- ^ Admin NFT AssetClass that allows free minting and state modification
  , botToken :: AssetClass
  -- ^ Bot NFT AssetClass that allows bot to mint Nitro tokens only
  , stateToken :: AssetClass
  -- ^ State NFT AssetClass that reprensents the current RacersState
  -- | see https://github.com/Plutonomicon/plutonomicon/blob/main/statethread.md
  , nitroToken :: TokenName
  -- ^ TokenName of Nitro token
  }
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''RacersParams

data RacersState = RacersState
  { nitroPrice :: Integer -- Nitro price in Lovelace
  , treasuryAddress :: Address
  , operatingAddress :: Address
  , driverPrices :: Map Rarity Integer
  , carPrices :: Map Rarity Integer
  }
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''RacersState

data GameAsset
  = Driver
  | Car
  deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''GameAsset
