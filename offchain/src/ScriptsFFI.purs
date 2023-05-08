module CardanoRacers.ScriptsFFI
  ( nitroMintingPolicyScript
  , racersStateValidatorScript
  , adminNftMintingPolicy
  , depositScript
  , assetRequestPolicy
  , gameAssetPolicy
  , racePositionPolicy
  , raceRegistryScript
  ) where

foreign import nitroMintingPolicyScript :: String
foreign import racersStateValidatorScript :: String
foreign import adminNftMintingPolicy :: String
foreign import depositScript :: String
foreign import assetRequestPolicy :: String
foreign import gameAssetPolicy :: String
foreign import racePositionPolicy :: String
foreign import raceRegistryScript :: String
