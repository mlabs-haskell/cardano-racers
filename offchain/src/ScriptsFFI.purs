module CardanoRacers.ScriptsFFI
  ( nitroMintingPolicyScript
  , racersStateValidatorScript
  , adminNftMintingPolicy
  , depositScript
  ) where

foreign import nitroMintingPolicyScript :: String
foreign import racersStateValidatorScript :: String
foreign import adminNftMintingPolicy :: String
foreign import depositScript :: String
