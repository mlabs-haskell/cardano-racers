module CardanoRacers.ScriptsFFI (nitroMintingPolicyScript,nitroStateValidatorScript,adminNftMintingPolicy,depositScript,gameAssetPolicy) where

foreign import nitroMintingPolicyScript :: String
foreign import nitroStateValidatorScript :: String
foreign import adminNftMintingPolicy :: String
foreign import depositScript :: String
foreign import gameAssetPolicy :: String
