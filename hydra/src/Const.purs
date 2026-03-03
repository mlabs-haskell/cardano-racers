module CardanoRacers.Hydra.Const
  ( appConst
  ) where

import Cardano.Types (BigNum)
import Cardano.Types.BigNum (fromInt) as BigNum

type AppConst =
  { collateralLovelace :: BigNum
  }

appConst :: AppConst
appConst =
  { collateralLovelace: BigNum.fromInt 10_000_000
  }
