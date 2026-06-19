module CardanoRacers.Hydra.Lib.Contract
  ( runContractNullCosts
  ) where

import Prelude

import Cardano.Types (ExUnits(ExUnits))
import Cardano.Types.Coin (zero) as Coin
import Contract.Monad (Contract, ContractEnv, runContractInEnv)
import Contract.Numeric.BigNum (fromInt, fromStringUnsafe, one, zero) as BigNum
import Contract.ProtocolParameters (getProtocolParameters)
import Control.Monad.Reader (local)
import Data.Newtype (modify, wrap)
import Data.UInt (UInt)
import Effect.Aff (Aff)

-- TODO(high-prio): inherit values from pparams.json
runContractNullCosts :: forall (a :: Type). ContractEnv -> Contract a -> Aff a
runContractNullCosts contractEnv contract =
  runContractInEnv contractEnv do
    pparams <- getProtocolParameters <#> modify \rec ->
      rec
        { txFeeFixed = Coin.zero
        , txFeePerByte = (zero :: UInt)
        , prices = wrap
            { memPrice: wrap { numerator: BigNum.zero, denominator: BigNum.one }
            , stepPrice: wrap { numerator: BigNum.zero, denominator: BigNum.one }
            }
        {-
        , maxTxExUnits =
            ExUnits
              { mem: BigNum.fromInt 16500000 
              , steps: BigNum.fromStringUnsafe "10000000000"
              }
        -}
        }
    contract # local _
      { ledgerConstants =
          contractEnv.ledgerConstants
            { pparams = pparams
            }
      }
