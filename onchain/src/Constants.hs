module Constants (
  contenderTokenName,
  nitroTokenName,
  raceStateTokenName,
  slotTokenName,
  valueEscrowTokenName,
  hydraGroupTokenName,
 ) where

import Plutus.V2.Ledger.Api (TokenName (TokenName))

slotTokenName :: TokenName
slotTokenName = TokenName "Slot"

contenderTokenName :: TokenName
contenderTokenName = TokenName "Contender"

nitroTokenName :: TokenName
nitroTokenName = TokenName "NITRO"

raceStateTokenName :: TokenName
raceStateTokenName = TokenName "RACE_STATE"

valueEscrowTokenName :: TokenName
valueEscrowTokenName = TokenName "VALUE_ESCROW"

hydraGroupTokenName :: TokenName
hydraGroupTokenName = TokenName "HYDRA_GROUP"
