module Constants (slotTokenName, contenderTokenName, nitroTokenName) where

import Plutus.V2.Ledger.Api (TokenName (TokenName))

slotTokenName :: TokenName
slotTokenName = TokenName "Slot"

contenderTokenName :: TokenName
contenderTokenName = TokenName "Contender"

nitroTokenName :: TokenName
nitroTokenName = TokenName "NITRO"
