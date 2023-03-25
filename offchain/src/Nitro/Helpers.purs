module CardanoRacers.Nitro.Helpers
  ( createRacersParams
  , mintAdminNft
  , mintBotNft
  , mintStateNft
  ) where

import Contract.Prelude

import CardanoRacers.Common.Types (RacersParams(RacersParams))
import CardanoRacers.Nft (mintManyNfts, mintNft)
import Contract.Monad (Contract, liftContractM, throwContractError)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Transaction (TransactionInput)
import Contract.Value (CurrencySymbol, TokenName, mkTokenName)

createRacersParams
  :: TransactionInput -> String -> Contract RacersParams
createRacersParams txi nitroTkStr = do
  tkNames <- liftContractM "Could not make required token names" $ traverse
    (mkTokenName <=< byteArrayFromAscii)
    [ "RacersAdminNFT", "RacersBotNFT", "RacersStateNFT" ]
  nitroTk <- liftContractM "Could not make nitro token name" $
    (mkTokenName <=< byteArrayFromAscii) nitroTkStr
  nfts <- mintManyNfts txi tkNames
  case nfts of
    [ adminAsset, botAsset, stateAsset ] -> pure $ RacersParams
      { adminToken: adminAsset
      , botToken: botAsset
      , stateToken: stateAsset
      , nitroToken: nitroTk
      }
    _ -> throwContractError "Impossible"

mintAdminNft :: TransactionInput -> Contract (CurrencySymbol /\ TokenName)
mintAdminNft txi = mintNft txi =<<
  ( liftContractM "Cannot make token name"
      <<< (mkTokenName <=< byteArrayFromAscii)
      $ "RacersAdminNFT"
  )

mintBotNft :: TransactionInput -> Contract (CurrencySymbol /\ TokenName)
mintBotNft txi = mintNft txi =<<
  ( liftContractM "Cannot make token name"
      <<< (mkTokenName <=< byteArrayFromAscii)
      $ "RacersNitroBotNFT"
  )

mintStateNft :: TransactionInput -> Contract (CurrencySymbol /\ TokenName)
mintStateNft txi = mintNft txi =<<
  ( liftContractM "Cannot make token name"
      <<< (mkTokenName <=< byteArrayFromAscii)
      $ "RacersStateNFT"
  )
