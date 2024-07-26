module CardanoRacers.Nitro.Helpers
  ( createRacersParams
  , mintAdminNft
  , mintBotNft
  , mintStateNft
  ) where

import Contract.Prelude

import Cardano.Plutus.Types.CurrencySymbol (CurrencySymbol) as PlutusData
import Cardano.Plutus.Types.CurrencySymbol (fromScriptHash)
import Cardano.Plutus.Types.TokenName (TokenName(TokenName))
import Cardano.Types.AssetName (AssetName, mkAssetName)
import Cardano.Types.ScriptHash (ScriptHash)
import CardanoRacers.Common.Types (RacersParams(RacersParams))
import CardanoRacers.Nft (mintManyNfts, mintNft)
import Contract.Monad (Contract, liftContractM, throwContractError)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Transaction (TransactionInput)
import Contract.Value (CurrencySymbol)
import Data.Bifunctor (bimap)

createRacersParams
  :: TransactionInput -> Contract RacersParams
createRacersParams txi = do
  tkNames <- liftContractM "Could not make required token names" $ traverse
    (mkAssetName <=< byteArrayFromAscii)
    [ "RacersAdminNFT", "RacersBotNFT", "RacersStateNFT" ]
  (nfts :: Array (ScriptHash /\ AssetName)) <- mintManyNfts txi tkNames
  case nfts of
    [ adminAsset, botAsset, stateAsset ] -> pure $ RacersParams
      { adminToken: toRacerParam adminAsset
      , botToken: toRacerParam botAsset
      , stateToken: toRacerParam stateAsset
      }
    _ -> throwContractError "Impossible"

  where
  toRacerParam
    :: (ScriptHash /\ AssetName) -> (PlutusData.CurrencySymbol /\ TokenName)
  toRacerParam = bimap fromScriptHash TokenName

mintAdminNft :: TransactionInput -> Contract (CurrencySymbol /\ AssetName)
mintAdminNft txi = mintNft txi =<<
  ( liftContractM "Cannot make token name"
      <<< (mkAssetName <=< byteArrayFromAscii)
      $ "RacersAdminNFT"
  )

mintBotNft :: TransactionInput -> Contract (CurrencySymbol /\ AssetName)
mintBotNft txi = mintNft txi =<<
  ( liftContractM "Cannot make token name"
      <<< (mkAssetName <=< byteArrayFromAscii)
      $ "RacersNitroBotNFT"
  )

mintStateNft :: TransactionInput -> Contract (ScriptHash /\ AssetName)
mintStateNft txi = mintNft txi =<<
  ( liftContractM "Cannot make token name"
      <<< (mkAssetName <=< byteArrayFromAscii)
      $ "RacersStateNFT"
  )
