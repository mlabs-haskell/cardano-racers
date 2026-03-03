module CardanoRacers.Hydra.Handlers.SignAnnounceDistrTx
  ( signAnnounceDistrTxHandler
  , signAnnounceDistrTxHandlerImpl
  ) where

import Prelude

import Cardano.Plutus.Types.Map (empty) as Plutus.Map
import Cardano.Types (Vkeywitness)
import CardanoRacers.Hydra.Contracts.AnnounceDistr (mkAnnounceRewardDistributionTx)
import CardanoRacers.Hydra.Handlers.SignAnnounceDistrTx.Types
  ( SignAnnounceDistrTxError(CouldNotDecodeTx, TxValidationFailed, CouldNotSignTx)
  , signAnnounceDistrTxRequestPayloadCodec
  , signAnnounceDistrTxResponseCodec
  )
import CardanoRacers.Hydra.Lib.Transaction (signTxReturnSignature)
import CardanoRacers.Hydra.Monad (AppM, liftContract)
import CardanoRacers.Hydra.Types.ServerResponse (fromEither, respCreatedOrBadRequest)
import Control.Error.Util ((!?))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Codec.Argonaut (printJsonDecodeError) as CA
import Data.Either (Either(Left, Right))
import HTTPure (Response) as HTTPure
import HydraSdk.Lib (caDecodeString)

signAnnounceDistrTxHandler :: String -> AppM HTTPure.Response
signAnnounceDistrTxHandler =
  (respCreatedOrBadRequest signAnnounceDistrTxResponseCodec <<< fromEither)
    <=< signAnnounceDistrTxHandlerImpl

signAnnounceDistrTxHandlerImpl :: String -> AppM (Either SignAnnounceDistrTxError Vkeywitness)
signAnnounceDistrTxHandlerImpl bodyStr =
  runExceptT do
    case caDecodeString signAnnounceDistrTxRequestPayloadCodec bodyStr of
      Left decodeErr ->
        throwError $ CouldNotDecodeTx $ CA.printJsonDecodeError decodeErr
      Right { tx, collateralAddress } -> do
        -- TODO: Reward distribution is currently hardcoded.
        -- Pass actual (consensual) distribution.
        expectedTx <- lift $ mkAnnounceRewardDistributionTx collateralAddress Plutus.Map.empty
        when (tx /= expectedTx) $ throwError TxValidationFailed
        liftContract (signTxReturnSignature tx) !? CouldNotSignTx
