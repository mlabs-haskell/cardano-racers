module CardanoRacers.Hydra.Handlers.SignAnnounceDistrTx
  ( signAnnounceDistrTxHandler
  , signAnnounceDistrTxHandlerImpl
  ) where

import Prelude

import Cardano.Types (Vkeywitness)
import CardanoRacers.Hydra.Contracts.AnnounceDistr (mkAnnounceRewardDistributionTx)
import CardanoRacers.Hydra.Handlers.SignAnnounceDistrTx.Types
  ( SignAnnounceDistrTxError
      ( CouldNotDecodeTx
      , RaceDataNotAvailable
      , UnexpectedRaceStatus
      , TxValidationFailed
      , CouldNotSignTx
      )
  , signAnnounceDistrTxRequestPayloadCodec
  , signAnnounceDistrTxResponseCodec
  )
import CardanoRacers.Hydra.Lib.Transaction (signTxReturnSignature)
import CardanoRacers.Hydra.Monad (AppM, liftContract)
import CardanoRacers.Hydra.RewardDistribution (distributeRewards)
import CardanoRacers.Hydra.Types.RaceStatus (RaceStatus(DistributingRewards))
import CardanoRacers.Hydra.Types.ServerResponse (fromEither, respCreatedOrBadRequest)
import Control.Error.Util ((!?))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Codec.Argonaut (printJsonDecodeError) as CA
import Data.Either (Either(Left, Right))
import Effect.Class (liftEffect)
import Effect.Ref (read) as Ref
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
        { raceDataRef, raceStatusRef } <- ask
        { raceParams } <- liftEffect (Ref.read raceDataRef) !? RaceDataNotAvailable
        raceStatus <- liftEffect $ Ref.read raceStatusRef
        case raceStatus of
          DistributingRewards { finalResults } -> do
            let rewardDistr = distributeRewards finalResults raceParams
            expectedTx <- lift $ mkAnnounceRewardDistributionTx collateralAddress rewardDistr
            when (tx /= expectedTx) $ throwError TxValidationFailed
            liftContract (signTxReturnSignature tx) !? CouldNotSignTx
          _ ->
            throwError UnexpectedRaceStatus
