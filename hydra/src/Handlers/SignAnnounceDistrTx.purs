module CardanoRacers.Hydra.Handlers.SignAnnounceDistrTx
  ( signAnnounceDistrTxHandler
  , signAnnounceDistrTxHandlerImpl
  ) where

import Prelude

import Cardano.Types (Vkeywitness)
import CardanoRacers.Hydra.Contracts.AnnounceDistr (mkAnnounceRewardDistributionTx)
import CardanoRacers.Hydra.Handlers.SignAnnounceDistrTx.Types
  ( SignAnnounceDistrTxError
      ( CouldNotDecodeReqBody
      , RequestedRaceNotHosted
      , UnexpectedRaceStatus
      , TxValidationFailed
      , CouldNotSignTx
      )
  , signAnnounceDistrTxRequestPayloadCodec
  , signAnnounceDistrTxResponseCodec
  )
import CardanoRacers.Hydra.Lib.Transaction (signTxReturnSignature)
import CardanoRacers.Hydra.Monad (AppM, findRaceEntryByRaceCs, liftContract)
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
        throwError $ CouldNotDecodeReqBody
          { decodeError: CA.printJsonDecodeError decodeErr
          }
      Right { raceCs, tx, changeAddress } -> do
        { raceData, raceStatusRef } <- findRaceEntryByRaceCs raceCs !? RequestedRaceNotHosted
        raceStatus <- liftEffect $ Ref.read raceStatusRef
        case raceStatus of
          DistributingRewards { finalResults } -> do
            let rewardDistr = distributeRewards finalResults raceData.raceParams
            expectedTx <- lift $ mkAnnounceRewardDistributionTx raceData changeAddress
              rewardDistr
            when (tx /= expectedTx) $ throwError TxValidationFailed
            liftContract (signTxReturnSignature tx) !? CouldNotSignTx
          _ ->
            throwError UnexpectedRaceStatus
