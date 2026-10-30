module CardanoRacers.Hydra.Handlers.SignAnnounceDistrTx
  ( signAnnounceDistrTxHandler
  , signAnnounceDistrTxHandlerImpl
  ) where

import Prelude

import Aeson (stringifyAeson)
import Cardano.Types (Vkeywitness)
import CardanoRacers.Hydra.Codec (vkeyWitnessCodec)
import CardanoRacers.Hydra.Contracts.AnnounceDistr (mkAnnounceRewardDistributionTx)
import CardanoRacers.Hydra.Handlers.SignAnnounceDistrTx.Types
  ( SignAnnounceDistrTxError
      ( CouldNotDecodeReqBody
      , RequestedRaceNotHosted
      , UnexpectedRaceStatus
      , TxValidationFailed
      , CouldNotSignTx
      )
  , signAnnounceDistrTxErrorCodec
  , signAnnounceDistrTxRequestPayloadCodec
  )
import CardanoRacers.Hydra.Lib.Transaction (signTxReturnSignature)
import CardanoRacers.Hydra.Monad (AppM, findRaceEntryByRaceCs, liftContract)
import CardanoRacers.Hydra.RewardDistribution (distributeRewards)
import CardanoRacers.Hydra.Types.RaceStatus (RaceStatus(DistributingRewards))
import Control.Error.Util ((!?))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Codec.Argonaut (encode, printJsonDecodeError) as CA
import Data.Either (Either(Left, Right), either)
import Effect.Class (liftEffect)
import Effect.Ref (read) as Ref
import HTTPure (Response) as HTTPure
import HTTPure (Status, response)
import HTTPure.Status (badRequest, conflict, created, forbidden, internalServerError) as Status
import HydraSdk.Lib (caDecodeString)

signAnnounceDistrTxHandler :: String -> AppM HTTPure.Response
signAnnounceDistrTxHandler =
  either
    ( \e -> response (errorStatus e) $ stringifyAeson $ CA.encode signAnnounceDistrTxErrorCodec
        e
    )
    ( \wit -> response Status.created $ stringifyAeson $ CA.encode vkeyWitnessCodec wit
    )
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

errorStatus :: SignAnnounceDistrTxError -> Status
errorStatus =
  case _ of
    CouldNotDecodeReqBody _ ->
      Status.badRequest
    RequestedRaceNotHosted ->
      Status.conflict
    UnexpectedRaceStatus ->
      Status.conflict
    TxValidationFailed ->
      Status.forbidden
    CouldNotSignTx ->
      Status.internalServerError
