{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Frontend
  ( GetPersonFlowStatusRes,
    FrontendEvent (..),
    NotifyEventReq (..),
    NotifyEventResp,
    getPersonFlowStatus,
    notifyEvent,
  )
where

import qualified Data.HashMap.Strict as HM
import Domain.Action.UI.Quote
import Domain.Types.CancellationReason
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as SRide
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.ValueAddNP as QNP
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Notifications as Notify
import TransactionLogs.Types

data GetPersonFlowStatusRes = GetPersonFlowStatusRes
  { oldStatus :: Maybe DPFS.FlowStatus,
    currentStatus :: DPFS.FlowStatus,
    isValueAddNP :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FrontendEvent = RATE_DRIVER_SKIPPED | SEARCH_CANCELLED
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype NotifyEventReq = NotifyEventReq
  { event :: FrontendEvent
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type NotifyEventResp = APISuccess

getPersonFlowStatus :: Id DP.Person -> Id DM.Merchant -> Maybe Bool -> Flow GetPersonFlowStatusRes
getPersonFlowStatus personId merchantId mIsPolling = do
  -- should not be run in replica
  personStatus <- QPFS.getStatus personId >>= fromMaybeM (PersonNotFound personId.getId)
  logTagDebug "person-status" . show $ personStatus
  case personStatus of
    DPFS.SEARCHING _ _ -> expirePersonStatusIfNeeded personStatus Nothing
    DPFS.GOT_ESTIMATE _ _ -> expirePersonStatusIfNeeded personStatus Nothing
    DPFS.WAITING_FOR_DRIVER_OFFERS estimateId _ _ -> do
      estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
      findValueAddNP estimate.providerId personStatus
    DPFS.DRIVER_OFFERED_QUOTE estimateId _ -> do
      estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
      findValueAddNP estimate.providerId personStatus
    DPFS.WAITING_FOR_DRIVER_ASSIGNMENT _ _ _ -> expirePersonStatusIfNeeded personStatus Nothing
    DPFS.RIDE_PICKUP {} -> handleRideTracking personId merchantId mIsPolling personStatus
    DPFS.RIDE_STARTED {} -> handleRideTracking personId merchantId mIsPolling personStatus
    DPFS.DRIVER_ARRIVED {} -> handleRideTracking personId merchantId mIsPolling personStatus
    a -> return $ GetPersonFlowStatusRes Nothing a Nothing
  where
    findValueAddNP providerId personStatus = do
      isValueAddNP_ <- QNP.isValueAddNP providerId
      expirePersonStatusIfNeeded personStatus (Just isValueAddNP_)

    expirePersonStatusIfNeeded personStatus isValueAddNp = do
      now <- getCurrentTime
      if now < personStatus.validTill
        then return $ GetPersonFlowStatusRes Nothing personStatus isValueAddNp
        else do
          _ <- QPFS.updateStatus personId DPFS.IDLE
          return $ GetPersonFlowStatusRes (Just personStatus) DPFS.IDLE isValueAddNp

notifyEvent :: (KvDbFlow m r, EncFlow m r, HasField "shortDurationRetryCfg" r RetryCfg, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], HasFlowEnv m r '["nwAddress" ::: BaseUrl], Esq.EsqDBReplicaFlow m r, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig]) => Id DP.Person -> NotifyEventReq -> m NotifyEventResp
notifyEvent personId req = do
  _ <- case req.event of
    RATE_DRIVER_SKIPPED -> QPFS.updateStatus personId DPFS.IDLE
    SEARCH_CANCELLED -> do
      activeBooking <- B.runInReplica $ QB.findLatestByRiderId personId
      whenJust activeBooking $ \booking -> processActiveBooking booking OnSearch
      QPFS.updateStatus personId DPFS.IDLE
  QPFS.clearCache personId
  pure APISuccess.Success

handleRideTracking ::
  ( KvDbFlow m r,
    EncFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Maybe Bool ->
  DPFS.FlowStatus ->
  m GetPersonFlowStatusRes
handleRideTracking _ _ Nothing DPFS.RIDE_STARTED {..} = return $ GetPersonFlowStatusRes Nothing (DPFS.RIDE_ASSIGNED rideId) Nothing -- handle backward compatibility, if isPolling is Nothing means old version of API Call , isValueAddNP is Nothing as its not needed in rideStage
handleRideTracking personId _ (Just isPolling) DPFS.RIDE_STARTED {..} = do
  trackUrl <- getTrackUrl rideId trackingUrl
  newDriverLocation <- if isPolling then Just . (.currPoint) <$> CallBPP.callGetDriverLocation trackUrl else return Nothing
  let updatedStatus = DPFS.RIDE_STARTED {trackingUrl = trackUrl, driverLocation = newDriverLocation, ..}
  updateStatus personId updatedStatus
  return $ GetPersonFlowStatusRes Nothing updatedStatus Nothing
handleRideTracking _ _ Nothing DPFS.RIDE_PICKUP {..} = return $ GetPersonFlowStatusRes Nothing (DPFS.RIDE_ASSIGNED rideId) Nothing -- handle backward compatibility, if isPolling is Nothing means old version of API Call, isValueAddNP is Nothing as its not needed in rideStage
handleRideTracking personId merchantId (Just isPolling) DPFS.RIDE_PICKUP {..} = do
  trackUrl <- getTrackUrl rideId trackingUrl
  newDriverLocation <-
    if isPolling
      then do
        location <- CallBPP.callGetDriverLocation trackUrl
        notifyOnTheWayOrReached location
        return $ Just location.currPoint
      else return Nothing
  let updatedStatus = DPFS.RIDE_PICKUP {trackingUrl = trackUrl, driverLocation = newDriverLocation, ..}
  updateStatus personId updatedStatus
  return $ GetPersonFlowStatusRes Nothing updatedStatus Nothing -- isValueAddNP is Nothing as its not needed in rideStage
  where
    notifyOnTheWayOrReached location = do
      merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
      mbIsOnTheWayNotified <- Redis.get @() driverOnTheWay
      mbHasReachedNotified <- Redis.get @() driverHasReached

      when (isNothing mbIsOnTheWayNotified || isNothing mbHasReachedNotified) $ do
        let distance = highPrecMetersToMeters $ distanceBetweenInMeters fromLocation location.currPoint
        mbStartDistance <- Redis.get @Meters distanceUpdates
        case mbStartDistance of
          Nothing -> Redis.setExp distanceUpdates distance 3600
          Just startDistance -> when (startDistance - 50 > distance) $ do
            unless (isJust mbIsOnTheWayNotified) $ do
              Notify.notifyDriverOnTheWay personId
              Redis.setExp driverOnTheWay () merchant.driverOnTheWayNotifyExpiry.getSeconds
            when (isNothing mbHasReachedNotified && distance <= distanceToMeters merchant.arrivedPickupThreshold) $ do
              Notify.notifyDriverHasReached personId otp vehicleNumber
              Redis.setExp driverHasReached () 1500
      where
        distanceUpdates = "Ride:GetDriverLoc:DriverDistance " <> rideId.getId
        driverOnTheWay = "Ride:GetDriverLoc:DriverIsOnTheWay " <> rideId.getId
        driverHasReached = "Ride:GetDriverLoc:DriverHasReached " <> rideId.getId
handleRideTracking _ _ Nothing DPFS.DRIVER_ARRIVED {..} = return $ GetPersonFlowStatusRes Nothing (DPFS.RIDE_ASSIGNED rideId) Nothing -- handle backward compatibility, if isPolling is Nothing means old version of API Call, isValueAddNP is Nothing as its not needed in rideStage
handleRideTracking personId _ (Just isPolling) DPFS.DRIVER_ARRIVED {..} = do
  trackUrl <- getTrackUrl rideId trackingUrl
  newDriverLocation <- if isPolling then Just . (.currPoint) <$> CallBPP.callGetDriverLocation trackUrl else return Nothing
  let updatedStatus = DPFS.DRIVER_ARRIVED {trackingUrl = trackUrl, driverLocation = newDriverLocation, ..}
  updateStatus personId updatedStatus
  return $ GetPersonFlowStatusRes Nothing updatedStatus Nothing -- isValueAddNP is Nothing as its not needed in rideStage
handleRideTracking _ _ _ status = return $ GetPersonFlowStatusRes Nothing status Nothing -- isValueAddNP is Nothing as its not needed in rideStage

updateStatus :: KvDbFlow m r => Id DP.Person -> DPFS.FlowStatus -> m ()
updateStatus personId updatedStatus = do
  _ <- QPFS.updateStatus personId updatedStatus
  QPFS.clearCache personId

getTrackUrl :: (Esq.EsqDBReplicaFlow m r, KvDbFlow m r) => Id SRide.Ride -> Maybe BaseUrl -> m (Maybe BaseUrl)
getTrackUrl rideId mTrackUrl = do
  case mTrackUrl of
    Nothing -> do
      ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
      return ride.trackingUrl
    a -> return a
