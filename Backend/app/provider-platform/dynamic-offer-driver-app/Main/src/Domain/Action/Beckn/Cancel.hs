{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Cancel
  ( cancel,
    CancelReq (..),
    CancelSearchReq (..),
    validateCancelRequest,
    validateCancelSearchRequest,
    cancelSearch,
  )
where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequest as DSR
import EulerHS.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqLocDBFlow, EsqLocRepDBFlow)
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.DriverLocation as DLoc
import qualified SharedLogic.DriverMode as DMode
import qualified SharedLogic.Ride as SRide
import qualified SharedLogic.SearchTryLocker as CS
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Person as QPers
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import Tools.Metrics
import qualified Tools.Notifications as Notify

newtype CancelReq = CancelReq
  { bookingId :: Id SRB.Booking
  }

newtype CancelSearchReq = CancelSearchReq
  { transactionId :: Text
  }

cancel ::
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EsqLocDBFlow m r,
    EsqLocRepDBFlow m r,
    HedisFlow m r,
    CacheFlow m r,
    HasHttpClientOptions r c,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasLongDurationRetryCfg r c,
    CoreMetrics m
  ) =>
  -- Id DM.Merchant ->
  -- SignatureAuthResult ->
  CancelReq ->
  DM.Merchant ->
  SRB.Booking ->
  m ()
cancel req merchant booking = do
  -- merchant <-
  --   QM.findById merchantId
  --     >>= fromMaybeM (MerchantNotFound merchantId.getId)
  -- booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  -- let merchantId' = booking.providerId
  -- unless (merchantId' == merchantId) $ throwError AccessDenied
  mbRide <- QRide.findActiveByRBId req.bookingId
  bookingCR <- buildBookingCancellationReason
  Esq.runTransaction $ do
    QBCR.upsert bookingCR
    QRB.updateStatus booking.id SRB.CANCELLED
    whenJust mbRide $ \ride -> do
      QRide.updateStatus ride.id SRide.CANCELLED
      driverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      QDFS.updateStatus ride.driverId $ DMode.getDriverStatus driverInfo.mode driverInfo.active
    whenJust mbRide $ \ride -> do
      QDI.updateOnRide (cast ride.driverId) False
  whenJust mbRide $ \ride -> do
    SRide.clearCache $ cast ride.driverId
    DLoc.updateOnRideCacheForCancelledOrEndRide (cast ride.driverId) booking.providerId

  logTagInfo ("bookingId-" <> getId req.bookingId) ("Cancellation reason " <> show bookingCR.source)
  fork "cancelBooking - Notify BAP" $ do
    let mbRideId = case mbRide of
          Just ride -> Just ride.id
          Nothing -> Nothing
    BP.sendBookingCancelledToBAP booking merchant mbRideId bookingCR.source
  whenJust mbRide $ \ride ->
    fork "cancelRide - Notify driver" $ do
      driver <- QPers.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      Notify.notifyOnCancel merchant.id booking driver.id driver.deviceToken bookingCR.source
  where
    buildBookingCancellationReason = do
      return $
        DBCR.BookingCancellationReason
          { bookingId = req.bookingId,
            rideId = Nothing,
            merchantId = Just booking.providerId,
            source = DBCR.ByUser,
            reasonCode = Nothing,
            driverId = Nothing,
            additionalInfo = Nothing,
            driverCancellationLocation = Nothing,
            driverDistToPickup = Nothing,
            ..
          }

cancelSearch ::
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EsqLocDBFlow m r,
    EsqLocRepDBFlow m r,
    HedisFlow m r,
    CacheFlow m r,
    HasHttpClientOptions r c,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasLongDurationRetryCfg r c,
    CoreMetrics m
  ) =>
  Id DM.Merchant ->
  CancelSearchReq ->
  Id DSR.SearchRequest ->
  m ()
cancelSearch merchantId req searchRequestId = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  searchTry <- QST.findActiveTryByRequestId searchRequestId >>= fromMaybeM (SearchTryDoesNotExist $ "searchRequestId-" <> searchRequestId.getId)
  CS.whenSearchTryCancellable searchTry.id $ do
    driverSearchReqs <- QSRD.findAllActiveBySRId searchRequestId
    logTagInfo ("transactionId-" <> req.transactionId) "Search Request Cancellation"
    DB.runTransaction $ do
      QST.cancelActiveTriesByRequestId searchRequestId
      QSRD.setInactiveBySRId searchRequestId
      QDQ.setInactiveBySRId searchRequestId
    fork "cancelSearchReq - Notify BAP" $ do
      BP.sendSearchReqCancelToBAP merchant searchRequestId
    for_ driverSearchReqs $ \driverReq -> do
      driver_ <- QPerson.findById driverReq.driverId >>= fromMaybeM (PersonNotFound driverReq.driverId.getId)
      Notify.notifyOnCancelSearchRequest merchantId driverReq.driverId driver_.deviceToken driverReq.searchTryId

validateCancelSearchRequest ::
  ( EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  SignatureAuthResult ->
  CancelSearchReq ->
  m (Id DSR.SearchRequest)
validateCancelSearchRequest _ _ req = do
  let transactionId = req.transactionId
  QSR.findByTransactionId transactionId >>= fromMaybeM (SearchRequestNotFound $ "transactionId-" <> transactionId)

validateCancelRequest ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  SignatureAuthResult ->
  CancelReq ->
  m (DM.Merchant, SRB.Booking)
validateCancelRequest merchantId _ req = do
  merchant <-
    QM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  let merchantId' = booking.providerId
  unless (merchantId' == merchantId) $ throwError AccessDenied
  return (merchant, booking)
