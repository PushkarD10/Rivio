{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Cancel where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DMerc
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import Domain.Types.TransporterConfig (TransporterConfig)
import qualified Domain.Types.Vehicle as DVeh
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics (DeploymentVersion)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler (SchedulerType)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import qualified SharedLogic.CallBAP as BP
import SharedLogic.DriverPool
import qualified SharedLogic.DriverPool as DP
import qualified SharedLogic.DriverPool.Types as SDT
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FarePolicy
import SharedLogic.GoogleTranslate (TranslateFlow)
import SharedLogic.Ride (multipleRouteKey, searchRequestKey)
import SharedLogic.SearchTry
import qualified Storage.Cac.TransporterConfig as QTC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import qualified Tools.Metrics as Metrics
import TransactionLogs.Types
import Utils.Common.Cac.KeyNameConstants

reAllocateBookingIfPossible ::
  ( MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime,
    HasField "version" r DeploymentVersion,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    TranslateFlow m r,
    LT.HasLocationService m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasShortDurationRetryCfg r c,
    Redis.HedisFlow m r
  ) =>
  Bool ->
  Bool ->
  DMerc.Merchant ->
  SRB.Booking ->
  DRide.Ride ->
  DP.Person ->
  DVeh.Vehicle ->
  SBCR.BookingCancellationReason ->
  Bool ->
  m Bool
reAllocateBookingIfPossible isValueAddNP userReallocationEnabled merchant booking ride driver vehicle bookingCReason isForceReallocation = do
  now <- getCurrentTime
  case booking.tripCategory of
    DTC.OneWay DTC.OneWayOnDemandDynamicOffer -> do
      driverQuote <- QDQ.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
      searchTry <- QST.findById driverQuote.searchTryId >>= fromMaybeM (SearchTryNotFound driverQuote.searchTryId.getId)
      searchReq <- QSR.findById searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
      transporterConfig <- QTC.findByMerchantOpCityId booking.merchantOperatingCityId (Just (TransactionId $ Id booking.transactionId)) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
      isRepeatSearch <- checkIfRepeatSearch searchTry ride.driverArrivalTime searchReq.isReallocationEnabled now booking.isScheduled transporterConfig
      if isRepeatSearch
        then do
          DP.addDriverToSearchCancelledList searchReq.id ride.driverId

          tripQuoteDetails <-
            if length searchTry.estimateIds > 1
              then traverse (createQuoteDetails searchReq searchTry) searchTry.estimateIds
              else do
                quoteDetail <- createQuoteDetails searchReq searchTry driverQuote.estimateId.getId
                return [quoteDetail]

          let driverSearchBatchInput =
                DriverSearchBatchInput
                  { sendSearchRequestToDrivers = sendSearchRequestToDrivers',
                    merchant,
                    searchReq,
                    tripQuoteDetails,
                    customerExtraFee = searchTry.customerExtraFee,
                    messageId = searchTry.messageId,
                    isRepeatSearch
                  }
          result <- try @_ @SomeException (initiateDriverSearchBatch driverSearchBatchInput)
          case result of
            Right _ -> do
              if isValueAddNP
                then do
                  BP.sendEstimateRepetitionUpdateToBAP booking ride (Id searchTry.estimateId) bookingCReason.source driver vehicle
                  return True
                else cancelRideTransactionForNonReallocation Nothing (Just searchTry.estimateId)
            Left _ -> cancelRideTransactionForNonReallocation Nothing (Just searchTry.estimateId)
        else cancelRideTransactionForNonReallocation Nothing (Just searchTry.estimateId)
    DTC.Rental DTC.OnDemandStaticOffer -> reallocateStaticOffer now
    DTC.InterCity DTC.OneWayOnDemandStaticOffer _ -> reallocateStaticOffer now
    _ -> cancelRideTransactionForNonReallocation Nothing Nothing
  where
    reallocateStaticOffer now = do
      quote <- QQuote.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
      searchReq <- QSR.findById quote.searchRequestId >>= fromMaybeM (SearchRequestNotFound quote.searchRequestId.getId)
      searchTry <- QST.findLastByRequestId quote.searchRequestId >>= fromMaybeM (SearchTryNotFound quote.searchRequestId.getId)
      transporterConfig <- QTC.findByMerchantOpCityId booking.merchantOperatingCityId (Just (TransactionId $ Id booking.transactionId)) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
      isRepeatSearch <- checkIfRepeatSearch searchTry ride.driverArrivalTime searchReq.isReallocationEnabled now booking.isScheduled transporterConfig
      if isRepeatSearch || isForceReallocation
        then do
          DP.addDriverToSearchCancelledList searchReq.id ride.driverId
          bookingId <- generateGUID
          quoteId <- generateGUID
          fareParamsId <- generateGUID
          searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
          let newIsScheduled = booking.isScheduled && transporterConfig.scheduleRideBufferTime `addUTCTime` now < searchReq.startTime
              newFareParams = quote.fareParams{id = fareParamsId, updatedAt = now}
              newQuote = quote{id = Id quoteId, fareParams = newFareParams, validTill = searchRequestExpirationSeconds `addUTCTime` now, isScheduled = booking.isScheduled} -- check if validTill req'D
              newBooking = booking{id = bookingId, quoteId = quoteId, status = SRB.NEW, isScheduled = newIsScheduled, startTime = max now booking.startTime, createdAt = now, updatedAt = now}
              mbDriverExtraFeeBounds = ((,) <$> searchReq.estimatedDistance <*> (join $ (.driverExtraFeeBounds) <$> quote.farePolicy)) <&> \(dist, driverExtraFeeBounds) -> DFP.findDriverExtraFeeBoundsByDistance dist driverExtraFeeBounds
              driverPickUpCharge = join $ USRD.extractDriverPickupCharges <$> ((.farePolicyDetails) <$> quote.farePolicy)
              driverParkingCharge = join $ (.parkingCharge) <$> quote.farePolicy
          tripQuoteDetail <- buildTripQuoteDetail searchReq booking.tripCategory booking.vehicleServiceTier quote.vehicleServiceTierName booking.estimatedFare (Just booking.isDashboardRequest) (mbDriverExtraFeeBounds <&> (.minFee)) (mbDriverExtraFeeBounds <&> (.maxFee)) (mbDriverExtraFeeBounds <&> (.stepFee)) (mbDriverExtraFeeBounds <&> (.defaultStepFee)) driverPickUpCharge driverParkingCharge quoteId
          void $ clearCachedFarePolicyByEstOrQuoteId booking.quoteId
          QQuote.create newQuote
          QRB.createBooking newBooking
          let driverSearchBatchInput =
                DriverSearchBatchInput
                  { sendSearchRequestToDrivers = sendSearchRequestToDrivers',
                    merchant,
                    searchReq,
                    tripQuoteDetails = [tripQuoteDetail],
                    customerExtraFee = searchTry.customerExtraFee,
                    messageId = newBooking.id.getId,
                    isRepeatSearch
                  }
          result <- try @_ @SomeException (initiateDriverSearchBatch driverSearchBatchInput) --  remove CONSTRAINT quote_unique_reqid_bppid_quoteid UNIQUE (request_id, provider_id);, check searchTry key for scheduling
          case result of
            Right _ -> do
              if isValueAddNP
                then do
                  BP.sendQuoteRepetitionUpdateToBAP booking ride newBooking.id bookingCReason.source driver vehicle
                  return True
                else do
                  cancelRideTransactionForNonReallocation (Just newBooking) Nothing
            Left _ -> do
              cancelRideTransactionForNonReallocation (Just newBooking) Nothing
        else cancelRideTransactionForNonReallocation Nothing Nothing
    createQuoteDetails ::
      ( MonadFlow m,
        CacheFlow m r,
        EsqDBFlow m r,
        EsqDBReplicaFlow m r
      ) =>
      DSR.SearchRequest ->
      DST.SearchTry ->
      Text ->
      m SDT.TripQuoteDetail
    createQuoteDetails searchReq searchTry estimateId = do
      estimate <- QEst.findById (Id estimateId) >>= fromMaybeM (EstimateNotFound estimateId)
      let mbDriverExtraFeeBounds = ((,) <$> estimate.estimatedDistance <*> (join $ (.driverExtraFeeBounds) <$> estimate.farePolicy)) <&> \(dist, driverExtraFeeBounds) -> DFP.findDriverExtraFeeBoundsByDistance dist driverExtraFeeBounds
          driverPickUpCharge = join $ USRD.extractDriverPickupCharges <$> ((.farePolicyDetails) <$> estimate.farePolicy)
          driverParkingCharge = join $ (.parkingCharge) <$> estimate.farePolicy
      buildTripQuoteDetail searchReq estimate.tripCategory estimate.vehicleServiceTier estimate.vehicleServiceTierName (estimate.minFare + fromMaybe 0 searchTry.customerExtraFee) (Just booking.isDashboardRequest) (mbDriverExtraFeeBounds <&> (.minFee)) (mbDriverExtraFeeBounds <&> (.maxFee)) (mbDriverExtraFeeBounds <&> (.stepFee)) (mbDriverExtraFeeBounds <&> (.defaultStepFee)) driverPickUpCharge driverParkingCharge estimate.id.getId
    cancelRideTransactionForNonReallocation ::
      ( MonadFlow m,
        Redis.HedisFlow m r,
        EsqDBReplicaFlow m r,
        CacheFlow m r,
        EsqDBFlow m r
      ) =>
      Maybe SRB.Booking ->
      Maybe Text ->
      m Bool
    cancelRideTransactionForNonReallocation mbNewBooking mbEstimateId = do
      Redis.del $ multipleRouteKey booking.transactionId
      Redis.del $ searchRequestKey booking.transactionId
      whenJust mbEstimateId $ \estimateId ->
        void $ clearCachedFarePolicyByEstOrQuoteId estimateId
      whenJust mbNewBooking $ \newBooking -> do
        bookingCancellationReason <- buildBookingCancellationReason newBooking
        QBCR.upsert bookingCancellationReason
        QRB.updateStatus newBooking.id SRB.CANCELLED
      void $ clearCachedFarePolicyByEstOrQuoteId booking.quoteId -- shouldn't be required for new booking
      return False
    checkIfRepeatSearch :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DST.SearchTry -> Maybe UTCTime -> Maybe Bool -> UTCTime -> Bool -> TransporterConfig -> m Bool
    checkIfRepeatSearch searchTry driverArrivalTime isReallocationEnabled now isScheduled transporterConfig = do
      let searchRepeatLimit = transporterConfig.searchRepeatLimit
          isSearchTryValid = searchTry.validTill > now
          arrivedPickupThreshold = highPrecMetersToMeters transporterConfig.arrivedPickupThreshold
          driverHasNotArrived = isNothing driverArrivalTime || maybe True (> arrivedPickupThreshold) bookingCReason.driverDistToPickup
      return $
        searchTry.searchRepeatCounter < searchRepeatLimit
          && (bookingCReason.source == SBCR.ByDriver || (bookingCReason.source == SBCR.ByUser && userReallocationEnabled))
          && (isSearchTryValid || isScheduled)
          && fromMaybe False isReallocationEnabled
          && driverHasNotArrived

    buildBookingCancellationReason newBooking = do
      return $
        SBCR.BookingCancellationReason
          { bookingId = newBooking.id,
            rideId = Nothing,
            merchantId = Just newBooking.providerId,
            source = SBCR.ByApplication,
            reasonCode = Nothing,
            driverId = Nothing,
            additionalInfo = Just "Reallocation Failed",
            driverCancellationLocation = Nothing,
            driverDistToPickup = Nothing,
            distanceUnit = newBooking.distanceUnit,
            ..
          }
