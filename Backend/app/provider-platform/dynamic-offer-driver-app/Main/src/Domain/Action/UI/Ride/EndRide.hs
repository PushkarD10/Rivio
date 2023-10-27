{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride.EndRide
  ( ServiceHandle (..),
    DriverEndRideReq (..),
    CallBasedEndRideReq (..),
    DashboardEndRideReq (..),
    CronJobEndRideReq (..),
    EndRideResp (..),
    callBasedEndRide,
    buildEndRideHandle,
    driverEndRide,
    dashboardEndRide,
    cronJobEndRide,
  )
where

import AWS.S3 as S3
import Data.OpenApi.Internal.Schema (ToSchema)
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Action.UI.Ride.EndRide.Internal as RideEndInt
import Domain.Action.UI.Route as DMaps
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import Domain.Types.FareParameters as Fare
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Location as DL
import qualified Domain.Types.MediaFile as Domain
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Merchant.TransporterConfig as DTConf
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderDetails as RD
import qualified Domain.Types.Vehicle as DVeh
import Environment (Flow)
import EulerHS.Prelude hiding (id, pi)
import Kernel.External.Maps
import qualified Kernel.External.Maps.Interface.Types as Maps
import qualified Kernel.External.Maps.Types as Maps
import Kernel.Prelude (roundToIntegral)
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Kernel.Utils.DatastoreLatencyCalculator
import qualified Lib.LocationUpdates as LocUpd
import qualified SharedLogic.CallBAP as CallBAP
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.FareCalculator as Fare
import qualified SharedLogic.FarePolicy as FarePolicy
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.GoHomeConfig as CQGHC
import qualified Storage.CachedQueries.Merchant as MerchantS
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.TransporterConfig as CQTC
import qualified Storage.CachedQueries.Merchant.TransporterConfig as QTConf
import qualified Storage.Queries.Booking as QRB
import Storage.Queries.Driver.GoHomeFeature.DriverGoHomeRequest as QDGR
import qualified Storage.Queries.MediaFile as MFQuery
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.SMS as Sms

data EndRideReq = DriverReq DriverEndRideReq | DashboardReq DashboardEndRideReq | CallBasedReq CallBasedEndRideReq | CronJobReq CronJobEndRideReq

data EndRideResp = EndRideResp
  { result :: Text,
    homeLocationReached :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DriverEndRideReq = DriverEndRideReq
  { point :: LatLong,
    requestor :: DP.Person,
    uiDistanceCalculationWithAccuracy :: Maybe Int,
    uiDistanceCalculationWithoutAccuracy :: Maybe Int,
    odometerEndReading :: Maybe Centesimal,
    odometerEndImage :: Maybe Text,
    odometerEndImageExtension :: Maybe Text,
    endRideOtp :: Maybe Text
  }

data DashboardEndRideReq = DashboardEndRideReq
  { point :: Maybe LatLong,
    merchantId :: Id DM.Merchant
  }

data CronJobEndRideReq = CronJobEndRideReq
  { point :: Maybe LatLong,
    merchantId :: Id DM.Merchant
  }

newtype CallBasedEndRideReq = CallBasedEndRideReq
  { requestor :: DP.Person
  }

data ServiceHandle m = ServiceHandle
  { findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    findRideById :: Id DRide.Ride -> m (Maybe DRide.Ride),
    getMerchant :: Id DM.Merchant -> m (Maybe DM.Merchant),
    endRideTransaction :: Id DP.Driver -> SRB.Booking -> DRide.Ride -> Maybe FareParameters -> Maybe (Id RD.RiderDetails) -> FareParameters -> DTConf.TransporterConfig -> Id DM.Merchant -> m (),
    notifyCompleteToBAP :: SRB.Booking -> DRide.Ride -> Fare.FareParameters -> Maybe DMPM.PaymentMethodInfo -> Maybe Text -> m (),
    getFarePolicy :: Id DM.Merchant -> DVeh.Variant -> Maybe DFareProduct.Area -> DFareProduct.FlowType -> m DFP.FullFarePolicy,
    calculateFareParameters :: Fare.CalculateFareParametersParams -> m Fare.FareParameters,
    putDiffMetric :: Id DM.Merchant -> Money -> Meters -> m (),
    isDistanceCalculationFailed :: Id DP.Person -> m Bool,
    finalDistanceCalculation :: Id DRide.Ride -> Id DP.Person -> NonEmpty LatLong -> Meters -> Bool -> m (),
    getInterpolatedPoints :: Id DP.Person -> m [LatLong],
    clearInterpolatedPoints :: Id DP.Person -> m (),
    findConfig :: m (Maybe DTConf.TransporterConfig),
    whenWithLocationUpdatesLock :: Id DP.Person -> m () -> m (),
    getDistanceBetweenPoints :: LatLong -> LatLong -> [LatLong] -> m Meters,
    findPaymentMethodByIdAndMerchantId :: Id DMPM.MerchantPaymentMethod -> Id DM.Merchant -> m (Maybe DMPM.MerchantPaymentMethod),
    sendDashboardSms :: Id DM.Merchant -> Sms.DashboardMessageType -> Maybe DRide.Ride -> Id DP.Person -> Maybe SRB.Booking -> HighPrecMoney -> m (),
    uiDistanceCalculation :: Id DRide.Ride -> Maybe Int -> Maybe Int -> m ()
  }

buildEndRideHandle :: Id DM.Merchant -> Flow (ServiceHandle Flow)
buildEndRideHandle merchantId = do
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler merchantId True
  return $
    ServiceHandle
      { findBookingById = QRB.findById,
        findRideById = QRide.findById,
        getMerchant = MerchantS.findById,
        notifyCompleteToBAP = CallBAP.sendRideCompletedUpdateToBAP,
        endRideTransaction = RideEndInt.endRideTransaction,
        getFarePolicy = FarePolicy.getFarePolicy,
        calculateFareParameters = Fare.calculateFareParameters,
        putDiffMetric = RideEndInt.putDiffMetric,
        isDistanceCalculationFailed = LocUpd.isDistanceCalculationFailed defaultRideInterpolationHandler,
        finalDistanceCalculation = LocUpd.finalDistanceCalculation defaultRideInterpolationHandler,
        getInterpolatedPoints = LocUpd.getInterpolatedPoints defaultRideInterpolationHandler,
        clearInterpolatedPoints = LocUpd.clearInterpolatedPoints defaultRideInterpolationHandler,
        findConfig = QTConf.findByMerchantId merchantId,
        whenWithLocationUpdatesLock = LocUpd.whenWithLocationUpdatesLock,
        getDistanceBetweenPoints = RideEndInt.getDistanceBetweenPoints merchantId,
        findPaymentMethodByIdAndMerchantId = CQMPM.findByIdAndMerchantId,
        sendDashboardSms = Sms.sendDashboardSms,
        uiDistanceCalculation = QRide.updateUiDistanceCalculation
      }

type EndRideFlow m r = (MonadFlow m, CoreMetrics m, MonadReader r m, HasField "enableAPILatencyLogging" r Bool, HasField "enableAPIPrometheusMetricLogging" r Bool, HasField "s3Env" r (S3Env m), LT.HasLocationService m r)

driverEndRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DriverEndRideReq ->
  m EndRideResp
driverEndRide handle rideId req = do
  withLogTag ("requestorId-" <> req.requestor.id.getId)
    . endRide handle rideId
    $ DriverReq req

callBasedEndRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  CallBasedEndRideReq ->
  m EndRideResp
callBasedEndRide handle rideId = endRide handle rideId . CallBasedReq

dashboardEndRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DashboardEndRideReq ->
  m APISuccess.APISuccess
dashboardEndRide handle rideId req = do
  void $
    withLogTag ("merchantId-" <> req.merchantId.getId)
      . endRide handle rideId
      $ DashboardReq req
  return APISuccess.Success

cronJobEndRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  CronJobEndRideReq ->
  m APISuccess.APISuccess
cronJobEndRide handle rideId req = do
  void $
    withLogTag ("merchantId-" <> req.merchantId.getId)
      . endRide handle rideId
      $ CronJobReq req
  return APISuccess.Success

endRide ::
  (EndRideFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  EndRideReq ->
  m EndRideResp
endRide handle@ServiceHandle {..} rideId req = withLogTag ("rideId-" <> rideId.getId) do
  rideOld <- findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  let driverId = rideOld.driverId
  booking <- findBookingById rideOld.bookingId >>= fromMaybeM (BookingNotFound rideOld.bookingId.getId)
  case req of
    DriverReq driverReq -> do
      let requestor = driverReq.requestor
      case rideOld.rideDetails of
        DRide.DetailsOnDemand _ -> pure ()
        DRide.DetailsRental details -> do
          when (isJust driverReq.odometerEndReading) $ do
            when (driverReq.endRideOtp /= details.endRideOtp) $ throwError IncorrectOTP
      uiDistanceCalculation rideOld.id driverReq.uiDistanceCalculationWithAccuracy driverReq.uiDistanceCalculationWithoutAccuracy -- more checks?
      case requestor.role of
        DP.DRIVER -> unless (requestor.id == driverId) $ throwError NotAnExecutor
        _ -> throwError AccessDenied
    DashboardReq dashboardReq -> do
      unless (booking.providerId == dashboardReq.merchantId) $ throwError (RideDoesNotExist rideOld.id.getId)
    CronJobReq cronJobReq -> do
      unless (booking.providerId == cronJobReq.merchantId) $ throwError (RideDoesNotExist rideOld.id.getId)
    CallBasedReq callBasedEndRideReq -> do
      let requestor = callBasedEndRideReq.requestor
      case requestor.role of
        DP.DRIVER -> unless (requestor.id == driverId) $ throwError NotAnExecutor
        _ -> throwError AccessDenied

  unless (rideOld.status == DRide.INPROGRESS) $ throwError $ RideInvalidStatus "This ride cannot be ended"

  (tripEndPoint, mbOdometerEndReading) <-
    case booking.bookingDetails of
      SRB.DetailsOnDemand details -> do
        case req of
          DriverReq driverReq -> do
            logTagInfo "driver -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
            pure (driverReq.point, driverReq.odometerEndReading :: Maybe Centesimal) -- Irrelevant
          DashboardReq dashboardReq -> do
            logTagInfo "dashboard -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
            case dashboardReq.point of
              Just point -> pure (point, Nothing)
              Nothing -> do
                pure (getCoordinates details.toLocation, Nothing) -- FIXME dashboardReq.odometerEndReading
          CronJobReq cronJobReq -> do
            logTagInfo "cron job -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
            case cronJobReq.point of
              Just point -> pure (point, Nothing)
              Nothing -> do
                pure (getCoordinates details.toLocation, Nothing)
          CallBasedReq _ -> do
            pure (getCoordinates details.toLocation, Nothing)
      SRB.DetailsRental _ -> do
        case req of
          DriverReq driverReq -> do
            logTagInfo "driver -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
            case driverReq.odometerEndImage of
              Nothing -> pure ()
              Just image -> do
                person <- Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
                let merchantId = person.merchantId
                transporterConfig <- CQTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound (getId merchantId))
                imagePath <- createPath (getId rideId) driverReq.odometerEndImageExtension
                let imageUrl =
                      transporterConfig.mediaFileUrlPattern
                        & T.replace "<DOMAIN>" "driver/odometer/endImage"
                        & T.replace "<FILE_PATH>" imagePath
                _ <- try @_ @SomeException $ S3.put (T.unpack imagePath) image
                createMediaFile imageUrl rideId
            pure (driverReq.point, driverReq.odometerEndReading :: Maybe Centesimal)
          DashboardReq dashboardReq -> do
            logTagInfo "dashboard -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
            case dashboardReq.point of
              Just point -> pure (point, Nothing)
              Nothing -> do
                throwError $ InvalidRequest "End point required"
          CronJobReq cronJobReq -> do
            logTagInfo "cron job -> endRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId rideOld.id)
            case cronJobReq.point of
              Just point -> pure (point, Nothing)
              Nothing -> do
                throwError $ InvalidRequest "End point required"
          CallBasedReq _ -> throwError $ InvalidRequest "Not allowed call based ending using rental for now"

  goHomeConfig <- CQGHC.findByMerchantId booking.providerId
  ghInfo <- CQDGR.getDriverGoHomeRequestInfo driverId booking.providerId (Just goHomeConfig)

  homeLocationReached' <-
    if isNothing mbOdometerEndReading && ghInfo.status == Just DDGR.ACTIVE && goHomeConfig.enableGoHome
      then do
        case ghInfo.driverGoHomeRequestId of
          Nothing -> do
            logError "DriverGoHomeRequestId not present even though status is active."
            return Nothing
          Just ghrId -> do
            mbDriverGoHomeReq <- QDGR.findById ghrId
            case mbDriverGoHomeReq of
              Just driverGoHomeReq -> do
                let driverHomeLocation = Maps.LatLong {lat = driverGoHomeReq.lat, lon = driverGoHomeReq.lon}
                routesResp <- DMaps.getTripRoutes (driverId, booking.providerId) (buildRoutesReq tripEndPoint driverHomeLocation)
                logDebug $ "Routes resp for EndRide API :" <> show routesResp <> "(source, dest) :" <> show (tripEndPoint, driverHomeLocation)
                let driverHomeDists = mapMaybe (.distance) routesResp
                if any ((<= goHomeConfig.destRadiusMeters) . getMeters) driverHomeDists
                  then do
                    CQDGR.deactivateDriverGoHomeRequest booking.providerId driverId DDGR.SUCCESS ghInfo (Just True)
                    return $ Just True
                  else do
                    CQDGR.resetDriverGoHomeRequest booking.providerId driverId goHomeConfig ghInfo
                    return $ Just False
              Nothing -> return Nothing
      else return Nothing
  whenWithLocationUpdatesLock driverId $ do
    thresholdConfig <- findConfig >>= fromMaybeM (InternalError "TransportConfigNotFound")
    now <- getCurrentTime
    tripEndPoints <- do
      res <- LF.rideEnd rideId tripEndPoint.lat tripEndPoint.lon booking.providerId driverId
      pure $ toList res.loc
    (chargeableDistance, finalFare, mbUpdatedFareParams, ride, pickupDropOutsideOfThreshold, distanceCalculationFailed) <-
      case booking.bookingDetails of
        SRB.DetailsOnDemand details ->
          case req of
            CronJobReq _ -> do
              logTagInfo "cron job -> endRide : " "Do not call snapToRoad, return estimates as final values."
              (chargeableDistance, finalFare, mbUpdatedFareParams) <- recalculateFareForDistance handle booking rideOld booking.estimatedDistance thresholdConfig
              pure (chargeableDistance, finalFare, mbUpdatedFareParams, rideOld, Nothing, Nothing)
            _ -> do
              -- here we update the current ride, so below we fetch the updated version
              pickupDropOutsideOfThreshold <- isPickupDropOutsideOfThreshold booking details.toLocation rideOld tripEndPoint thresholdConfig
              whenJust (nonEmpty tripEndPoints) \tripEndPoints' -> do
                withTimeAPI "endRide" "finalDistanceCalculation" $ finalDistanceCalculation rideOld.id driverId tripEndPoints' booking.estimatedDistance pickupDropOutsideOfThreshold

              ride <- findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)

              distanceCalculationFailed <- withTimeAPI "endRide" "isDistanceCalculationFailed" $ isDistanceCalculationFailed driverId
              when distanceCalculationFailed $ logWarning $ "Failed to calculate distance for this ride: " <> ride.id.getId
              (chargeableDistance, finalFare, mbUpdatedFareParams) <-
                if distanceCalculationFailed
                  then calculateFinalValuesForFailedDistanceCalculations handle booking ride tripEndPoint pickupDropOutsideOfThreshold thresholdConfig
                  else calculateFinalValuesForCorrectDistanceCalculations handle booking ride booking.maxEstimatedDistance pickupDropOutsideOfThreshold thresholdConfig
              pure (chargeableDistance, finalFare, mbUpdatedFareParams, ride, Just pickupDropOutsideOfThreshold, Just distanceCalculationFailed)
        SRB.DetailsRental _ -> do
          rideDetails <- case rideOld.rideDetails of
            DRide.DetailsOnDemand _ -> throwError (InternalError "on demand ride is not allowed for rental booking")
            DRide.DetailsRental details -> pure details
          odometerEndReading <- mbOdometerEndReading & fromMaybeM (InvalidRequest "odometerEndReading is mandatory for rentals")
          farePolicy <- getFarePolicy booking.providerId booking.vehicleVariant booking.area DFareProduct.RENTAL
          logInfo $ "farePolicia :" <> show farePolicy
          (recalcDistance, finalFare, mbUpdatedFareParams) <- do
            recalculateFareForDistance handle booking rideOld (Meters $ round (odometerEndReading - fromMaybe 0 rideDetails.odometerStartReading) * 1000) thresholdConfig
          pure (recalcDistance, finalFare, mbUpdatedFareParams, rideOld, Nothing, Nothing)
    let newFareParams = fromMaybe booking.fareParams mbUpdatedFareParams
    let updRide =
          ride{tripEndTime = Just now,
               chargeableDistance = Just chargeableDistance,
               fare = Just finalFare,
               tripEndPos = Just tripEndPoint,
               fareParametersId = Just newFareParams.id,
               distanceCalculationFailed = distanceCalculationFailed,
               pickupDropOutsideOfThreshold = pickupDropOutsideOfThreshold,
               rideDetails = case rideOld.rideDetails of
                 DRide.DetailsOnDemand DRide.RideDetailsOnDemand {} -> rideOld.rideDetails
                 DRide.DetailsRental details -> DRide.DetailsRental (details{odometerEndReading = mbOdometerEndReading})
              }
    -- we need to store fareParams only when they changed
    withTimeAPI "endRide" "endRideTransaction" $ endRideTransaction (cast @DP.Person @DP.Driver driverId) booking updRide mbUpdatedFareParams booking.riderId newFareParams thresholdConfig booking.providerId
    withTimeAPI "endRide" "clearInterpolatedPoints" $ clearInterpolatedPoints driverId

    mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
      findPaymentMethodByIdAndMerchantId paymentMethodId booking.providerId
        >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
    let mbPaymentUrl = DMPM.getPostpaidPaymentUrl =<< mbPaymentMethod
    let mbPaymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
    withTimeAPI "endRide" "notifyCompleteToBAP" $ notifyCompleteToBAP booking updRide newFareParams mbPaymentMethodInfo mbPaymentUrl

    fork "sending dashboardSMS - CallbasedEndRide " $ do
      case req of
        CallBasedReq callBasedEndRideReq -> do
          let requestor = callBasedEndRideReq.requestor
          sendDashboardSms requestor.merchantId Sms.ENDRIDE (Just ride) driverId (Just booking) (fromIntegral finalFare)
        _ -> pure ()

  return $ EndRideResp {result = "Success", homeLocationReached = homeLocationReached'}
  where
    createPath ::
      (MonadTime m, Log m, MonadThrow m, MonadReader r m, HasField "s3Env" r (S3.S3Env m)) =>
      Text ->
      Maybe Text ->
      m Text
    createPath rideId' imageType = do
      pathPrefix <- asks (.s3Env.pathPrefix)
      let fileName = T.pack "endOdometerReadingImage"
      extension <- case imageType of
        Just "image/jpeg" -> pure "jpg"
        _ -> throwError $ InvalidRequest "File Format Not Supported"
      return
        ( pathPrefix <> "/ride-odometer-reading/" <> "/"
            <> rideId'
            <> "/"
            <> fileName
            <> extension
        )
    createMediaFile fileUrl rideId' = do
      id' <- generateGUID
      now <- getCurrentTime
      let fileEntity =
            Domain.MediaFile
              { id = id',
                _type = Domain.Image,
                url = fileUrl,
                createdAt = now
              }
      MFQuery.create fileEntity
      QRide.updateOdometerEndReadingImageId rideId' (Just $ getId fileEntity.id)
    buildRoutesReq tripEndPoint driverHomeLocation =
      Maps.GetRoutesReq
        { waypoints = tripEndPoint :| [driverHomeLocation],
          mode = Nothing,
          calcPoints = True
        }

recalculateFareForDistance :: (MonadThrow m, Log m, MonadTime m, MonadGuid m) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> Meters -> DTConf.TransporterConfig -> m (Meters, Money, Maybe FareParameters)
recalculateFareForDistance ServiceHandle {..} booking ride recalcDistance thresholdConfig = do
  let merchantId = booking.providerId
      oldDistance = booking.estimatedDistance
  now <- getCurrentTime
  actualDuration <- case ride.tripStartTime of
    Nothing -> throwError $ InternalError "No start reading found" -- Impossible case
    Just startTime -> pure $ round (now `diffUTCTime` startTime)
  -- maybe compare only distance fare?
  let estimatedFare = Fare.fareSum booking.fareParams
  tripEndTime <- getCurrentTime
  let flowType = SRB.castFlowType booking.bookingType
  farePolicy <- getFarePolicy merchantId booking.vehicleVariant booking.area flowType
  fareParams <- case ride.rideDetails of
    DRide.DetailsOnDemand DRide.RideDetailsOnDemand {} ->
      calculateFareParameters
        Fare.CalculateFareParametersParams
          { farePolicy = farePolicy,
            distance = recalcDistance,
            rideTime = booking.startTime,
            waitingTime = secondsToMinutes . roundToIntegral <$> (diffUTCTime <$> ride.tripStartTime <*> ride.driverArrivalTime),
            actualRideDuration = roundToIntegral <$> (diffUTCTime <$> Just tripEndTime <*> ride.tripStartTime),
            avgSpeedOfVehicle = thresholdConfig.avgSpeedOfVehicle,
            driverSelectedFare = booking.fareParams.driverSelectedFare,
            customerExtraFee = booking.fareParams.customerExtraFee,
            nightShiftCharge = booking.fareParams.nightShiftCharge, -- TODO: Make below checks on bookingType
            rentalRideParams = Nothing
          }
    DRide.DetailsRental DRide.RideDetailsRental {odometerStartReading} ->
      calculateFareParameters
        Fare.CalculateFareParametersParams
          { farePolicy = farePolicy,
            distance = recalcDistance,
            rideTime = booking.startTime,
            waitingTime = secondsToMinutes . roundToIntegral <$> (diffUTCTime <$> ride.tripStartTime <*> ride.driverArrivalTime),
            actualRideDuration = roundToIntegral <$> (diffUTCTime <$> Just tripEndTime <*> ride.tripStartTime),
            avgSpeedOfVehicle = thresholdConfig.avgSpeedOfVehicle,
            driverSelectedFare = booking.fareParams.driverSelectedFare,
            customerExtraFee = booking.fareParams.customerExtraFee,
            nightShiftCharge = booking.fareParams.nightShiftCharge, -- TODO: Make below checks on bookingType
            rentalRideParams =
              if isJust odometerStartReading
                then
                  Just $
                    Fare.RentalRideParams
                      { rideStartTime = ride.tripStartTime,
                        rideEndTime = now,
                        actualDistanceInKm = recalcDistance.getMeters `div` 1000,
                        chargedDurationInHr = (max actualDuration booking.estimatedDuration.getSeconds) `div` 3600,
                        nightShiftOverlapChecking = True,
                        timeDiffFromUtc = thresholdConfig.timeDiffFromUtc
                      }
                else Nothing
          }
  let finalFare = Fare.fareSum fareParams
      distanceDiff = recalcDistance - oldDistance
      fareDiff = finalFare - estimatedFare
  logTagInfo "Fare recalculation" $
    "Fare difference: "
      <> show (realToFrac @_ @Double fareDiff)
      <> ", Distance difference: "
      <> show distanceDiff
  putDiffMetric merchantId fareDiff distanceDiff
  return (recalcDistance, finalFare, Just fareParams)

isPickupDropOutsideOfThreshold :: (MonadThrow m, Log m, MonadTime m, MonadGuid m) => SRB.Booking -> DL.Location -> DRide.Ride -> LatLong -> DTConf.TransporterConfig -> m Bool
isPickupDropOutsideOfThreshold booking bookingToLocation ride tripEndPoint thresholdConfig = do
  let mbTripStartLoc = ride.tripStartPos
  -- for old trips with mbTripStartLoc = Nothing we always recalculate fare
  case mbTripStartLoc of
    Nothing -> pure True
    Just tripStartLoc -> do
      let pickupLocThreshold = metersToHighPrecMeters thresholdConfig.pickupLocThreshold
      let dropLocThreshold = metersToHighPrecMeters thresholdConfig.dropLocThreshold
      let pickupDifference = abs $ distanceBetweenInMeters (getCoordinates booking.fromLocation) tripStartLoc
      let dropDifference = abs $ distanceBetweenInMeters (getCoordinates bookingToLocation) tripEndPoint
      let pickupDropOutsideOfThreshold = (pickupDifference >= pickupLocThreshold) || (dropDifference >= dropLocThreshold)

      logTagInfo "Locations differences" $
        "Pickup difference: "
          <> show pickupDifference
          <> ", Drop difference: "
          <> show dropDifference
          <> ", Locations outside of thresholds: "
          <> show pickupDropOutsideOfThreshold
      pure pickupDropOutsideOfThreshold

getDistanceDiff :: (MonadThrow m, Log m, MonadTime m, MonadGuid m) => SRB.Booking -> Meters -> m HighPrecMeters
getDistanceDiff booking distance = do
  let rideDistanceDifference = distance - booking.estimatedDistance
  logTagInfo "RideDistance differences" $
    "Distance Difference: "
      <> show rideDistanceDifference
  pure $ metersToHighPrecMeters rideDistanceDifference

calculateFinalValuesForCorrectDistanceCalculations ::
  (MonadThrow m, Log m, MonadTime m, MonadGuid m) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> Maybe HighPrecMeters -> Bool -> DTConf.TransporterConfig -> m (Meters, Money, Maybe FareParameters)
calculateFinalValuesForCorrectDistanceCalculations handle booking ride mbMaxDistance pickupDropOutsideOfThreshold thresholdConfig = do
  distanceDiff <- getDistanceDiff booking (highPrecMetersToMeters ride.traveledDistance)
  let maxDistance = fromMaybe ride.traveledDistance mbMaxDistance + thresholdConfig.actualRideDistanceDiffThreshold
  if not pickupDropOutsideOfThreshold
    then
      if distanceDiff > thresholdConfig.actualRideDistanceDiffThreshold
        then recalculateFareForDistance handle booking ride (roundToIntegral $ min ride.traveledDistance maxDistance) thresholdConfig
        else recalculateFareForDistance handle booking ride booking.estimatedDistance thresholdConfig
    else
      if distanceDiff < 0
        then recalculateFareForDistance handle booking ride (roundToIntegral ride.traveledDistance) thresholdConfig
        else
          if distanceDiff < thresholdConfig.actualRideDistanceDiffThreshold
            then recalculateFareForDistance handle booking ride booking.estimatedDistance thresholdConfig
            else recalculateFareForDistance handle booking ride (roundToIntegral ride.traveledDistance) thresholdConfig

calculateFinalValuesForFailedDistanceCalculations ::
  (MonadThrow m, Log m, MonadTime m, MonadGuid m) => ServiceHandle m -> SRB.Booking -> DRide.Ride -> LatLong -> Bool -> DTConf.TransporterConfig -> m (Meters, Money, Maybe FareParameters)
calculateFinalValuesForFailedDistanceCalculations handle@ServiceHandle {..} booking ride tripEndPoint pickupDropOutsideOfThreshold thresholdConfig = do
  let tripStartPoint = case ride.tripStartPos of
        Nothing -> getCoordinates booking.fromLocation
        Just tripStartPos -> tripStartPos
  interpolatedPoints <- getInterpolatedPoints ride.driverId
  approxTraveledDistance <- getDistanceBetweenPoints tripStartPoint tripEndPoint interpolatedPoints
  logTagInfo "endRide" $ "approxTraveledDistance: " <> show approxTraveledDistance
  distanceDiff <- getDistanceDiff booking approxTraveledDistance

  if not pickupDropOutsideOfThreshold
    then recalculateFareForDistance handle booking ride booking.estimatedDistance thresholdConfig
    else
      if distanceDiff < 0
        then recalculateFareForDistance handle booking ride approxTraveledDistance thresholdConfig
        else
          if distanceDiff < thresholdConfig.actualRideDistanceDiffThreshold
            then do
              recalculateFareForDistance handle booking ride booking.estimatedDistance thresholdConfig
            else do
              if distanceDiff < thresholdConfig.upwardsRecomputeBuffer
                then recalculateFareForDistance handle booking ride approxTraveledDistance thresholdConfig
                else do
                  logTagInfo "Inaccurate Location Updates and Pickup/Drop Deviated." ("DistanceDiff: " <> show distanceDiff)
                  recalculateFareForDistance handle booking ride (booking.estimatedDistance + highPrecMetersToMeters thresholdConfig.upwardsRecomputeBuffer) thresholdConfig
