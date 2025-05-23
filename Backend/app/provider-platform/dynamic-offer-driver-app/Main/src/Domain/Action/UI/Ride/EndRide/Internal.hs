{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride.EndRide.Internal
  ( endRideTransaction,
    putDiffMetric,
    getRouteAndDistanceBetweenPoints,
    safeMod,
    getCurrentDate,
    getRidesAndDistancefromZscore,
    getRouteInfoWithShortestDuration,
    mkDriverFeeCalcJobFlagKey,
    getDriverFeeCalcJobFlagKey,
    getPlan,
    pickWaypoints,
    getDriverFeeBillNumberKey,
    mkDriverFeeBillNumberKey,
    mkDriverFee,
    setDriverFeeBillNumberKey,
    getDriverFeeCalcJobCache,
    setDriverFeeCalcJobCache,
    getStartDateMonth,
    getEndDateMonth,
    makeDriverLeaderBoardKey,
    getMonth,
  )
where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import qualified Domain.Action.UI.Plan as Plan
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.CancellationCharges as DCC
import Domain.Types.DailyStats as DDS
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.DriverPlan
import qualified Domain.Types.FareParameters as DFare
import qualified Domain.Types.LeaderBoardConfigs as LConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Domain.Types.Plan
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RideRelatedNotificationConfig as DRN
import qualified Domain.Types.RiderDetails as RD
import Domain.Types.TransporterConfig
import qualified Domain.Types.Vehicle as DV
import EulerHS.Prelude hiding (elem, foldr, id, length, mapM_, null)
import GHC.Float (double2Int)
import GHC.Num.Integer (integerFromInt, integerToInt)
import Kernel.External.Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude hiding (forM_, whenJust)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common hiding (getCurrentTime)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import Lib.Scheduler.Environment (JobCreatorEnv)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import Lib.Scheduler.Types (SchedulerType)
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Allocator
import SharedLogic.DriverOnboarding
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import SharedLogic.Ride (multipleRouteKey, searchRequestKey, updateOnRideStatusWithAdvancedRideCheck)
import qualified SharedLogic.ScheduledNotifications as SN
import SharedLogic.TollsDetector
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.CachedQueries.Merchant.LeaderBoardConfig as QLeaderConfig
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.CachedQueries.Plan as CQP
import qualified Storage.CachedQueries.RideRelatedNotificationConfig as CRN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CancellationCharges as QCC
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import Storage.Queries.DriverPlan (findByDriverIdWithServiceName)
import qualified Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FareParameters as QFare
import Storage.Queries.Person as SQP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.Vehicle as QV
import Tools.Error
import Tools.Event
import qualified Tools.Maps as Maps
import qualified Tools.Metrics as Metrics
import Tools.Notifications
import qualified Tools.PaymentNudge as PaymentNudge
import Utils.Common.Cac.KeyNameConstants

endRideTransaction ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadFlow m,
    Esq.EsqDBReplicaFlow m r,
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters),
    HasField "maxShards" r Int,
    EventStreamFlow m r,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool)
  ) =>
  Id DP.Driver ->
  SRB.Booking ->
  Ride.Ride ->
  Maybe DFare.FareParameters ->
  Maybe (Id RD.RiderDetails) ->
  DFare.FareParameters ->
  TransporterConfig ->
  m ()
endRideTransaction driverId booking ride mbFareParams mbRiderDetailsId newFareParams thresholdConfig = do
  updateOnRideStatusWithAdvancedRideCheck ride.driverId (Just ride)
  QRB.updateStatus booking.id SRB.COMPLETED
  whenJust mbFareParams QFare.create
  QRide.updateAll ride.id ride
  driverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  QDriverStats.updateIdleTime driverId
  QDriverStats.incrementTotalRidesAndTotalDist (cast ride.driverId) (fromMaybe 0 ride.chargeableDistance)
  Hedis.del $ multipleRouteKey booking.transactionId
  Hedis.del $ searchRequestKey booking.transactionId
  clearCachedFarePolicyByEstOrQuoteId booking.quoteId
  clearTollStartGateBatchCache ride.driverId
  when (thresholdConfig.subscription) $ do
    maxShards <- asks (.maxShards)
    let serviceName = YATRI_SUBSCRIPTION
    createDriverFee booking.providerId booking.merchantOperatingCityId driverId ride.fare ride.currency newFareParams maxShards driverInfo booking serviceName

  triggerRideEndEvent RideEventData {ride = ride{status = Ride.COMPLETED}, personId = cast driverId, merchantId = booking.providerId}
  triggerBookingCompletedEvent BookingEventData {booking = booking{status = SRB.COMPLETED}, personId = cast driverId, merchantId = booking.providerId}

  mbRiderDetails <- join <$> QRD.findById `mapM` mbRiderDetailsId

  sendReferralFCM ride booking mbRiderDetails thresholdConfig
  updateLeaderboardZScore booking.providerId booking.merchantOperatingCityId ride
  DS.driverScoreEventHandler booking.merchantOperatingCityId DST.OnRideCompletion {merchantId = booking.providerId, driverId = cast driverId, ride = ride, fareParameter = Just newFareParams}
  let currency = booking.currency
  let customerCancellationDues = fromMaybe 0.0 newFareParams.customerCancellationDues
  when (thresholdConfig.canAddCancellationFee && customerCancellationDues > 0.0) $ do
    case mbRiderDetails of
      Just riderDetails -> do
        id <- generateGUID
        let cancellationCharges =
              DCC.CancellationCharges
                { driverId = cast driverId,
                  rideId = Just ride.id,
                  cancellationCharges = customerCancellationDues,
                  ..
                }
        calDisputeChances <-
          if thresholdConfig.cancellationFee == 0.0
            then do
              logWarning "Unable to calculate dispute chances used"
              return 0
            else do
              return $ round (customerCancellationDues / thresholdConfig.cancellationFee)
        QRD.updateDisputeChancesUsedAndCancellationDues (max 0 (riderDetails.disputeChancesUsed - calDisputeChances)) 0 (riderDetails.id) >> QCC.create cancellationCharges
      _ -> logWarning $ "Unable to update customer cancellation dues as RiderDetailsId is NULL with rideId " <> ride.id.getId
  now <- getCurrentTime
  rideRelatedNotificationConfigList <- CRN.findAllByMerchantOperatingCityIdAndTimeDiffEvent booking.merchantOperatingCityId DRN.END_TIME
  forM_ rideRelatedNotificationConfigList (SN.pushReminderUpdatesInScheduler booking ride now driverId)

sendReferralFCM ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters)
  ) =>
  Ride.Ride ->
  SRB.Booking ->
  Maybe RD.RiderDetails ->
  TransporterConfig ->
  m ()
sendReferralFCM ride booking mbRiderDetails transporterConfig = do
  minTripDistanceForReferralCfg <- asks (.minTripDistanceForReferralCfg)
  now <- getCurrentTime
  let shouldUpdateRideComplete =
        case minTripDistanceForReferralCfg of
          Just distance -> (metersToHighPrecMeters <$> ride.chargeableDistance) >= Just distance && maybe True (not . (.hasTakenValidRide)) mbRiderDetails
          Nothing -> True
  whenJust mbRiderDetails $ \riderDetails -> do
    fork "REFERRAL_ACTIVATED FCM to Driver" $ do
      when shouldUpdateRideComplete $
        QRD.updateHasTakenValidRide True (Just now) riderDetails.id
      case riderDetails.referredByDriver of
        Just referredDriverId -> do
          let referralMessage = "Congratulations!"
          let referralTitle = "Your referred customer has completed their first Namma Yatri ride"
          driver <- SQP.findById referredDriverId >>= fromMaybeM (PersonNotFound referredDriverId.getId)
          when shouldUpdateRideComplete $ do
            sendNotificationToDriver driver.merchantOperatingCityId FCM.SHOW Nothing FCM.REFERRAL_ACTIVATED referralTitle referralMessage driver driver.deviceToken
            logDebug "Driver Referral Coin Event"
            fork "DriverToCustomerReferralCoin Event : " $ DC.driverCoinsEvent driver.id driver.merchantId driver.merchantOperatingCityId (DCT.DriverToCustomerReferral ride.chargeableDistance)
          mbVehicle <- QV.findById referredDriverId
          let vehicleCategory = fromMaybe DV.AUTO_CATEGORY ((.category) =<< mbVehicle)
          payoutConfig <- CPC.findByPrimaryKey driver.merchantOperatingCityId vehicleCategory >>= fromMaybeM (InternalError "Payout config not present")
          when (isNothing riderDetails.firstRideId && payoutConfig.isPayoutEnabled) $ do
            let mobileNumberHash = (.hash) riderDetails.mobileNumber
            localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
            mbDailyStats <- QDailyStats.findByDriverIdAndDate referredDriverId (utctDay localTime)
            (isValidRideForPayout, mbFlagReason) <- fraudChecksForReferralPayout mobileNumberHash riderDetails mbDailyStats
            QRD.updateFirstRideIdAndFlagReason (Just ride.id.getId) mbFlagReason riderDetails.id
            when (isValidRideForPayout && isConsideredForPayout riderDetails) $ fork "Updating Payout Stats of Driver : " $ updateReferralStats referredDriverId mbDailyStats localTime driver driver.merchantOperatingCityId payoutConfig
        Nothing -> pure ()
  where
    isConsideredForPayout riderDetails = maybe False (\referredAt -> referredAt >= getDefaultTime) riderDetails.referredAt
    updateReferralStats referredDriverId mbDailyStats localTime driver merchantOpCityId payoutConfig = do
      driverInfo <- QDI.findById (cast referredDriverId) >>= fromMaybeM (PersonNotFound referredDriverId.getId)
      when (isNothing driverInfo.payoutVpa) do
        mbMerchantPN_ <- CPN.findMatchingMerchantPN merchantOpCityId "PAYOUT_VPA_ALERT" driver.language
        whenJust mbMerchantPN_ $ \merchantPN_ -> do
          let title = T.replace "{#rewardAmount#}" (show payoutConfig.referralRewardAmountPerRide) merchantPN_.title
              entityData = NotifReq {entityId = referredDriverId.getId, title = title, message = merchantPN_.body}
          notifyDriverOnEvents merchantOpCityId driver.id driver.deviceToken entityData merchantPN_.fcmNotificationType -- Sending PN to Add Vpa
      mbMerchantPN <- CPN.findMatchingMerchantPN merchantOpCityId "PAYOUT_REFERRAL_REWARD" driver.language
      whenJust mbMerchantPN $ \merchantPN -> do
        let title = T.replace "{#rewardAmount#}" (show payoutConfig.referralRewardAmountPerRide) merchantPN.title
            entityData = NotifReq {entityId = referredDriverId.getId, title = title, message = merchantPN.body}
        notifyDriverOnEvents merchantOpCityId driver.id driver.deviceToken entityData merchantPN.fcmNotificationType -- Sending PN for Reward
      let referralRewardAmount = payoutConfig.referralRewardAmountPerRide
      driverStats <- QDriverStats.findByPrimaryKey referredDriverId >>= fromMaybeM (PersonNotFound referredDriverId.getId)
      QDriverStats.updateTotalValidRidesAndPayoutEarnings (driverStats.totalValidActivatedRides + 1) (driverStats.totalPayoutEarnings + referralRewardAmount) referredDriverId
      case mbDailyStats of
        Just stats -> do
          Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey referredDriverId.getId) 3 3 $ do
            QDailyStats.updateReferralStatsByDriverId (stats.activatedValidRides + 1) (stats.referralEarnings + referralRewardAmount) DDS.Verifying referredDriverId (utctDay localTime)
        Nothing -> do
          id <- generateGUIDText
          now <- getCurrentTime
          let dailyStatsOfDriver' =
                DDS.DailyStats
                  { id = id,
                    driverId = referredDriverId,
                    totalEarnings = 0.0,
                    numRides = 0,
                    totalDistance = 0,
                    tollCharges = 0.0,
                    bonusEarnings = 0.0,
                    merchantLocalDate = utctDay localTime,
                    currency = ride.currency,
                    distanceUnit = ride.distanceUnit,
                    activatedValidRides = 1,
                    referralEarnings = referralRewardAmount,
                    referralCounts = 1,
                    payoutStatus = DDS.Verifying,
                    payoutOrderId = Nothing,
                    payoutOrderStatus = Nothing,
                    createdAt = now,
                    updatedAt = now
                  }
          QDailyStats.create dailyStatsOfDriver'

    fraudChecksForReferralPayout mobileNumberHash riderDetails mbDailyStats = do
      availablePersonWithNumber <- SQP.findAllMerchantIdByPhoneNo riderDetails.mobileCountryCode mobileNumberHash
      let isValidForMinPickupThreshold = maybe True (>= transporterConfig.minPickupDistanceThresholdForReferralPayout) booking.distanceToPickup
          isValidForMinRideDistance = ride.traveledDistance >= transporterConfig.minRideDistanceThresholdForReferralPayout
          isMaxReferralExceeded = maybe True ((<= transporterConfig.maxPayoutReferralForADay) . (.activatedValidRides)) mbDailyStats
          isMultipleDeviceIdExists = isJust riderDetails.payoutFlagReason
      let mbFlagReason =
            case (listToMaybe availablePersonWithNumber, isValidForMinRideDistance, isValidForMinPickupThreshold, isMaxReferralExceeded) of
              (Just _, _, _, _) -> Just RD.CustomerExistAsDriver
              (_, False, _, _) -> Just RD.MinRideDistanceInvalid
              (_, _, False, _) -> Just RD.MinPickupDistanceInvalid
              (_, _, _, False) -> Just RD.ExceededMaxReferral
              _ -> riderDetails.payoutFlagReason
      let isValid = null availablePersonWithNumber && isValidForMinPickupThreshold && isValidForMinRideDistance && not isMultipleDeviceIdExists
      return (isValid, mbFlagReason)

    payoutProcessingLockKey driverId = "Payout:Processing:DriverId" <> driverId

getDefaultTime :: UTCTime
getDefaultTime = defaultTime
  where
    day = fromGregorian 2024 7 26
    time = secondsToDiffTime 0
    defaultTime = UTCTime day time

updateLeaderboardZScore :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) => Id Merchant -> Id DMOC.MerchantOperatingCity -> Ride.Ride -> m ()
updateLeaderboardZScore merchantId merchantOpCityId ride = do
  fork "Updating ZScore for driver" . Hedis.withNonCriticalRedis $ mapM_ updateLeaderboardZScore' [LConfig.DAILY, LConfig.WEEKLY, LConfig.MONTHLY]
  where
    updateLeaderboardZScore' :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) => LConfig.LeaderBoardType -> m ()
    updateLeaderboardZScore' leaderBoardType = do
      currentTime <- getCurrentTime
      leaderBoardConfig <- QLeaderConfig.findLeaderBoardConfigbyType leaderBoardType merchantOpCityId >>= fromMaybeM (InternalError "Leaderboard configs not present")
      when leaderBoardConfig.isEnabled $ do
        let rideDate = getCurrentDate currentTime
            (fromDate, toDate) = calculateFromDateToDate leaderBoardType rideDate
            leaderBoardKey = makeDriverLeaderBoardKey leaderBoardType False merchantOpCityId fromDate toDate
        driverZscore <- Hedis.zScore leaderBoardKey $ ride.driverId.getId
        updateDriverZscore ride rideDate fromDate toDate driverZscore ride.chargeableDistance merchantId merchantOpCityId leaderBoardConfig

    calculateFromDateToDate :: LConfig.LeaderBoardType -> Day -> (Day, Day)
    calculateFromDateToDate leaderBoardType rideDate =
      case leaderBoardType of
        LConfig.DAILY -> (rideDate, rideDate)
        LConfig.WEEKLY ->
          let (_, currDayIndex) = sundayStartWeek rideDate
              weekStartDate = addDays (fromIntegral (- currDayIndex)) rideDate
              weekEndDate = addDays (fromIntegral (6 - currDayIndex)) rideDate
           in (weekStartDate, weekEndDate)
        LConfig.MONTHLY ->
          let monthStartDate = getStartDateMonth rideDate
              monthEndDate = getEndDateMonth rideDate 1
           in (monthStartDate, monthEndDate)

updateDriverZscore :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) => Ride.Ride -> Day -> Day -> Day -> Maybe Double -> Maybe Meters -> Id Merchant -> Id DMOC.MerchantOperatingCity -> LConfig.LeaderBoardConfigs -> m ()
updateDriverZscore ride rideDate fromDate toDate driverZscore rideChargeableDistance _ merchantOpCityId leaderBoardConfig = do
  (LocalTime _ localTime) <- utcToLocalTime timeZoneIST <$> getCurrentTime
  let leaderBoardExpiry = calculateLeaderBoardExpiry - secondsFromTimeOfDay localTime
      driverLeaderBoardKey = makeDriverLeaderBoardKey leaderBoardConfig.leaderBoardType False merchantOpCityId fromDate toDate
      cachedDriverLeaderBoardKey = makeDriverLeaderBoardKey leaderBoardConfig.leaderBoardType True merchantOpCityId fromDate toDate
  Hedis.zAddExp driverLeaderBoardKey ride.driverId.getId calculateCurrentZscore leaderBoardExpiry.getSeconds
  let limit = integerFromInt leaderBoardConfig.leaderBoardLengthLimit
  driversListWithScores' <- Hedis.zrevrangeWithscores driverLeaderBoardKey 0 (limit - 1)
  Hedis.setExp cachedDriverLeaderBoardKey driversListWithScores' calculateTotalExpiry.getSeconds
  where
    calculateLeaderBoardExpiry :: Seconds
    calculateLeaderBoardExpiry = do
      case leaderBoardConfig.leaderBoardType of
        LConfig.DAILY -> dailyExpiry
        LConfig.WEEKLY -> weeklyExpiry
        LConfig.MONTHLY -> monthlyExpiry
      where
        dailyExpiry = leaderBoardConfig.leaderBoardExpiry
        weeklyExpiry = do
          let (_, currDayIndex) = sundayStartWeek rideDate
          leaderBoardConfig.leaderBoardExpiry - Seconds ((currDayIndex + 1) * 86400) + Seconds 86400
        monthlyExpiry = Seconds $ integerToInt $ diffDays toDate rideDate * 86400 + 86400

    calculateCurrentZscore :: Integer
    calculateCurrentZscore =
      fromIntegral $ case driverZscore of
        Nothing -> leaderBoardConfig.zScoreBase + getMeters (fromMaybe 0 rideChargeableDistance)
        Just zscore -> do
          let (prevTotalRides, prevTotalDistance) = getRidesAndDistancefromZscore zscore leaderBoardConfig.zScoreBase
          let currTotalRides = prevTotalRides + 1
          let currTotalDist = prevTotalDistance + fromMaybe 0 rideChargeableDistance
          currTotalRides * leaderBoardConfig.zScoreBase + getMeters currTotalDist

    calculateTotalExpiry :: Seconds
    calculateTotalExpiry =
      case leaderBoardConfig.leaderBoardType of
        LConfig.MONTHLY -> Seconds $ integerToInt $ diffDays (getEndDateMonth rideDate leaderBoardConfig.numberOfSets) fromDate * 86400
        _ -> Seconds $ leaderBoardConfig.leaderBoardExpiry.getSeconds * leaderBoardConfig.numberOfSets

makeDriverLeaderBoardKey :: LConfig.LeaderBoardType -> Bool -> Id DMOC.MerchantOperatingCity -> Day -> Day -> Text
makeDriverLeaderBoardKey leaderBoardType isCached merchantOpCityId fromDate toDate =
  case leaderBoardType of
    LConfig.DAILY -> if isCached then makeCachedDailyDriverLeaderBoardKey else makeDailyDriverLeaderBoardKey
    LConfig.WEEKLY -> if isCached then makeCachedWeeklyDriverLeaderBoardKey else makeWeeklyDriverLeaderBoardKey
    LConfig.MONTHLY -> if isCached then makeCachedMonthlyDriverLeaderBoardKey else makeMonthlyDriverLeaderBoardKey
  where
    makeCachedDailyDriverLeaderBoardKey :: Text
    makeCachedDailyDriverLeaderBoardKey = "DDLBCK:" <> merchantOpCityId.getId <> ":" <> show fromDate

    makeDailyDriverLeaderBoardKey :: Text
    makeDailyDriverLeaderBoardKey = "DDLBK:" <> merchantOpCityId.getId <> ":" <> show fromDate

    makeCachedWeeklyDriverLeaderBoardKey :: Text
    makeCachedWeeklyDriverLeaderBoardKey = "DWLBCK:" <> merchantOpCityId.getId <> ":" <> show fromDate <> ":" <> show toDate

    makeWeeklyDriverLeaderBoardKey :: Text
    makeWeeklyDriverLeaderBoardKey = "DWLBK:" <> merchantOpCityId.getId <> ":" <> show fromDate <> ":" <> show toDate

    makeMonthlyDriverLeaderBoardKey :: Text
    makeMonthlyDriverLeaderBoardKey =
      let month = getMonth fromDate
       in "DMLBK:" <> merchantOpCityId.getId <> ":" <> show month

    makeCachedMonthlyDriverLeaderBoardKey :: Text
    makeCachedMonthlyDriverLeaderBoardKey =
      let month = getMonth fromDate
       in "DMLBCK:" <> merchantOpCityId.getId <> ":" <> show month

getRidesAndDistancefromZscore :: Double -> Int -> (Int, Meters)
getRidesAndDistancefromZscore dzscore dailyZscoreBase =
  let (totalRides, totalDistance) = quotRem (double2Int dzscore) dailyZscoreBase
   in (totalRides, Meters totalDistance)

getCurrentDate :: UTCTime -> Day
getCurrentDate time =
  let currentDate = localDay $ utcToLocalTime timeZoneIST time
   in currentDate

getStartDateMonth :: Day -> Day
getStartDateMonth day = fromGregorian y m 1
  where
    (y, m, _) = toGregorian day

getMonth :: Day -> Int
getMonth = (\(_, m, _) -> m) . toGregorian

getEndDateMonth :: Day -> Int -> Day
getEndDateMonth day addMonths = pred $ addGregorianMonthsClip (integerFromInt addMonths) $ getStartDateMonth day

putDiffMetric :: (Metrics.HasBPPMetrics m r, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> HighPrecMoney -> Meters -> m ()
putDiffMetric merchantId money mtrs = do
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  Metrics.putFareAndDistanceDeviations org.name (roundToIntegral money) mtrs

getRouteAndDistanceBetweenPoints ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id DMOC.MerchantOperatingCity ->
  LatLong ->
  LatLong ->
  [LatLong] ->
  Meters ->
  m ([LatLong], Meters)
getRouteAndDistanceBetweenPoints merchantId merchantOpCityId origin destination interpolatedPoints estimatedDistance = do
  -- somehow interpolated points pushed to redis in reversed order, so we need to reverse it back
  let pickedWaypoints = origin :| (pickWaypoints interpolatedPoints <> [destination])
  logTagInfo "endRide" $ "pickedWaypoints: " <> show pickedWaypoints
  routeResponse <-
    Maps.getRoutes merchantId merchantOpCityId $
      Maps.GetRoutesReq
        { waypoints = pickedWaypoints,
          mode = Just Maps.CAR,
          calcPoints = True
        }
  let mbShortestRoute = getRouteInfoWithShortestDuration routeResponse
      routePoints = maybe [] (.points) mbShortestRoute
      distance = maybe estimatedDistance (\route -> fromMaybe estimatedDistance route.distance) mbShortestRoute
  -- Next error is impossible, because we never receive empty list from directions api
  --mbShortestRouteDistance & fromMaybeM (InvalidRequest "Couldn't calculate route distance")
  return (routePoints, distance)

-- TODO reuse code from rider-app
getRouteInfoWithShortestDuration :: [Maps.RouteInfo] -> Maybe Maps.RouteInfo
getRouteInfoWithShortestDuration [] = Nothing
getRouteInfoWithShortestDuration (routeInfo : routeInfoArray) =
  if null routeInfoArray
    then Just routeInfo
    else do
      restRouteResult <- getRouteInfoWithShortestDuration routeInfoArray
      Just $ comparator routeInfo restRouteResult
  where
    comparator route1 route2 =
      if route1.duration < route2.duration
        then route1
        else route2

-- for distance api we can't pick more than 10 waypoints
pickWaypoints :: [a] -> [a]
pickWaypoints waypoints = do
  let step = length waypoints `div` 10
  take 7 $ foldr (\(n, waypoint) list -> if n `safeMod` step == 0 then waypoint : list else list) [] $ zip [1 ..] waypoints

safeMod :: Int -> Int -> Int
_ `safeMod` 0 = 0
a `safeMod` b = a `mod` b

createDriverFee ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadFlow m,
    JobCreatorEnv r,
    HasField "schedulerType" r SchedulerType
  ) =>
  Id Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Driver ->
  Maybe HighPrecMoney ->
  Currency ->
  DFare.FareParameters ->
  Int ->
  DI.DriverInformation ->
  SRB.Booking ->
  ServiceNames ->
  m ()
createDriverFee merchantId merchantOpCityId driverId rideFare currency newFareParams maxShards driverInfo booking serviceName = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  freeTrialDaysLeft <- getFreeTrialDaysLeft transporterConfig.freeTrialDays driverInfo
  let govtCharges = fromMaybe 0.0 newFareParams.govtCharges
  let (platformFee, cgst, sgst, isSpecialZoneCharge) = case newFareParams.fareParametersDetails of
        DFare.ProgressiveDetails _ -> (0, 0, 0, False)
        DFare.SlabDetails fpDetails -> (fromMaybe 0 fpDetails.platformFee, fromMaybe 0 fpDetails.cgst, fromMaybe 0 fpDetails.sgst, True)
        DFare.RentalDetails _ -> (0, 0, 0, False)
        DFare.InterCityDetails _ -> (0, 0, 0, False)
        DFare.AmbulanceDetails fpDetails -> (fromMaybe 0 fpDetails.platformFee, fromMaybe 0 fpDetails.cgst, fromMaybe 0 fpDetails.sgst, False)
  let totalDriverFee = govtCharges + platformFee + cgst + sgst
  mbDriverPlan <- getPlanAndPushToDefualtIfEligible transporterConfig freeTrialDaysLeft isSpecialZoneCharge
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  lastDriverFee <- QDF.findLatestFeeByDriverIdAndServiceName driverId serviceName
  driverFee <- mkDriverFee serviceName now Nothing Nothing merchantId driverId rideFare govtCharges platformFee cgst sgst currency transporterConfig (Just booking) isSpecialZoneCharge
  vehicle <- QV.findById driverId
  let isEnableForVariant = maybe True (`elem` transporterConfig.variantsToEnableForSubscription) (vehicle <&> (.variant))
  let toUpdateOrCreateDriverfee = (totalDriverFee > 0 || (totalDriverFee <= 0 && transporterConfig.isPlanMandatory && isJust mbDriverPlan)) && isEnableForVariant
  when (toUpdateOrCreateDriverfee && isEligibleForCharge transporterConfig freeTrialDaysLeft isSpecialZoneCharge) $ do
    numRides <- case lastDriverFee of
      Just ldFee ->
        if now >= ldFee.startTime && now < ldFee.endTime
          then do
            QDF.updateFee ldFee.id rideFare govtCharges platformFee cgst sgst now True booking isSpecialZoneCharge
            return (ldFee.numRides + 1)
          else do
            QDF.create driverFee
            return 1
      Nothing -> do
        QDF.create driverFee
        return 1
    plan <- getPlan mbDriverPlan serviceName merchantOpCityId
    fork "Sending switch plan nudge" $ PaymentNudge.sendSwitchPlanNudge transporterConfig driverInfo plan mbDriverPlan numRides serviceName
    scheduleJobs transporterConfig driverFee merchantId merchantOpCityId maxShards now
  where
    isEligibleForCharge transporterConfig freeTrialDaysLeft isSpecialZoneCharge =
      if isSpecialZoneCharge
        then transporterConfig.considerSpecialZoneRideChargesInFreeTrial || freeTrialDaysLeft <= 0
        else freeTrialDaysLeft <= 0

    getPlanAndPushToDefualtIfEligible :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => TransporterConfig -> Int -> Bool -> m (Maybe DriverPlan)
    getPlanAndPushToDefualtIfEligible transporterConfig freeTrialDaysLeft' isSpecialZoneCharge = do
      mbDriverPlan' <- findByDriverIdWithServiceName (cast driverId) serviceName
      let planMandatory = transporterConfig.isPlanMandatory
          chargeSPZRides = transporterConfig.considerSpecialZoneRideChargesInFreeTrial
          isEligibleForDefaultPlanAfterFreeTrial = freeTrialDaysLeft' <= 0 && planMandatory && transporterConfig.allowDefaultPlanAllocation
          isEligibleForDefaultPlanBeforeFreeTrial = freeTrialDaysLeft' > 0 && chargeSPZRides && planMandatory
      if isNothing mbDriverPlan'
        then do
          case (isSpecialZoneCharge, isEligibleForDefaultPlanBeforeFreeTrial, isEligibleForDefaultPlanAfterFreeTrial) of
            (True, True, _) -> assignDefaultPlan
            (_, _, True) -> assignDefaultPlan
            _ -> return mbDriverPlan'
        else return mbDriverPlan'
    assignDefaultPlan :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m (Maybe DriverPlan)
    assignDefaultPlan = do
      plans <- CQP.findByMerchantOpCityIdAndTypeWithServiceName merchantOpCityId DEFAULT serviceName
      case plans of
        (plan' : _) -> do
          newDriverPlan <- Plan.mkDriverPlan plan' (driverId, merchantId, merchantOpCityId)
          QDPlan.create newDriverPlan
          Plan.updateSubscriptionStatus serviceName (driverId, merchantId, merchantOpCityId) (Just DI.PENDING) Nothing
          QDI.updatPayerVpa Nothing (cast driverId)
          return $ Just newDriverPlan
        _ -> return Nothing

scheduleJobs :: (CacheFlow m r, EsqDBFlow m r, JobCreatorEnv r, HasField "schedulerType" r SchedulerType) => TransporterConfig -> DF.DriverFee -> Id Merchant -> Id MerchantOperatingCity -> Int -> UTCTime -> m ()
scheduleJobs transporterConfig driverFee merchantId merchantOpCityId maxShards now = do
  void $
    case transporterConfig.driverFeeCalculationTime of
      Nothing -> pure ()
      Just dfCalcTime -> do
        whenWithLockRedis (mkLockKeyForDriverFeeCalculation driverFee.startTime driverFee.endTime merchantOpCityId) 60 $ do
          isDfCaclculationJobScheduled <- getDriverFeeCalcJobCache driverFee.startTime driverFee.endTime merchantOpCityId driverFee.serviceName
          let dfCalculationJobTs = diffUTCTime (addUTCTime dfCalcTime driverFee.endTime) now
          case isDfCaclculationJobScheduled of
            ----- marker ---
            Nothing -> do
              createJobIn @_ @'CalculateDriverFees dfCalculationJobTs maxShards $
                CalculateDriverFeesJobData
                  { merchantId = merchantId,
                    merchantOperatingCityId = Just merchantOpCityId,
                    startTime = driverFee.startTime,
                    serviceName = Just (driverFee.serviceName),
                    scheduleNotification = Just True,
                    scheduleOverlay = Just True,
                    scheduleManualPaymentLink = Just True,
                    scheduleDriverFeeCalc = Just True,
                    createChildJobs = Just True,
                    endTime = driverFee.endTime
                  }
              setDriverFeeCalcJobCache driverFee.startTime driverFee.endTime merchantOpCityId driverFee.serviceName dfCalculationJobTs
              setDriverFeeBillNumberKey merchantOpCityId 1 36000 (driverFee.serviceName)
            _ -> pure ()

mkDriverFee ::
  ( MonadFlow m,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  ServiceNames ->
  UTCTime ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Id Merchant ->
  Id DP.Driver ->
  Maybe HighPrecMoney ->
  HighPrecMoney ->
  HighPrecMoney ->
  HighPrecMoney ->
  HighPrecMoney ->
  Currency ->
  TransporterConfig ->
  Maybe SRB.Booking ->
  Bool ->
  m DF.DriverFee
mkDriverFee serviceName now startTime' endTime' merchantId driverId rideFare govtCharges platformFee cgst sgst currency transporterConfig _mbBooking isSpecialZoneCharge = do
  id <- generateGUID
  let potentialStart = addUTCTime transporterConfig.driverPaymentCycleStartTime (UTCTime (utctDay now) (secondsToDiffTime 0))
      startTime = if now >= potentialStart then potentialStart else addUTCTime (-1 * transporterConfig.driverPaymentCycleDuration) potentialStart
      endTime = addUTCTime transporterConfig.driverPaymentCycleDuration startTime
      payBy = if isNothing transporterConfig.driverFeeCalculationTime then addUTCTime transporterConfig.driverPaymentCycleBuffer endTime else addUTCTime (transporterConfig.driverAutoPayNotificationTime + transporterConfig.driverAutoPayExecutionTime) endTime
      platformFee_ = if isNothing transporterConfig.driverFeeCalculationTime then DF.PlatformFee {fee = platformFee, cgst, sgst, currency} else DF.PlatformFee {fee = 0, cgst = 0, sgst = 0, currency}
      govtCharges_ = if isNothing transporterConfig.driverFeeCalculationTime then govtCharges else 0
      isPlanMandatory = transporterConfig.isPlanMandatory
      totalFee = platformFee + cgst + sgst
      (specialZoneRideCount, specialZoneAmount) = specialZoneMetricsIntialization totalFee
      numRides = if serviceName == YATRI_SUBSCRIPTION then 1 else 0
  mbDriverPlan <- findByDriverIdWithServiceName (cast driverId) serviceName -- what if its changed? needed inside lock?
  plan <- getPlan mbDriverPlan serviceName transporterConfig.merchantOperatingCityId
  return $
    DF.DriverFee
      { status = DF.ONGOING,
        collectedBy = Nothing,
        collectedAt = Nothing,
        createdAt = now,
        updatedAt = now,
        platformFee = platformFee_,
        totalEarnings = fromMaybe 0 rideFare,
        feeType = case (plan, isPlanMandatory) of
          (Nothing, _) -> DF.RECURRING_INVOICE
          (Just plan_, True) -> if plan_.paymentMode == MANUAL then DF.RECURRING_INVOICE else DF.RECURRING_EXECUTION_INVOICE
          (Just _, False) -> DF.RECURRING_INVOICE,
        govtCharges = govtCharges_,
        offerId = Nothing,
        planOfferTitle = Nothing,
        autopayPaymentStage = Nothing,
        stageUpdatedAt = Nothing,
        billNumber = Nothing,
        schedulerTryCount = 0,
        feeWithoutDiscount = Nothing, -- Only for NY rn
        overlaySent = False,
        amountPaidByCoin = Nothing,
        planId = Nothing,
        planMode = Nothing,
        notificationRetryCount = 0,
        badDebtDeclarationDate = Nothing,
        badDebtRecoveryDate = Nothing,
        vehicleNumber = case mbDriverPlan <&> (.subscriptionServiceRelatedData) of
          Just (RentedVehicleNumber t) -> Just t
          _ -> Nothing,
        merchantOperatingCityId = transporterConfig.merchantOperatingCityId,
        startTime = fromMaybe startTime startTime',
        endTime = fromMaybe endTime endTime',
        ..
      }
  where
    specialZoneMetricsIntialization totalFee' = do
      if isSpecialZoneCharge then (1, totalFee') else (0, 0)

getPlan ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Maybe DriverPlan ->
  ServiceNames ->
  Id DMOC.MerchantOperatingCity ->
  m (Maybe Plan)
getPlan mbDriverPlan serviceName merchantOpCityId = do
  case mbDriverPlan of
    Just dp -> CQP.findByIdAndPaymentModeWithServiceName dp.planId dp.planType serviceName
    Nothing -> do
      plans <- CQP.findByMerchantOpCityIdAndTypeWithServiceName merchantOpCityId DEFAULT serviceName
      case plans of
        [] -> pure Nothing
        [pl] -> pure (Just pl)
        _ -> throwError $ InternalError "Multiple default plans found"

getDriverFeeCalcJobCache :: CacheFlow m r => UTCTime -> UTCTime -> Id MerchantOperatingCity -> ServiceNames -> m (Maybe Bool)
getDriverFeeCalcJobCache startTime endTime merchantOpCityId serviceName = Hedis.get (mkDriverFeeCalcJobCacheKey startTime endTime merchantOpCityId serviceName)

mkDriverFeeCalcJobCacheKey :: UTCTime -> UTCTime -> Id MerchantOperatingCity -> ServiceNames -> Text
mkDriverFeeCalcJobCacheKey startTime endTime merchantOpCityId serviceName = "DriverFeeCalculation:MerchantOpCityId:" <> merchantOpCityId.getId <> ":StartTime:" <> show startTime <> ":EndTime:" <> show endTime <> ":ServiceName:" <> show serviceName

mkDriverFeeCalcJobFlagKey :: UTCTime -> UTCTime -> Id MerchantOperatingCity -> ServiceNames -> Text
mkDriverFeeCalcJobFlagKey startTime endTime merchantOpCityId serviceName = "DriverFeeCalculationFlag:MerchantOpCityId:" <> merchantOpCityId.getId <> ":StartTime:" <> show startTime <> ":EndTime:" <> show endTime <> ":ServiceName:" <> show serviceName

getDriverFeeCalcJobFlagKey :: CacheFlow m r => UTCTime -> UTCTime -> Id MerchantOperatingCity -> ServiceNames -> m (Maybe Bool)
getDriverFeeCalcJobFlagKey startTime endTime merchantOpCityId serviceName = Hedis.get (mkDriverFeeCalcJobFlagKey startTime endTime merchantOpCityId serviceName)

setDriverFeeCalcJobCache :: CacheFlow m r => UTCTime -> UTCTime -> Id MerchantOperatingCity -> ServiceNames -> NominalDiffTime -> m ()
setDriverFeeCalcJobCache startTime endTime merchantOpCityId serviceName expTime = do
  Hedis.setExp (mkDriverFeeCalcJobFlagKey startTime endTime merchantOpCityId serviceName) True (round $ expTime + 86399)
  Hedis.setExp (mkDriverFeeCalcJobCacheKey startTime endTime merchantOpCityId serviceName) False (round $ expTime + 86399)

mkDriverFeeBillNumberKey :: Id MerchantOperatingCity -> ServiceNames -> Text
mkDriverFeeBillNumberKey merchantOpCityId service = "DriverFeeCalulation:BillNumber:Counter" <> merchantOpCityId.getId <> ":service:" <> show service

getDriverFeeBillNumberKey :: CacheFlow m r => Id MerchantOperatingCity -> ServiceNames -> m (Maybe Int)
getDriverFeeBillNumberKey merchantOpCityId serviceName = Hedis.get (mkDriverFeeBillNumberKey merchantOpCityId serviceName)

setDriverFeeBillNumberKey :: CacheFlow m r => Id MerchantOperatingCity -> Int -> NominalDiffTime -> ServiceNames -> m ()
setDriverFeeBillNumberKey merchantOpCityId count expTime serviceName = Hedis.setExp (mkDriverFeeBillNumberKey merchantOpCityId serviceName) count (round expTime)

mkLockKeyForDriverFeeCalculation :: UTCTime -> UTCTime -> Id MerchantOperatingCity -> Text
mkLockKeyForDriverFeeCalculation startTime endTime merchantOpCityId = "DriverFeeCalculation:Lock:MerchantId:" <> merchantOpCityId.getId <> ":StartTime:" <> show startTime <> ":EndTime:" <> show endTime
