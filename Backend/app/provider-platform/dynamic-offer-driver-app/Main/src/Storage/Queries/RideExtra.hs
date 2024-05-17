{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RideExtra where

import Control.Monad.Extra hiding (fromMaybeM, whenJust)
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Ride as Common
import Data.Either
import qualified Data.HashMap.Strict as HashMap
import Data.Int
import Data.List (zip7)
import Data.Maybe
import Data.Time hiding (getCurrentTime)
import qualified Database.Beam as B
import Database.Beam.Backend (autoSqlValueSyntax)
import qualified Database.Beam.Backend as BeamBackend
import Database.Beam.Postgres
import Domain.Types.Booking as Booking
import Domain.Types.Booking as DBooking
import Domain.Types.Common as DTC
import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import Domain.Types.DriverInformation
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Merchant
import Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Person
import Domain.Types.Ride as DDR
import Domain.Types.Ride as DR
import Domain.Types.Ride as Ride
import qualified Domain.Types.Ride as DRide
import Domain.Types.RideDetails as RideDetails
import Domain.Types.RiderDetails as RiderDetails
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (all, elem, id, length, null, sum, traverse_, whenJust)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Maps.Types (LatLong (..), lat, lon)
import Kernel.Prelude hiding (foldl', map)
import Kernel.Storage.ClickhouseV2 as CH
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Common as SBC
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.Ride as BeamR
import qualified Storage.Beam.RideDetails as BeamRD
import qualified Storage.Beam.RiderDetails as BeamRDR
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.Clickhouse.Ride (getCompletedRidesByDriver, getEarningsByDriver, getEarningsByIds, getRidesByIdAndStatus)
import Storage.Clickhouse.RideDetails (findIdsByFleetOwner, findIdsByFleetOwnerAndVehicle)
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Storage.Queries.OrphanInstances.DriverInformation ()
import Storage.Queries.OrphanInstances.Ride ()
import Storage.Queries.RideDetails ()
import Storage.Queries.RiderDetails ()
import Tools.Error

data DatabaseWith2 table1 table2 f = DatabaseWith2
  { dwTable1 :: f (B.TableEntity table1),
    dwTable2 :: f (B.TableEntity table2)
  }
  deriving (Generic, B.Database be)

data DatabaseWith4 table1 table2 table3 table4 f = DatabaseWith4
  { dwTable1 :: f (B.TableEntity table1),
    dwTable2 :: f (B.TableEntity table2),
    dwTable3 :: f (B.TableEntity table3),
    dwTable4 :: f (B.TableEntity table4)
  }
  deriving (Generic, B.Database be)

createRide' :: KvDbFlow m r => Ride -> m ()
createRide' = createWithKV

create :: KvDbFlow m r => Ride -> m ()
create ride = do
  void $ whenNothingM_ (QL.findById ride.fromLocation.id) $ do QL.create ride.fromLocation
  whenJust ride.toLocation $ \toLocation -> whenNothingM_ (QL.findById toLocation.id) $ do QL.create toLocation
  createRide' ride

createRide :: KvDbFlow m r => Ride -> m ()
createRide ride = do
  fromLocationMap <- SLM.buildPickUpLocationMapping ride.fromLocation.id ride.id.getId DLM.RIDE ride.merchantId (Just ride.merchantOperatingCityId)
  QLM.create fromLocationMap
  whenJust ride.toLocation $ \toLocation -> do
    toLocationMaps <- SLM.buildDropLocationMapping toLocation.id ride.id.getId DLM.RIDE ride.merchantId (Just ride.merchantOperatingCityId)
    QLM.create toLocationMaps
  create ride

findById :: KvDbFlow m r => Id Ride -> m (Maybe Ride)
findById (Id rideId) = findOneWithKV [Se.Is BeamR.id $ Se.Eq rideId]

findAllRidesByDriverId ::
  KvDbFlow m r =>
  Id Person ->
  m [Ride]
findAllRidesByDriverId (Id driverId) = findAllWithKV [Se.Is BeamR.driverId $ Se.Eq driverId]

findCompletedRideByGHRId :: KvDbFlow m r => Id DDGR.DriverGoHomeRequest -> m (Maybe Ride)
findCompletedRideByGHRId (Id ghrId) = findAllWithOptionsKV [Se.And [Se.Is BeamR.driverGoHomeRequestId $ Se.Eq (Just ghrId), Se.Is BeamR.status $ Se.Eq DRide.COMPLETED]] (Se.Desc BeamR.createdAt) (Just 1) Nothing <&> listToMaybe

findActiveByRBId :: KvDbFlow m r => Id Booking -> m (Maybe Ride)
findActiveByRBId (Id rbId) = findOneWithKV [Se.And [Se.Is BeamR.bookingId $ Se.Eq rbId, Se.Is BeamR.status $ Se.Not $ Se.Eq Ride.CANCELLED]]

findAllRidesWithSeConditionsCreatedAtDesc :: KvDbFlow m r => [Se.Clause Postgres BeamR.RideT] -> m [Ride]
findAllRidesWithSeConditionsCreatedAtDesc conditions = findAllWithOptionsKV conditions (Se.Desc BeamR.createdAt) Nothing Nothing

findAllDriverInfromationFromRides :: KvDbFlow m r => [Ride] -> m [DriverInformation]
findAllDriverInfromationFromRides rides = findAllWithKV [Se.And [Se.Is BeamDI.driverId $ Se.In $ getId . DR.driverId <$> rides]]

findAllBookingsWithSeConditions :: KvDbFlow m r => [Se.Clause Postgres BeamB.BookingT] -> m [Booking]
findAllBookingsWithSeConditions = findAllWithKV

findAllBookingsWithSeConditionsCreatedAtDesc :: KvDbFlow m r => [Se.Clause Postgres BeamB.BookingT] -> m [Booking]
findAllBookingsWithSeConditionsCreatedAtDesc conditions = findAllWithOptionsKV conditions (Se.Desc BeamB.createdAt) Nothing Nothing

findAllRidesWithSeConditions :: KvDbFlow m r => [Se.Clause Postgres BeamR.RideT] -> m [Ride]
findAllRidesWithSeConditions = findAllWithKV

findAllRidesBookingsByRideId :: KvDbFlow m r => Id Merchant -> [Id Ride] -> m [(Ride, Booking)]
findAllRidesBookingsByRideId (Id merchantId) rideIds = do
  rides <- findAllRidesWithSeConditions [Se.Is BeamR.id $ Se.In $ getId <$> rideIds]
  let bookingSeCondition =
        [ Se.And
            [ Se.Is BeamB.id $ Se.In $ getId . DR.bookingId <$> rides,
              Se.Is BeamB.providerId $ Se.Eq merchantId
            ]
        ]
  bookings <- findAllBookingsWithSeConditions bookingSeCondition
  let rideBooking = foldl' (getRideWithBooking bookings) [] rides
  pure rideBooking
  where
    getRideWithBooking bookings acc ride' =
      let bookings' = filter (\x -> x.id == ride'.bookingId) bookings
       in acc <> ((ride',) <$> bookings')

findOneByBookingId :: KvDbFlow m r => Id Booking -> m (Maybe Ride)
findOneByBookingId (Id bookingId) = findAllWithOptionsKV [Se.Is BeamR.bookingId $ Se.Eq bookingId] (Se.Desc BeamR.createdAt) (Just 1) Nothing <&> listToMaybe

findAllByDriverId :: KvDbFlow m r => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe Ride.RideStatus -> Maybe Day -> m [(Ride, Booking)]
findAllByDriverId (Id driverId) mbLimit mbOffset mbOnlyActive mbRideStatus mbDay = do
  let limitVal = maybe 10 fromInteger mbLimit
      offsetVal = maybe 0 fromInteger mbOffset
      isOnlyActive = Just True == mbOnlyActive
  rides <-
    findAllWithOptionsKV
      [ Se.And
          ( [Se.Is BeamR.driverId $ Se.Eq driverId]
              <> if isOnlyActive
                then [Se.Is BeamR.status $ Se.Not $ Se.In [Ride.COMPLETED, Ride.CANCELLED]]
                else
                  []
                    <> ([Se.Is BeamR.status $ Se.Eq (fromJust mbRideStatus) | isJust mbRideStatus])
                    <> ([Se.And [Se.Is BeamR.updatedAt $ Se.GreaterThanOrEq (minDayTime (fromJust mbDay)), Se.Is BeamR.updatedAt $ Se.LessThanOrEq (maxDayTime (fromJust mbDay))] | isJust mbDay])
          )
      ]
      (Se.Desc BeamR.createdAt)
      (Just limitVal)
      (Just offsetVal)
  bookings <- findAllWithOptionsKV [Se.Is BeamB.id $ Se.In $ getId . DR.bookingId <$> rides] (Se.Desc BeamB.createdAt) Nothing Nothing

  let rideWithBooking = foldl' (getRideWithBooking bookings) [] rides
  pure $ take limitVal rideWithBooking
  where
    getRideWithBooking bookings acc ride =
      let bookings' = filter (\b -> b.id == ride.bookingId) bookings
       in acc <> ((ride,) <$> bookings')
    minDayTime date = UTCTime (addDays (-1) date) 66600
    maxDayTime date = UTCTime date 66600

findOneByDriverId :: KvDbFlow m r => Id Person -> m (Maybe Ride)
findOneByDriverId (Id personId) = findAllWithOptionsKV [Se.Is BeamR.driverId $ Se.Eq personId] (Se.Desc BeamR.createdAt) (Just 1) Nothing <&> listToMaybe

getInProgressByDriverId :: KvDbFlow m r => Id Person -> m (Maybe Ride)
getInProgressByDriverId (Id personId) = findOneWithKV [Se.And [Se.Is BeamR.driverId $ Se.Eq personId, Se.Is BeamR.status $ Se.Eq Ride.INPROGRESS]]

getInProgressOrNewRideIdAndStatusByDriverId :: KvDbFlow m r => Id Person -> m (Maybe (Id Ride, RideStatus))
getInProgressOrNewRideIdAndStatusByDriverId (Id driverId) = do
  ride' <- findOneWithKV [Se.And [Se.Is BeamR.driverId $ Se.Eq driverId, Se.Is BeamR.status $ Se.In [Ride.INPROGRESS, Ride.NEW]]]
  let rideData = (,) <$> (DR.id <$> ride') <*> (DR.status <$> ride')
  pure rideData

getActiveByDriverId :: KvDbFlow m r => Id Person -> m (Maybe Ride)
getActiveByDriverId (Id personId) =
  findOneWithKV [Se.And [Se.Is BeamR.driverId $ Se.Eq personId, Se.Is BeamR.status $ Se.In [Ride.INPROGRESS, Ride.NEW]]]

getActiveBookingAndRideByDriverId :: KvDbFlow m r => Id Person -> m [(Ride, Booking)]
getActiveBookingAndRideByDriverId (Id personId) = do
  maybeM
    (return [])
    ( \ride ->
        maybeM
          (return [])
          (\booking -> return [(ride, booking)])
          (QBooking.findById ride.bookingId)
    )
    (findOneWithKV [Se.And [Se.Is BeamR.driverId $ Se.Eq personId, Se.Is BeamR.status $ Se.In [Ride.INPROGRESS, Ride.NEW]]])

updateStatus :: KvDbFlow m r => Id Ride -> RideStatus -> m ()
updateStatus rideId status = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.status status,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateRideEndedBy :: KvDbFlow m r => Id Ride -> RideEndedBy -> m ()
updateRideEndedBy rideId rideEndedBy = do
  updateOneWithKV
    [ Se.Set BeamR.rideEndedBy $ Just rideEndedBy
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateUiDistanceCalculation :: KvDbFlow m r => Id Ride -> Maybe Int -> Maybe Int -> m ()
updateUiDistanceCalculation rideId dist1 dist2 = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.uiDistanceCalculationWithAccuracy dist1,
      Se.Set BeamR.uiDistanceCalculationWithoutAccuracy dist2,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateDriverDeviatedFromRoute :: KvDbFlow m r => Id Ride -> Bool -> m ()
updateDriverDeviatedFromRoute rideId deviation = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.driverDeviatedFromRoute $ Just deviation,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateDriverDeviatedToTollRoute :: KvDbFlow m r => Id Ride -> Bool -> m ()
updateDriverDeviatedToTollRoute rideId deviation = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.driverDeviatedToTollRoute $ Just deviation,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateStartTimeAndLoc :: KvDbFlow m r => Id Ride -> LatLong -> m ()
updateStartTimeAndLoc rideId point = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.tripStartTime $ Just now,
      Se.Set BeamR.tripStartLat $ Just point.lat,
      Se.Set BeamR.tripStartLon $ Just point.lon,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateEndRideOtp :: KvDbFlow m r => Id Ride -> Maybe Text -> m ()
updateEndRideOtp rideId endOtp = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.endOtp endOtp,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateStartOdometerReading :: KvDbFlow m r => Id Ride -> OdometerReading -> m ()
updateStartOdometerReading rideId odometer = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.startOdometerReadingValue $ Just odometer.value,
      Se.Set BeamR.startOdometerReadingFileId $ (getId <$> odometer.fileId),
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateEndOdometerReading :: KvDbFlow m r => Id Ride -> OdometerReading -> m ()
updateEndOdometerReading rideId odometer = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.endOdometerReadingValue $ Just odometer.value,
      Se.Set BeamR.endOdometerReadingFileId $ (getId <$> odometer.fileId),
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

updateStatusByIds :: KvDbFlow m r => [Id Ride] -> RideStatus -> m ()
updateStatusByIds rideIds status = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamR.status status,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.In $ getId <$> rideIds)]

updateDistance :: KvDbFlow m r => Id Person -> HighPrecMeters -> Int -> Int -> Maybe Int -> m ()
updateDistance driverId distance googleSnapCalls osrmSnapsCalls selfTunedCount = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamR.traveledDistance distance,
      Se.Set BeamR.numberOfSnapToRoadCalls (Just googleSnapCalls),
      Se.Set BeamR.numberOfOsrmSnapToRoadCalls (Just osrmSnapsCalls),
      Se.Set BeamR.numberOfSelfTuned selfTunedCount,
      Se.Set BeamR.updatedAt now
    ]
    [Se.And [Se.Is BeamR.driverId (Se.Eq $ getId driverId), Se.Is BeamR.status (Se.Eq Ride.INPROGRESS)]]

updateTollChargesAndNames :: KvDbFlow m r => Id Person -> HighPrecMoney -> [Text] -> m ()
updateTollChargesAndNames driverId tollCharges tollNames = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamR.tollCharges (Just tollCharges),
      Se.Set BeamR.tollNames (Just tollNames),
      Se.Set BeamR.updatedAt now
    ]
    [Se.And [Se.Is BeamR.driverId (Se.Eq $ getId driverId), Se.Is BeamR.status (Se.Eq Ride.INPROGRESS)]]

updateAll :: KvDbFlow m r => Id Ride -> Ride -> m ()
updateAll rideId ride = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamR.chargeableDistance ride.chargeableDistance,
      Se.Set BeamR.fare $ roundToIntegral <$> ride.fare,
      Se.Set BeamR.fareAmount $ ride.fare,
      Se.Set BeamR.tripEndTime ride.tripEndTime,
      Se.Set BeamR.tripEndLat (ride.tripEndPos <&> (.lat)),
      Se.Set BeamR.tripEndLon (ride.tripEndPos <&> (.lon)),
      Se.Set BeamR.fareParametersId (getId <$> ride.fareParametersId),
      Se.Set BeamR.distanceCalculationFailed ride.distanceCalculationFailed,
      Se.Set BeamR.pickupDropOutsideOfThreshold ride.pickupDropOutsideOfThreshold,
      Se.Set BeamR.endOdometerReadingValue (ride.endOdometerReading <&> (.value)),
      Se.Set BeamR.updatedAt now,
      Se.Set BeamR.rideEndedBy ride.rideEndedBy
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

getCountByStatus :: KvDbFlow m r => Id Merchant -> m [(RideStatus, Int)]
getCountByStatus merchantId = do
  -- Tricky query to be able to insert meaningful Point
  dbConf <- getMasterBeamConfig
  resp <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.aggregate_ (\(ride, _) -> (B.group_ (BeamR.status ride), B.as_ @Int B.countAll_)) $
          B.filter_' (\(_, BeamB.BookingT {..}) -> providerId B.==?. B.val_ (getId merchantId)) $
            do
              ride <- B.all_ (BeamCommon.ride BeamCommon.atlasDB)
              booking <- B.join_' (BeamCommon.booking BeamCommon.atlasDB) (\booking -> BeamB.id booking B.==?. BeamR.bookingId ride)
              pure (ride, booking)
  pure (EulerHS.Prelude.fromRight [] resp)

getRidesForDate :: KvDbFlow m r => Id Person -> Day -> Seconds -> m [Ride]
getRidesForDate driverId date diffTime = do
  let minDayTime = UTCTime (addDays (-1) date) (86400 - secondsToDiffTime (toInteger diffTime.getSeconds))
  let maxDayTime = UTCTime date (86400 - secondsToDiffTime (toInteger diffTime.getSeconds))
  findAllWithKV
    [ Se.And
        [ Se.Is BeamR.driverId $ Se.Eq $ getId driverId,
          Se.Is BeamR.tripEndTime $ Se.GreaterThanOrEq $ Just minDayTime,
          Se.Is BeamR.tripEndTime $ Se.LessThan $ Just maxDayTime,
          Se.Is BeamR.status $ Se.Eq Ride.COMPLETED
        ]
    ]

updateArrival :: KvDbFlow m r => Id Ride -> UTCTime -> m ()
updateArrival rideId now = do
  updateOneWithKV
    [ Se.Set BeamR.driverArrivalTime $ Just now,
      Se.Set BeamR.updatedAt now
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

data RideItem = RideItem
  { rideShortId :: ShortId Ride,
    rideCreatedAt :: UTCTime,
    rideDetails :: RideDetails,
    riderDetails :: RiderDetails,
    customerName :: Maybe Text,
    fareDiff :: Maybe Price,
    bookingStatus :: Common.BookingStatus,
    tripCategory :: DTC.TripCategory
  }

instance Num (Maybe HighPrecMoney) where
  (-) = liftA2 (-)
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = Just . fromInteger

instance Num (Maybe Money) where
  (-) = liftA2 (-)
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = Just . fromInteger

instance BeamBackend.BeamSqlBackend be => B.HasSqlEqualityCheck be Common.BookingStatus

instance BeamBackend.HasSqlValueSyntax be String => BeamBackend.HasSqlValueSyntax be Common.BookingStatus where
  sqlValueSyntax = autoSqlValueSyntax

updateSafetyAlertTriggered :: KvDbFlow m r => Id Ride -> m ()
updateSafetyAlertTriggered rideId = do
  updateOneWithKV
    [ Se.Set BeamR.safetyAlertTriggered True
    ]
    [Se.Is BeamR.id (Se.Eq $ getId rideId)]

roundToMidnightUTC :: UTCTime -> UTCTime
roundToMidnightUTC (UTCTime day _) = UTCTime day 0

roundToMidnightUTCToDate :: UTCTime -> UTCTime
roundToMidnightUTCToDate (UTCTime day _) = UTCTime (addDays 1 day) 0

findAllRideItems ::
  KvDbFlow m r =>
  Merchant ->
  DMOC.MerchantOperatingCity ->
  Int ->
  Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Ride) ->
  Maybe DbHash ->
  Maybe DbHash ->
  Maybe HighPrecMoney ->
  UTCTime ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  m [RideItem]
findAllRideItems merchant opCity limitVal offsetVal mbBookingStatus mbRideShortId mbCustomerPhoneDBHash mbDriverPhoneDBHash mbFareDiff now mbFrom mbTo = do
  dbConf <- getMasterBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ (fromIntegral limitVal) $
          B.offset_ (fromIntegral offsetVal) $
            B.filter_'
              ( \(booking, ride, rideDetails, riderDetails) ->
                  booking.providerId B.==?. B.val_ (getId merchant.id)
                    B.&&?. (booking.merchantOperatingCityId B.==?. B.val_ (Just $ getId opCity.id) B.||?. (B.sqlBool_ (B.isNothing_ booking.merchantOperatingCityId) B.&&?. B.sqlBool_ (B.val_ (merchant.city == opCity.city))))
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\rideShortId -> ride.shortId B.==?. B.val_ (getShortId rideShortId)) mbRideShortId
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\hash -> riderDetails.mobileNumberHash B.==?. B.val_ hash) mbCustomerPhoneDBHash
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\hash -> rideDetails.driverNumberHash B.==?. B.val_ (Just hash)) mbDriverPhoneDBHash
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\defaultFrom -> B.sqlBool_ $ ride.createdAt B.>=. B.val_ (roundToMidnightUTC defaultFrom)) mbFrom
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\defaultTo -> B.sqlBool_ $ ride.createdAt B.<=. B.val_ (roundToMidnightUTCToDate defaultTo)) mbTo
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\bookingStatus -> mkBookingStatusVal ride B.==?. B.val_ bookingStatus) mbBookingStatus
                    B.&&?. maybe
                      (B.sqlBool_ $ B.val_ True)
                      ( \fareDiff_ -> do
                          -- is it correct? fare - estimatedFare > fareDiff_ || fare - estimatedFare < fareDiff_
                          let oldCond = B.sqlBool_ $ (ride.fare - B.just_ (B.floor_ booking.estimatedFare)) B.>. B.val_ (Just $ roundToIntegral fareDiff_) B.||. (ride.fare - B.just_ (B.floor_ booking.estimatedFare)) B.<. B.val_ (Just $ roundToIntegral fareDiff_)
                          let newCond = B.sqlBool_ $ (ride.fareAmount - B.just_ booking.estimatedFare) B.>. B.val_ (Just fareDiff_) B.||. (ride.fareAmount - B.just_ booking.estimatedFare) B.<. B.val_ (Just fareDiff_)
                          B.bool_ newCond oldCond (B.isNothing_ ride.fareAmount)
                      )
                      mbFareDiff
              )
              do
                booking' <- B.all_ (BeamCommon.booking BeamCommon.atlasDB)
                ride' <- B.join_' (BeamCommon.ride BeamCommon.atlasDB) (\ride'' -> BeamR.bookingId ride'' B.==?. BeamB.id booking')
                rideDetails' <- B.join_' (BeamCommon.rideDetails BeamCommon.atlasDB) (\rideDetails'' -> ride'.id B.==?. BeamRD.id rideDetails'')
                riderDetails' <- B.join_' (BeamCommon.rDetails BeamCommon.atlasDB) (\riderDetails'' -> B.just_ (BeamRDR.id riderDetails'') B.==?. BeamB.riderId booking')
                pure (booking', ride', rideDetails', riderDetails')
  res' <- case res of
    Right x -> do
      let bookings = fst' <$> x
          rides = snd' <$> x
          rideDetails = thd' <$> x
          riderDetails = fth' <$> x
      b <- catMaybes <$> mapM fromTType' bookings
      r <- catMaybes <$> mapM fromTType' rides
      rd <- catMaybes <$> mapM fromTType' rideDetails
      rdr <- catMaybes <$> mapM fromTType' riderDetails
      -- TODO test
      let fareDiffs = zipWith (\ride booking -> mkPrice (Just ride.currency) <$> ride.fare - Just booking.estimatedFare) r b
      pure $ zip7 (DR.shortId <$> r) (DR.createdAt <$> r) rd rdr b fareDiffs (mkBookingStatus now <$> r)
    Left err -> do
      logError $ "FAILED_TO_FETCH_RIDE_LIST" <> show err
      pure []
  pure $ mkRideItem <$> res'
  where
    mkBookingStatusVal ride =
      B.ifThenElse_ (ride.status B.==. B.val_ Ride.COMPLETED) (B.val_ Common.COMPLETED) $
        B.ifThenElse_ (ride.status B.==. B.val_ Ride.NEW B.&&. B.not_ (ride.createdAt B.<=. B.val_ (addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now))) (B.val_ Common.UPCOMING) $
          B.ifThenElse_ (ride.status B.==. B.val_ Ride.NEW B.&&. (ride.createdAt B.<=. B.val_ (addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now))) (B.val_ Common.UPCOMING_6HRS) $
            B.ifThenElse_ (ride.status B.==. B.val_ Ride.INPROGRESS B.&&. B.not_ (ride.tripStartTime B.<=. B.val_ (Just $ addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now))) (B.val_ Common.ONGOING) $
              B.ifThenElse_ (ride.status B.==. B.val_ Ride.CANCELLED) (B.val_ Common.CANCELLED) (B.val_ Common.ONGOING_6HRS)
    fst' (x, _, _, _) = x
    snd' (_, y, _, _) = y
    thd' (_, _, z, _) = z
    fth' (_, _, _, r) = r
    mkBookingStatus now' ride
      | ride.status == Ride.COMPLETED = Common.COMPLETED
      | ride.status == Ride.NEW && (ride.createdAt) > addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now' = Common.UPCOMING
      | ride.status == Ride.NEW && ride.createdAt <= addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now' = Common.UPCOMING_6HRS
      | ride.status == Ride.INPROGRESS && ride.tripStartTime > Just (addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now') = Common.ONGOING
      | ride.status == Ride.CANCELLED = Common.CANCELLED
      | otherwise = Common.ONGOING_6HRS
    mkRideItem (rideShortId, rideCreatedAt, rideDetails, riderDetails, booking, fareDiff, bookingStatus) =
      RideItem {customerName = booking.riderName, tripCategory = booking.tripCategory, ..}

data StuckRideItem = StuckRideItem
  { rideId :: Id Ride,
    bookingId :: Id Booking,
    driverId :: Id Person,
    driverActive :: Bool
  }

findStuckRideItems :: KvDbFlow m r => Merchant -> DMOC.MerchantOperatingCity -> [Id Booking] -> UTCTime -> m [StuckRideItem]
findStuckRideItems merchant opCity bookingIds now = do
  let now6HrBefore = addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now
  bookings <-
    findAllWithKV
      [ Se.And
          [ Se.Is BeamB.providerId $ Se.Eq merchant.id.getId,
            Se.Is BeamB.merchantOperatingCityId (Se.Eq $ Just opCity.id.getId),
            Se.Is BeamB.id $ Se.In $ getId <$> bookingIds,
            Se.Is BeamB.tripCategory $ Se.In [Nothing, Just (DTC.OneWay DTC.OneWayOnDemandDynamicOffer), Just (DTC.OneWay DTC.OneWayOnDemandStaticOffer), Just (DTC.OneWay DTC.OneWayRideOtp)],
            Se.Or
              ( [Se.Is BeamB.merchantOperatingCityId $ Se.Eq (Just $ getId opCity.id)]
                  <> [Se.Is BeamB.merchantOperatingCityId $ Se.Eq Nothing | merchant.city == opCity.city]
              )
          ]
      ]
  rides <-
    findAllWithKV
      [ Se.And
          [ Se.Is BeamR.status $ Se.Eq Ride.NEW,
            Se.Is BeamR.createdAt $ Se.LessThanOrEq now6HrBefore,
            Se.Is BeamR.bookingId $ Se.In $ getId . DBooking.id <$> bookings
          ]
      ]
  driverInfos <- findAllDriverInfromationFromRides rides
  let rideBooking = foldl' (getRideWithBooking bookings) [] rides
  let rideBookingDriverInfo = foldl' (getRideWithBookingDriverInfo driverInfos) [] rideBooking
  pure $ mkStuckRideItem <$> rideBookingDriverInfo
  where
    getRideWithBooking bookings acc ride' =
      let bookings' = filter (\x -> x.id == ride'.bookingId) bookings
       in acc <> ((\x -> (ride', x.id)) <$> bookings')

    getRideWithBookingDriverInfo driverInfos acc (ride', booking') =
      let driverInfos' = filter (\x -> x.driverId == ride'.driverId) driverInfos
       in acc <> ((\x -> (ride'.id, booking', x.driverId, x.active)) <$> driverInfos')

    mkStuckRideItem (rideId, bookingId, driverId, driverActive) = StuckRideItem {..}

findLastRideAssigned :: KvDbFlow m r => Id Person -> m (Maybe Ride)
findLastRideAssigned (Id driverId) = findAllWithOptionsKV [Se.Is BeamR.driverId $ Se.Eq driverId] (Se.Desc BeamR.createdAt) (Just 1) Nothing <&> listToMaybe

findRideBookingsById :: KvDbFlow m r => Merchant -> DMOC.MerchantOperatingCity -> [Id Booking] -> m (HashMap.HashMap Text (Booking, Maybe DRide.Ride))
findRideBookingsById merchant moCity bookingIds = do
  bookings <- findBookingsById merchant moCity bookingIds
  rides <- findRidesByBookingId (bookings <&> (.id))
  let tuple = map (\booking -> (getId booking.id, (booking, Kernel.Prelude.find (\ride -> ride.bookingId == booking.id) rides))) bookings
  pure $ HashMap.fromList tuple

findBookingsById :: KvDbFlow m r => Merchant -> DMOC.MerchantOperatingCity -> [Id Booking] -> m [Booking]
findBookingsById merchant moCity bookingIds =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamB.id $ Se.In $ getId <$> bookingIds,
          Se.Or
            ( [Se.Is BeamB.merchantOperatingCityId $ Se.Eq $ Just (getId moCity.id)]
                <> [Se.Is BeamB.merchantOperatingCityId $ Se.Eq Nothing | moCity.city == merchant.city]
            )
        ]
    ]

findRidesByBookingId :: KvDbFlow m r => [Id Booking] -> m [DRide.Ride]
findRidesByBookingId bookingIds = findAllWithKV [Se.Is BeamR.bookingId $ Se.In $ getId <$> bookingIds]

findCancelledBookingId :: KvDbFlow m r => Id Person -> m [Id Booking]
findCancelledBookingId (Id driverId) = findAllWithKV [Se.And [Se.Is BeamR.driverId $ Se.Eq driverId, Se.Is BeamR.status $ Se.Eq Ride.CANCELLED]] <&> (Ride.bookingId <$>)

findRideByRideShortId :: KvDbFlow m r => ShortId Ride -> m (Maybe Ride)
findRideByRideShortId (ShortId shortId) = findOneWithKV [Se.Is BeamR.shortId $ Se.Eq shortId]

createMapping :: KvDbFlow m r => Text -> Text -> Maybe (Id Merchant) -> Maybe (Id MerchantOperatingCity) -> m (DLM.LocationMapping, Maybe DLM.LocationMapping)
createMapping bookingId rideId merchantId merchantOperatingCityId = do
  fromLocationMapping <- QLM.getLatestStartByEntityId bookingId >>= fromMaybeM (FromLocationMappingNotFound bookingId)
  fromLocationRideMapping <- SLM.buildPickUpLocationMapping fromLocationMapping.locationId rideId DLM.RIDE merchantId merchantOperatingCityId

  mbToLocationMapping <- QLM.getLatestEndByEntityId bookingId
  mbToLocationRideMapping <- (\toLocMap -> SLM.buildDropLocationMapping toLocMap.locationId rideId DLM.RIDE merchantId merchantOperatingCityId) `mapM` mbToLocationMapping

  QLM.create fromLocationRideMapping
  whenJust mbToLocationRideMapping QLM.create
  return (fromLocationRideMapping, mbToLocationRideMapping)

findTotalRidesInDay :: KvDbFlow m r => Id Person -> UTCTime -> m [DRide.Ride]
findTotalRidesInDay (Id driverId) time = do
  let todayStart = UTCTime (utctDay time) 0
  findAllWithKV
    [ Se.And
        [ Se.Is BeamR.status $ Se.Eq Ride.COMPLETED,
          Se.Is BeamR.createdAt $ Se.GreaterThanOrEq todayStart,
          Se.Is BeamR.driverId $ Se.Eq driverId
        ]
    ]

totalRidesByFleetOwner :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Maybe Text -> m Int
totalRidesByFleetOwner fleetOwnerId = do
  rideIds <- findIdsByFleetOwner fleetOwnerId
  pure $ length rideIds

totalEarningsByFleetOwner :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Maybe Text -> m Int
totalEarningsByFleetOwner fleetOwnerId = do
  rideIds <- findIdsByFleetOwner fleetOwnerId
  getEarningsByIds (cast <$> rideIds)

totalRidesCompletedInFleet :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Maybe Text -> m Int
totalRidesCompletedInFleet fleetOwnerId = do
  rideIds <- findIdsByFleetOwner fleetOwnerId
  getRidesByIdAndStatus (cast <$> rideIds) DRide.COMPLETED

totalRidesCancelledInFleet :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Maybe Text -> m Int
totalRidesCancelledInFleet fleetOwnerId = do
  rideIds <- findIdsByFleetOwner fleetOwnerId
  getRidesByIdAndStatus (cast <$> rideIds) DRide.CANCELLED

totalEarningsByFleetOwnerPerVehicle :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Maybe Text -> Text -> m Int
totalEarningsByFleetOwnerPerVehicle fleetOwnerId vehicleNumber = do
  rideIds <- findIdsByFleetOwnerAndVehicle fleetOwnerId vehicleNumber
  getEarningsByIds (cast <$> rideIds)

totalEarningsByFleetOwnerPerDriver :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Maybe Text -> Id Person -> m Int
totalEarningsByFleetOwnerPerDriver fleetOwnerId driverId = do
  rideIds <- findIdsByFleetOwner fleetOwnerId
  getEarningsByDriver (cast <$> rideIds) driverId

totalRidesByFleetOwnerPerVehicle :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Maybe Text -> Text -> m Int
totalRidesByFleetOwnerPerVehicle fleetOwnerId vehicleNumber = do
  rideIds <- findIdsByFleetOwnerAndVehicle fleetOwnerId vehicleNumber
  getRidesByIdAndStatus (cast <$> rideIds) DRide.COMPLETED

totalRidesByFleetOwnerPerDriver :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Maybe Text -> Id Person -> m Int
totalRidesByFleetOwnerPerDriver fleetOwnerId driverId = do
  rideIds <- findIdsByFleetOwner fleetOwnerId
  getCompletedRidesByDriver (cast <$> rideIds) driverId

totalRidesByFleetOwnerPerVehicleAndDriver :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Maybe Text -> Text -> Id Person -> m Int
totalRidesByFleetOwnerPerVehicleAndDriver fleetOwnerId vehicleNumber driverId = do
  rideIds <- findIdsByFleetOwnerAndVehicle fleetOwnerId vehicleNumber
  getCompletedRidesByDriver (cast <$> rideIds) driverId

totalEarningsByFleetOwnerPerVehicleAndDriver :: (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Maybe Text -> Text -> Id Person -> m Int
totalEarningsByFleetOwnerPerVehicleAndDriver fleetOwnerId vehicleNumber driverId = do
  rideIds <- findIdsByFleetOwnerAndVehicle fleetOwnerId vehicleNumber
  getEarningsByDriver (cast <$> rideIds) driverId
