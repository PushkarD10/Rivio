module Storage.Queries.Person.GetNearestDriversCurrentlyOnRide where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as DL
import qualified Data.Text as T
import Domain.Types.Common
import Domain.Types.DriverInformation as DriverInfo
import Domain.Types.Merchant
import Domain.Types.Person as Person
import Domain.Types.ServiceTierType as DVST
import Domain.Types.Vehicle as DV
import Domain.Types.VehicleServiceTier as DVST
import Kernel.External.Maps as Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common hiding (Value)
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.VehicleServiceTier
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverInformation.Internal as Int
import qualified Storage.Queries.DriverLocation.Internal as Int
-- import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person.Internal as Int
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Vehicle.Internal as Int

data NearestDriversResultCurrentlyOnRide = NearestDriversResultCurrentlyOnRide
  { driverId :: Id Driver,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    language :: Maybe Maps.Language,
    onRide :: Bool,
    lat :: Double,
    lon :: Double,
    variant :: DV.Variant,
    serviceTier :: DVST.ServiceTierType,
    serviceTierDowngradeLevel :: Int,
    isAirConditioned :: Maybe Bool,
    previousRideDropLat :: Double,
    previousRideDropLon :: Double,
    distanceToDriver :: Meters,
    distanceFromDriverToDestination :: Meters,
    mode :: Maybe DriverInfo.DriverMode,
    clientSdkVersion :: Maybe Version,
    clientBundleVersion :: Maybe Version,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device,
    vehicleAge :: Maybe Months,
    backendConfigVersion :: Maybe Version,
    backendAppVersion :: Maybe Text,
    latestScheduledBooking :: Maybe UTCTime,
    latestScheduledPickup :: Maybe LatLong
  }
  deriving (Generic, Show, HasCoordinates)

data NearestDriversOnRideReq = NearestDriversOnRideReq
  { cityServiceTiers :: [DVST.VehicleServiceTier],
    serviceTiers :: [ServiceTierType],
    fromLocLatLong :: LatLong,
    nearestRadius :: Meters,
    merchantId :: Id Merchant,
    driverPositionInfoExpiry :: Maybe Seconds,
    currentRideTripCategoryValidForForwardBatching :: [Text],
    isRental :: Bool,
    isInterCity :: Bool,
    isValueAddNP :: Bool,
    onlinePayment :: Bool,
    now :: UTCTime
  }

getNearestDriversCurrentlyOnRide ::
  (MonadFlow m, MonadTime m, MonadReader r m, LT.HasLocationService m r, CoreMetrics m, CacheFlow m r, EsqDBFlow m r, ServiceFlow m r) =>
  NearestDriversOnRideReq ->
  m [NearestDriversResultCurrentlyOnRide]
getNearestDriversCurrentlyOnRide NearestDriversOnRideReq {..} = do
  let onRideRadius = nearestRadius
  logDebug $ "On Ride radius " <> show onRideRadius
  logDebug $ "lat long" <> show fromLocLatLong
  driverLocs <- Int.getDriverLocsWithCond merchantId driverPositionInfoExpiry fromLocLatLong onRideRadius
  logDebug $ "GetNearestDriversCurrentlyOnRide - DLoc:- " <> show driverLocs
  driverInfos <- Int.getDriverInfosWithCond (driverLocs <&> (.driverId)) False True isRental isInterCity isValueAddNP
  logDebug $ "GetNearestDriversCurrentlyOnRide - DInfo:- " <> show driverInfos
  vehicles <- Int.getVehicles driverInfos
  drivers <- Int.getDrivers vehicles
  driverBankAccounts <-
    if onlinePayment
      then QDBA.getDrivers (driverLocs <&> (.driverId))
      else return []
  -- driverStats <- QDriverStats.findAllByDriverIds drivers
  rideCurrentlyInProgress <- filter (\ride -> show ride.tripCategory `elem` currentRideTripCategoryValidForForwardBatching) <$> QRide.getInProgressByDriverIds (map (.id) drivers)
  let rideEndLocations = mapMaybe (.toLocation) rideCurrentlyInProgress
  logDebug $ "GetNearestDriversCurrentlyOnRide - DLoc:- " <> show (length driverLocs) <> " DInfo:- " <> show (length driverInfos) <> " Vehicle:- " <> show (length vehicles) <> " Drivers:- " <> show (length drivers) <> "Rides" <> show (length rideCurrentlyInProgress) <> "RideLocs:- " <> show (length rideEndLocations)
  let res = linkArrayListForOnRide rideCurrentlyInProgress rideEndLocations driverLocs driverInfos vehicles drivers driverBankAccounts (fromIntegral onRideRadius :: Double)
  logDebug $ "GetNearestDriversCurrentlyOnRide Result:- " <> show (length res)
  return res
  where
    linkArrayListForOnRide rideCurrentlyInProgress rideEndLocations driverLocations driverInformations vehicles persons driverBankAccounts onRideRadius =
      let locationHashMap = HashMap.fromList $ (\loc -> (loc.driverId, loc)) <$> driverLocations
          personHashMap = HashMap.fromList $ (\p -> (p.id, p)) <$> persons
          -- driverStatsHashMap = HashMap.fromList $ (\stats -> (stats.driverId, stats)) <$> driverStats
          rideByDriverIdHashMap = HashMap.fromList $ (\ride -> (ride.driverId, ride)) <$> rideCurrentlyInProgress
          rideToLocsHashMap = HashMap.fromList $ (\loc -> (loc.id, loc)) <$> rideEndLocations
          driverInfoHashMap = HashMap.fromList $ (\info -> (info.driverId, info)) <$> driverInformations
          driverBankAccountHashMap = HashMap.fromList $ catMaybes $ (\dba -> if dba.chargesEnabled then Just (dba.driverId, dba.accountId) else Nothing) <$> driverBankAccounts
       in concat $ mapMaybe (buildFullDriverListOnRide rideByDriverIdHashMap rideToLocsHashMap locationHashMap driverInfoHashMap personHashMap driverBankAccountHashMap onRideRadius) vehicles

    buildFullDriverListOnRide rideByDriverIdHashMap rideToLocsHashMap locationHashMap driverInfoHashMap personHashMap driverBankAccountHashMap onRideRadius vehicle = do
      let driverId' = vehicle.driverId
      location <- HashMap.lookup driverId' locationHashMap
      ride <- HashMap.lookup driverId' rideByDriverIdHashMap
      rideToLoc <- ride.toLocation
      rideToLocation <- HashMap.lookup rideToLoc.id rideToLocsHashMap
      info <- HashMap.lookup driverId' driverInfoHashMap
      person <- HashMap.lookup driverId' personHashMap
      _ <- if onlinePayment then HashMap.lookup driverId' driverBankAccountHashMap else Just T.empty -- is there any better way to do this?
      -- driverStats <- HashMap.lookup driverId' driverStatsHashMap
      let driverLocationPoint = LatLong {lat = location.lat, lon = location.lon}
          destinationPoint = LatLong {lat = rideToLocation.lat, lon = rideToLocation.lon}
          distanceFromDriverToDestination = realToFrac $ distanceBetweenInMeters driverLocationPoint destinationPoint
          distanceFromDestinationToPickup = realToFrac $ distanceBetweenInMeters fromLocLatLong destinationPoint
          onRideRadiusValidity = (distanceFromDriverToDestination + distanceFromDestinationToPickup) < onRideRadius
      -- ideally should be there inside the vehicle.selectedServiceTiers but still to make sure we have a default service tier for the driver
      let cityServiceTiersHashMap = HashMap.fromList $ (\vst -> (vst.serviceTierType, vst)) <$> cityServiceTiers
      let mbDefaultServiceTierForDriver = find (\vst -> vehicle.variant `elem` vst.defaultForVehicleVariant) cityServiceTiers
      let availableTiersWithUsageRestriction = selectVehicleTierForDriverWithUsageRestriction False info vehicle cityServiceTiers
      let ifUsageRestricted = any (\(_, usageRestricted) -> usageRestricted) availableTiersWithUsageRestriction
      let selectedDriverServiceTiers =
            if ifUsageRestricted
              then do
                (.serviceTierType) <$> (map fst $ filter (not . snd) availableTiersWithUsageRestriction) -- no need to check for user selection
              else do
                DL.intersect vehicle.selectedServiceTiers ((.serviceTierType) <$> (map fst $ filter (not . snd) availableTiersWithUsageRestriction))
      if onRideRadiusValidity
        then do
          if null serviceTiers
            then Just $ mapMaybe (mkDriverResult mbDefaultServiceTierForDriver person info location rideToLocation distanceFromDriverToDestination distanceFromDestinationToPickup cityServiceTiersHashMap) selectedDriverServiceTiers
            else do
              Just $
                mapMaybe
                  ( \serviceTier -> do
                      if serviceTier `elem` selectedDriverServiceTiers
                        then mkDriverResult mbDefaultServiceTierForDriver person info location rideToLocation distanceFromDriverToDestination distanceFromDestinationToPickup cityServiceTiersHashMap serviceTier
                        else Nothing
                  )
                  serviceTiers
        else Nothing
      where
        mkDriverResult mbDefaultServiceTierForDriver person info location rideToLocation distanceFromDriverToDestination distanceFromDestinationToPickup cityServiceTiersHashMap serviceTier = do
          serviceTierInfo <- HashMap.lookup serviceTier cityServiceTiersHashMap
          Just $
            NearestDriversResultCurrentlyOnRide
              { driverId = cast person.id,
                driverDeviceToken = person.deviceToken,
                language = person.language,
                onRide = info.onRide,
                lat = location.lat,
                lon = location.lon,
                variant = vehicle.variant,
                serviceTier = serviceTier,
                serviceTierDowngradeLevel = maybe 0 (\d -> d.priority - serviceTierInfo.priority) mbDefaultServiceTierForDriver,
                isAirConditioned = serviceTierInfo.isAirConditioned,
                previousRideDropLat = rideToLocation.lat,
                previousRideDropLon = rideToLocation.lon,
                distanceToDriver = roundToIntegral $ distanceFromDriverToDestination + distanceFromDestinationToPickup,
                distanceFromDriverToDestination = roundToIntegral distanceFromDriverToDestination,
                mode = info.mode,
                clientSdkVersion = person.clientSdkVersion,
                clientBundleVersion = person.clientBundleVersion,
                clientConfigVersion = person.clientConfigVersion,
                vehicleAge = getVehicleAge vehicle.mYManufacturing now,
                clientDevice = person.clientDevice,
                backendConfigVersion = person.backendConfigVersion,
                backendAppVersion = person.backendAppVersion,
                latestScheduledBooking = info.latestScheduledBooking,
                latestScheduledPickup = info.latestScheduledPickup
              }
