module Storage.Queries.Person.GetNearestDrivers where

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
import qualified Storage.Queries.Person.Internal as Int
import qualified Storage.Queries.Vehicle.Internal as Int

data NearestDriversResult = NearestDriversResult
  { driverId :: Id Driver,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    language :: Maybe Maps.Language,
    onRide :: Bool,
    distanceToDriver :: Meters,
    variant :: DV.Variant,
    serviceTier :: DVST.ServiceTierType,
    serviceTierDowngradeLevel :: Int,
    isAirConditioned :: Maybe Bool,
    lat :: Double,
    lon :: Double,
    mode :: Maybe DriverInfo.DriverMode,
    clientSdkVersion :: Maybe Version,
    clientBundleVersion :: Maybe Version,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device,
    vehicleAge :: Maybe Months,
    backendConfigVersion :: Maybe Version,
    backendAppVersion :: Maybe Text,
    latestScheduledBooking :: Maybe UTCTime,
    latestScheduledPickup :: Maybe Maps.LatLong
  }
  deriving (Generic, Show, HasCoordinates)

data NearestDriversReq = NearestDriversReq
  { cityServiceTiers :: [DVST.VehicleServiceTier],
    serviceTiers :: [ServiceTierType],
    fromLocLatLong :: LatLong,
    nearestRadius :: Meters,
    merchantId :: Id Merchant,
    driverPositionInfoExpiry :: Maybe Seconds,
    isRental :: Bool,
    isInterCity :: Bool,
    isValueAddNP :: Bool,
    onlinePayment :: Bool,
    now :: UTCTime
  }

getNearestDrivers ::
  (MonadFlow m, MonadTime m, LT.HasLocationService m r, CoreMetrics m, EsqDBFlow m r, CacheFlow m r, ServiceFlow m r) =>
  NearestDriversReq ->
  m [NearestDriversResult]
getNearestDrivers NearestDriversReq {..} = do
  driverLocs <- Int.getDriverLocsWithCond merchantId driverPositionInfoExpiry fromLocLatLong nearestRadius
  driverInfos <- Int.getDriverInfosWithCond (driverLocs <&> (.driverId)) True False isRental isInterCity isValueAddNP
  vehicle <- Int.getVehicles driverInfos
  drivers <- Int.getDrivers vehicle
  -- driverStats <- QDriverStats.findAllByDriverIds drivers
  driverBankAccounts <-
    if onlinePayment
      then QDBA.getDrivers (driverLocs <&> (.driverId))
      else return []

  logDebug $ "GetNearestDriver - DLoc:- " <> show (length driverLocs) <> " DInfo:- " <> show (length driverInfos) <> " Vehicles:- " <> show (length vehicle) <> " Drivers:- " <> show (length drivers)
  let res = linkArrayList driverLocs driverInfos vehicle drivers driverBankAccounts
  logDebug $ "GetNearestDrivers Result:- " <> show (length res)
  return res
  where
    linkArrayList driverLocations driverInformations vehicles persons driverBankAccounts =
      let personHashMap = HashMap.fromList $ (\p -> (p.id, p)) <$> persons
          -- driverStatsHashMap = HashMap.fromList $ (\stats -> (stats.driverId, stats)) <$> driverStats
          driverInfoHashMap = HashMap.fromList $ (\info -> (info.driverId, info)) <$> driverInformations
          vehicleHashMap = HashMap.fromList $ (\v -> (v.driverId, v)) <$> vehicles
          driverBankAccountHashMap = HashMap.fromList $ catMaybes $ (\dba -> if dba.chargesEnabled then Just (dba.driverId, dba.accountId) else Nothing) <$> driverBankAccounts
       in concat $ mapMaybe (buildFullDriverList personHashMap vehicleHashMap driverInfoHashMap driverBankAccountHashMap) driverLocations

    buildFullDriverList personHashMap vehicleHashMap driverInfoHashMap driverBankAccountHashMap location = do
      let driverId' = location.driverId
      person <- HashMap.lookup driverId' personHashMap
      -- driverStats <- HashMap.lookup driverId' driverStatsHashMap
      vehicle <- HashMap.lookup driverId' vehicleHashMap
      info <- HashMap.lookup driverId' driverInfoHashMap
      _ <- if onlinePayment then HashMap.lookup driverId' driverBankAccountHashMap else Just T.empty -- is there any better way to do this?
      let dist = (realToFrac $ distanceBetweenInMeters fromLocLatLong $ LatLong {lat = location.lat, lon = location.lon}) :: Double
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
      if null serviceTiers
        then Just $ mapMaybe (mkDriverResult mbDefaultServiceTierForDriver person vehicle info dist cityServiceTiersHashMap) selectedDriverServiceTiers
        else do
          Just $
            mapMaybe
              ( \serviceTier -> do
                  if serviceTier `elem` selectedDriverServiceTiers
                    then mkDriverResult mbDefaultServiceTierForDriver person vehicle info dist cityServiceTiersHashMap serviceTier
                    else Nothing
              )
              serviceTiers
      where
        mkDriverResult mbDefaultServiceTierForDriver person vehicle info dist cityServiceTiersHashMap serviceTier = do
          serviceTierInfo <- HashMap.lookup serviceTier cityServiceTiersHashMap
          Just $ NearestDriversResult (cast person.id) person.deviceToken person.language info.onRide (roundToIntegral dist) vehicle.variant serviceTier (maybe 0 (\d -> d.priority - serviceTierInfo.priority) mbDefaultServiceTierForDriver) serviceTierInfo.isAirConditioned location.lat location.lon info.mode person.clientSdkVersion person.clientBundleVersion person.clientConfigVersion person.clientDevice (getVehicleAge vehicle.mYManufacturing now) person.backendConfigVersion person.backendAppVersion info.latestScheduledBooking info.latestScheduledPickup
