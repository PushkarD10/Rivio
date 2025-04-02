{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.NearbyBuses (postNearbyBusBooking) where

import qualified API.Types.UI.NearbyBuses
import qualified BecknV2.FRFS.Enums as Spe
import qualified BecknV2.OnDemand.Enums
import Data.Text.Encoding (decodeUtf8)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.RecentLocation
import qualified Domain.Types.VehicleRouteMapping as DTVRM
import qualified Environment
import EulerHS.Prelude hiding (decodeUtf8, id)
import qualified Kernel.External.Maps.Types as Maps
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import Kernel.Utils.Logging
import qualified SharedLogic.FRFSUtils as FRFSUtils
import Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRiderConfig
import qualified Storage.Queries.IntegratedBPPConfig as QIntegratedBPPConfig
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RecentLocation as QRecentLocation
import qualified Storage.Queries.Route as QRoute
import qualified Storage.Queries.RouteStopMapping as QRouteStopMapping
import qualified Storage.Queries.VehicleRouteMapping as QVehicleRouteMapping
import Tools.Error

nearbyBusKey :: Text
nearbyBusKey = "bus_locations"

postNearbyBusBooking ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.NearbyBuses.NearbyBusesRequest ->
    Environment.Flow API.Types.UI.NearbyBuses.NearbyBusesResponse
  )
postNearbyBusBooking (mbPersonId, merchantId) req = do
  riderId <- fromMaybeM (PersonNotFound "No person found") mbPersonId
  person <- QP.findById riderId >>= fromMaybeM (PersonNotFound "No person found")
  riderConfig <- QRiderConfig.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)

  let radius :: Double = fromMaybe 0.5 riderConfig.nearbyDriverSearchRadius --TODO: To be moved to config.

  -- Convert ByteString to Text after geo search
  busesBS :: [ByteString] <- CQMMB.withCrossAppRedisNew $ Hedis.geoSearch nearbyBusKey (Hedis.FromLonLat req.userLat req.userLon) (Hedis.ByRadius radius "km")

  logDebug $ "BusesBS: " <> show busesBS
  let buses'' = map decodeUtf8 busesBS
  logDebug $ "Buses: " <> show buses''
  busesBS' :: [ByteString] <- CQMMB.withCrossAppRedisNew $ Hedis.geoSearch nearbyBusKey (Hedis.FromLonLat req.userLon req.userLat) (Hedis.ByRadius radius "km")
  let buses' = map decodeUtf8 busesBS'
  logDebug $ "BusesBS': " <> show busesBS'
  logDebug $ "Buses': " <> show buses'

  let buses = buses'' <> buses'

  busRouteMapping <- QVehicleRouteMapping.findAllByVehicleNumber buses
  let routeIds :: [Text] = map DTVRM.routeId busRouteMapping

  logDebug $ "Route IDs: " <> show routeIds

  recentLocations <- QRecentLocation.findRecentLocationsByRouteIds riderId routeIds

  -- Process recent locations to build RecentRide objects
  recentRidesNested <- forM recentLocations $ \recentLoc -> do
    case recentLoc.entityType of
      Domain.Types.RecentLocation.BUS -> do
        if (isJust recentLoc.fromStopCode && isJust recentLoc.routeCode)
          then do
            integratedBPPConfig' <- QIntegratedBPPConfig.findByDomainAndCityAndVehicleCategory "FRFS" person.merchantOperatingCityId BecknV2.OnDemand.Enums.BUS req.platformType
            case integratedBPPConfig' of
              Just integratedBPPConfig -> do
                stopMapping <- Kernel.Prelude.listToMaybe <$> QRouteStopMapping.findByStopCode (fromMaybe "" recentLoc.stopCode) integratedBPPConfig.id
                if isJust stopMapping
                  then do
                    getFares <- Kernel.Prelude.listToMaybe <$> FRFSUtils.getFares (Just riderId) Spe.BUS integratedBPPConfig.id merchantId person.merchantOperatingCityId (fromMaybe "" recentLoc.routeCode) (fromMaybe "" recentLoc.fromStopCode) (fromMaybe "" recentLoc.stopCode)
                    -- need to validate this.
                    if isNothing getFares
                      then
                        return $
                          [ -- Return a single element list instead of Just
                            API.Types.UI.NearbyBuses.RecentRide
                              { fare = (Kernel.Prelude.fromJust getFares).price,
                                fromStopCode = fromMaybe "" recentLoc.fromStopCode,
                                fromStopName = fromMaybe "" recentLoc.fromStopName,
                                routeCode = recentLoc.routeCode,
                                toStopName = (Kernel.Prelude.fromJust stopMapping).stopName,
                                toStopCode = fromMaybe "" recentLoc.stopCode -- Using same stop as placeholder
                              }
                          ]
                      else return []
                  else return []
              Nothing -> return []
          else return []
      _ -> return []

  -- Flatten the nested list and prepare the response
  let recentRides = concat recentRidesNested
  allBusesForRides <- mapM CQMMB.getRoutesBuses routeIds
  logDebug $ "All buses for rides: " <> show allBusesForRides
  let allBusesData =
        map
          ( \routeData -> do
              let filteredBus = filter (\bus -> elem bus.vehicleNumber buses) routeData.buses
              (routeData {buses = filteredBus})
          )
          allBusesForRides
  logDebug $ "All buses data: " <> show allBusesData

  -- Create nearby bus objects
  nearbyBuses <-
    mapM
      ( \busData -> do
          mapM
            ( \bus -> do
                route <- QRoute.findByRouteId (Id busData.routeId)
                let busEta = Kernel.Prelude.listToMaybe $ fromMaybe [] bus.busData.etaData
                return $
                  API.Types.UI.NearbyBuses.NearbyBus
                    { capacity = Nothing,
                      currentLocation = Maps.LatLong bus.busData.latitude bus.busData.longitude,
                      distance = Nothing,
                      eta = busEta >>= (\etaD -> Just etaD.arrivalTime),
                      nextStop = busEta >>= (\etaD -> Just etaD.stopName),
                      occupancy = Nothing,
                      routeCode = busData.routeId,
                      serviceType = Nothing,
                      vehicleNumber = Just $ bus.vehicleNumber,
                      routeLongName = (\route' -> Just route'.longName) =<< route,
                      routeShortName = (\route' -> Just route'.shortName) =<< route
                    }
            )
            busData.buses
      )
      allBusesData

  -- Return the complete response
  return $ API.Types.UI.NearbyBuses.NearbyBusesResponse (concat nearbyBuses) recentRides
