module Lib.JourneyModule.Utils where

import BecknV2.FRFS.Enums as Spec
import Data.List (groupBy, nub, sort, sortBy)
import Data.Ord (comparing)
import Data.Time hiding (getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import qualified Domain.Types.FRFSFarePolicy as FRFSFarePolicy
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.IntegratedBPPConfig as DIntegratedBPPConfig
import Domain.Types.Journey
import qualified Domain.Types.MultimodalPreferences as DMP
import Domain.Types.Route
import Domain.Types.RouteStopTimeTable
import qualified Domain.Types.Trip as DTrip
import Domain.Utils (utctTimeToDayOfWeek)
import Kernel.External.MultiModal.Interface as MultiModal hiding (decode, encode)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FRFSFarePolicy as QFRFSFarePolicy
import qualified Storage.Queries.FRFSRouteFareProduct as QFRFSRouteFareProduct
import qualified Storage.Queries.FRFSRouteStopStageFare as QFRFSRouteStopStageFare
import qualified Storage.Queries.FRFSStageFare as QFRFSStageFare
import qualified Storage.Queries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier
import qualified Storage.Queries.Route as QRoute
import qualified Storage.Queries.RouteStopCalender as QRouteCalendar
import qualified Storage.Queries.RouteStopMapping as QRouteStopMapping
import qualified Storage.Queries.RouteStopTimeTable as QRouteStopTiming
import qualified Storage.Queries.Station as QStation

mapWithIndex :: (MonadFlow m) => (Int -> a -> m b) -> [a] -> m [b]
mapWithIndex f = go 0
  where
    go _ [] = return []
    go idx (x : xs') = do
      y <- f idx x
      ys <- go (idx + 1) xs'
      return (y : ys)

convertMultiModalModeToTripMode :: MultiModal.GeneralVehicleType -> Meters -> Meters -> Meters -> Meters -> DTrip.MultimodalTravelMode
convertMultiModalModeToTripMode input straightLineDistance distance maximumWalkDistance straightLineThreshold = case input of
  MultiModal.MetroRail -> DTrip.Metro
  MultiModal.Subway -> DTrip.Subway
  MultiModal.Walk -> if distance > maximumWalkDistance && straightLineDistance > straightLineThreshold then DTrip.Taxi else DTrip.Walk
  MultiModal.Bus -> DTrip.Bus
  MultiModal.Unspecified -> DTrip.Taxi

mkJourneyUpdateInProgressKey :: Id Journey -> Text
mkJourneyUpdateInProgressKey journeyId = "Journey:UpdateInProgress:JourneyId-" <> journeyId.getId

journeyUpdateInProgress ::
  CacheFlow m r =>
  Id Journey ->
  m Bool
journeyUpdateInProgress journeyId = do
  fromMaybe False <$> (Hedis.withMasterRedis $ Hedis.get (mkJourneyUpdateInProgressKey journeyId))

withJourneyUpdateInProgress ::
  CacheFlow m r =>
  Id Journey ->
  m a ->
  m a
withJourneyUpdateInProgress journeyId actions = do
  Hedis.withMasterRedis $ Hedis.setExp (mkJourneyUpdateInProgressKey journeyId) True 120
  a <- actions
  Hedis.withMasterRedis $ Hedis.setExp (mkJourneyUpdateInProgressKey journeyId) False 120
  return a

whenJourneyUpdateInProgress ::
  CacheFlow m r =>
  Id Journey ->
  m a ->
  m a
whenJourneyUpdateInProgress journeyId actions = do
  gotLock <- journeyUpdateInProgress journeyId
  if gotLock
    then do
      threadDelayMilliSec 200
      whenJourneyUpdateInProgress journeyId actions
    else actions

data UpcomingBusInfo = UpcomingBusInfo
  { routeCode :: Text,
    serviceType :: Spec.ServiceTierType,
    arrivalTimeInSeconds :: Seconds
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

-- | Data type representing upcoming trip information
data UpcomingTripInfo = UpcomingTripInfo
  { busFrequency :: Maybe Seconds,
    upcomingBuses :: [UpcomingBusInfo]
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data LegServiceTier = LegServiceTier
  { serviceTierName :: Text,
    serviceTierType :: Spec.ServiceTierType,
    serviceTierDescription :: Text,
    fare :: PriceAPIEntity,
    quoteId :: Id DFRFSQuote.FRFSQuote
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Data structure to represent available routes grouped by service tier
data AvailableRoutesByTier = AvailableRoutesByTier
  { serviceTier :: Spec.ServiceTierType,
    serviceTierName :: Maybe Text,
    serviceTierDescription :: Maybe Text,
    quoteId :: Maybe (Id DFRFSQuote.FRFSQuote),
    availableRoutes :: [Text],
    nextAvailableBuses :: [Seconds],
    fare :: PriceAPIEntity
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

getISTArrivalTime :: TimeOfDay -> UTCTime -> UTCTime
getISTArrivalTime timeOfDay currentTime = do
  let currentTimeIST = addUTCTime (secondsToNominalDiffTime $ round istOffset) currentTime
  UTCTime (utctDay currentTimeIST) (timeOfDayToTime timeOfDay)
  where
    istOffset :: Double = 5.5 * 3600

-- | Helper function to get IST offset and current time in IST timezone
getISTTimeInfo :: UTCTime -> (Double, UTCTime)
getISTTimeInfo currentTime =
  let istOffset :: Double = 5.5 * 3600
      currentTimeIST = addUTCTime (secondsToNominalDiffTime $ round istOffset) currentTime
   in (istOffset, currentTimeIST)

-- | Helper function to check which trips are serviceable on the current day
filterServiceableTrips ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    Monad m
  ) =>
  [Id RouteStopTimeTable] -> -- List of trip IDs to check
  UTCTime -> -- Current time
  Id DIntegratedBPPConfig.IntegratedBPPConfig ->
  m [Id RouteStopTimeTable] -- List of serviceable trip IDs
filterServiceableTrips tripIds currentTime integratedBppConfigId = do
  routeCalendars <- QRouteCalendar.findByTripIds tripIds integratedBppConfigId
  let (_, currentTimeIST) = getISTTimeInfo currentTime
      today = fromEnum $ utctTimeToDayOfWeek currentTimeIST
      serviceableTrips = filter (\rc -> if length rc.serviceability > today then (rc.serviceability !! today) > 0 else False) routeCalendars
  return $ map (.tripId) serviceableTrips

-- | Calculate stage-based fare for a route between two stops
calculateStageFare ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m
  ) =>
  Text -> -- Route code
  Text -> -- From stop code
  Text -> -- To stop code
  Spec.ServiceTierType -> -- Service tier type
  Id DIntegratedBPPConfig.IntegratedBPPConfig ->
  m (Maybe PriceAPIEntity)
calculateStageFare routeCode fromStopCode toStopCode serviceTierType integratedBppConfigId = do
  -- First get the fare policy for this route and service tier
  routeFareProducts <- QFRFSRouteFareProduct.findByRouteCode routeCode integratedBppConfigId

  -- Find a fare product that matches the service tier
  mbFarePolicyId <- do
    matchingProducts <-
      filterM
        ( \rp -> do
            mbVehicleServiceTier <- QFRFSVehicleServiceTier.findById rp.vehicleServiceTierId
            case mbVehicleServiceTier of
              Just vst -> return $ vst._type == serviceTierType
              Nothing -> return False
        )
        routeFareProducts
    return $ (listToMaybe matchingProducts) <&> (.farePolicyId)

  case mbFarePolicyId of
    Nothing -> return Nothing
    Just farePolicyId -> do
      -- Get the fare policy to confirm if it's stage-based
      mbFarePolicy <- QFRFSFarePolicy.findById farePolicyId

      case mbFarePolicy of
        Nothing -> return Nothing
        Just farePolicy ->
          if farePolicy._type == FRFSFarePolicy.StageBased
            then do
              -- For stage-based, we need to find which stage the stops belong to
              fromStopStage <- QFRFSRouteStopStageFare.findByRouteAndStopCode farePolicyId routeCode fromStopCode
              toStopStage <- QFRFSRouteStopStageFare.findByRouteAndStopCode farePolicyId routeCode toStopCode

              case (fromStopStage, toStopStage) of
                (Just fromStage, Just toStage) -> do
                  -- Calculate number of stages traveled
                  let stagesTraveled = max 1 $ abs (toStage.stage - fromStage.stage)

                  -- Get the stage fare for this many stages
                  stageFares <- QFRFSStageFare.findAllByFarePolicyId farePolicyId

                  -- Find the fare for this number of stages
                  let mbStageFare = find (\sf -> sf.stage == stagesTraveled) stageFares

                  -- Return the price
                  return $ fmap (\sf -> mkPriceAPIEntity (mkPrice (Just sf.currency) sf.amount)) mbStageFare
                _ -> return Nothing -- Either of the stops doesn't have a stage mapping
            else return Nothing -- handle it later

-- | Find all possible routes from originStop to destinationStop with trips in the next hour
-- grouped by service tier type
findPossibleRoutes ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m
  ) =>
  Maybe [LegServiceTier] ->
  Text ->
  Text ->
  UTCTime ->
  Id DIntegratedBPPConfig.IntegratedBPPConfig ->
  m [AvailableRoutesByTier]
findPossibleRoutes mbAvailableServiceTiers fromStopCode toStopCode currentTime integratedBppConfigId = do
  -- Get route mappings that contain the origin stop
  fromRouteStopMappings <- QRouteStopMapping.findByStopCode fromStopCode integratedBppConfigId

  -- Get route mappings that contain the destination stop
  toRouteStopMappings <- QRouteStopMapping.findByStopCode toStopCode integratedBppConfigId

  -- Find common routes that have both the origin and destination stops
  -- and ensure that from-stop comes before to-stop in the route sequence
  let fromRouteStopMap = map (\mapping -> (mapping.routeCode, mapping.sequenceNum)) fromRouteStopMappings
      toRouteStopMap = map (\mapping -> (mapping.routeCode, mapping.sequenceNum)) toRouteStopMappings
      validRoutes =
        [ fromRouteCode
          | (fromRouteCode, fromSeq) <- fromRouteStopMap,
            (toRouteCode, toSeq) <- toRouteStopMap,
            fromRouteCode == toRouteCode && fromSeq < toSeq -- Ensure correct sequence
        ]

  -- Get the timing information for these routes at the origin stop
  routeStopTimings <- QRouteStopTiming.findByRouteCodeAndStopCode validRoutes fromStopCode integratedBppConfigId

  -- Get IST time info
  let (_, currentTimeIST) = getISTTimeInfo currentTime
      nextHourCutoff = addUTCTime 3600 currentTimeIST

  -- Get trip ids for calendar checking
  let tripIds = map (.tripId) routeStopTimings

  -- Check which trips are serviceable today
  serviceableTripIds <- filterServiceableTrips tripIds currentTime integratedBppConfigId

  -- Filter timings by serviceable trips and future arrivals within the next hour
  let validTimings =
        [ timing
          | timing <- routeStopTimings,
            timing.tripId `elem` serviceableTripIds,
            let arrivalTime = getISTArrivalTime timing.timeOfArrival currentTime,
            arrivalTime > currentTimeIST && arrivalTime <= nextHourCutoff
        ]

  -- Group by service tier
  let groupedByTier = groupBy (\a b -> a.serviceTierType == b.serviceTierType) $ sortBy (comparing (.serviceTierType)) validTimings

  -- For each service tier, collect route information
  results <- forM groupedByTier $ \timingsForTier -> do
    let serviceTierType = if null timingsForTier then Spec.ORDINARY else (head timingsForTier).serviceTierType
        routeCodesForTier = nub $ map (.routeCode) timingsForTier

    -- Get route details to include the short name
    routeDetails <- mapM (\routeCode -> QRoute.findByRouteCode routeCode integratedBppConfigId) routeCodesForTier
    let validRouteDetails = catMaybes routeDetails
        routeShortNames = nub $ map (.shortName) validRouteDetails

    -- Calculate arrival times in seconds
    let arrivalTimes =
          [ nominalDiffTimeToSeconds $ diffUTCTime (getISTArrivalTime timing.timeOfArrival currentTime) currentTimeIST
            | timing <- timingsForTier
          ]

    logDebug $ "routeShortNames: " <> show routeShortNames

    (mbFare, serviceTierName, serviceTierDescription, quoteId) <- do
      case mbAvailableServiceTiers of
        Just availableServiceTiers -> do
          let availableServiceTier = find (\tier -> tier.serviceTierType == serviceTierType) availableServiceTiers
              quoteId = availableServiceTier <&> (.quoteId)
              serviceTierName = availableServiceTier <&> (.serviceTierName)
              serviceTierDescription = availableServiceTier <&> (.serviceTierDescription)
              mbFare = availableServiceTier <&> (.fare)
          return (mbFare, serviceTierName, serviceTierDescription, quoteId)
        Nothing -> do
          mbFare <-
            if null routeCodesForTier
              then return Nothing
              else calculateStageFare (head routeCodesForTier) fromStopCode toStopCode serviceTierType integratedBppConfigId
          return (mbFare, Nothing, Nothing, Nothing)

    logDebug $ "mbFare: " <> show mbFare
    return $
      ( \fare ->
          AvailableRoutesByTier
            { serviceTier = serviceTierType,
              availableRoutes = routeShortNames,
              nextAvailableBuses = sort arrivalTimes,
              serviceTierName = serviceTierName,
              serviceTierDescription = serviceTierDescription,
              quoteId = quoteId,
              fare = fare
            }
      )
        <$> mbFare

  -- Only return service tiers that have available routes
  return $ filter (\r -> not (null $ r.availableRoutes)) (catMaybes results)

-- | Find the top upcoming trips for a given route code and stop code
-- Returns arrival times in seconds for the upcoming trips along with route ID and service type
findUpcomingTrips ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m
  ) =>
  [Text] ->
  Text ->
  Maybe Spec.ServiceTierType ->
  UTCTime ->
  Id DIntegratedBPPConfig.IntegratedBPPConfig ->
  m UpcomingTripInfo
findUpcomingTrips routeCodes stopCode mbServiceType currentTime integratedBppConfigId = do
  -- Get IST time info
  let (_, currentTimeIST) = getISTTimeInfo currentTime

  routeStopTimings <- QRouteStopTiming.findByRouteCodeAndStopCode routeCodes stopCode integratedBppConfigId

  let filteredByService = case mbServiceType of
        Just serviceType -> filter (\rst -> rst.serviceTierType == serviceType) routeStopTimings
        Nothing -> routeStopTimings

  let tripIds = map (.tripId) filteredByService

  -- Check which trips are serviceable today
  serviceableTripIds <- filterServiceableTrips tripIds currentTime integratedBppConfigId

  -- Combine stop timings with their calendars and get arrival times
  -- Filter out trips that have already passed the stop
  let tripTimingsWithCalendars =
        [ UpcomingBusInfo
            { routeCode = rst.routeCode,
              serviceType = rst.serviceTierType,
              arrivalTimeInSeconds = nominalDiffTimeToSeconds $ diffUTCTime (getISTArrivalTime rst.timeOfArrival currentTime) currentTimeIST
            }
          | rst <- filteredByService,
            rst.tripId `elem` serviceableTripIds,
            (getISTArrivalTime rst.timeOfArrival currentTime) > currentTimeIST -- Only include future arrivals
        ]

  let upcomingBuses = sortBy (\a b -> compare (arrivalTimeInSeconds a) (arrivalTimeInSeconds b)) tripTimingsWithCalendars

  let busFrequency =
        if length upcomingBuses >= 3
          then do
            let gaps =
                  zipWith
                    (\a b -> arrivalTimeInSeconds b - arrivalTimeInSeconds a)
                    upcomingBuses
                    (tail upcomingBuses)

            let avgGap :: Double = fromIntegral (sum gaps) / fromIntegral (length gaps)
                gapsAreEqual = all (\gap -> abs (fromIntegral gap - avgGap) <= (0.2 * avgGap)) gaps

            if gapsAreEqual
              then Just $ round avgGap
              else Nothing
          else Nothing

  let upcomingTripInfo =
        UpcomingTripInfo
          { busFrequency = busFrequency,
            upcomingBuses = upcomingBuses
          }
  return upcomingTripInfo

data StopDetails = StopDetails
  { stopCode :: Text,
    stopName :: Text,
    stopLat :: Double,
    stopLon :: Double,
    stopArrivalTime :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data BusRouteDetails = BusRouteDetails
  { fromStop :: StopDetails,
    toStop :: StopDetails,
    route :: Route,
    availableRoutes :: [Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

getBusRouteDetails ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m
  ) =>
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Id DIntegratedBPPConfig.IntegratedBPPConfig ->
  m (Maybe BusRouteDetails)
getBusRouteDetails (Just routeCode) (Just originStopCode) (Just destinationStopCode) integratedBppConfigId = do
  mbFromStop <- QStation.findByStationCode originStopCode integratedBppConfigId
  mbToStop <- QStation.findByStationCode destinationStopCode integratedBppConfigId
  mbRoute <- QRoute.findByRouteCode routeCode integratedBppConfigId
  currentTime <- getCurrentTime

  case (mbFromStop, mbToStop, mbRoute) of
    (Just fromStop, Just toStop, Just route) -> do
      case (fromStop.lat, fromStop.lon, toStop.lat, toStop.lon) of
        (Just fromStopLat, Just fromStopLon, Just toStopLat, Just toStopLon) -> do
          -- Get timing information for this route at the origin stop
          originStopTimings <- QRouteStopTiming.findByRouteCodeAndStopCode [routeCode] originStopCode integratedBppConfigId
          destStopTimings <- QRouteStopTiming.findByRouteCodeAndStopCode [routeCode] destinationStopCode integratedBppConfigId

          -- Get trip IDs for calendar checking
          let tripIds = map (.tripId) originStopTimings

          -- Get IST time info
          let (_, currentTimeIST) = getISTTimeInfo currentTime

          -- Check which trips are serviceable today
          serviceableTripIds <- filterServiceableTrips tripIds currentTime integratedBppConfigId

          -- Find the earliest upcoming departure from origin stop
          let validOriginTimings =
                [ timing
                  | timing <- originStopTimings,
                    timing.tripId `elem` serviceableTripIds,
                    let departureTime = getISTArrivalTime timing.timeOfDeparture currentTime,
                    departureTime > currentTimeIST
                ]

          let mbEarliestOriginTiming =
                listToMaybe $
                  sortBy
                    ( \a b ->
                        compare
                          (getISTArrivalTime a.timeOfDeparture currentTime)
                          (getISTArrivalTime b.timeOfDeparture currentTime)
                    )
                    validOriginTimings

          -- Find the corresponding destination arrival for the same trip
          let mbDestinationTiming = do
                originTiming <- mbEarliestOriginTiming
                find (\dt -> dt.tripId == originTiming.tripId) destStopTimings

          -- Create stop details
          let mbDepartureTime = getISTArrivalTime . (.timeOfDeparture) <$> mbEarliestOriginTiming <*> pure currentTime
              mbArrivalTime = getISTArrivalTime . (.timeOfArrival) <$> mbDestinationTiming <*> pure currentTime
              fromStopDetails = StopDetails fromStop.code fromStop.name fromStopLat fromStopLon (fromMaybe currentTime mbDepartureTime)
              toStopDetails = StopDetails toStop.code toStop.name toStopLat toStopLon (fromMaybe currentTime mbArrivalTime)
          possibleRoutes <- findPossibleRoutes Nothing originStopCode destinationStopCode currentTime integratedBppConfigId
          return $ Just $ BusRouteDetails fromStopDetails toStopDetails route (concatMap (.availableRoutes) possibleRoutes)
        _ -> return Nothing
    _ -> return Nothing
getBusRouteDetails _ _ _ _ = return Nothing

convertSortingType :: DMP.JourneyOptionsSortingType -> MultiModal.SortingType
convertSortingType sortType = case sortType of
  DMP.FASTEST -> MultiModal.Fastest
  DMP.MINIMUM_TRANSITS -> MultiModal.MinimumTransits
  DMP.MOST_RELEVANT -> MultiModal.MostRelevant
  _ -> MultiModal.Fastest -- Default case for any other values
