module ExternalBPP.Flow where

import qualified BecknV2.FRFS.Enums as Spec
import Data.List (sortOn)
import Domain.Action.Beckn.FRFS.Common
import Domain.Action.Beckn.FRFS.OnInit
import Domain.Action.Beckn.FRFS.OnSearch
import Domain.Types
import Domain.Types.BecknConfig
import Domain.Types.FRFSConfig
import Domain.Types.FRFSQuote as DFRFSQuote
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Domain.Types.RouteStopMapping
import Domain.Types.Station
import Domain.Types.StationType
import qualified ExternalBPP.ExternalAPI.CallAPI as CallAPI
import ExternalBPP.ExternalAPI.Types
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto.Config as DB
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FRFSUtils
import Storage.Queries.Route as QRoute
import Storage.Queries.RouteStopMapping as QRouteStopMapping
import Storage.Queries.Station as QStation
import Tools.Error

getFares :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Id Person -> Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> Text -> Text -> Text -> Spec.VehicleCategory -> m [FRFSFare]
getFares riderId merchant merchantOperatingCity integratedBPPConfig _bapConfig routeCode startStationCode endStationCode vehicleCategory = CallAPI.getFares riderId merchant merchantOperatingCity integratedBPPConfig routeCode startStationCode endStationCode vehicleCategory

search :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> DFRFSSearch.FRFSSearch -> [FRFSRouteDetails] -> [Spec.ServiceTierType] -> m DOnSearch
search merchant merchantOperatingCity integratedBPPConfig bapConfig searchReq routeDetails serviceTypes = do
  quotes <- buildQuotes routeDetails
  validTill <- mapM (\ttl -> addUTCTime (intToNominalDiffTime ttl) <$> getCurrentTime) bapConfig.searchTTLSec
  messageId <- generateGUID
  return $
    DOnSearch
      { bppSubscriberId = bapConfig.subscriberId,
        bppSubscriberUrl = showBaseUrl bapConfig.subscriberUrl,
        providerDescription = Nothing,
        providerId = bapConfig.uniqueKeyId,
        providerName = CallAPI.getProviderName integratedBPPConfig,
        quotes = quotes,
        validTill = validTill,
        transactionId = searchReq.id.getId,
        messageId = messageId,
        bppDelayedInterest = Nothing
      }
  where
    -- Build Single Transit Route Quote
    buildQuotes [routeDetail] = buildSingleTransitRouteQuote routeDetail
    -- Build Multiple Transit Routes Quotes
    buildQuotes routeDetails_@(_ : _) = buildMultipleNonTransitRouteQuotes routeDetails_
    buildQuotes [] = return []

    buildSingleTransitRouteQuote FRFSRouteDetails {..} = do
      case routeCode of
        Just routeCode' -> do
          route <- QRoute.findByRouteCode routeCode' integratedBPPConfig.id >>= fromMaybeM (RouteNotFound routeCode')
          let routeInfo =
                RouteStopInfo
                  { route,
                    totalStops = Nothing,
                    stops = Nothing,
                    startStopCode = startStationCode,
                    endStopCode = endStationCode,
                    travelTime = Nothing
                  }
          stations <- buildStations routeInfo.route.code routeInfo.startStopCode routeInfo.endStopCode integratedBPPConfig.id START END
          mkSingleRouteQuote searchReq.vehicleType routeInfo stations
        Nothing -> do
          routesInfo <- getPossibleRoutesBetweenTwoStops startStationCode endStationCode integratedBPPConfig
          quotes <-
            mapM
              ( \routeInfo -> do
                  stations <- buildStations routeInfo.route.code routeInfo.startStopCode routeInfo.endStopCode integratedBPPConfig.id START END
                  mkSingleRouteQuote searchReq.vehicleType routeInfo stations
              )
              routesInfo
          return $ concat quotes

    buildMultipleNonTransitRouteQuotes routesDetails = do
      let lastStopIndex = length routesDetails - 1
      stationsArray <- do
        mapWithIndexM
          ( \idx routeDetail -> do
              case routeDetail.routeCode of
                Just routeCode' -> do
                  let startStopType = if idx == 0 then START else TRANSIT
                  let endStopType = if idx == lastStopIndex then END else TRANSIT
                  stations <- buildStations routeCode' routeDetail.startStationCode routeDetail.endStationCode integratedBPPConfig.id startStopType endStopType
                  return stations
                Nothing -> return []
          )
          routesDetails
      let stations = concat stationsArray
      let routeDetail = mergeFFRFSRouteDetails routesDetails
      case (routeDetail, routeDetail >>= (.routeCode)) of
        (Just routeDetail', Just routeCode') -> do
          route <- QRoute.findByRouteCode routeCode' integratedBPPConfig.id >>= fromMaybeM (RouteNotFound routeCode')
          let routeInfo =
                RouteStopInfo
                  { route,
                    totalStops = Nothing,
                    stops = Nothing,
                    startStopCode = routeDetail'.startStationCode,
                    endStopCode = routeDetail'.endStationCode,
                    travelTime = Nothing
                  }
          mkSingleRouteQuote searchReq.vehicleType routeInfo stations
        _ -> return []

    buildStations :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Text -> Text -> Text -> Id IntegratedBPPConfig -> StationType -> StationType -> m [DStation]
    buildStations routeCode startStationCode endStationCode integratedBPPConfigId startStopType endStopType = do
      fromStation <- QStation.findByStationCode startStationCode integratedBPPConfigId >>= fromMaybeM (StationNotFound startStationCode)
      toStation <- QStation.findByStationCode endStationCode integratedBPPConfigId >>= fromMaybeM (StationNotFound endStationCode)
      stops <- QRouteStopMapping.findByRouteCode routeCode integratedBPPConfigId
      stations <- mkStations fromStation toStation stops startStopType endStopType & fromMaybeM (StationsNotFound fromStation.id.getId toStation.id.getId)
      return stations

    mkStations :: Station -> Station -> [RouteStopMapping] -> StationType -> StationType -> Maybe [DStation]
    mkStations fromStation toStation stops startStopType endStopType =
      ((,) <$> find (\stop -> stop.stopCode == fromStation.code) stops <*> find (\stop -> stop.stopCode == toStation.code) stops)
        <&> \(startStop, endStop) ->
          do
            let startStation = DStation startStop.stopCode startStop.stopName (Just startStop.stopPoint.lat) (Just startStop.stopPoint.lon) startStopType (Just startStop.sequenceNum) Nothing
                endStation = DStation endStop.stopCode endStop.stopName (Just endStop.stopPoint.lat) (Just endStop.stopPoint.lon) endStopType (Just endStop.sequenceNum) Nothing
                intermediateStations =
                  (sortOn (.sequenceNum) $ filter (\stop -> stop.sequenceNum > startStop.sequenceNum && stop.sequenceNum < endStop.sequenceNum) stops)
                    <&> (\stop -> DStation stop.stopCode stop.stopName (Just stop.stopPoint.lat) (Just stop.stopPoint.lon) INTERMEDIATE (Just stop.sequenceNum) Nothing)
            [startStation] ++ intermediateStations ++ [endStation]

    mkSingleRouteQuote :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Spec.VehicleCategory -> RouteStopInfo -> [DStation] -> m [DQuote]
    mkSingleRouteQuote vehicleType routeInfo stations = do
      fares <- CallAPI.getFares searchReq.riderId merchant merchantOperatingCity integratedBPPConfig routeInfo.route.code routeInfo.startStopCode routeInfo.endStopCode vehicleType
      let filteredFares = if null serviceTypes then fares else filter (\fare -> fare.vehicleServiceTier.serviceTierType `elem` serviceTypes) fares
      return $
        map
          ( \FRFSFare {..} ->
              let routeStations =
                    [ DRouteStation
                        { routeCode = routeInfo.route.code,
                          routeLongName = routeInfo.route.longName,
                          routeShortName = routeInfo.route.shortName,
                          routeStartPoint = routeInfo.route.startPoint,
                          routeEndPoint = routeInfo.route.endPoint,
                          routeStations = stations,
                          routeTravelTime = routeInfo.travelTime,
                          routeServiceTier = Just $ mkDVehicleServiceTier vehicleServiceTier,
                          routePrice = price,
                          routeSequenceNum = Nothing,
                          routeColor = Nothing
                        }
                    ]
               in DQuote
                    { bppItemId = CallAPI.getProviderName integratedBPPConfig,
                      _type = DFRFSQuote.SingleJourney,
                      routeStations = routeStations,
                      discounts = map mkDDiscount discounts,
                      ..
                    }
          )
          filteredFares

    mkDVehicleServiceTier FRFSVehicleServiceTier {..} = DVehicleServiceTier {..}

    mkDDiscount FRFSDiscount {..} = DDiscount {..}

    mapWithIndexM f xs = zipWithM f [0 ..] xs

init :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r) => Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> (Maybe Text, Maybe Text) -> DFRFSTicketBooking.FRFSTicketBooking -> m DOnInit
init merchant merchantOperatingCity integratedBPPConfig bapConfig (mRiderName, mRiderNumber) booking = do
  validTill <- mapM (\ttl -> addUTCTime (intToNominalDiffTime ttl) <$> getCurrentTime) bapConfig.initTTLSec
  paymentDetails <- mkPaymentDetails bapConfig.collectedBy
  bankAccountNumber <- paymentDetails.bankAccNumber & fromMaybeM (InternalError "Bank Account Number Not Found")
  bankCode <- paymentDetails.bankCode & fromMaybeM (InternalError "Bank Code Not Found")
  let quantity = booking.quantity
  let quantityRational = fromIntegral quantity :: Rational
  let totalPrice =
        Price
          { amount = HighPrecMoney $ getHighPrecMoney booking.price.amount * quantityRational,
            amountInt = Money $ booking.price.amountInt.getMoney * quantity,
            currency = booking.price.currency
          }

  return $
    DOnInit
      { providerId = bapConfig.uniqueKeyId,
        totalPrice = totalPrice,
        fareBreakUp = [],
        bppItemId = CallAPI.getProviderName integratedBPPConfig,
        validTill = validTill,
        transactionId = booking.searchId.getId,
        messageId = booking.id.getId,
        bankAccNum = bankAccountNumber,
        bankCode = bankCode
      }
  where
    mkPaymentDetails = \case
      Spec.BAP -> do
        let paymentParams :: (Maybe BknPaymentParams) = decodeFromText =<< bapConfig.paymentParamsJson
        paymentParams & fromMaybeM (InternalError "BknPaymentParams Not Found")
      Spec.BPP -> CallAPI.getPaymentDetails merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking

confirm :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Merchant -> MerchantOperatingCity -> FRFSConfig -> IntegratedBPPConfig -> BecknConfig -> (Maybe Text, Maybe Text) -> DFRFSTicketBooking.FRFSTicketBooking -> m DOrder
confirm _merchant _merchantOperatingCity frfsConfig integratedBPPConfig bapConfig (mRiderName, mRiderNumber) booking = do
  order <- CallAPI.createOrder integratedBPPConfig frfsConfig.busStationTtl (mRiderName, mRiderNumber) booking
  let tickets =
        map
          ( \ticket ->
              DTicket
                { qrData = ticket.qrData,
                  vehicleNumber = ticket.vehicleNumber,
                  bppFulfillmentId = CallAPI.getProviderName integratedBPPConfig,
                  ticketNumber = ticket.ticketNumber,
                  validTill = ticket.qrValidity,
                  status = ticket.qrStatus,
                  description = ticket.description,
                  qrRefreshAt = ticket.qrRefreshAt
                }
          )
          order.tickets
  return $
    DOrder
      { providerId = bapConfig.uniqueKeyId,
        totalPrice = booking.price.amount,
        fareBreakUp = [],
        bppOrderId = order.orderId,
        bppItemId = CallAPI.getProviderName integratedBPPConfig,
        transactionId = booking.searchId.getId,
        orderStatus = Nothing,
        messageId = booking.id.getId,
        tickets = tickets
      }

status :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Id Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> DFRFSTicketBooking.FRFSTicketBooking -> m DOrder
status _merchantId _merchantOperatingCity integratedBPPConfig bapConfig booking = do
  bppOrderId <- booking.bppOrderId & fromMaybeM (InternalError "BPP Order Id Not Found")
  tickets' <- CallAPI.getTicketStatus integratedBPPConfig booking
  let tickets =
        map
          ( \ticket ->
              DTicket
                { qrData = ticket.qrData,
                  vehicleNumber = ticket.vehicleNumber,
                  bppFulfillmentId = CallAPI.getProviderName integratedBPPConfig,
                  ticketNumber = ticket.ticketNumber,
                  validTill = ticket.qrValidity,
                  status = ticket.qrStatus,
                  qrRefreshAt = ticket.qrRefreshAt,
                  description = ticket.description
                }
          )
          tickets'
  return $
    DOrder
      { providerId = bapConfig.uniqueKeyId,
        totalPrice = booking.price.amount,
        fareBreakUp = [],
        bppOrderId = bppOrderId,
        bppItemId = CallAPI.getProviderName integratedBPPConfig,
        transactionId = booking.searchId.getId,
        orderStatus = Nothing,
        messageId = booking.id.getId,
        tickets = tickets
      }

verifyTicket :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r) => Id Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> Text -> m DTicketPayload
verifyTicket _merchantId _merchantOperatingCity integratedBPPConfig _bapConfig encryptedQrData = do
  TicketPayload {..} <- CallAPI.verifyTicket integratedBPPConfig encryptedQrData
  return DTicketPayload {..}
