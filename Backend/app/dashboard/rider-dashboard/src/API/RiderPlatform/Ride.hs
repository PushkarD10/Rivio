{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.Ride
  ( API,
    handler,
  )
where

import qualified "rider-app" API.Dashboard.Ride as BAP
import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Ride as Common
import qualified "rider-app" Domain.Action.Dashboard.Ride as Domain
import "lib-dashboard" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Domain.Types.ServerName
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter
import Kernel.Utils.Validation (runRequestValidation)
import qualified RiderPlatformClient.RiderApp.Operations as Client
import Servant
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Tools.Auth hiding (DRIVER_OFFER_BPP)
import Tools.Auth.Merchant

type API =
  "ride"
    :> ( Common.ShareRideInfoAPI
           :<|> Common.ShareRideInfoByShortIdAPI
           :<|> RideListAPI
           :<|> Common.TripRouteAPI
           :<|> Common.PickupRouteAPI
           :<|> RideInfoAPI
           :<|> MultipleRideCancelAPI
           :<|> MultipleRideSyncAPI
           :<|> TicketRideListAPI
       )

type RideListAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'RIDES 'RIDE_LIST
    :> Common.RideListAPI

type RideInfoAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'CUSTOMERS 'RIDE_INFO_CUSTOMER
    :> Common.RideInfoAPI

type MultipleRideCancelAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'RIDES 'MULTIPLE_RIDE_CANCEL
    :> BAP.MultipleRideCancelAPI

type MultipleRideSyncAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'RIDES 'MULTIPLE_RIDE_SYNC
    :> Common.MultipleRideSyncAPI

type TicketRideListAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'RIDES 'TICKET_RIDE_LIST_API
    :> Common.TicketRideListAPI

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  shareRideInfo merchantId city
    :<|> shareRideInfoByShortId merchantId city
    :<|> rideList merchantId city
    :<|> tripRoute merchantId city
    :<|> pickupRoute merchantId city
    :<|> rideInfo merchantId city
    :<|> multipleRideCancel merchantId city
    :<|> multipleRideSync merchantId city
    :<|> ticketRideList merchantId city

rideInfoHitsCountKey :: Text -> Text
rideInfoHitsCountKey rideId = "RideInfoHits:" <> rideId <> ":hitsCount"

-- merchantCityAccessChecks can be removed from this file?
buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.RideEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.RideAPI endpoint) (Just APP_BACKEND_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing

shareRideInfo ::
  ShortId DM.Merchant ->
  City.City ->
  Id Common.Ride ->
  FlowHandler Common.ShareRideInfoRes
shareRideInfo merchantShortId opCity rideId = withFlowHandlerAPI' $ do
  shareRideApiRateLimitOptions <- asks (.shareRideApiRateLimitOptions)
  checkSlidingWindowLimitWithOptions (rideInfoHitsCountKey $ getId rideId) shareRideApiRateLimitOptions
  checkedMerchantId <- merchantCityAccessCheck merchantShortId merchantShortId opCity opCity
  Client.callRiderAppOperations checkedMerchantId opCity (.rides.shareRideInfo) rideId

shareRideInfoByShortId ::
  ShortId DM.Merchant ->
  City.City ->
  ShortId Common.Ride ->
  FlowHandler Common.ShareRideInfoRes
shareRideInfoByShortId merchantShortId opCity rideShortId = withFlowHandlerAPI' $ do
  shareRideApiRateLimitOptions <- asks (.shareRideApiRateLimitOptions)
  checkSlidingWindowLimitWithOptions (rideInfoHitsCountKey $ getShortId rideShortId) shareRideApiRateLimitOptions
  checkedMerchantId <- merchantCityAccessCheck merchantShortId merchantShortId opCity opCity
  Client.callRiderAppOperations checkedMerchantId opCity (.rides.shareRideInfoByShortId) rideShortId

rideList ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  FlowHandler Common.RideListRes
rideList merchantShortId opCity _ mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone mbFrom mbTo =
  withFlowHandlerAPI' $ do
    checkedMerchantId <- merchantCityAccessCheck merchantShortId merchantShortId opCity opCity
    Client.callRiderAppOperations checkedMerchantId opCity (.rides.rideList) mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone mbFrom mbTo

tripRoute ::
  ShortId DM.Merchant ->
  City.City ->
  Id Common.Ride ->
  Double ->
  Double ->
  FlowHandler Maps.GetRoutesResp
tripRoute merchantShortId opCity rideId pickupLocationLat pickupLocationLon = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId merchantShortId opCity opCity
  Client.callRiderAppOperations checkedMerchantId opCity (.rides.tripRoute) rideId pickupLocationLat pickupLocationLon

pickupRoute ::
  ShortId DM.Merchant ->
  City.City ->
  Id Common.Ride ->
  Double ->
  Double ->
  FlowHandler Maps.GetRoutesResp
pickupRoute merchantShortId opCity rideId pickupLocationLat pickupLocationLon = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId merchantShortId opCity opCity
  Client.callRiderAppOperations checkedMerchantId opCity (.rides.pickupRoute) rideId pickupLocationLat pickupLocationLon

rideInfo ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Id Common.Ride ->
  FlowHandler Common.RideInfoRes
rideInfo merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity opCity
  Client.callRiderAppOperations checkedMerchantId opCity (.rides.rideInfo) rideId

multipleRideCancel :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Domain.MultipleRideCancelReq -> FlowHandler APISuccess
multipleRideCancel merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity opCity
  transaction <- buildTransaction Common.MultipleRideCancelEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (.rides.multipleRideCancel) req

multipleRideSync :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.MultipleRideSyncReq -> FlowHandler Common.MultipleRideSyncResp
multipleRideSync merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  runRequestValidation Common.validateMultipleRideSyncReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity opCity
  transaction <- buildTransaction Common.MultipleRideSyncEndpoint apiTokenInfo (Just req)
  T.withResponseTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (.rides.multipleRideSync) req

ticketRideList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe (ShortId Common.Ride) -> Maybe Text -> Maybe Text -> Maybe Text -> FlowHandler Common.TicketRideListRes
ticketRideList merchantShortId opCity apiTokenInfo mbRideShortId mbCountryCode mbPhoneNumber mbSupportPhoneNumber = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity opCity
  transaction <- buildTransaction Common.TicketRideListEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (.rides.ticketRideList) mbRideShortId mbCountryCode mbPhoneNumber mbSupportPhoneNumber
