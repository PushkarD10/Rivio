{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TicketBookingFlow.MetroTicketBooking.ScreenData where

import Prelude
import Screens.Types as ST
import ConfigProvider
import Services.API (MetroBookingConfigRes(..), GetBusRouteResp(..))
import Data.Maybe as Mb
import Data.Array 
import Services.API as API

initData :: ST.MetroTicketBookingScreenState
initData = {
  data: {
    ticketType : ST.ONE_WAY_TICKET
  , ticketCount : 1
  , srcLoc : ""
  , destLoc : ""
  , srcCode : ""
  , destCode : ""
  , searchId : ""
  , ticketPrice : 0
  , bookingId : ""
  , quoteId : ""
  , quoteResp : []
  , routeSearchedList : []
  , stopsSearchedList : []
  , metroBookingConfigResp : MetroBookingConfigRes {bookingEndTime: "", bookingStartTime: "", oneWayTicketLimit: 0, roundTripTicketLimit: 0, metroStationTtl: 10080, discount: 0, customEndTime : "", customDates : [], isEventOngoing : Mb.Nothing, freeTicketInterval : Mb.Nothing, maxFreeTicketCashback : Mb.Nothing, ticketsBookedInEvent : Mb.Nothing , isCancellationAllowed : Mb.Just true}
  , eventDiscountAmount : Mb.Nothing
  },
  props: {
    isLimitExceeded : false
    , termsAndConditionsSelected : true
    , currentStage : ST.MetroTicketSelection
    , isButtonActive : false
    , showMetroBookingTimeError : false
    , showShimmer : true
    , busClicked : false
    , routeList : false
    , showRouteOptions : false
    , isEmptyRoute : ""
    , ticketServiceType : API.METRO
    
  },
  config :  getAppConfig appConfig
}
