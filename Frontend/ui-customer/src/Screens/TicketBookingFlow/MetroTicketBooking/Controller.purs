{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.MetroTicketBooking.Controller where

import Prelude
import Screens.Types as ST
import Components.GenericHeader as GenericHeader
import Components.PrimaryEditText as PrimaryEditText
import Components.PrimaryButton as PrimaryButton
import Prelude (class Show, pure, unit, bind, discard, ($), (/=), (==))
import PrestoDOM
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Services.API
import Data.Array
import Data.Maybe
import Debug (spy)
import JBridge (toast, toggleBtnLoader)
import Components.RequestInfoCard as InfoCard
import Language.Strings
import Language.Types

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    _ -> pure unit

data Action = BackPressed
            | NoAction
            | GenericHeaderAC GenericHeader.Action
            | UpdateButtonAction PrimaryButton.Action
            | MyMetroTicketAction
            | ChangeTicketTab ST.TicketType
            | IncrementTicket
            | DecrementTicket
            | MetroRouteMapAction
            | ToggleTermsAndConditions
            | GetMetroQuotesAction (Array MetroQuote)
            | SelectLocation ST.LocationActionId
            | ShowMetroBookingTimeError Boolean
            | InfoCardAC InfoCard.Action 
            | GetSDKPollingAC CreateOrderRes

data ScreenOutput = GoBack ST.MetroTicketBookingScreenState
                  | UpdateAction ST.MetroTicketBookingScreenState
                  | MyMetroTicketScreen
                  | GoToMetroRouteMap
                  | GoToHome
                  | SelectSrcDest ST.LocationActionId ST.MetroTicketBookingScreenState
                  | Refresh ST.MetroTicketBookingScreenState
                  | GotoPaymentPage CreateOrderRes String

eval :: Action -> ST.MetroTicketBookingScreenState -> Eval Action ScreenOutput ST.MetroTicketBookingScreenState

eval BackPressed state =  exit $ GoToHome
eval (UpdateButtonAction (PrimaryButton.OnClick)) state = do
    updateAndExit state $ UpdateAction state

eval MyMetroTicketAction state = exit $ MyMetroTicketScreen

eval IncrementTicket state = do
  if state.data.ticketCount < 6
    then continue state { data {ticketCount = state.data.ticketCount + 1 }, props {currentStage  = ST.MetroTicketSelection}}
    else continue state

eval DecrementTicket state = do
  if state.data.ticketCount > 1
    then continue state { data {ticketCount = state.data.ticketCount - 1 }, props {currentStage  = ST.MetroTicketSelection}}
    else continue state

eval MetroRouteMapAction state = exit $ GoToMetroRouteMap

eval ToggleTermsAndConditions state = continue state{props{termsAndConditionsSelected = not state.props.termsAndConditionsSelected, currentStage  = ST.MetroTicketSelection}}

eval (ChangeTicketTab ticketType) state = do 
  if state.props.currentStage == ST.ConfirmMetroQuote then do
    let ticketTypeUpdatedState = state {data {ticketType = ticketType}}
        quoteData = getquoteData ticketTypeUpdatedState state.data.quoteResp 
        updatedState = ticketTypeUpdatedState { data {ticketPrice = quoteData.price, quoteId = quoteData.quoteId }}
    updateAndExit updatedState $ Refresh updatedState
  else do
    let ticketCount = if ticketType == ST.ONE_WAY_TRIP then state.data.ticketCount else 1
    continue state { data {ticketType = ticketType, ticketCount = ticketCount }, props {currentStage  = ST.MetroTicketSelection}}

eval (SelectLocation loc ) state = updateAndExit state{props{currentStage  = ST.MetroTicketSelection}} $ SelectSrcDest loc state{props{currentStage  = ST.MetroTicketSelection}}

eval (GetMetroQuotesAction resp) state = do 
  void $ pure $ toggleBtnLoader "" false
  if null resp then do
    void $ pure $ toast $ getString NO_QOUTES_AVAILABLE
    continue state
  else do
    let quoteData = getquoteData state resp
        updatedState = state { data {ticketPrice = quoteData.price, quoteId = quoteData.quoteId, quoteResp = resp }, props { currentStage = ST.ConfirmMetroQuote}}
    updateAndExit updatedState $ Refresh updatedState

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

eval (ShowMetroBookingTimeError withinTimeRange) state = 
  continue state {
    props{
      showMetroBookingTimeError = not withinTimeRange
    }
  }

eval (InfoCardAC (InfoCard.Close)) state = 
  exit $ GoBack state { 
    props { 
      showMetroBookingTimeError = false
    }
  }

eval (GetSDKPollingAC createOrderRes) state = exit $ GotoPaymentPage createOrderRes state.data.bookingId

eval _ state = continue state


getquoteData :: ST.MetroTicketBookingScreenState -> Array MetroQuote -> {"price" :: Int, "quoteId" :: String}--Int
getquoteData state  metroQuote =
  let quote = filter (\(MetroQuote item) -> (getTicketType item._type) == (state.data.ticketType)) metroQuote
      quoteData = quote !! 0
  in
    {
      "price": maybe 0 (\(MetroQuote item) -> item.price) quoteData,
      "quoteId" : maybe "" (\(MetroQuote item) -> item.quoteId) quoteData
    }
  where
    getTicketType :: String -> ST.TicketType
    getTicketType quoteType = case quoteType of 
      "SingleJourney" -> ST.ONE_WAY_TRIP
      "ReturnJourney" -> ST.ROUND_TRIP
      _ -> ST.ONE_WAY_TRIP
