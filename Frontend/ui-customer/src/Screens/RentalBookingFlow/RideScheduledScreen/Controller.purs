{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RentalBookingFlow.RideScheduledScreen.Controller where

import Accessor (_lat, _lon, _selectedQuotes)
import Components.GenericHeader.Controller as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.SourceToDestination.Controller as SourceToDestinationActionController
import Components.SelectListModal.Controller as CancelRidePopUp
import Components.PopUpModal.Controller as PopUpModalController
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Array ((!!), head)
import Log (trackAppActionClick, trackAppEndScreen)
import Prelude
import PrestoDOM (class Loggable, Eval, update, continue, exit, continueWithCmd)
import Screens (getScreen, ScreenName(..))
import Helpers.Utils (performHapticFeedback)
import Screens.Types (RideScheduledScreenState, City(..))
import Resources.Constants (cancelReasons, dummyCancelReason)
import JBridge (hideKeyboardOnNavigation, toast)
import Services.API
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.SearchLocationScreen.ScreenData as SearchLocationScreenData
import Data.Lens ((^.))
import Resources.Constants (getAddressFromBooking, decodeAddress, DecodeAddress(..))
import Screens.HomeScreen.Transformer (getFareProductType)
import Screens.Types (FareProductType(..)) as FPT
import Language.Strings (getString)
import Language.Types as STR
import Debug (spy)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    PrimaryButtonActionController _-> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "primary_button" "on_click"
    SourceToDestinationAC _ -> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "source_to_destination" "dummy_action"
    CancelRide -> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "cancel_ride" "on_click"
    AddFirstStop -> do 
      trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "add_first_stop" "on_click"
      trackAppEndScreen appId (getScreen RIDE_SCHEDULED_SCREEN)
    _ -> pure unit

data Action
  = NoAction
  | PrimaryButtonActionController PrimaryButtonController.Action
  | SourceToDestinationAC SourceToDestinationActionController.Action
  | CancelRide
  | AddFirstStop
  | GenericHeaderAC GenericHeaderController.Action
  | GetBookingList RideBookingListRes
  | CheckFlowStatusAction
  | GoBack
  | CancelRideActionController PopUpModalController.Action
  | NotificationListener String

data ScreenOutput = GoToHomeScreen RideScheduledScreenState
                  | GoToSearchLocationScreen RideScheduledScreenState
                  | CancelRentalRide RideScheduledScreenState
                  | GoToMyRidesScreen RideScheduledScreenState
                  | NotificationListenerSO String

eval :: Action -> RideScheduledScreenState -> Eval Action ScreenOutput RideScheduledScreenState

eval GoBack state = do
  if state.data.fromScreen == getScreen MY_RIDES_SCREEN then exit $ GoToMyRidesScreen state
    else exit $ GoToHomeScreen state

eval (PrimaryButtonActionController (PrimaryButtonController.OnClick)) state = exit $ GoToHomeScreen state

eval AddFirstStop state = exit $ GoToSearchLocationScreen state

eval (SourceToDestinationAC (SourceToDestinationActionController.DestinationClicked)) state = exit $ GoToSearchLocationScreen state

eval (GenericHeaderAC (GenericHeaderController.PrefixImgOnClick)) state = continueWithCmd state [pure GoBack]

eval CancelRide state = continue state{ props{isCancelRide = true}, data{cancellationReasons = cancelReasons false}}

eval (CancelRideActionController (PopUpModalController.OnButton1Click)) state = exit $ CancelRentalRide state

eval (CancelRideActionController (PopUpModalController.OnButton2Click)) state = continue state{props{isCancelRide = false}}
-- eval (CancelRidePopUpAction (CancelRidePopUp.Button2 PrimaryButtonController.OnClick)) state = do
--   void $ pure $ performHapticFeedback unit
--   let newState = state{props{isCancelRide = false}}
--   case state.props.cancelRideActiveIndex of
--     Just index -> if ( (fromMaybe dummyCancelReason (state.data.cancellationReasons !! index)).reasonCode == "OTHER" || (fromMaybe dummyCancelReason (state.data.cancellationReasons !! index)).reasonCode == "TECHNICAL_GLITCH" ) then exit $ CancelRentalRide newState{props{cancelDescription = if (newState.props.cancelDescription == "") then (fromMaybe dummyCancelReason (state.data.cancellationReasons !!index)).description else newState.props.cancelDescription }}
--                     else exit $ CancelRentalRide newState{props{cancelDescription = (fromMaybe dummyCancelReason (state.data.cancellationReasons !!index)).description , cancelReasonCode = (fromMaybe dummyCancelReason (state.data.cancellationReasons !! index)).reasonCode }}
--     Nothing    -> continue state

eval (GetBookingList resp) state =
  let (RideBookingListRes listResp) = resp
      (RideBookingRes resp) = fromMaybe HomeScreenData.dummyRideBooking $ head listResp.list
      (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
      (RideBookingDetails contents) = bookingDetails.contents
      fareProductType = getFareProductType (bookingDetails.fareProductType)
  in continue state 
    { data
      { source = SearchLocationScreenData.dummyLocationInfo { lat = Just (resp.fromLocation ^._lat) , lon = Just (resp.fromLocation ^._lon), placeId = Nothing, city = AnyCity, addressComponents = getAddressFromBooking resp.fromLocation, address = decodeAddress (Booking resp.fromLocation)}
      , destination = maybe (Nothing) (\toLocation -> Just $ SearchLocationScreenData.dummyLocationInfo {lat = Just (toLocation^._lat), lon = Just (toLocation^._lon), placeId = Nothing, city = AnyCity, addressComponents = getAddressFromBooking toLocation, address = decodeAddress (Booking toLocation)}) $ if fareProductType == FPT.INTER_CITY then contents.toLocation else contents.stopLocation
      , startTime = fromMaybe "" resp.rideScheduledTime
      , finalPrice = show resp.estimatedTotalFare
      , baseDuration = show $ (fromMaybe 7200 resp.estimatedDuration)/3600
      , baseDistance = show $ (fromMaybe 20000 resp.estimatedDistance)/1000
      , bookingId = resp.id
      , fareProductType = fareProductType
      }
    , props
      { driverAllocationTime = "15" -- TODO-codex : Need to get the driver allocation time from the API 
      }
    }

eval CheckFlowStatusAction state = update state

eval (NotificationListener notificationType) state = exit $ NotificationListenerSO notificationType
eval _ state = update state
