{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DriverProfileScreen.Controller where

import Components.BottomNavBar.Controller as BottomNavBar
import Components.PopUpModal.Controller as PopUpModal
import Data.Maybe (fromMaybe)
import Helpers.Utils (launchAppSettings)
import JBridge (firebaseLogEvent, goBackPrevWebPage)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress)
import Prelude (class Show, pure, unit, ($), discard, bind)
import PrestoDOM (Eval, continue, exit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Screens.DriverProfileScreen.ScreenData (MenuOptions(..))
import Screens.Types (DriverProfileScreenState,VehicleP)
import Services.APITypes (GetDriverInfoResp(..), Vehicle(..))
import Services.Backend (dummyVehicleObject)
import Storage (setValueToLocalNativeStore, KeyStore(..))
import Engineering.Helpers.Commons (getNewIDWithTag)

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of 
    AfterRender -> trackAppScreenRender appId "screen" (getScreen DRIVER_PROFILE_SCREEN)
    BackPressed flag -> do
      trackAppBackPress appId (getScreen DRIVER_PROFILE_SCREEN)
      if flag then trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "in_screen" "backpress_in_logout_modal"
        else trackAppEndScreen appId (getScreen DRIVER_PROFILE_SCREEN)
    OptionClick optionIndex -> do
      trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "in_screen" "profile_options_click"
      trackAppEndScreen appId (getScreen DRIVER_PROFILE_SCREEN)
    BottomNavBarAction (BottomNavBar.OnNavigate item) -> do
      trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "bottom_nav_bar" "on_navigate"
      trackAppEndScreen appId (getScreen DRIVER_PROFILE_SCREEN)
    PopUpModalAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_logout" "on_goback"
      PopUpModal.OnButton2Click -> do
        trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_logout" "logout"
        trackAppEndScreen appId (getScreen DRIVER_PROFILE_SCREEN)
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_logout" "no_action"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_logout" "primary_edit_text_changed"
      PopUpModal.CountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_logout" "countdown_updated"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_logout" "image_onclick"
      PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_action" "tip_clicked"
      PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_action" "popup_dismissed"
    GetDriverInfoResponse resp -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "in_screen" "get_driver_info_response"
    HideLiveDashboard val -> trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "in_screen" "hide_live_stats_dashboard"
    NoAction -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "in_screen" "no_action"

data ScreenOutput = GoToDriverDetailsScreen DriverProfileScreenState
                    | GoToVehicleDetailsScreen DriverProfileScreenState
                    | GoToBookingOptions DriverProfileScreenState
                    | GoToSelectLanguageScreen
                    | GoToHelpAndSupportScreen
                    | GoToDriverHistoryScreen
                    | GoToNotifications
                    | GoToAboutUsScreen
                    | OnBoardingFlow
                    | GoToHomeScreen
                    | GoToReferralScreen
                    | GoToLogout
                    | GoBack

data Action = BackPressed Boolean
            | NoAction 
            | OptionClick MenuOptions 
            | BottomNavBarAction BottomNavBar.Action 
            | GetDriverInfoResponse GetDriverInfoResp
            | PopUpModalAction PopUpModal.Action
            | AfterRender
            | HideLiveDashboard String

eval :: Action -> DriverProfileScreenState -> Eval Action ScreenOutput DriverProfileScreenState

eval AfterRender state = continue state

eval (BackPressed flag) state = if state.props.logoutModalView then continue $ state { props{ logoutModalView = false}}
                                else if state.props.showLiveDashboard then do
                                continueWithCmd state [do
                                  _ <- pure $ goBackPrevWebPage (getNewIDWithTag "webview")
                                  pure NoAction
                                ]
                                else exit GoBack

eval (BottomNavBarAction (BottomNavBar.OnNavigate screen)) state = do 
  case screen of
    "Home" -> exit $ GoToHomeScreen
    "Rides" -> exit $ GoToDriverHistoryScreen
    "Alert" -> do
      _ <- pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
      _ <- pure $ firebaseLogEvent "ny_driver_alert_click"
      exit $ GoToNotifications
    "Contest" -> do
      _ <- pure $ setValueToLocalNativeStore REFERRAL_ACTIVATED "false"
      exit $ GoToReferralScreen
    _ -> continue state

eval (OptionClick optionIndex) state = do
  case optionIndex of
    DRIVER_PRESONAL_DETAILS -> exit $ GoToDriverDetailsScreen state
    DRIVER_VEHICLE_DETAILS -> exit $ GoToVehicleDetailsScreen state
    DRIVER_BOOKING_OPTIONS -> exit $ GoToBookingOptions state
    MULTI_LANGUAGE -> exit $ GoToSelectLanguageScreen
    HELP_AND_FAQS -> exit $ GoToHelpAndSupportScreen
    ABOUT_APP -> exit $ GoToAboutUsScreen
    DRIVER_LOGOUT -> continue $ (state {props = state.props {logoutModalView = true}})
    REFER -> exit $ OnBoardingFlow 
    APP_INFO_SETTINGS -> do
      _ <- pure $ launchAppSettings unit
      continue state
    LIVE_STATS_DASHBOARD -> continue state {props {showLiveDashboard = true}}

eval (HideLiveDashboard val) state = continue state {props {showLiveDashboard = false}}

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue $ (state {props {logoutModalView = false}}) 

eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = exit $ GoToLogout

eval (GetDriverInfoResponse (GetDriverInfoResp driverProfileResp)) state = do
  let (Vehicle linkedVehicle) = (fromMaybe dummyVehicleObject driverProfileResp.linkedVehicle)
  continue (state {data = state.data {driverName = driverProfileResp.firstName,
                                      driverVehicleType = linkedVehicle.variant,
                                      driverRating = driverProfileResp.rating,
                                      driverMobile = driverProfileResp.mobileNumber,
                                      vehicleRegNumber = linkedVehicle.registrationNo,
                                      drivingLicenseNo = "",
                                      vehicleModelName = linkedVehicle.model,
                                      vehicleColor = linkedVehicle.color,
                                      vehicleSelected = getDowngradeOptionsSelected  (GetDriverInfoResp driverProfileResp)
                                      }})

eval _ state = continue state

getTitle :: MenuOptions -> String
getTitle menuOption = 
  case menuOption of
    DRIVER_PRESONAL_DETAILS -> getString PERSONAL_DETAILS
    DRIVER_VEHICLE_DETAILS -> getString VEHICLE_DETAILS
    DRIVER_BOOKING_OPTIONS -> getString BOOKING_OPTIONS
    MULTI_LANGUAGE -> getString LANGUAGES
    HELP_AND_FAQS -> getString HELP_AND_FAQ
    ABOUT_APP -> getString ABOUT
    REFER -> getString ADD_YOUR_FRIEND
    DRIVER_LOGOUT -> getString LOGOUT
    APP_INFO_SETTINGS -> getString APP_INFO
    LIVE_STATS_DASHBOARD -> getString LIVE_DASHBOARD


getDowngradeOptionsSelected :: GetDriverInfoResp -> Array VehicleP
getDowngradeOptionsSelected (GetDriverInfoResp driverInfoResponse) =
  [
    {vehicleName: "HATCHBACK", isSelected: driverInfoResponse.canDowngradeToHatchback}
  , {vehicleName: "SEDAN", isSelected: driverInfoResponse.canDowngradeToSedan}
  ]
