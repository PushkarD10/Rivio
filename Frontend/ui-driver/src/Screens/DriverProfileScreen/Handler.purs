{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverProfileScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard,(==))
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.DriverProfileScreen.Controller (ScreenOutput(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Screens.DriverProfileScreen.View as DriverProfileScreen
import Types.App (FlowBT, GlobalState(..), DRIVER_PROFILE_SCREEN_OUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)

driverProfileScreen :: FlowBT String DRIVER_PROFILE_SCREEN_OUTPUT
driverProfileScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ DriverProfileScreen.screen state.driverProfileScreen
  case action of
    GoToDriverDetailsScreen updatedState -> do
      modifyScreenState $ DriverDetailsScreenStateType (\driverDetails -> 
        driverDetails { data { driverName = updatedState.data.driverName,
        driverVehicleType = updatedState.data.driverVehicleType,
        driverRating = updatedState.data.driverRating,
        base64Image = updatedState.data.base64Image,
        driverMobile = updatedState.data.driverMobile,
        driverAlternateMobile = updatedState.data.driverAlternateNumber
        },
        props {
          checkAlternateNumber = (if (updatedState.data.driverAlternateNumber == Nothing) then true else false)
        }})
      App.BackT $ App.BackPoint <$> pure DRIVER_DETAILS_SCREEN
    
    GoToVehicleDetailsScreen updatedState -> do
      modifyScreenState $ VehicleDetailsScreenStateType (\vehicleDetails -> 
        vehicleDetails { data { vehicleRegNumber = updatedState.data.vehicleRegNumber,
        vehicleType = updatedState.data.driverVehicleType,
        vehicleModel = updatedState.data.vehicleModelName,
        vehicleColor = updatedState.data.vehicleColor
        }})
      App.BackT $ App.BackPoint <$> pure VEHICLE_DETAILS_SCREEN
    
    GoToAboutUsScreen -> App.BackT $ App.BackPoint <$> pure ABOUT_US_SCREEN
    GoToLogout -> App.BackT $ App.BackPoint <$> pure GO_TO_LOGOUT
    GoToHelpAndSupportScreen -> App.BackT $ App.BackPoint <$> pure HELP_AND_SUPPORT_SCREEN
    GoToHomeScreen -> App.BackT $ App.BackPoint <$> pure GO_TO_HOME_FROM_PROFILE
    GoToReferralScreen -> App.BackT $ App.BackPoint <$> pure GO_TO_REFERRAL_SCREEN_FROM_DRIVER_PROFILE_SCREEN
    GoToDriverHistoryScreen -> App.BackT $ App.BackPoint <$> pure GO_TO_DRIVER_HISTORY_SCREEN
    GoToSelectLanguageScreen -> App.BackT $ App.BackPoint <$> pure SELECT_LANGUAGE_SCREEN
    OnBoardingFlow -> App.BackT $ App.BackPoint <$> pure ON_BOARDING_FLOW
    GoToNotifications -> App.BackT $ App.BackPoint <$> pure NOTIFICATIONS_SCREEN
    GoToBookingOptions state -> App.BackT $ App.BackPoint <$> pure (GO_TO_BOOKING_OPTIONS_SCREEN state)
    GoBack -> App.BackT $ pure App.GoBack