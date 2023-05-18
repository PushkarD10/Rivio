{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HelpAndSupportScreen.Controller where

import Prelude (class Show, pure, unit, ($), discard, bind)
import PrestoDOM (Eval, continue, exit)
import Screens.Types (HelpAndSupportScreenState)
import PrestoDOM.Types.Core (class Loggable)
import Components.SourceToDestination as SourceToDestinationController
import Screens.HelpAndSupportScreen.ScreenData(ListOptions(..), Listtype)
import Language.Strings (getString)
import Services.APITypes (GetRidesHistoryResp)
import Language.Types(STR(..))
import Services.Config (getSupportNumber)
import JBridge (showDialer)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen HELP_AND_SUPPORT_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen HELP_AND_SUPPORT_SCREEN)
      trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
    SourceToDestinationAction (SourceToDestinationController.Dummy) -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "source_to_destination" "dummy"
    OptionClick optionIndex -> do
      trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "view_options"
      trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
    ViewAllRides -> do
      trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "view_rides"
      trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
    ReportIssue -> do
      trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "report_issue"
      trackAppEndScreen appId (getScreen HELP_AND_SUPPORT_SCREEN)
    RideHistoryAPIResponse resp -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "ride_history_api_resp"
    NoRidesAction -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "no_action_view_rides"
    NoAction -> trackAppScreenEvent appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "no_action"


data ScreenOutput = GoBack 
                  | GoToWriteToUsScreen 
                  | GoToTripDetailsScreen HelpAndSupportScreenState
                  | GoToMyRidesScreen

data Action = NoAction
             | BackPressed 
             | SourceToDestinationAction SourceToDestinationController.Action 
             | OptionClick ListOptions 
             | ReportIssue 
             | ViewAllRides 
             | RideHistoryAPIResponse GetRidesHistoryResp
             | AfterRender
             | NoRidesAction


eval :: Action -> HelpAndSupportScreenState -> Eval Action ScreenOutput HelpAndSupportScreenState
eval AfterRender state = continue state
eval BackPressed state = exit GoBack
eval (SourceToDestinationAction (SourceToDestinationController.Dummy)) state = continue state
eval ReportIssue state = exit $ GoToTripDetailsScreen state
eval ViewAllRides state = exit $ GoToMyRidesScreen

eval (OptionClick optionIndex) state = do
  case optionIndex of
    GettingStartedFaq -> continue state
    OtherIssues -> exit $ GoToWriteToUsScreen
    CallSupportCenter -> do
      _ <- pure $ showDialer (getSupportNumber "")
      continue state
eval _ state = continue state

getTitle :: ListOptions -> String
getTitle menuOption = 
  case menuOption of
    GettingStartedFaq -> (getString GETTING_STARTED_AND_FAQ)
    OtherIssues -> (getString FOR_OTHER_ISSUES_WRITE_TO_US)
    CallSupportCenter -> (getString CALL_SUPPORT_CENTER)

optionList :: Array Listtype
optionList =
    [
      {menuOptions: GettingStartedFaq , icon:"ny_ic_help_circle_transparent," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_help_circle_transparent.png"},
      {menuOptions: OtherIssues , icon:"ny_ic_clip_board," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_clip_board.png"},
      {menuOptions: CallSupportCenter , icon:"ny_ic_head_phones," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_head_phones.png"}
    ]