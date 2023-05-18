{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HelpAndSupportScreen.ComponentConfig where

import Common.Types.App
import Components.SourceToDestination as SourceToDestination
import Font.Size as FontSize
import Font.Style as FontStyle
import PrestoDOM
import Screens.Types as ST
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))

sourceToDestinationConfig :: ST.HelpAndSupportScreenState -> SourceToDestination.Config
sourceToDestinationConfig state =
 SourceToDestination.config
    {
      margin = (Margin 0 13 0 0)
    , sourceMargin = (Margin 0 0 0 14)
    , sourceImageConfig {
        imageUrl = "ny_ic_green_circle," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_green_circle.png"
      , margin = (MarginTop 3)
      }
    , sourceTextConfig {
        text = state.data.source
      , padding = (Padding 2 0 2 2)
      , margin = (Margin 7 0 15 0)
      , ellipsize = true
      , maxLines = 1
      , textStyle = FontStyle.Body1
      }
    , destinationImageConfig {
        imageUrl = "ny_ic_red_circle," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_red_circle.png"
      , margin = (MarginTop 3)
      }
    , destinationTextConfig {
        text = state.data.destination
      , padding = (Padding 2 0 2 2)
      , margin = (Margin 7 0 15 0)
      , maxLines = 1
      , ellipsize = true
      }
    }