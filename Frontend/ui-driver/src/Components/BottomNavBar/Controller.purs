{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.BottomNavBar.Controller where

import Common.Types.App (LazyCheck(..))
import Data.Maybe as Maybe
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude ((<>))
import Prelude (unit, (<>), (==), negate)
import Screens as ScreenNames
import Screens.Types (BottomNavBarState)
import Storage (getValueToLocalNativeStore, KeyStore(..))

data Action = OnNavigate String

navData :: ScreenNames.ScreenName -> BottomNavBarState
navData screenName = {
   activeIndex : getActiveIndex  screenName,
   navButton: [
    {
      activeIcon: "ny_ic_home_active," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_home_active.png",
      defaultIcon: "ny_ic_home_inactive," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_home_inactive.png",
      text: "Home"
    },
    {
      activeIcon: if (getMerchant FunctionCall == NAMMAYATRI) then "ny_ic_rides_active," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_rides_active.png" else "ny_ic_cab_active,https://assets.juspay.in/beckn/merchantcommon/images/ny_ic_cab_active.png",
      defaultIcon: if (getMerchant FunctionCall == NAMMAYATRI) then "ny_ic_rides_inactive," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_rides_inactive.png" else "ny_ic_cab_inactive,https://assets.juspay.in/beckn/merchantcommon/images/ny_ic_cab_inactive.png",
      text: "Rides"
    }] <> 
    (if (getMerchant FunctionCall == NAMMAYATRI) then [{
      activeIcon: "ic_referral_active," <> (getCommonAssetStoreLink FunctionCall) <> "ic_referral_active.png",
      defaultIcon: if (getValueToLocalNativeStore REFERRAL_ACTIVATED) == "true" then  "ny_ic_contest_alert," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_contest_alert.png" else "ic_referral_inactive," <> (getCommonAssetStoreLink FunctionCall) <> "ic_referral_inactive.png",
      text: "Rankings"
    }] else []) <> 
    [{
      activeIcon: "ny_ic_alerts_active",
      defaultIcon: "ny_ic_alerts_inactive," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_alerts_inactive.png",
      text: "Alert"
    }
    -- , -- TODO::- DEPRECATE THE DESIGN
    -- {
    --   activeIcon: "ic_profile_active," <> (getCommonAssetStoreLink FunctionCall) <> "ic_profile_active.png",
    --   defaultIcon: "ic_profile_inactive," <> (getCommonAssetStoreLink FunctionCall) <> "ic_profile_inactive.png",
    --   text: "Profile"
    -- }
  ]
}

getActiveIndex :: ScreenNames.ScreenName -> Int
getActiveIndex screenName = case screenName of
  ScreenNames.HOME_SCREEN -> 0
  ScreenNames.RIDE_HISTORY_SCREEN -> 1
  ScreenNames.REFERRAL_SCREEN -> if (getMerchant FunctionCall == NAMMAYATRI) then 2 else -1
  ScreenNames.ALERTS_SCREEN -> if (getMerchant FunctionCall == NAMMAYATRI) then 3 else 2
  ScreenNames.DRIVER_PROFILE_SCREEN -> 4
  _ -> -1