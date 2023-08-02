{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverProfileScreen.ScreenData where

import Data.Maybe

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Language.Types (STR(..)) as STR
import Screens.Types (DriverProfileScreenState, BottomNavBarState, DriverProfileScreenType(..))
import Prelude (class Eq, unit, (<>), (==), (||), (/=))
import Common.Types.App (CheckBoxOptions, LazyCheck(..))
import Foreign.Object (empty)

initData :: DriverProfileScreenState
initData = {
  data:  {
    driverName : "",
    driverVehicleType : "",
    driverRating : Just 2,
    base64Image : "",
    drivingLicenseNo : "",
    driverMobile : Just "",
    vehicleRegNumber : "",
    vehicleModelName : "",
    vehicleColor : "",
    driverAlternateNumber : Nothing,
    driverGender : Nothing,
    logField : empty ,

    capacity : 0,
    vehicleSelected: [],
    downgradeOptions : [],
    genderTypeSelect : Nothing,
    alterNumberEditableText : false,
    driverEditAlternateMobile : Nothing,
    otpLimit : 5,
    otpBackAlternateNumber : Nothing,
    languageList : languagesChoices,
    gender : Nothing,
    rcNumber : "",
    isRCActive : false,
    rcDataArray : [],
    inactiveRCArray : [],
    activeRCData : { rcStatus  : true
                  , rcDetails : { certificateNumber   : ""
                                , vehicleColor : ""
                                , vehicleModel : ""
                                }
                  },
    openInactiveRCViewOrNotArray : [],
    vehicleAge : 0,
    vehicleName : "",
    languagesSpoken : [],
    analyticsData : {
        totalEarnings : ""
      , bonusEarned : ""
      , totalCompletedTrips : 0
      , totalUsersRated : 0
      , rating : Just 0.0
      , chipRailData : []
      , badges : []
      , missedEarnings : 0
      , ridesCancelled : 0
      , cancellationRate : 0
      , totalRidesAssigned : 0
      , totalDistanceTravelled : ""
      }
    },
  props: {
    logoutModalView: false,
    showLiveDashboard : false,
    screenType : DRIVER_DETAILS,
    openSettings : false,
    updateDetails : false,
    showGenderView : false,
    alternateNumberView : false,
    removeAlternateNumber : false,
    enterOtpModal : false,
    checkAlternateNumber : true,
    otpAttemptsExceeded: false,
    enterOtpFocusIndex : 0,
    otpIncorrect : false,
    alternateMobileOtp : "",
    isEditAlternateMobile : false,
    numberExistError : false,
    mNumberEdtFocused : false,
    updateLanguages : false,
    activateRcView : false,
    activateOrDeactivateRcView : false,
    activeRcIndex : 0,
    deleteRcView : false,
    alreadyActive : false,
    callDriver : false,
    openRcView : false,
    detailsUpdationType : Nothing,
    btnActive : false
   }
}


data MenuOptions = DRIVER_PRESONAL_DETAILS |DRIVER_BANK_DETAILS | DRIVER_VEHICLE_DETAILS | ABOUT_APP | MULTI_LANGUAGE | HELP_AND_FAQS | DRIVER_LOGOUT | DRIVER_BOOKING_OPTIONS | REFER | APP_INFO_SETTINGS | LIVE_STATS_DASHBOARD
derive instance genericMenuoptions :: Generic MenuOptions _
instance eqMenuoptions :: Eq MenuOptions where eq = genericEq

type Listtype =
    { icon :: String,
      menuOptions :: MenuOptions
    }


languagesChoices :: Array CheckBoxOptions
languagesChoices =
  [ { value : "EN_US"
    , text : "English"
    , subText : "ಆಂಗ್ಲ"
    , isSelected : false
    }
  , { value: "KN_IN"
    , text: "ಕನ್ನಡ"
    , subText : "Kannada"
    , isSelected : false
    }
  , { value: "HI_IN"
    , text: "हिंदी"
    , subText : "Hindi"
    , isSelected : false
    }
  , { value: "TA_IN"
    , text: "தமிழ்"
    , subText : "Tamil"
    , isSelected : false
    }
  , { value: "TE_IN"
    , text: "తెలుగు"
    , subText : "Telugu"
    , isSelected : false
    }
  ]
