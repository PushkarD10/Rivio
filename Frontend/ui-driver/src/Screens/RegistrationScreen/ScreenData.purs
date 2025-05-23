{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RegistrationScreen.ScreenData where

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Language.Strings (getString)
import Common.Types.Config (CityConfig)
import Prelude (class Eq)
import Screens.Types (RegisterationStep(..), RegistrationScreenState, StageStatus(..))
import ConfigProvider
import Foreign.Object (empty)
import Screens.Types as ST
import Prelude (class Eq, class Show)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Language.Strings (getString)
import Language.Types (STR(..))
import Data.Array as DA
import Common.Types.App as Common

initData :: RegistrationScreenState
initData = {
      data: {
        activeIndex : 1,
        registerationStepsCabs : [],
        registerationStepsAuto : [],
        registerationStepsBike : [],
        drivingLicenseStatus : NOT_STARTED,
        vehicleDetailsStatus : NOT_STARTED,
        permissionsStatus : NOT_STARTED,
        vehicleTypeMismatch : false,
        documentStatusList : [],
        variantList : [],
        phoneNumber : "",
        lastUpdateTime : "",
        cityConfig : dummyCityConfig,
        config : getAppConfig appConfig,
        referralCode : "",
        referral_code_input_data : "",
        logField : empty,
        enteredDL : "",
        enteredRC : "",
        vehicleCategory : Nothing,
        linkedRc : Nothing
      },
      props: {
        limitReachedFor : Nothing,
        logoutModalView : false,
        isValidReferralCode : true,
        enterOtpFocusIndex : 0,
        enterReferralCodeModal : false,
        referralCodeSubmitted : false,
        contactSupportView : true,
        contactSupportModal : ST.HIDE,
        selectedVehicleIndex : Nothing,
        optionalDocsExpanded : true,
        confirmChangeVehicle : false,
        refreshAnimation : false,
        driverEnabled : false,
        menuOptions : false,
        manageVehicle : false,
        manageVehicleCategory : Nothing
      }
  }

dummyCityConfig :: CityConfig
dummyCityConfig = {
                    cityName : "",
                    mapImage : "",
                    cityCode : "",
                    showSubscriptions : false,
                    enableAdvancedBooking : false,
                    advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
                    callDriverInfoPost : false,
                    cityLat : 0.0,
                    cityLong : 0.0,
                    supportNumber : "",
                    languageKey : "",
                    showDriverReferral : false,
                    showCustomerReferral : false,
                    uploadRCandDL : true,
                    enableYatriCoins : false,
                    vehicleNSImg : "",
                    registration : { 
                      callSupport : false,
                      supportWAN : "", 
                      whatsappSupport : false
                    },
                    variantSubscriptionConfig : {
                      enableVariantBasedSubscription : true,
                      variantList : ["AutoCategory"],
                      enableCabsSubscriptionView : false,
                      staticViewPlans : []
                    },
                    showEarningSection: true,
                    referral : {
                        domain : ""
                      , customerAppId : ""
                      , driverAppId : ""
                    },
                    waitingCharges : 1.50,
                    waitingChargesConfig : {
                      cab : {
                        freeSeconds : 300,
                        perMinCharges : 1.0
                      },
                      auto : {
                        freeSeconds : 180,
                        perMinCharges : 1.50
                      },
                      bike : {
                        freeSeconds : 180,
                        perMinCharges : 1.0
                      }
                    },
                    rentalWaitingChargesConfig : {
                      cab : {
                        freeSeconds : 180,
                        perMinCharges : 2.0
                      },
                      auto : {
                        freeSeconds : 180,
                        perMinCharges : 2.0
                      },
                      bike : {
                        freeSeconds : 180,
                        perMinCharges : 2.0
                      }
                    },
                    rateCardConfig : {
                      showLearnMore : false,
                      learnMoreVideoLink : ""
                    },
                    assets :{
                        auto_image :  "ny_ic_black_yellow_auto_side_view",
                        onboarding_auto_image : ""
                      },
                    gstPercentage : "18"
}
