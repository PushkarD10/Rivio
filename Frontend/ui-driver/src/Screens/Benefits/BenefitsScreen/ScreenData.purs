{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Screens.Benefits.BenefitsScreen.ScreenData where

import Data.Maybe
import Screens.Types (BenefitsScreenState, DriverReferralType(..), ReferralInfoPopType(..))
import Services.API (ModuleCompletionCriteria(..), LmsModuleRes(..), LmsCategory(..), ModuleCompletionStatus(..))
import Foreign.Object (empty)
import ConfigProvider
import Prelude

initData :: BenefitsScreenState
initData = {
    data : { logField : empty 
         , config : getAppConfig appConfig
         , totalReferredDrivers : 0
         , totalActivatedCustomers : 0
         , totalReferredCustomers : 0
         , referralCode : ""
         , rank : Nothing
         , totalEligibleDrivers : Nothing
         , moduleList : {
            completed : [],
            remaining : []
         }
         , bannerData : { bannerItem: Nothing
                        , currentBanner: 0
                        , bannerScrollState: "0"
                        , currentPage: 0
                        }
        },
    props : {
      showDriverReferralQRCode : false,
      showNewDriverReferralText : true,
      driverReferralType : CUSTOMER,
      referralInfoPopType : NO_REFERRAL_POPUP,
      selectedModule : Nothing,
      showShimmer : true,
      isPayoutEnabled: Nothing,
      bannerLength : 0
    }
}