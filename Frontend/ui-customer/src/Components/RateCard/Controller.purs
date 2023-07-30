{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RateCard.Controller where
import Screens.Types(RateCardType(..))

data Action = Close | BackPressed | NoAction | GoToDefaultStart | GoToDriverAddition | GoToFareUpdate


type Config = {
  nightCharges :: Boolean,
  nightShiftMultiplier :: String, 
  rateCardArray :: Array RateCardDetails,
  currentRateCardType :: RateCardType,
  nightChargesApplicable :: Boolean,
  onFirstPage :: Boolean,
  vehicleType :: String,
  driverAdditionsImage :: String,
  driverAdditionsLogic :: String,
  title :: String ,
  showDetails :: Boolean,
  alertDialogPrimaryColor :: String
}

type RateCardDetails = {
  title :: String,
  description :: String 
}

config :: Config 
config = {
  nightCharges : false,
  nightShiftMultiplier : "1.5",
  currentRateCardType : DefaultRateCard,
  onFirstPage : false,
  vehicleType : "",
  nightChargesApplicable : true,
  rateCardArray : [],
  driverAdditionsImage : "",
  driverAdditionsLogic : "",
  title : "",
  showDetails : true,
  alertDialogPrimaryColor: "#2194FF"
}
