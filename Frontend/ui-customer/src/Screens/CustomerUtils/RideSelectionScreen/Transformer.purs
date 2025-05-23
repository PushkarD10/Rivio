{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideSelectionScreen.Transformer where

import Accessor (_computedPrice, _contents, _driverName, _estimatedDistance, _id, _otpCode, _rideRating, _toLocation, _vehicleNumber, _vehicleVariant, _stopLocation)
import Common.Types.App (LazyCheck(..))
import Common.Types.App as CTP
import Data.Array (filter, null, (!!), any, elem)
import Data.Lens ((^.))
import Data.Maybe 
import Data.String (Pattern(..), split)
import Engineering.Helpers.Commons (convertUTCtoISC, os)
import Helpers.Utils (FetchImageFrom(..), fetchImage, isHaveFare, withinTimeRange, getCityFromString, getVehicleVariantImage, getCityConfig)
import Language.Types (STR(..))
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Prelude (map, show, ($), (&&), (+), (-), (/=), (<>), (==), (||))
import PrestoDOM.Types.Core (toPropValue)
import Resources.Constants (DecodeAddress(..), decodeAddress, getFaresList, getFareFromArray, getKmMeter, fetchVehicleVariant)
import Resources.Localizable.EN (getEN)
import Language.Strings (getString, getVarString)
import Screens.Types (Fares, IndividualRideCardState, ItemState, Stage(..), ZoneType(..), City(..), VehicleViewType(..))
import Storage (isLocalStageOn, getValueToLocalStore, KeyStore(..))
import Data.Ord (abs)
import ConfigProvider
import Screens.RideSelectionScreen.ScreenData 
import JBridge (differenceBetweenTwoUTCInMinutes)
import Data.Function.Uncurried (runFn2)
import Helpers.SpecialZoneAndHotSpots (getSpecialTag)
import Engineering.Helpers.Utils (getFixedTwoDecimals)
import Screens.HomeScreen.Transformer (dummyRideAPIEntity, getFareProductType)
import Services.API (FareBreakupAPIEntity, RideAPIEntity(..), RideBookingRes(..), RideBookingAPIDetails(..))
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails)
import Screens.Types (FareProductType(..)) as FPT
import Language.Strings (getVarString)
import MerchantConfig.Types (AppConfig(..))

myRideListTransformerProp :: Array RideBookingRes  -> Array ItemState
myRideListTransformerProp listRes =  filter (\item -> elem item.status $ map toPropValue ["COMPLETED", "CANCELLED", "CONFIRMED"]) (map (\(RideBookingRes ride) -> 

  let
    rideApiEntity = fromMaybe dummyRideAPIEntity (ride.rideList !!0)
    imageInfo = case fetchVehicleVariant (rideApiEntity^._vehicleVariant) of
                    Just variant -> split (Pattern ",") (getVehicleVariantImage (show variant) RIGHT_VIEW)
                    Nothing -> ["",""]
    imageName = fromMaybe "" $ imageInfo !!0
    imageUrl = fromMaybe "" $ imageInfo !!1
    (RideBookingAPIDetails rideApiDetails) = ride.bookingDetails
    destination = fromMaybe dummyBookingDetails $ if ride.status == "CONFIRMED" then (ride.bookingDetails ^._contents^._stopLocation) else (ride.bookingDetails ^._contents^._toLocation)
  in {
    date : toPropValue (( (fromMaybe "" ((split (Pattern ",") (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "llll")) !!0 )) <> ", " <>  (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "Do MMM") )),
    time : toPropValue (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "h:mm A"),
    source : toPropValue $ decodeAddress $ Booking ride.fromLocation,
    destination : toPropValue $ decodeAddress $ Booking $ destination  , --ride.bookingDetails ^._contents^._toLocation,
    totalAmount : toPropValue $ (getCurrency appConfig) <> " " <> (show $ fromMaybe 0 $ (fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^. _computedPrice),
    cardVisibility : toPropValue "visible",
    shimmerVisibility : toPropValue "gone",
    driverImage : toPropValue $ fetchImage FF_ASSET "ny_ic_user",
    isCancelled : toPropValue if ride.status == "CANCELLED" then "visible" else "gone",
    isSuccessfull : toPropValue if ride.status == "COMPLETED" then "visible" else "gone",
    isScheduled : toPropValue if ride.status == "CONFIRMED" then "visible" else "gone",
    rating : toPropValue $ fromMaybe 0 $ rideApiEntity^. _rideRating,
    driverName : toPropValue $ rideApiEntity^. _driverName,
    rideStartTime : toPropValue $ convertUTCtoISC (fromMaybe "" ride.rideStartTime) "h:mm A",
    rideEndTime : toPropValue $ convertUTCtoISC (fromMaybe "" ride.rideEndTime) "h:mm A",
    vehicleNumber : toPropValue $ rideApiEntity^._vehicleNumber,
    rideId : toPropValue $ rideApiEntity^._id,
    status : toPropValue ride.status,
    rideEndTimeUTC : toPropValue $ fromMaybe ride.createdAt ride.rideEndTime,
    alpha : toPropValue if isLocalStageOn HomeScreen then "1.0" else "0.5",
    zoneVisibility : toPropValue if (getSpecialTag ride.specialLocationTag).priorityTag == METRO then "visible" else "gone",
    variantImage : toPropValue $ if os == "IOS" then "url->" <> imageUrl <> "," <> imageName else  "url->" <> imageUrl,
    showVariantImage : toPropValue $ if ride.status == "CONFIRMED" then "gone" else "visible",
    showRepeatRide : toPropValue if (getFareProductType $ rideApiDetails.fareProductType) == FPT.RENTAL then "gone" else "visible",
    showDestination : toPropValue if (decodeAddress $ Booking destination) == "" then "gone" else "visible"
  })
  
   listRes)


myRideListTransformer :: Boolean -> Array RideBookingRes -> AppConfig -> Array IndividualRideCardState
myRideListTransformer isSrcServiceable listRes config = filter (\item -> any (_ == item.status) ["COMPLETED", "CANCELLED", "CONFIRMED"]) (map (\(RideBookingRes ride) ->
  let
    (RideBookingAPIDetails rideApiDetails) = ride.bookingDetails
    fares = getFares ride.fareBreakup
    (RideAPIEntity rideDetails) = (fromMaybe dummyRideAPIEntity (ride.rideList !!0))
    baseDistanceVal = (getKmMeter (fromMaybe 0 (rideDetails.chargeableRideDistance)))
    timeVal = (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "HH:mm:ss")
    nightChargesVal = (withinTimeRange "22:00:00" "5:00:00" timeVal)
    updatedFareList = getFaresList ride.fareBreakup baseDistanceVal
    specialTags = getSpecialTag ride.specialLocationTag
    cityStr = getValueToLocalStore CUSTOMER_LOCATION
    city = getCityFromString cityStr    
    cityConfig = getCityConfig config.cityConfig cityStr
    rideType = getFareProductType rideApiDetails.fareProductType
    autoWaitingCharges = if rideType == FPT.RENTAL then cityConfig.rentalWaitingChargeConfig.auto else cityConfig.waitingChargeConfig.auto 
    cabsWaitingCharges = if rideType == FPT.RENTAL then cityConfig.rentalWaitingChargeConfig.cabs else cityConfig.waitingChargeConfig.cabs
    waitingCharges = 
      if rideDetails.vehicleVariant == "AUTO_RICKSHAW" then
          autoWaitingCharges
      else 
          cabsWaitingCharges
    nightChargeFrom = if city == Delhi then "11 PM" else "10 PM"
    nightChargeTill = "5 AM"
    referenceString' = (if nightChargesVal && (getMerchant CTP.FunctionCall) /= YATRI then "1.5" <> (getEN $ DAYTIME_CHARGES_APPLICABLE_AT_NIGHT nightChargeFrom nightChargeTill) else "")
                        <> (if isHaveFare "DRIVER_SELECTED_FARE" updatedFareList then "\n\n" <> getEN DRIVERS_CAN_CHARGE_AN_ADDITIONAL_FARE_UPTO else "")
                        <> (if isHaveFare "WAITING_CHARGES" updatedFareList || isHaveFare "WAITING_OR_PICKUP_CHARGES" updatedFareList then "\n\n" <> if cityConfig.enableWaitingConfig  then ( getVarString WAITING_CHARGE_DESCRIPTION [show waitingCharges.freeMinutes, show waitingCharges.perMinCharges]) else (getString ADDITIONAL_CHARGES_WILL_BE_APPLICABLE) else "")
                        <> (if isHaveFare "EARLY_END_RIDE_PENALTY" updatedFareList then "\n\n" <> getEN EARLY_END_RIDE_CHARGES_DESCRIPTION else "")
                        <> (if isHaveFare "CUSTOMER_SELECTED_FARE" updatedFareList then "\n\n" <> getEN CUSTOMER_TIP_DESCRIPTION else "")
                        <> (if isHaveFare "TOLL_CHARGES" updatedFareList then "\n\n" <> "⁺" <> getEN TOLL_CHARGES_DESC else "")
    startTime = fromMaybe "" ride.rideStartTime
    endTime = fromMaybe "" ride.rideEndTime
    isScheduled = ride.status == "CONFIRMED"
    destination = fromMaybe dummyBookingDetails $ if isScheduled then (ride.bookingDetails ^._contents^._stopLocation) else (ride.bookingDetails ^._contents^._toLocation)
  in {
    date : (fromMaybe "" ((split (Pattern ",") (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "llll")) !!0 )) <> ", " <>  (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "Do MMM") ,
    time :  convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "h:mm A",
    source :  decodeAddress $ Booking ride.fromLocation,
    destination : decodeAddress $ Booking destination,
    totalAmount :  ((getCurrency appConfig)) <> " " <> (show (fromMaybe 0 rideDetails.computedPrice)),
    cardVisibility :  "visible",
    shimmerVisibility :  "gone",
    driverImage :  fetchImage FF_ASSET "ny_ic_user",
    isCancelled :  if ride.status == "CANCELLED" then "visible" else "gone",
    isSuccessfull :  if ride.status == "COMPLETED" then "visible" else "gone",
    isScheduled : if isScheduled then "visible" else "gone",
    rating : fromMaybe 0 rideDetails.rideRating,
    driverName : rideDetails.driverName,
    rideStartTime : convertUTCtoISC (fromMaybe ride.createdAt $ if isScheduled then ride.rideScheduledTime else ride.rideStartTime) "h:mm A",
    rideEndTime : convertUTCtoISC endTime "h:mm A",
    vehicleNumber : rideDetails.vehicleNumber,
    rideId : rideDetails.id,
    status : ride.status,
    shortRideId : rideDetails.shortRideId,
    bookingId : ride.id,
    rideEndTimeUTC : fromMaybe "" ride.rideEndTime,
    sourceLocation : ride.fromLocation,
    destinationLocation : destination,
    alpha : if isLocalStageOn HomeScreen then "1.0" else "0.5"
  , fareBreakUpList : fares
  , faresList : updatedFareList
  , baseFare : fares.baseFare
  , pickupCharges : fares.pickupCharges
  , extraFare : (getCurrency appConfig) <> " " <> (show $ getFareFromArray ride.fareBreakup "EXTRA_DISTANCE_FARE")
  , waitingCharges : fares.waitingCharges
  , baseDistance : baseDistanceVal
  , extraDistance : getKmMeter $  abs $ (fromMaybe 0 rideDetails.chargeableRideDistance) - (fromMaybe 0 (((ride.bookingDetails)^._contents)^._estimatedDistance))
  , referenceString : referenceString'
  , nightCharges : nightChargesVal
  , isSpecialZone : null ride.rideList || isJust (ride.bookingDetails ^._contents^._otpCode)
  , zoneType : specialTags.priorityTag
  , optionsVisibility : false
  , isSrcServiceable
  , vehicleVariant : fetchVehicleVariant rideDetails.vehicleVariant
  , merchantExoPhone : ride.merchantExoPhone
  , serviceTierName : ride.serviceTierName
  , totalTime : show (runFn2 differenceBetweenTwoUTCInMinutes endTime startTime) <> " min"
  , vehicleModel : rideDetails.vehicleModel
  , rideStartTimeUTC : fromMaybe "" ride.rideStartTime
  , providerName : ride.agencyName
  , providerType : maybe CTP.ONUS (\valueAdd -> if valueAdd then CTP.ONUS else CTP.OFFUS) ride.isValueAddNP -- get from API
  , showRepeatRide : if getFareProductType rideApiDetails.fareProductType == FPT.RENTAL then "gone" else "visible"
  , rideType : getFareProductType rideApiDetails.fareProductType
  , estimatedDistance : fromMaybe 0 ride.estimatedDistance
  , estimatedDuration : fromMaybe 0 ride.estimatedDuration
  , estimatedFare : ride.estimatedFare
  , showDestination : if (decodeAddress $ Booking destination) == "" then "gone" else "visible"
  , rideScheduledTime : fromMaybe "" ride.rideScheduledTime
  }) listRes)

matchRidebyId :: IndividualRideCardState -> IndividualRideCardState -> Boolean
matchRidebyId rideOne rideTwo = rideOne.bookingId == rideTwo.bookingId

getFares ∷ Array FareBreakupAPIEntity → Fares
getFares fares = {
  baseFare : (getCurrency appConfig) <>  " " <> (getFixedTwoDecimals $ ((getFareFromArray fares "BASE_FARE") + (getFareFromArray fares "EXTRA_DISTANCE_FARE")) - 10.0)
, pickupCharges : (getCurrency appConfig) <> " 10.0"
, waitingCharges : (getCurrency appConfig) <> " " <> (getFixedTwoDecimals $ getFareFromArray fares "WAITING_CHARGES")
, nominalFare : (getCurrency appConfig) <> " " <> (getFixedTwoDecimals $ getFareFromArray fares "DRIVER_SELECTED_FARE")
}