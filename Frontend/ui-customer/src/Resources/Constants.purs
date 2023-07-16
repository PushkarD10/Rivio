{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Resources.Constants where

import Data.Array (filter, length, null, reverse, (!!), head, all)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), Replacement(..), contains, joinWith, replaceAll, split, trim)
import Prelude (map, show, (&&), (-), (<>), (==), (>), ($), (+), (/=), (<), (/), (*))
import Screens.Types as ST 
import Data.Lens ((^.))
import Services.API (AddressComponents(..), BookingLocationAPIEntity(..), SavedReqLocationAPIEntity(..), FareBreakupAPIEntity(..))
import Language.Strings (getString, getEN)
import Language.Types (STR(..))
import Accessor (_description, _amount)
import Helpers.Utils (toString, parseFloat)
import Data.Int (toNumber)
import MerchantConfig.Utils(getValueFromConfig)

type Language
  = { name :: String
    , value :: String
    }

getDelayForAutoComplete :: Int
getDelayForAutoComplete = 800

getDelayForLocateOnMap :: Int
getDelayForLocateOnMap = 1000

getSearchRadius :: Int
getSearchRadius = 100000

getLanguages :: Array Language
getLanguages =
  [ { name: "English", value: "EN_US" }
  , { name: "മലയാളം", value: "ML_IN" }
  ]

data DecodeAddress
  = Booking BookingLocationAPIEntity
  | SavedLoc SavedReqLocationAPIEntity

decodeAddress :: DecodeAddress -> String
decodeAddress addressWithCons =
  let
    (BookingLocationAPIEntity address) = case addressWithCons of
      Booking bookingLocation -> bookingLocation
      SavedLoc savedLocation -> getBookingEntity savedLocation
  in
    if (trim (fromMaybe "" address.city) == "" && trim (fromMaybe "" address.area) == "" && trim (fromMaybe "" address.street) == "" && trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (trim (fromMaybe "" address.area) == "" && trim (fromMaybe "" address.street) == "" && trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (trim (fromMaybe "" address.street) == "" && trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (trim (fromMaybe "" address.door) == "") then
      ((fromMaybe "" address.building) <> ", " <> (fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else
      ((fromMaybe "" address.door) <> ", " <> (fromMaybe "" address.building) <> ", " <> (fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))

encodeAddress :: String -> Array AddressComponents -> Maybe String -> ST.Address
encodeAddress fullAddress addressComponents placeId =
  let
    totalAddressComponents = length $ split (Pattern ", ") fullAddress

    splitedAddress = split (Pattern ", ") fullAddress
  in
    { area: splitedAddress !! (totalAddressComponents - 4)
    , areaCode: Just (getValueByComponent addressComponents "postal_code")
    , building: splitedAddress !! (totalAddressComponents - 6)
    , city: splitedAddress !! (totalAddressComponents - 3)
    , country: splitedAddress !! (totalAddressComponents - 1)
    , state: splitedAddress !! (totalAddressComponents - 2)
    , door:
        if totalAddressComponents > 7 then
          splitedAddress !! 0 <> Just ", " <> splitedAddress !! 1
        else if totalAddressComponents == 7 then
          splitedAddress !! 0
        else
          Nothing
    , street: splitedAddress !! (totalAddressComponents - 5)
    , ward:
        if null addressComponents then
          getWard Nothing (splitedAddress !! (totalAddressComponents - 4)) (splitedAddress !! (totalAddressComponents - 5)) (splitedAddress !! (totalAddressComponents - 6))
        else
          Just $ getValueByComponent addressComponents "sublocality"
    , placeId: placeId
    }

getValueByComponent :: Array AddressComponents -> String -> String
getValueByComponent address componentName = getAddress $ filter (\(AddressComponents item) -> length (getByTag item.types componentName) > 0) address

getByTag :: Array String -> String -> Array String
getByTag tags componentName = (filter (\item -> contains (Pattern componentName) item) tags)

getAddress :: Array AddressComponents -> String
getAddress address = joinWith ", " (reverse (map (\(AddressComponents item) -> item.longName) address))

getBookingEntity :: SavedReqLocationAPIEntity -> BookingLocationAPIEntity
getBookingEntity (SavedReqLocationAPIEntity savedLocation) =
  BookingLocationAPIEntity
    { "area": savedLocation.area
    , "state": savedLocation.state
    , "country": savedLocation.country
    , "building": savedLocation.building
    , "door": savedLocation.door
    , "street": savedLocation.street
    , "city": savedLocation.city
    , "areaCode": savedLocation.areaCode
    , "lat": savedLocation.lat
    , "lon": savedLocation.lon
    , "ward": savedLocation.ward
    , "placeId": savedLocation.placeId
    }

getAddressFromSaved :: SavedReqLocationAPIEntity -> ST.Address
getAddressFromSaved (SavedReqLocationAPIEntity savedLocation) =
  { "area": savedLocation.area
  , "state": savedLocation.state
  , "country": savedLocation.country
  , "building": savedLocation.building
  , "door": savedLocation.door
  , "street": savedLocation.street
  , "city": savedLocation.city
  , "areaCode": savedLocation.areaCode
  , "ward": savedLocation.ward -- (getWard savedLocation.ward savedLocation.area savedLocation.street savedLocation.building)
  , "placeId": savedLocation.placeId
  }

getAddressFromBooking :: BookingLocationAPIEntity -> ST.Address
getAddressFromBooking (BookingLocationAPIEntity address) =
  { "area": address.area
  , "state": address.state
  , "country": address.country
  , "building": address.building
  , "door": address.door
  , "street": address.street
  , "city": address.city
  , "areaCode": address.areaCode
  , "ward": getWard address.ward address.area address.street address.building
  , "placeId": address.placeId
  }

getWard :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String
getWard ward area street building =
  let
    actualWard = if (trim (replaceAll (Pattern ",") (Replacement "") (fromMaybe "" ward))) == "" then Nothing else ward

    actualArea = if (trim (fromMaybe "" area)) == "" then Nothing else (area <> Just ", ")

    actualStreet = if (trim (fromMaybe "" street)) == "" then Nothing else (street <> Just ", ")

    actualBuilding = if (trim (fromMaybe "" building)) == "" then Nothing else building
  in
    if isJust actualWard then actualWard else (actualArea <> actualStreet <> actualBuilding)

getKeyByLanguage :: String -> String
getKeyByLanguage language = case language of 
  "ENGLISH" -> "EN_US"
  "KANNADA" -> "KN_IN" 
  "HINDI"   -> "HI_IN" 
  "MALAYALAM" -> "ML_IN" 
  "TAMIL"   ->"TA_IN"
  "BENGALI" -> "BN_IN"
  _ -> "EN_US" 

getGender :: Maybe ST.Gender -> String -> String 
getGender gender placeHolderText = 
  case gender of 
    Just value -> case value of 
      ST.MALE -> (getString MALE)
      ST.FEMALE -> (getString FEMALE)
      ST.OTHER ->  (getString OTHER)
      _ -> (getString PREFER_NOT_TO_SAY)
    Nothing -> placeHolderText

getFaresList :: Array FareBreakupAPIEntity -> String -> Array ST.FareComponent
getFaresList fares baseDistance =
  map
    ( \(FareBreakupAPIEntity item) ->
          { fareType : item.description
          , price : (getValueFromConfig "currency") <> " " <> 
            (show $ case item.description of 
              "BASE_FARE" -> (item.amount + getFareFromArray fares "EXTRA_DISTANCE_FARE" + getFareFromArray fares "NIGHT_SHIFT_CHARGE") 
              "SGST" -> item.amount * 2 
              _ -> item.amount)
          , title : case item.description of
                      "BASE_FARE" -> (getEN BASE_FARES) <> if baseDistance == "0 m" then "" else " (" <> baseDistance <> ")"
                      "EXTRA_DISTANCE_FARE" -> getEN NOMINAL_FARE
                      "DRIVER_SELECTED_FARE" -> getEN DRIVER_ADDITIONS
                      "TOTAL_FARE" -> getEN TOTAL_PAID
                      "DEAD_KILOMETER_FARE" -> getEN PICKUP_CHARGE
                      "PICKUP_CHARGES" -> getEN PICKUP_CHARGE
                      "CUSTOMER_SELECTED_FARE" -> getEN CUSTOMER_SELECTED_FARE
                      "WAITING_CHARGES" -> getEN WAITING_CHARGE
                      "EARLY_END_RIDE_PENALTY" -> getEN EARLY_END_RIDE_CHARGES
                      "WAITING_OR_PICKUP_CHARGES" -> getEN MISC_WAITING_CHARGE 
                      "SERVICE_CHARGE" -> getEN SERVICE_CHARGES
                      "FIXED_GOVERNMENT_RATE" -> getEN GOVERNMENT_CHAGRES
                      "PLATFORM_FEE" -> getEN PLATFORM_FEE
                      "SGST" -> getEN PLATFORM_GST
                      _ -> getEN BASE_FARES
          }
    )
    (getFilteredFares fares)

getFareFromArray :: Array FareBreakupAPIEntity -> String -> Int
getFareFromArray fareBreakUp fareType = (fromMaybe dummyFareBreakUp (head (filter (\fare -> fare^._description == (fareType)) fareBreakUp)))^._amount

dummyFareBreakUp :: FareBreakupAPIEntity
dummyFareBreakUp = FareBreakupAPIEntity{amount: 0,description: ""}

getFilteredFares :: Array FareBreakupAPIEntity -> Array FareBreakupAPIEntity
getFilteredFares = filter (\(FareBreakupAPIEntity item) -> (all (_ /=  item.description) ["EXTRA_DISTANCE_FARE", "TOTAL_FARE", "BASE_DISTANCE_FARE", "CGST", "NIGHT_SHIFT_CHARGE"]) )

getKmMeter :: Int -> String
getKmMeter distance = if (distance < 1000) then toString distance <> " m" else (parseFloat ((toNumber distance)/ 1000.0)) 2 <> " km"

fetchVehicleVariant :: String -> Maybe ST.VehicleVariant
fetchVehicleVariant variant = case variant of  
  "SUV" -> Just ST.SUV
  "SEDAN" -> Just ST.SEDAN
  "HATCHBACK" -> Just ST.HATCHBACK
  "AUTO_RICKSHAW" -> Just ST.AUTO_RICKSHAW
  "TAXI" -> Just ST.TAXI 
  "TAXI_PLUS" -> Just ST.TAXI_PLUS
  _ -> Nothing

getVehicleImage :: String -> String
getVehicleImage variant = case fetchVehicleVariant variant of 
          Just ST.TAXI -> "ic_sedan_non_ac,https://assets.juspay.in/nammayatri/images/user/ic_sedan_non_ac.png"
          Just ST.TAXI_PLUS -> "ic_sedan_ac,https://assets.juspay.in/nammayatri/images/user/ic_sedan_ac.png"
          Just ST.SEDAN -> "ic_sedan,https://assets.juspay.in/nammayatri/images/user/ic_sedan.png"
          Just ST.SUV -> "ic_suv,https://assets.juspay.in/nammayatri/images/user/ic_suv.png"
          Just ST.HATCHBACK -> "ic_hatchback,https://assets.juspay.in/nammayatri/images/user/ic_hatchback.png"
          Just ST.AUTO_RICKSHAW -> "ic_vehicle_side,https://assets.juspay.in/beckn/merchantcommon/images/ic_auto_side_view.png"
          _ -> "ic_sedan_non_ac,https://assets.juspay.in/nammayatri/images/user/ic_sedan_non_ac.png"

getVehicleCapacity :: String -> String 
getVehicleCapacity variant = case fetchVehicleVariant variant of
          Just ST.TAXI -> (getString ECONOMICAL) <> ", 4 " <> (getString PEOPLE)
          Just ST.TAXI_PLUS -> (getString COMFY) <> ", 4 " <> (getString PEOPLE)
          Just ST.SEDAN -> (getString COMFY) <> ", " <>(getString UPTO) <>" 4 " <> (getString PEOPLE)
          Just ST.SUV -> (getString SPACIOUS) <> ", " <> (getString UPTO)<>" 6 " <> (getString PEOPLE)
          Just ST.HATCHBACK -> (getString EASY_ON_WALLET) <> ", "<> (getString UPTO) <> " 4 " <> (getString PEOPLE)
          _ -> (getString ECONOMICAL) <> ", 4 " <> (getString PEOPLE)