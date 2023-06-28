{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.ScreenData where

import Components.LocationListItem.Controller (dummyLocationListState)
import Components.QuoteListItem.Controller (QuoteListItemState)
import Components.SettingSideBar.Controller (SettingSideBarState, Status(..))
import Data.Maybe (Maybe(..))
import Styles.Colors as Color
import Screens.Types (Contact, DriverInfoCard, HomeScreenState, LocationListItemState, PopupType(..), RatingCard(..), SearchLocationModelType(..), Stage(..), Address, EmergencyHelpModelState,Location,RateCardType(..), ZoneType(..), SpecialTags, TipViewStage(..))
import Services.API (DriverOfferAPIEntity(..), QuoteAPIDetails(..), QuoteAPIEntity(..), PlaceName(..), LatLong(..), SpecialLocation(..), QuoteAPIContents(..))
import Prelude (($) ,negate)
import Data.Array (head)

initData :: HomeScreenState
initData = {
    data: {
      suggestedAmount : 0
    , finalAmount : 0
    , startedAt : ""
    , endedAt : ""
    , source : ""
    , destination : ""
    , eta : "2 mins"
    , vehicleDetails : "Bajaj RE Auto"
    , registrationNumber : "KA  01  YF  4921"
    , rating : 4.0
    , locationList : []
    , savedLocations : []
    , recentSearchs : { predictionArray : []}
    , previousCurrentLocations : {pastCurrentLocations:[]}
    , selectList : []
    , quoteListModelState : []
    , driverInfoCardState : dummyDriverInfo
    , previousRideRatingState : dummyPreviousRiderating
    , settingSideBar : dummySettingBar
    , sourceAddress : dummyAddress
    , destinationAddress : dummyAddress
    , route : Nothing
    , startedAtUTC : ""
    , rateCard : { baseFare : 0, extraFare : 0, pickUpCharges : 0, additionalFare : 0, nightShiftMultiplier : 0.0, nightCharges : false,currentRateCardType : DefaultRateCard,onFirstPage:false}
    , speed : 0
    , selectedLocationListItem : Nothing
    , saveFavouriteCard : {
        address : ""
      , tag : ""
      , tagExists : false
      , selectedItem : dummyLocationListState
      , tagData : []
      , isBtnActive : false
      }
    , rideDistance : "--"
    , rideDuration : "--"
    , showPreferences : false
    , messages : []
    , messagesSize : ""
    , suggestionsList : []
    , messageToBeSent : ""
    , nearByPickUpPoints : dummyPickUpPoints
    , polygonCoordinates : ""
    , specialZoneQuoteList : []
    , specialZoneSelectedQuote : Nothing
    , selectedEstimatesObject : {
      vehicleImage: ""
      , isSelected: false
      , vehicleVariant: ""
      , vehicleType: ""
      , capacity: ""
      , price: ""
      , isCheckBox: false
      , isEnabled: true
      , activeIndex: 0
      , index: 0
      , id: ""
      }
    , cancelRideConfirmationData : { delayInSeconds : 5, timerID : "", enableTimer : true, continueEnabled : false }
    },
    props: {
      rideRequestFlow : false
    , isSearchLocation : NoView
    , currentStage : HomeScreen
    , showCallPopUp : false
    , sourceLat : 0.0
    , isSource : Nothing
    , sourceLong : 0.0
    , destinationLat : 0.0
    , destinationLong : 0.0
    , sourcePlaceId : Nothing
    , destinationPlaceId : Nothing
    , estimateId : ""
    , selectedQuote : Nothing
    , locationRequestCount : 0
    , customerTip : {
        enableTips: false
      , tipForDriver: 10
      , tipActiveIndex: 1
      , isTipSelected: false
      }
    , searchId : ""
    , bookingId : ""
    , expiredQuotes : []
    , isCancelRide : false
    , cancellationReasons : []
    , cancelRideActiveIndex : Nothing
    , cancelDescription : ""
    , cancelReasonCode : ""
    , isPopUp : NoPopUp
    , forFirst : true
    , ratingModal : false
    , callbackInitiated : false
    , isLocationTracking : false
    , isInApp : true
    , locateOnMap : false
    , sourceSelectedOnMap : false
    , distance : 0
    , isSrcServiceable : true
    , isDestServiceable : true
    , isRideServiceable : true
    , showlocUnserviceablePopUp : false
    , autoSelecting : true
    , searchExpire : 90
    , isEstimateChanged : false
    , showRateCard : false
    , showRateCardIcon : false
    , sendMessageActive : false
    , chatcallbackInitiated : false
    , emergencyHelpModal : false
    , estimatedDistance : Nothing
    , waitingTimeTimerIds : []
    , tagType : Nothing
    , isSaveFavourite : false
    , showShareAppPopUp : false
    , showMultipleRideInfo : false
    , hasTakenRide : true
    , isReferred : false
    , storeCurrentLocs : false
    , unReadMessages : false
    , openChatScreen : false
    , emergencyHelpModelState : emergencyHelpModalData
    , showLiveDashboard : false
    , isBanner : true
    , callSupportPopUp : false
    , isMockLocation: false
    , isSpecialZone : false
    , defaultPickUpPoint : ""
    , cancelSearchCallDriver : false
    , zoneType : dummyZoneType
    , cancelRideConfirmationPopup : false
    , searchAfterEstimate : false
    , tipViewProps : {
        stage : DEFAULT
      , isVisible : false
      , onlyPrimaryText : false
      , isprimaryButtonVisible : false
      , primaryText : ""
      , secondaryText : ""
      , customerTipArray : ["₹10 🙂", "₹20 😄", "₹30 🤩"]
      , customerTipArrayWithValues : [10, 20, 30]
      , activeIndex : -1
      , primaryButtonText : ""
      }
    , timerId : ""
    , findingRidesAgain : false
    , routeEndPoints : Nothing
    }
}

dummyZoneType = {
    sourceTag : NOZONE
  , destinationTag : NOZONE
  , priorityTag : NOZONE
}

dummyContactData :: Array Contact
dummyContactData = []

selectedContactData ::  Contact
selectedContactData =
  { name : "", phoneNo : "" }

emergencyHelpModalData :: EmergencyHelpModelState
emergencyHelpModalData = {
  showCallPolicePopUp : false,
  showCallContactPopUp : false,
  showCallSuccessfulPopUp : false,
  showContactSupportPopUp : false,
  emergencyContactData : dummyContactData,
  currentlySelectedContact : selectedContactData,
  sosId : "",
  sosStatus : "",
  isSelectEmergencyContact : false
}

dummyQuoteList :: Array QuoteListItemState
dummyQuoteList = [
  {
   seconds : 10
  , id : "1"
  , timer : "0"
  , timeLeft : 0
  , driverRating : 4.0
  , profile : ""
  , price : "200"
  , vehicleType : "auto"
  , driverName : "Drive_Name"
  , selectedQuote : Nothing

  },
  {
   seconds : 10
  , id : "2"
  , timer : "0"
  , timeLeft : 0
  , driverRating : 4.0
  , profile : ""
  , price : "300"
  , vehicleType : "auto"
  , driverName : "Drive_Name"
  ,selectedQuote : Nothing
  },
  {
   seconds : 3
  , id : "3"
  , timer : "0"
  , timeLeft : 0
  , driverRating : 4.0
  , profile : ""
  , price : "3150"
  , vehicleType : "auto"
  , driverName : "Drive_Name"
  ,selectedQuote : Nothing
  }
]

dummyPreviousRiderating :: RatingCard
dummyPreviousRiderating = {
  rideId : ""
, rating : 0
, driverName : ""
, finalAmount : 0
, rideStartTime : ""
, rideEndTime : ""
, source : ""
, destination : ""
, rideStartDate : ""
, vehicleNumber : ""
, status : ""
, shortRideId : ""
, bookingId : ""
, rideEndTimeUTC : ""
, dateDDMMYY : ""
, offeredFare : 0
, distanceDifference : 0
, feedback : ""
}


dummyDriverInfo :: DriverInfoCard
dummyDriverInfo =
  { otp : ""
  , driverName : ""
  , eta : 0
  , vehicleDetails : ""
  , registrationNumber : ""
  , rating : 0.0
  , startedAt : ""
  , endedAt : ""
  , source : ""
  , destination : ""
  , rideId : ""
  , price : 0
  , sourceLat : 0.0
  , sourceLng : 0.0
  , destinationLat : 0.0
  , destinationLng : 0.0
  , driverLat : 0.0
  , driverLng : 0.0
  , distance : 0
  , waitingTime : "--"
  , driverArrived : false
  , estimatedDistance : ""
  , driverArrivalTime : 0
  , bppRideId : ""
  , driverNumber : Nothing
  , merchantExoPhone : ""
  , createdAt : ""
  , initDistance : Nothing
  }

dummySettingBar :: SettingSideBarState
dummySettingBar = {
    name : ""
  , number : ""
  , opened : CLOSED
  , email : Nothing
  , gender : Nothing
  }

dummyAddress :: Address
dummyAddress = {
              "area" : Nothing
            , "state" : Nothing
            , "country" : Nothing
            , "building" : Nothing
            , "door" : Nothing
            , "street" : Nothing
            , "city" : Nothing
            , "areaCode" : Nothing
            , "ward" : Nothing
            , "placeId" : Nothing
            }

dummyQuoteAPIEntity :: QuoteAPIEntity
dummyQuoteAPIEntity = QuoteAPIEntity {
  agencyNumber : "",
  createdAt : "",
  discount : Nothing,
  estimatedTotalFare : 0,
  agencyName : "",
  vehicleVariant : "",
  estimatedFare : 0,
  tripTerms : [],
  id : "",
  agencyCompletedRidesCount : 0,
  quoteDetails : QuoteAPIDetails {fareProductType : "", contents : dummyDriverOfferAPIEntity}
}

dummyDriverOfferAPIEntity :: QuoteAPIContents
dummyDriverOfferAPIEntity =
  DRIVER_OFFER
    $ DriverOfferAPIEntity
        { rating: Nothing
        , validTill: ""
        , driverName: ""
        , distanceToPickup: 0.0
        , durationToPickup: 0
        }

dummyLocationName :: PlaceName
dummyLocationName = PlaceName {
  "formattedAddress" : "",
  "location" : LatLong{
    "lat" : 0.0,
    "lon" : 0.0
  },
  "plusCode" : Nothing,
  "addressComponents" : []
}
dummyPickUpPoints :: Array Location
dummyPickUpPoints = [
  {place : "Kolkata airport arrival gate 1 ", lat : 12.941156, lng : 77.623510 },
  {place : "Kolkata airport arrival gate 2 ", lat : 12.940696, lng : 77.622877 }
]

specialLocation :: SpecialLocation
specialLocation = SpecialLocation{
    "category" :"",
     "gates": [],
     "locationName" : ""
 }

dummyLocation :: Location
dummyLocation = {
   place : "",
   lat : 0.0,
   lng : 0.0
 }

