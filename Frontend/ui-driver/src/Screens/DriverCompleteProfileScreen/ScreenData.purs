module Screens.DriverCompleteProfileScreen.ScreenData where

import Screens.Types (DriverCompleteProfileScreenState, Component(..))
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Commons as EHC
import Prelude

initData :: DriverCompleteProfileScreenState
initData = {
  data : {
    pledge : []
  , vehicalOffer : []
  , languages : []
  , aspirations : [] 
  , homeTown : Nothing
  , calendarState:
    { calendarPopup: false
    , endDate: Nothing
    , selectedTimeSpan: dummyDateItem
    , startDate: Just dummyDateItem
    , weeks: []
    }
  , drivingSince : Nothing
  , addImagesState: addImagesState'
  , viewImageState: { image : "", imageName : Nothing}
  , uploadedImagesIds: []
  , addedImages: []
  , datePickerState : datePickerState'
  , inputTextState : inputTextState'
  }
, props : {
  showImageModel : false,
  showViewImageModel : false,
  showInputTextView : false
  }
}

inputTextState' = {
  feedback : "",
  component : Empty,
  others : others'
}

others' = {
  pledge : "",
  aspirations : ""
}

datePickerState' = {
  activeIndex : 0,
  dates : EHC.getPastYears 30,
  id : ""
}

addImagesState' = {
  images: [],
  stateChanged: false,
  isLoading: false,
  imageMediaIds: []
}

dummyDateItem = { date: 0, isInRange: false, isStart: false, isEnd: false, utcDate: "", shortMonth: "", year: 0, intMonth: 0 }