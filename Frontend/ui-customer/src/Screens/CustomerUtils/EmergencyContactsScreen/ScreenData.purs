module Screens.EmergencyContactsScreen.ScreenData where

import Prelude
import Screens.Types (EmergencyContactsScreenState(..))

initData :: EmergencyContactsScreenState
initData = {
    data: {
      contactInfoState : []
    , contactsCount : 0
    , contactsList : []
    , contactsNewList : []
    , contactsUpdatedNewList: []
    , prestoListArrayItems: []
    , loadMoreDisabled: true
    , offsetForEmergencyContacts : 0
    , limitForEmergencyContacts : 20
    , removedContactDetail : { isSelected :false
                              , name : ""
                              , number : ""
                              }
    , editedText : ""
    },
    props:{
        showContactList : false
      , showInfoPopUp : false  
    }
}