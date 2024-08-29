module Screens.EmergencyContactsScreen.View where

import Animation (screenAnimation)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Engineering.Helpers.Commons (safeMarginTop, safeMarginBottom, os, screenWidth, getNewIDWithTag)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, discard, pure, unit, void, map, not, ($), (&&), (-), (<), (<<<), (<>), (==), (>))
import PrestoDOM (Accessiblity(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), accessibility, accessibilityHint, afterRender, alignParentBottom, background, color, cornerRadius, editText, fontStyle, gravity, height, hint, id, imageView, imageWithFallback, linearLayout, margin, onBackPressed, onChange, onClick, orientation, padding, pattern, relativeLayout, scrollBarY, stroke, text, textSize, textView, visibility, weight, width, adjustViewWithKeyboard, scrollView)
import Screens.EmergencyContactsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (EmergencyContactsScreenState, NewContacts, CheckBoxSelectionConfig)
import Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import Helpers.Utils (FetchImageFrom(FF_COMMON_ASSET), fetchImage, storeCallBackContacts)
import Data.Array (difference, length, null, take, (!!), mapWithIndex)
import Data.String as DS
import Data.Maybe (fromMaybe)
import Data.String (split, Pattern(..))
import Components.PopUpModal as PopUpModal
import Screens.CustomerUtils.EmergencyContactsScreen.ComponentConfig (genericHeaderConfig, primaryButtonConfig, removeContactPopUpModelConfig)
import Screens.CustomerUtils.EmergencyContactsScreen.ComponentConfig as CC
import PrestoDOM.List as PrestoList
import PrestoDOM.Events (globalOnScroll)
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple (Tuple(..))
import Components.PrimaryButton.Controller as PrimaryButtonConfig
import Helpers.Utils (FetchImageFrom(..), fetchImage, storeCallBackContacts)
import Screens.NammaSafetyFlow.Components.ContactsList as ContactsList
import Screens.NammaSafetyFlow.Components.HelperViews as HelperViews
import Components.DropDownWithHeader as DropDownWithHeader
import Debug (spy)
import Engineering.Helpers.Utils (terminateLoader)
import Mobility.Prelude (boolToInvisibility, boolToVisibility)

screen :: EmergencyContactsScreenState -> PrestoList.ListItem -> Screen Action EmergencyContactsScreenState ScreenOutput
screen initialState listItemm =
  { initialState
  , view: view listItemm
  , name: "EmergencyContactsScreen"
  , globalEvents:
      [ globalOnScroll "EmergencyContactsScreen"
      , ( \push -> do
            void $ storeCallBackContacts push ContactsCallback
            pure (pure unit)
        )
      ]
  , eval:
      \action state -> do
        let
          _ = spy "EmergencyContactsScreen action " action
        let
          _ = spy "EmergencyContactsScreen state " state
        eval action state
  }

view :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
view listItemm push state =
  screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push (const BackPressed)
        , background Color.white900
        , padding if os == "IOS" then (Padding 0 safeMarginTop 0 marginBottom) else (Padding 0 0 0 0)
        , afterRender push $ const NoAction
        ]
        ( [ linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , accessibility if state.props.showInfoPopUp then DISABLE_DESCENDANT else DISABLE
              ]
              [ GenericHeader.view (push <<< ContactListGenericHeaderActionController) (genericHeaderConfig state)
              , linearLayout
                  [ height $ V 1
                  , width $ V (screenWidth unit)
                  , background Color.greySmoke
                  ]
                  []
              , linearLayout
                  [ height MATCH_PARENT
                  , width MATCH_PARENT
                  , orientation VERTICAL
                  , visibility if state.props.showContactList then GONE else VISIBLE
                  ]
                  [ emergencyContactsView push state
                  , HelperViews.recommendContactsToInstallView state.props.appName $ state.props.saveEmergencyContacts && not state.props.getDefaultContacts && length state.data.selectedContacts > 0
                  , linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , padding $ Padding 16 0 16 16
                      ]
                      [ PrimaryButton.view (push <<< PrimaryButtonActionControll) (primaryButtonConfig state) ]
                  ]
              ]
          , if state.props.showContactList then (contactListView listItemm push state) else emptyTextView state
          ]
            <> if state.props.showInfoPopUp then [ removeContactPopUpView push state ] else [ emptyTextView state ]
        )
  where
  marginBottom =
    if state.props.showContactList then
      0
    else if safeMarginBottom == 0 && os == "IOS" then
      16
    else
      safeMarginBottom

------------------------ EmptyTextView ---------------------------
emptyTextView :: forall w. EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
emptyTextView state = textView []

------------------------ ContactsListView ---------------------------
contactListView :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
contactListView listItemm push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        ]
        [ GenericHeader.view (push <<< ContactListGenericHeaderActionController) (genericHeaderConfig state)
        , horizontalLine
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 44
        , orientation HORIZONTAL
        , cornerRadius 8.0
        , padding (Padding 2 2 2 2)
        , margin (Margin 16 16 16 16)
        , gravity LEFT
        , stroke ("1," <> Color.borderColorLight)
        ]
        [ editText
            [ height MATCH_PARENT
            , width WRAP_CONTENT
            , weight 1.0
            , textSize FontSize.a_16
            , padding (Padding 14 10 0 10)
            , color Color.black800
            , gravity LEFT
            , id (getNewIDWithTag "contactEditText")
            , background Color.white900
            , fontStyle $ FontStyle.semiBold LanguageStyle
            , text ""
            , hint $ getString SEARCH_CONTACTS
            , pattern "[^\n]*,255"
            , onChange push $ ContactTextChanged
            ]
        , imageView
            [ height $ V 17
            , width $ V 17
            , accessibilityHint "Cancel Search : Button"
            , accessibility ENABLE
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_cancel"
            , gravity CENTER
            , margin (Margin 10 10 10 10)
            , onClick push $ const ContactListClearText
            ]
        ]
    , showEmergencyContact listItemm push state
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , padding (Padding 16 16 16 0)
        , stroke $ "1," <> Color.grey900
        , alignParentBottom "true,-1"
        , margin (Margin 0 0 0 0)
        , adjustViewWithKeyboard "true"
        , alignParentBottom "true,-1"
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height if os == "IOS" then (V 68) else WRAP_CONTENT
            , gravity BOTTOM
            , padding $ PaddingBottom 16
            ]
            [ PrimaryButton.view getPushFn $ contactListPrimaryButtonConfig state
            ]
        ]
    ]
  where
  getPushFn =
    ( \action -> do
        void $ terminateLoader ""
        push $ PrimaryButtonAC action
    )

showEmergencyContact :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
showEmergencyContact listitemm push config =
  linearLayout
    [ width MATCH_PARENT
    , background Color.blue600
    , weight 1.0
    ]
    [ showEmergencyContactData listitemm push config
    ]

showEmergencyContactData :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
showEmergencyContactData listItemm push state =
  Keyed.linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ Tuple "contacts"
        $ PrestoList.list
            [ height MATCH_PARENT
            , scrollBarY false
            , width MATCH_PARENT
            , PrestoList.listItem listItemm
            , background Color.white900
            , PrestoList.listDataV2 $ state.data.prestoListArrayItems
            ]
    ]

startsWith :: String -> String -> Boolean
startsWith prefix str = DS.take (DS.length prefix) str == prefix

contactListPrimaryButtonConfig :: EmergencyContactsScreenState -> PrimaryButtonConfig.Config
contactListPrimaryButtonConfig state =
  let
    config' = PrimaryButtonConfig.config

    uniqueContacts = length $ difference state.data.selectedContacts state.data.emergencyContactsList

    enableBtn = if (uniqueContacts == 0) && (length state.data.selectedContacts) < (length state.data.emergencyContactsList) then true else uniqueContacts > 0

    primaryButtonConfig' =
      config'
        { textConfig
          { text = if enableBtn then (getString CONFIRM_EMERGENCY_CONTACTS) else (getString SELECT_CONTACTS)
          , accessibilityHint = (if enableBtn then (getString CONFIRM_EMERGENCY_CONTACTS) else (getString SELECT_CONTACTS)) <> " : Button"
          , color = if enableBtn then Color.yellow900 else Color.yellow800
          }
        , background = if enableBtn then Color.black900 else Color.black600
        , isClickable = if enableBtn then true else false
        , id = "ContactListPrimaryButton"
        , enableRipple = if enableBtn then true else false
        , margin = (MarginBottom 0)
        }
  in
    primaryButtonConfig'

horizontalLine :: forall w. PrestoDOM (Effect Unit) w
horizontalLine =
  linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.grey900
    ]
    []

--------------------------------------------------- emergencyContactsView -----------------------------------------------------
emergencyContactsView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
emergencyContactsView push state =
  linearLayout
    [ width MATCH_PARENT
    , orientation VERTICAL
    , weight 1.0
    ]
    [ emptyContactsView push state
    -- , emergencyContactsListView push state
    ]

--------------------------------------------------- emptyContactsView -----------------------------------------------------
emptyContactsView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
emptyContactsView push state =
  linearLayout
    [ height $ WRAP_CONTENT
    , width $ MATCH_PARENT
    , orientation VERTICAL
    ]
    [ scrollView
        [ width MATCH_PARENT
        , scrollBarY false
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ]
            [ imageView
                [ height $ V 200
                , width MATCH_PARENT
                , visibility $ boolToVisibility $ not $ state.props.saveEmergencyContacts && state.props.getDefaultContacts
                , imageWithFallback $ fetchImage GLOBAL_COMMON_ASSET "ny_ic_share_explain"
                ]
            , explanationContentView push state
            , checkBoxSelectionView push state
            ]
        ]
    ]

explanationContentView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
explanationContentView push state =
  let
    contactsLength = length state.data.selectedContacts
  in
    linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , visibility $ boolToVisibility state.props.saveEmergencyContacts
      , orientation VERTICAL
      , gravity CENTER_HORIZONTAL
      ]
      [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , padding $ Padding 16 16 16 0
          ]
          [ textView
              $ [ height WRAP_CONTENT
                , width MATCH_PARENT
                , text $ getString LIVE_RIDE_TRACKING
                , color Color.black900
                ]
              <> FontStyle.h3 TypoGraphy
          ]
      , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , padding $ Padding 16 16 16 0
          ]
          [ textView
              $ [ height WRAP_CONTENT
                , width MATCH_PARENT
                , text $ getString LIVE_RIDE_TRACKING_DESC
                , color Color.black700
                ]
              <> FontStyle.body5 TypoGraphy
          ]
      , emergencyContactsListView push state
      , if contactsLength < 3 && contactsLength > 0 then addContactsButtonView push state else emptyTextView state
      ]

addContactsButtonView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
addContactsButtonView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 16 24 16 24
    , margin $ Margin 16 16 16 16
    , cornerRadius 16.0
    , background Color.blue600
    , onClick push $ const AddContacts
    , gravity CENTER
    ]
    [ textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ getString ADD_CONTACTS
          , color Color.blue800
          ]
        <> FontStyle.body20 TypoGraphy
    ]

--------------------------------------------------- emergencyContactsListView -----------------------------------------------------
emergencyContactsListView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
emergencyContactsListView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    $ mapWithIndex
        ( \index contact ->
            linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , padding $ Padding 16 16 16 16
              , margin $ Margin 16 16 16 0
              , cornerRadius 16.0
              , background Color.blue600
              , orientation VERTICAL
              ]
              [ emergencyContactListItem push state contact index
              , dropDownWithHeaderView push state contact
              ]
        )
        state.data.selectedContacts

dropDownWithHeaderView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> NewContacts -> PrestoDOM (Effect Unit) w
dropDownWithHeaderView push state contact =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER_HORIZONTAL
    , padding (Padding 0 0 0 0)
    ]
    [ DropDownWithHeader.view (push <<< DropDownWithHeaderAC) $ CC.dropDownWithHeaderConfig state contact --(CC.dropDownWithHeaderConfig state)
    ]

emergencyContactListItem :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> NewContacts -> Int -> PrestoDOM (Effect Unit) w
emergencyContactListItem push state contact index =
  let
    userColor = case index of
      0 -> Color.yellow900
      1 -> Color.blue800
      2 -> Color.yellow800
      _ -> Color.grey700
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      ]
      [ linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , background userColor
          , padding $ Padding 12 12 12 12
          , cornerRadius 50.0
          , margin $ MarginRight 12
          ]
          [ textView
              $ [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text "AR"
                , color Color.black
                ]
          ]
      , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation VERTICAL
          , weight 1.0
          ]
          [ textView
              $ [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text contact.name
                , color Color.black900
                ]
              <> FontStyle.h3 TypoGraphy
          , textView
              $ [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text contact.number
                , color Color.black700
                ]
              <> FontStyle.body5 TypoGraphy
          ]
      , imageView
          [ height $ V 40
          , width $ V 40
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_delete_bin"
          , onClick push $ const $ RemoveButtonClicked contact
          ]
      ]

--------------------------------------------------- checkBoxSelectionView -----------------------------------------------------
checkBoxSelectionView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
checkBoxSelectionView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , visibility $ boolToVisibility state.props.getDefaultContacts
    , orientation VERTICAL
    , padding (Padding 0 0 0 0)
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , padding $ PaddingHorizontal 16 16
        ]
        [ textView
            $ [ height WRAP_CONTENT
              , width MATCH_PARENT
              , text $ getString APP_CALL_CHAT
              , color Color.black900
              ]
            <> FontStyle.h3 TypoGraphy
        , textView
            $ [ height WRAP_CONTENT
              , width MATCH_PARENT
              , text $ getString TRUSTED_CONTACT_DESC
              , color Color.black700
              , margin $ MarginTop 6
              ]
            <> FontStyle.body5 TypoGraphy
        , textView
            $ [ height WRAP_CONTENT
              , width MATCH_PARENT
              , text $ getString DEFAULT_CONTACT
              , margin $ MarginTop 16
              , color Color.black700
              ]
            <> FontStyle.body1 TypoGraphy
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ Padding 16 16 16 16
        , margin $ Margin 16 12 16 16
        , cornerRadius 16.0
        , background Color.blue600
        , orientation VERTICAL
        ]
        $ mapWithIndex (\index contact -> contactListViewCheckBox push state contact index) state.data.selectedContacts
    ]

contactListViewCheckBox :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> NewContacts -> Int -> PrestoDOM (Effect Unit) w
contactListViewCheckBox push state contact index =
  let
    isDefault = contact.priority == 0 --contact.number == state.data.defaultSelectedContact.number

    strokeColor = if isDefault then Color.black800 else Color.black500

    userColor = case index of
      0 -> Color.yellow900
      1 -> Color.blue800
      2 -> Color.yellow800
      _ -> Color.grey700
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , margin $ MarginVertical 12 12
      , orientation HORIZONTAL
      , onClick push $ const $ DefaultContactSelected contact
      , gravity CENTER_VERTICAL
      ]
      [ linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , background userColor
          , padding $ Padding 8 8 8 8
          , cornerRadius 50.0
          , margin $ MarginRight 12
          ]
          [ textView
              $ [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text "AR"
                , color Color.black
                ]
          ]
      , textView
          $ [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text contact.name
            , color Color.black900
            , weight 1.0
            ]
          <> FontStyle.body5 TypoGraphy
      , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , padding $ Padding 4 4 4 4
          , stroke $ "1," <> Color.black800
          , cornerRadius 50.0
          ]
          [ linearLayout
              [ height $ V 12
              , width $ V 12
              , cornerRadius 50.0
              , visibility $ boolToInvisibility isDefault
              , background Color.black800
              ]
              []
          ]
      ]

--------------------------------------------------- emergencyContactsListView -----------------------------------------------------
getNameInitials :: String -> (Array String)
getNameInitials fullName = (take 2 (split (Pattern " ") (fullName)))

getFirstChar :: String -> String
getFirstChar name = DS.take 1 (fromMaybe "" ((getNameInitials name) !! 0))

getLastChar :: String -> String
getLastChar name = DS.take 1 (fromMaybe "" ((getNameInitials name) !! 1))

removeContactPopUpView :: forall w. (Action -> Effect Unit) -> EmergencyContactsScreenState -> PrestoDOM (Effect Unit) w
removeContactPopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ PopUpModal.view (push <<< PopUpModalAction) (removeContactPopUpModelConfig state) ]
