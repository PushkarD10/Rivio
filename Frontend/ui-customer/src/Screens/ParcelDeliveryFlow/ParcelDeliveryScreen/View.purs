module Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.View where

import Prelude

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Common.Resources.Constants (zoomLevel)
import Components.ChooseVehicle.View as ChooseVehicle
import Components.GenericHeader.View as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination.View as SourceToDestinationView
import Components.PopUpModal as PopUpModal
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (isJust, isNothing, maybe, fromMaybe, Maybe(..))
import Data.String as DS
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.CommonView (dummyView)
import Helpers.Utils (FetchImageFrom(..), fetchImage, decodeError, storeCallBackCustomer)
import JBridge as JB
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Presto.Core.Types.Language.Flow (Flow, doAff, delay)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..) ,background, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, relativeLayout, scrollView, shimmerFrameLayout, onBackPressed, alignParentBottom, singleLine, accessibilityHint,accessibility,accessibilityHint, Accessiblity(..), id, afterRender, layoutGravity, rippleColor, maxLines, ellipsize, onAnimationEnd)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Resources.Constants
import Resources.Localizable.EN (getEN)
import Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.ComponentConfig (chooseVehicleConfig, deliveryPickupDetialsModalConfig, genericHeaderConfig, primaryButtonConfig)
import Components.ParcelDeliveryInstruction as ParcelDeliveryInstruction
import Screens.HomeScreen.ScreenData (dummyRideBooking)
import Screens.Types as ST
import Services.API
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (GlobalState, defaultGlobalState)

parcelDeliveryScreen :: ST.ParcelDeliveryScreenState -> Screen Action ST.ParcelDeliveryScreenState ScreenOutput
parcelDeliveryScreen initialState =
  { initialState
  , view
  , name: "ParcelDeliveryScreen"
  , globalEvents: []
  , eval:
      \action state -> do
        let _ = spy "ParcelDeliveryScreen action " action
        let _ = spy "ParcelDeliveryScreen state " state
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
view push state = 
  case state.data.currentStage of
    ST.DELIVERY_INSTRUCTIONS -> deliveryInstructionView push state
    _ -> deliveryDetailsView push state
  

deliveryInstructionView :: forall w . (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
deliveryInstructionView push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][ParcelDeliveryInstruction.view (push <<< ParcelDeliveryInstructionAC)]

deliveryDetailsView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
deliveryDetailsView push state =
  Anim.screenAnimation $
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , onBackPressed push $ const GoBack
    , padding $ PaddingVertical EHC.safeMarginTop EHC.safeMarginBottom
    , onClick push $ const NoAction
    ] $
    [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ]
      [ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
      , separatorView push state 
      ]
    , scrollView
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , margin $ Margin 16 60 16 100
      ]
      [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        [ mapViewLayout push state
        , pickupView push state
        , dropView push state
        , deliveryGuidelinesView push state
        ]
      ]
    , separatorView push state
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity BOTTOM
      , margin (MarginBottom 24)
      , alignParentBottom "true,-1"
      , stroke ("1,"<> Color.grey900)
      , cornerRadii (Corners 16.0 true true false false)
      ]
      [ ChooseVehicle.view (push <<< ChooseVehicleAC) $ chooseVehicleConfig state
      , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)
      ]
    ] <>
    ( case state.data.currentStage of 
        ST.SENDER_DETAILS -> [deliveryDetailPopupView push state]
        ST.RECEIVER_DETAILS -> [deliveryDetailPopupView push state]
        _ -> [] )

mapViewLayout :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
mapViewLayout push state = 
  relativeLayout
  [ height $ V $ JB.getHeightFromPercent 20
  , width MATCH_PARENT
  , cornerRadius 24.0
  , margin $ MarginTop 16
  , id $ EHC.getNewIDWithTag idTag
  , afterRender (\action ->
                      if isNotInstructionsPage
                        then void $ JB.showMap (EHC.getNewIDWithTag idTag) false "satellite" zoomLevel state.data.sourceLat state.data.sourceLong push MapViewLoaded
                        else pure unit
                    ) $ const NoAction
  , visibility $ boolToVisibility isNotInstructionsPage
  ] []
  where
    idTag :: String
    idTag = "ParcelDetailsMapView"

    isNotInstructionsPage :: Boolean
    isNotInstructionsPage = state.data.currentStage /= ST.DELIVERY_INSTRUCTIONS

pickupView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
pickupView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginTop 20
  ]
  [ textView $
    [ text "Pickup"
    , color Color.black900
    ] <> FontStyle.subHeading1 TypoGraphy
  , pickupDropItemView push state true
  ]

dropView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
dropView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginTop 20
  ]
  [ textView $
    [ text "Drop"
    , color Color.black900
    ] <> FontStyle.subHeading1 TypoGraphy
  , pickupDropItemView push state false
  ]

pickupDropItemView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> Boolean -> PrestoDOM (Effect Unit) w
pickupDropItemView push state isSource =
  let 
    (DeliveryDetails deliveryDetails) = state.data.deliveryDetailsInfo
    (PersonLocationAndInstruction personDetails) = if isSource then deliveryDetails.senderDetails else deliveryDetails.receiverDetails
    (InstructionAndAddress address) = personDetails.address
  in linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 8
    , cornerRadius 16.0
    , padding $ Padding 12 12 12 12
    , stroke $ "1," <> Color.borderGreyColor
    ]
    [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , gravity CENTER
      ]
      [ linearLayout
        [ width WRAP_CONTENT
        , orientation VERTICAL
        , gravity LEFT
        , weight 1.0
        ]
        [ textView $
          [ text personDetails.name
          , color Color.black800
          ] <> FontStyle.body1 TypoGraphy
        , textView $
          [ text personDetails.phoneNumber
          , color Color.black800
          ] <> FontStyle.body1 TypoGraphy
        ]
      , editButtonView push state isSource
      ]
    , sourceDestinationAddressView push state isSource
    ]

sourceDestinationAddressView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> Boolean -> PrestoDOM (Effect Unit) w
sourceDestinationAddressView push state isSource =
  let
    (DeliveryDetails deliveryDetails) = state.data.deliveryDetailsInfo
    (PersonLocationAndInstruction personDetails) = if isSource then deliveryDetails.senderDetails else deliveryDetails.receiverDetails
    (InstructionAndAddress extraAddress) = personDetails.address
  in 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 8
    , background Color.blue600
    , cornerRadius 8.0
    , padding $ Padding 12 8 12 8
    ]
    [ linearLayout
      [ orientation HORIZONTAL
      , height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      ]
      [ linearLayout
        [ orientation VERTICAL
        , height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity LEFT
        , accessibility ENABLE
        , weight 1.0
        ]
        [ linearLayout
          [ width MATCH_PARENT
          , orientation HORIZONTAL
          , gravity CENTER
          ]
          [ imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_source_dot"
            , height $ V 14
            , width $ V 14
            , margin $ MarginRight 12
            , layoutGravity "center_vertical"
            ]
          , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            ]
            [ textView $
              [ text extraAddress.extras
              , maxLines 1
              , ellipsize true
              , gravity LEFT
              , color Color.black900
              , margin $ MarginBottom 2
              ] <> FontStyle.tags TypoGraphy
            , textView $
              [ text $ decodeAddress' $ if isSource then state.data.sourceAddress else state.data.destinationAddress
              , color Color.black700
              , maxLines $ if (isJust extraAddress.instruction) then 1 else 2
              , ellipsize true
              , margin $ MarginBottom 2
              ] <> FontStyle.body3 TypoGraphy
            , textView $
              [ textFromHtml $ "<em>Pickup Instruction: " <> fromMaybe "" extraAddress.instruction <> "</em>"
              , color Color.black700
              , visibility $ boolToVisibility $ isJust extraAddress.instruction
              ] <> FontStyle.body3 TypoGraphy
            ]
          ]
        ]  
      ]
    ]

deliveryGuidelinesView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
deliveryGuidelinesView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ MarginTop 20
  ]
  [ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.blue600
    , padding $ Padding 16 16 16 16
    , cornerRadius 16.0
    ]
    [ textView $ 
      [ text "Delivery Guidelines"
      , color Color.black800
      , margin $ MarginBottom 20
      ] 
      <> FontStyle.subHeading3 TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ]
      ( map (\item -> instructionItem item) instructionData)
    , textView $ 
      [ text "View all Guidelines"
      , color Color.blue800
      , layoutGravity "center_horizontal"
      , margin $ Margin 16 16 16 0
      , onClick push $ const ExpandInstructions
      ]
      <> FontStyle.body1 TypoGraphy
    ]
]

instructionItem :: forall w. { title :: String, image :: String } -> PrestoDOM (Effect Unit) w
instructionItem item =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginBottom 16
  ]
  [ imageView
    [ width $ V 20
    , height $ V 20
    , imageWithFallback $ fetchImage FF_ASSET item.image
    , margin $ MarginRight 8
    ]
  , textView
    $ [ text item.title
      , color Color.black800
      ]
    <> FontStyle.body20 TypoGraphy
  ]
  
editButtonView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> Boolean -> PrestoDOM (Effect Unit) w
editButtonView push state isSource =
  textView $ 
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text $ getString EDIT
  , cornerRadius if EHC.os == "IOS" then 20.0 else 32.0
  , stroke $ "1," <> Color.grey900
  , padding $ Padding 16 8 16 8
  , onClick push $ const $ EditAddress isSource
  , rippleColor Color.rippleShade
  , layoutGravity "right"
  , gravity CENTER_VERTICAL
  ] <> FontStyle.body1 TypoGraphy

primaryButtonView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
primaryButtonView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity BOTTOM
  , margin (MarginBottom 24)
  , alignParentBottom "true,-1"
  , stroke $ "1," <> Color.grey900
  ]
  [ PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)]

separatorView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
separatorView push state =
  linearLayout
  [ width MATCH_PARENT
  , height $ V 1
  , margin $ MarginVertical 5 5
  , background Color.grey900
  ]
  []

instructionData :: Array { title :: String, image :: String }
instructionData = 
  [ { title: "Items should fit in a backpack (max: 5kg)", image: "ny_ic_backpack" }
  , { title: "Avoid sending high-value / fragile items", image: "ny_ic_streamline_fragile_solid" }
  , { title: "Illegal items are prohibited", image: "ny_ic_prohibited" }
  ]


deliveryDetailPopupView :: forall w. (Action -> Effect Unit) -> ST.ParcelDeliveryScreenState -> PrestoDOM (Effect Unit) w
deliveryDetailPopupView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , accessibility DISABLE
    ]
    [ PopUpModal.view (push <<< DeliveryDetailAction) (deliveryPickupDetialsModalConfig state) ]

decodeAddress' :: ST.Address -> String
decodeAddress' address =
  if (DA.all isNothing [address.city, address.area, address.country, address.building, address.door, address.street, address.city, address.areaCode, address.ward]) then
      ""
    else if (DS.trim (fromMaybe "" address.city) == "" && DS.trim (fromMaybe "" address.area) == "" && DS.trim (fromMaybe "" address.street) == "" && DS.trim (fromMaybe "" address.door) == "" && DS.trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (DS.trim (fromMaybe "" address.area) == "" && DS.trim (fromMaybe "" address.street) == "" && DS.trim (fromMaybe "" address.door) == "" && DS.trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (DS.trim (fromMaybe "" address.street) == "" && DS.trim (fromMaybe "" address.door) == "" && DS.trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (DS.trim (fromMaybe "" address.door) == "" && DS.trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (DS.trim (fromMaybe "" address.door) == "") then
      ((fromMaybe "" address.building) <> ", " <> (fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else
      ((fromMaybe "" address.door) <> ", " <> (fromMaybe "" address.building) <> ", " <> (fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))