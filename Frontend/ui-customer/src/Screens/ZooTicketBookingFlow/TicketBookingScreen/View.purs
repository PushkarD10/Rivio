module Screens.TicketBookingScreen.View where

import Common.Types.App
import Screens.TicketBookingScreen.ComponentConfig

import Animation as Anim
import Animation.Config (translateYAnimConfig, translateYAnimMapConfig, removeYAnimFromTopConfig)
import Common.Types.App as Common
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Data.Array as DA
import Data.Foldable (or)
import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth)
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), decodeError, fetchAndUpdateCurrentLocation, getAssetsBaseUrl, getCurrentLocationMarker, getLocationName, getNewTrackingId, getPreviousVersion, getSearchType, parseFloat, storeCallBackCustomer)
import JBridge as JB
import Prelude (Unit, bind, const, pure, unit, ($), (&&), (/=), (<<<), (<>), (==), map, show, (||))
import PrestoDOM (FlexWrap(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), afterRender, alignParentBottom, background, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, scrollView, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import Screens.TicketBookingScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Screens.TicketBookingScreen.ComponentConfig 
import Resources.Constants -- TODO:: Replace these constants with API response


screen :: ST.TicketBookingScreenState -> Screen Action ST.TicketBookingScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "ChooseLanguageScreen"
  , globalEvents : []
  , eval
  }

view :: forall w . (Action -> Effect Unit) -> ST.TicketBookingScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $ relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
   ][
    linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    , margin $ MarginBottom if bookingStatus then 0 else 84
    ][ if bookingStatus then linearLayout[visibility GONE][] else GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
    , linearLayout
      [ height $ V 1 
      , width MATCH_PARENT
      , background Color.grey900][]
    , scrollView[
        height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.white900
      ][  linearLayout
          [height MATCH_PARENT
          , width MATCH_PARENT
          , gravity CENTER
          , orientation VERTICAL
          , padding $ PaddingBottom 20]
          ( if state.props.currentStage == ST.DescriptionStage then 
          [ imageView
            [ height $ V 370
            , width MATCH_PARENT
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_animal_1" 
            , margin $ MarginBottom 15
            ]
          , descriptionView state push ]
        else if state.props.currentStage == ST.ChooseTicketStage then [ chooseTicketsView state push ]
        else if state.props.currentStage == ST.BookingConfirmationStage then [ bookingStatusView state push state.props.paymentStatus ]
        else if state.props.currentStage == ST.ViewTicketStage then [ ticketInfoView state push ]
        else [])]
    ]
    , if bookingStatus then actionButtons state push state.props.paymentStatus else 
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , alignParentBottom "true,-1"
      , margin $ MarginBottom 16
      , orientation VERTICAL
      , background Color.white900
      ][  linearLayout  
          [height $ V 1
          , width MATCH_PARENT
          , margin $ MarginBottom 16
          , background Color.grey900][]
        , PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)
      ]
  ]
  where bookingStatus = state.props.currentStage == ST.BookingConfirmationStage

descriptionView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
descriptionView state push = 
  linearLayout[
    height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginHorizontal 16 16 
  ][  textView $ 
      [ text "Zoological Garden, Alipore"
      , color Color.black900
      ] <> FontStyle.h3 TypoGraphy 
    , textView $ 
      [ text "The Zoological Garden, Alipore is India's oldest formally stated zoological park (as opposed to royal and British menageries) and a big tourist attraction in Kolkata, West Bengal."
      , color Color.black800 
      ] <> FontStyle.body1 TypoGraphy 
    , locationView state push 
    , feeBreakUpView state push  
  ]


locationView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
locationView state push =
  linearLayout
  [ height WRAP_CONTENT 
  , width MATCH_PARENT
  , margin $ MarginTop 24
  , orientation VERTICAL
  ][  textView $ 
      [ text "Location"
      , color Color.black800
      , margin $ MarginBottom 8
      ] <> FontStyle.subHeading1 TypoGraphy
    , imageView
      [ height $ V 200
      , width MATCH_PARENT
      , cornerRadius 8.0 
      , layoutGravity "center_horizontal"
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_zoo_alipore_map" 
      ]
  ]

feeBreakUpView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
feeBreakUpView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 8.0 
  , background Color.blue600
  , orientation VERTICAL 
  , padding $ Padding 20 20 20 20
  , margin $ MarginTop 24
  ][  textView $
      [ text "Fee & Zoo Hours"
      , color Color.black800
      ] <> FontStyle.subHeading1 TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ](map ( \item -> 
              linearLayout
                [ height WRAP_CONTENT
                , margin $ MarginTop 16
                , width MATCH_PARENT
                ][  imageView
                  [ height $ V 24 
                  , width $ V 24
                  , margin $ MarginRight 16
                  , imageWithFallback $ fetchImage FF_COMMON_ASSET item.image 
                  ]
                , linearLayout
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , orientation VERTICAL 
                  ][  textView $
                      [ text item.headingText 
                      , color Color.black800
                      ] <> FontStyle.body6 TypoGraphy
                    , linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , orientation VERTICAL
                      ](map ( \subTextItem -> 
                                  textView $ 
                                  [ text subTextItem 
                                  , color Color.black700 
                                  , margin $ MarginTop 6
                                  ] <> FontStyle.body1 TypoGraphy ) item.subtext
                        )
                    ]                             
                  ]
      ) (feeBreakUpArray)
    )

  ]

feeBreakUpArray :: Array {headingText :: String , subtext :: Array String, image :: String}
feeBreakUpArray = [{
  headingText : "Zoo Timings",
  subtext : ["Garden Time: 9:00 AM to 5:30 PM","Aquarium Time: 10:00 AM to 5:30 PM" ],
  image : "ny_ic_timing"
  },
  {
  headingText : "Entrance Fee",
  subtext : ["Up to the age of 5 years: ₹20" , "Visitors above the age of 5 years - ₹50"],
  image : "ny_ic_entry"

  },
  {
  headingText : "Aquarium Fee",
  subtext : ["Up to the age of 5 years - ₹10" , "Visitors above the age of 5 years - ₹20"],
  image : "ny_ic_aquarium"

  },
  {
  headingText : "Zoo Timings",
  subtext : ["You’ll be charged ₹250"],
  image : "ny_ic_videography"
  }
  ]

chooseTicketsView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chooseTicketsView state push = 
  linearLayout[
    height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , margin $ MarginHorizontal 16 16
  , orientation VERTICAL
  ][  textView $ 
      [ text "Date of Visit"
      , color Color.black900
      , margin $ MarginBottom 9
      ] <> FontStyle.subHeading1 TypoGraphy 
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , cornerRadius 8.0 
      , background Color.white900
      , stroke $ "1," <> if state.props.validDate || (state.data.dateOfVisit == "") then Color.grey900 else Color.red
      , padding $ Padding 20 15 20 15
      ][  imageView
          [ height $ V 22 
          , width $ V 22
          , margin $ MarginRight 8
          , layoutGravity "bottom"
          , onClick (\action -> do
                _ <- push action
                JB.datePicker "" push $ DatePicker "DATE_OF_VISIT"
          ) (const NoAction)
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_calendar" 
          ]
        , textView $ 
          [ text if state.data.dateOfVisit == "" then "Select Date Of Visit" else state.data.dateOfVisit
          , color Color.black800
          ] <> FontStyle.h3 TypoGraphy
      ]
    , textView $
      [ text "Tickets are available next day onwards" -- Tickets are available for upto 90 days in advance
      , visibility if state.props.validDate || state.data.dateOfVisit == "" then GONE else VISIBLE
      , color Color.red 
      ] <> FontStyle.tags TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ MarginTop 20
      ](map (\item -> ticketInputView { ticketType : item.title , ticketID : item.ticketID, ticketOption : item.ticketOption , isExpanded : item.isExpanded } push ) 
        [ {title : "Zoo Entry", ticketID : ticketEntryId , ticketOption : zooEntryTicketOption state, isExpanded : state.data.zooEntry.availed}
        , {title : "Aquarium Entry", ticketID : ticketAquariumId , ticketOption : aquariumEntryTicketOption state , isExpanded : state.data.aquariumEntry.availed}
        , {title : "Photo / Videography", ticketID : ticketCamId , ticketOption : photoVideographyTicketOption state , isExpanded : state.data.photoOrVideoGraphy.availed}
          ])
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity BOTTOM
      ][  imageView
          [ height $ V 16
          , width $ V 16 
          , layoutGravity "center_vertical"
          , margin $ MarginRight 8
          , onClick push $ const ToggleTermsAndConditions
          , imageWithFallback $ fetchImage FF_COMMON_ASSET (if state.props.termsAndConditionsSelected then "ny_ic_checked" else "ny_ic_unchecked")
          ]
        , textView $ 
          [ text "I agree to the"
          , color Color.black800
          ] <> FontStyle.body1 TypoGraphy
        , textView $ 
          [ text " Terms & Conditions"
          , color Color.blue900
          , onClick (\action -> do
                  _<- push action
                  _ <- JB.openUrlInApp $ "https://wbza.org/onlineticket/eticket/tnconlineticket"
                  pure unit
                  ) (const NoAction)
          ] <> FontStyle.body1 TypoGraphy
      ]
    , textView $ 
      [ textFromHtml " &#8226;&ensp; Cancellation of tickets is not applicable"
      , margin $ MarginTop 13
      , color Color.black700
      ] <> FontStyle.tags TypoGraphy
  ]

ticketInputView config push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 8.0 
  , background Color.blue600 
  , orientation VERTICAL
  , padding $ Padding 20 20 20 20
  , margin $ MarginBottom 20
  ][ linearLayout[
      height WRAP_CONTENT
    , width MATCH_PARENT
    , onClick push $ const (ToggleTicketOption config.ticketID)
    ][ textView $
        [ text config.ticketType --"Zoo Entry"
        , color Color.black800
        ] <> FontStyle.h2 TypoGraphy
      , linearLayout
        [weight 1.0][]
      , imageView 
        [ height $ V 20 
        , width $ V 20 
        , imageWithFallback $ fetchImage FF_COMMON_ASSET if config.isExpanded then "ny_ic_checked" else "ny_ic_unchecked" 
        ]
      ]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , visibility if config.isExpanded then VISIBLE else GONE
        , orientation VERTICAL
        ](map (\item  -> incrementDecrementView item config.ticketID push) (config.ticketOption))
      
  ]

zooEntryTicketOption :: forall w. ST.TicketBookingScreenState -> Array  {title :: String, currentValue :: Int, subcategory :: String}
zooEntryTicketOption state = [{
  title : "Adult Ticket (₹200 per person)",
  currentValue : state.data.zooEntry.adult ,
  subcategory : "ADULT"
  },
  {
  title : "Child Ticket (₹50 per person)",
  currentValue : state.data.zooEntry.child ,
  subcategory : "CHILD"
  }]

aquariumEntryTicketOption :: forall w. ST.TicketBookingScreenState -> Array  {title :: String, currentValue :: Int, subcategory :: String}
aquariumEntryTicketOption state = [{
  title : "Adult Ticket (20 per person)",
  currentValue : state.data.aquariumEntry.adult ,
  subcategory : "ADULT"
  },
  {
  title : "Child Ticket (₹10 per person)",
  currentValue : state.data.aquariumEntry.child ,
  subcategory : "CHILD"
  }]

photoVideographyTicketOption :: forall w. ST.TicketBookingScreenState -> Array  {title :: String, currentValue :: Int , subcategory :: String}
photoVideographyTicketOption state = [{
  title : "Devices (₹250 per person)",
  currentValue : state.data.photoOrVideoGraphy.noOfDevices ,
  subcategory : "DEVICES"
  }]


incrementDecrementView :: forall w. {title :: String, currentValue :: Int, subcategory :: String} -> String -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
incrementDecrementView config ticketID push  = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginTop 24
  ][  textView $
      [ text config.title 
      , color Color.black800
      , margin $ MarginBottom 8
      ] <> FontStyle.subHeading1 TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , padding $ Padding 4 4 4 4
      , cornerRadius 8.0
      , background Color.white900
      , stroke $ "1," <> Color.grey900
      ][  textView $
          [ background Color.grey700
          , text "-"
          , gravity CENTER
          , cornerRadius 4.0
          , width WRAP_CONTENT
          , padding $ Padding 28 1 28 7
          , onClick push $ const (DecrementTicket ticketID config.subcategory)
          , height WRAP_CONTENT
          ] <> FontStyle.body10 TypoGraphy
        , textView $
          [ background Color.white900
          , text $ show config.currentValue
          , height WRAP_CONTENT
          , color Color.black800
          , weight 1.0
          , gravity CENTER
          ] <> FontStyle.body13 TypoGraphy
        , textView $
          [ background Color.black900
          , text "+"
          , color Color.yellow900
          , padding $ Padding 28 1 28 7
          , cornerRadius 4.0
          , onClick push $ const (IncrementTicket ticketID config.subcategory)
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER
          ] <> FontStyle.body10 TypoGraphy

      ]

  ]

ticketInfoView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
ticketInfoView state push = 
  linearLayout[][]

bookingStatusView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> Common.PaymentStatus -> PrestoDOM (Effect Unit) w
bookingStatusView state push paymentStatus = 
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , padding $ PaddingTop 20
  , background "#E2EAFF"
  ][ linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER
      ][  paymentStatusHeader state push paymentStatus
        , if paymentStatus == Common.Failed then linearLayout[visibility GONE][] else bookingStatusBody state push paymentStatus
      ]
  ]

copyTransactionIdView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
copyTransactionIdView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , gravity CENTER
  , onClick push $ const Copy
  ][  commonTV push "TransactionID" Color.black700 (FontStyle.body3 TypoGraphy) 0 CENTER NoAction
    , textView $ 
      [ text state.data.transactionId
      , margin $ MarginLeft 3
      , color Color.black700
      , padding $ PaddingBottom 1
      ] <> FontStyle.h3 TypoGraphy
  , imageView
     [ width $ V 16
     , height $ V 16
     , margin $ MarginLeft 3
     , imageWithFallback $ fetchImage FF_ASSET "ny_ic_copy"
     ] 
  ]

bookingStatusBody :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> Common.PaymentStatus ->  PrestoDOM (Effect Unit) w
bookingStatusBody state push paymentStatus = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , weight 1.0
  , orientation VERTICAL
  , margin $ Margin 16 16 16 16
  ][ scrollView
      [ width MATCH_PARENT
      , height MATCH_PARENT
      ][ linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , gravity CENTER
          , orientation VERTICAL
          , padding $ Padding 10 10 10 10
          , cornerRadius 8.0
          , background Color.white900
          ][ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              ][ imageView
                  [ width $ V 24
                  , height $ V 24
                  , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_ticket" 
                  , margin $ MarginRight 4
                  ]
                , commonTV push state.data.zooName Color.black900 (FontStyle.subHeading1 TypoGraphy) 0 LEFT NoAction
              ]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          ](DA.mapWithIndex ( \index item ->  keyValueView state item.key item.val index) state.data.keyValArray)
          , if paymentStatus == Common.Success then PrimaryButton.view (push <<< ShareTicketAC) (shareTicketButtonConfig state) else linearLayout[visibility GONE][]
          ]
      ]
  ]

actionButtons :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> Common.PaymentStatus -> PrestoDOM (Effect Unit) w
actionButtons state push paymentStatus = 
  linearLayout
  [ width MATCH_PARENT
  , gravity CENTER
  , orientation VERTICAL
  , padding $ PaddingBottom 20
  , background Color.white900
  , alignParentBottom "true,-1"
  ][ linearLayout
      [ width MATCH_PARENT
      , height $ V 1
      , background Color.grey900
      , margin $ MarginBottom 10
      , visibility GONE
      ][]
   , if paymentStatus /= Common.Pending then PrimaryButton.view (push <<< ViewTicketAC) (viewTicketButtonConfig primaryButtonText) else linearLayout[visibility GONE][]
   , commonTV push secondaryButtonText Color.black900 (FontStyle.subHeading1 TypoGraphy) 5 CENTER NoAction
  ]
  where primaryButtonText = case paymentStatus of
                              Common.Success -> "View Ticket"
                              Common.Failed -> "Try Again"
                              _ -> ""
        secondaryButtonText = case paymentStatus of
                              Common.Success -> "Go Home"
                              _ -> "Go Back"

paymentStatusHeader :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> Common.PaymentStatus -> PrestoDOM (Effect Unit) w
paymentStatusHeader state push paymentStatus = 
  let transcationConfig = getTransactionConfig paymentStatus
  in
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , gravity CENTER
    ][ imageView
        [ width $ V 65
        , height $ V 65
        , imageWithFallback transcationConfig.image
        ]
      , commonTV push transcationConfig.title Color.black900 (FontStyle.h2 TypoGraphy) 14 CENTER NoAction
      , commonTV push transcationConfig.statusTimeDesc Color.black700 (FontStyle.body3 TypoGraphy) 5 CENTER NoAction
      , if paymentStatus == Common.Failed then copyTransactionIdView state push else linearLayout[visibility GONE][]
    ]

commonTV :: forall w. (Action -> Effect Unit) -> String -> String -> (forall properties. (Array (Prop properties))) -> Int -> Gravity -> Action -> PrestoDOM (Effect Unit) w
commonTV push text' color' fontStyle marginTop gravity' action =
  textView $
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text text'
  , color color'
  , gravity gravity'
  , onClick push $ const action
  , margin $ MarginTop marginTop
  ] <> fontStyle

keyValueView :: ST.TicketBookingScreenState -> String -> String -> Int -> forall w . PrestoDOM (Effect Unit) w
keyValueView state key value index = 
  linearLayout 
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , orientation VERTICAL
  ][ linearLayout
      [ width MATCH_PARENT
      , margin $ Margin 5 12 5 12
      , height $ V 1
      , background Color.grey700
      ][]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginHorizontal 5 5
      ][ textView $ 
        [ text key
        , margin $ MarginRight 8
        , color Color.black700
        ] <> FontStyle.body3 TypoGraphy
      , linearLayout
        [ width MATCH_PARENT
        , gravity RIGHT
        ][ if index == 1 then bookingForView state else 
           textView $ 
            [ text value
            , color Color.black800
            ] <> FontStyle.body6 TypoGraphy
          ]
      ]
  ]

bookingForView :: forall w. ST.TicketBookingScreenState -> PrestoDOM (Effect Unit) w
bookingForView state = 
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    ](map ( \item -> 
      textView $
      [ text item
      , padding $ Padding 6 4 6 4
      , cornerRadius 20.0
      , margin $ MarginLeft 5
      , background Color.blue600
      ] <> FontStyle.tags TypoGraphy
    ) state.data.bookedForArray)

getTransactionConfig :: Common.PaymentStatus -> {image :: String, title :: String, statusTimeDesc :: String}
getTransactionConfig status = 
  case status of
    Common.Success -> {image : fetchImage FF_COMMON_ASSET "ny_ic_green_tick", statusTimeDesc : "Your ticket has been generated below", title : "Your booking is Confirmed!"}
    Common.Pending -> {image : fetchImage FF_COMMON_ASSET "ny_ic_transaction_pending", statusTimeDesc : "Please check back in a few minutes.", title : "Your booking is Pending!"}
    Common.Failed  -> {image : fetchImage FF_COMMON_ASSET "ny_ic_payment_failed", statusTimeDesc : "Please retry booking.", title : "Booking Failed!"}
    Common.Scheduled  -> {image : fetchImage FF_COMMON_ASSET "ny_ic_pending", statusTimeDesc : "", title : ""}