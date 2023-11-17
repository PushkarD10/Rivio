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
import Engineering.Helpers.Commons (screenWidth, flowRunner)
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), decodeError, fetchAndUpdateCurrentLocation, getAssetsBaseUrl, getCurrentLocationMarker, getLocationName, getNewTrackingId, getPreviousVersion, getSearchType, parseFloat, storeCallBackCustomer)
import JBridge as JB
import Prelude (Unit, discard, void, bind, const, pure, unit, ($), (&&), (/=), (<<<), (<>), (==), map, show, (||), show, (-))
import PrestoDOM (FlexWrap(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), shimmerFrameLayout, afterRender, alignParentBottom, background, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, scrollView, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, clickable, id)
import PrestoDOM.Animation as PrestoAnim
import Screens.TicketBookingScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Screens.TicketBookingScreen.ComponentConfig 
import Resources.Constants -- TODO:: Replace these constants with API response
import Engineering.Helpers.Commons (screenWidth, convertUTCtoISC, getNewIDWithTag)
import Services.API (BookingStatus(..), TicketPlaceResp(..))
import Animation (fadeInWithDelay, translateInXBackwardAnim, translateInXBackwardFadeAnimWithDelay, translateInXForwardAnim, translateInXForwardFadeAnimWithDelay)
import Halogen.VDom.DOM.Prop (Prop)
import Data.Array ((..))
import Data.Maybe (fromMaybe, Maybe(..))
import Debug
import Effect.Aff (launchAff)
import Types.App (defaultGlobalState)
import Control.Monad.Except.Trans (runExceptT , lift)
import Control.Transformers.Back.Trans (runBackT)
import Services.Backend as Remote
import Data.Either (Either(..))
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Class (liftEffect)

screen :: ST.TicketBookingScreenState -> Screen Action ST.TicketBookingScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "TicketBookingScreen"
  , globalEvents : [getPlaceDataEvent]
  , eval :
    \action state -> do
        let _ = spy "ZooTicketBookingFlow action " action
        let _ = spy "ZooTicketBookingFlow state " state
        eval action state
  }
  where
  getPlaceDataEvent push = do
    void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ getPlaceDataEvent' push
    pure (pure unit)

  getPlaceDataEvent' push = do
    if initialState.props.currentStage == ST.DescriptionStage then do
      placesResp <- Remote.getTicketPlacesBT ""
      lift $ lift $ doAff do liftEffect $ push $ UpdatePlacesData (Just placesResp)
    else lift $ lift $ doAff do liftEffect $ push $ UpdatePlacesData Nothing

view :: forall w . (Action -> Effect Unit) -> ST.TicketBookingScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $ relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    ]
    [ shimmerView state
    , linearLayout 
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , orientation VERTICAL
        , visibility if (state.props.currentStage == ST.DescriptionStage && state.props.showShimmer) then GONE else VISIBLE
        , margin $ MarginBottom if state.props.currentStage == ST.BookingConfirmationStage then 0 else 84
        ]
        [ headerView state push
        , linearLayout
          [ height $ V 1 
          , width MATCH_PARENT
          , background Color.grey900
          ] []
        , separatorView Color.greySmoke
        , scrollView
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , background Color.white900
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                , orientation VERTICAL
                , padding $ PaddingBottom 20
                ]
                (mainView state push)
            ]
        ]
    , actionsView state push
    ]
  where
  actionsView state push =
    case state.props.currentStage of
      ST.BookingConfirmationStage -> bookingConfirmationActions state push state.props.paymentStatus
      ST.TicketInfoStage -> linearLayout [ visibility GONE ] []
      ST.DescriptionStage -> generalActionButtons state push
      _ -> generalActionButtons state push

  headerView state push =
    case state.props.currentStage of
      ST.BookingConfirmationStage -> linearLayout [ visibility GONE ][]
      _ -> GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)

  mainView state push =
    if (state.props.currentStage == ST.DescriptionStage) 
      then
        case state.data.placeInfo of
          Just placeInfo -> descriptionStateMainView state push placeInfo
          Nothing -> [ noDataView state push ]
    else if (state.props.currentStage == ST.ChooseTicketStage) then [ chooseTicketsView state push ]
    else if (state.props.currentStage == ST.BookingConfirmationStage) then [ bookingStatusView state push state.props.paymentStatus ]
    else if (state.props.currentStage == ST.ViewTicketStage) then [ ticketsListView state push ]
    else if (state.props.currentStage == ST.TicketInfoStage) then [ individualBookingInfoView state push ]
    else []

  descriptionStateMainView state push placeInfo =
      [ imageView
          [ height $ V 370
          , width MATCH_PARENT
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_animal_1" 
          , margin $ MarginBottom 15
          ]
      , descriptionView state push placeInfo
      ]
  
  noDataView state push =
    linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER
      , margin $ MarginHorizontal 16 16
      ]
      [ textView $
        [ text "No ticketing zones in this area"
        , color Color.black900
        ] <> FontStyle.h3 TypoGraphy 
      ]

shimmerView :: forall w . ST.TicketBookingScreenState -> PrestoDOM (Effect Unit) w
shimmerView state =
  shimmerFrameLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , visibility if state.props.showShimmer then VISIBLE else GONE
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height (V 235)
        , margin (Margin 16 15 16 0)
        , background Color.greyDark
        , cornerRadius 16.0
        ] []
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , margin (MarginTop 258)
        ] (DA.mapWithIndex 
            (\index item ->
                linearLayout
                  [ width MATCH_PARENT
                  , height (V 60)
                  , margin (Margin 16 16 16 0)
                  , cornerRadius 12.0
                  , background Color.greyDark
                  ][]
            ) (1 .. 7)
          )
    ]

generalActionButtons :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
generalActionButtons state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , alignParentBottom "true,-1"
    , margin $ MarginBottom 16
    , orientation VERTICAL
    , background Color.white900
    ]
    [ linearLayout  
        [ height $ V 1
        , width MATCH_PARENT
        , margin $ MarginBottom 16
        , background Color.grey900
        ][]
    , PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)
    ]

descriptionView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> TicketPlaceResp -> PrestoDOM (Effect Unit) w
descriptionView state push (TicketPlaceResp placeInfo) = 
  linearLayout[
    height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginHorizontal 16 16 
  ][  textView $ 
      [ text placeInfo.name
      , color Color.black900
      ] <> FontStyle.h3 TypoGraphy 
    , textView $ 
      [ text (fromMaybe placeInfo.name placeInfo.description)
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
  title : "Devices (₹250 per device)",
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

ticketsListView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
ticketsListView state push = 
  linearLayout
  [ width $ MATCH_PARENT
  , height $ MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginTop 12
  , padding $ PaddingHorizontal 16 16
  ][ if DA.null state.props.ticketBookingList.booked then linearLayout[][] else ticketsCardListView state push state.props.ticketBookingList.booked "Booked Trips"
  ,  if DA.null state.props.ticketBookingList.pendingBooking then linearLayout[][] else ticketsCardListView state push state.props.ticketBookingList.pendingBooking "Pending Payment"
  ]

ticketsCardListView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> Array ST.TicketBookingItem -> String -> PrestoDOM (Effect Unit) w
ticketsCardListView state push list title =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][ textView $
     [ text title
     , width $ WRAP_CONTENT
     , height $ WRAP_CONTENT
     , margin $ MarginBottom 12
     , color Color.black900
     ] <> FontStyle.subHeading1 TypoGraphy
  ,  linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , orientation VERTICAL
     , margin $ MarginBottom 12
     ](map (\item -> ticketInfoCardView state push item) list)
  ]

ticketInfoCardView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> ST.TicketBookingItem -> PrestoDOM (Effect Unit) w
ticketInfoCardView state push booking = 
  linearLayout
  [ width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  , padding $ Padding 16 16 16 16
  , orientation HORIZONTAL
  , cornerRadius 8.0
  , stroke $ "1," <> Color.grey700
  , margin $ MarginBottom 12
  ][  imageView
      [ imageWithFallback $ getTicketStatusImage booking.status
      , width $ V 20
      , height $ V 20
      , margin $ Margin 0 4 12 0
      ]
    , linearLayout
      [ width $ MATCH_PARENT
      , height $ WRAP_CONTENT
      , orientation VERTICAL
      ][ textView $ 
         [ text booking.ticketPlaceName
         , width MATCH_PARENT
         , height WRAP_CONTENT
         , margin $ MarginBottom 8
         , color Color.black900
         ] <> FontStyle.subHeading1 TypoGraphy
      ,  textView
         [ text (convertUTCtoISC booking.visitDate "Do MMM, YYYY")
         , width MATCH_PARENT
         , height WRAP_CONTENT
         , margin $ MarginBottom 12
         ]
      ,  linearLayout
         [ width WRAP_CONTENT
         , height WRAP_CONTENT
         , orientation HORIZONTAL
         , gravity CENTER_VERTICAL
         , onClick push $ const $ GetBookingInfo booking.shortId
         , clickable true
         ][ textView 
            [ text "View"
            , color Color.blue900
            , margin $ MarginRight 8]
          , imageView
            [ width $ V 10
            , height $ V 8
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_blue_arrow"
            ]
         ]
      ]
  ]

separatorView :: forall w. String -> PrestoDOM (Effect Unit) w
separatorView color =
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , background color
  ][]

getTicketStatusImage :: BookingStatus -> String
getTicketStatusImage status = fetchImage FF_ASSET $ case status of 
  Pending -> "ny_ic_pending"
  Booked -> "ny_ic_success"
  Failed -> "ny_ic_failed"


individualBookingInfoView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
individualBookingInfoView state push =
  PrestoAnim.animationSet
  [ translateInXForwardAnim true , translateInXForwardAnim true] $ linearLayout
  [ height WRAP_CONTENT
  , width $ MATCH_PARENT
  , background Color.white900
  , orientation VERTICAL
  , padding $ Padding 16 20 16 20
  , onBackPressed push $ const BackPressed
  , afterRender push (const AfterRender)
  ][ zooTicketView state push
  ,  carouselDotView state push
  ]

noDataView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
noDataView state push =
  linearLayout
  [ width $ MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , background $ Color.blue600
  , gravity CENTER
  ][ 
  ]


zooTicketView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
zooTicketView state push =
  let activeItem = state.props.activeListItem
  in
  PrestoAnim.animationSet
  [ translateInXForwardAnim true , translateInXForwardAnim true] $
  linearLayout
  [ height $ WRAP_CONTENT
  , width $ MATCH_PARENT
  , background $ getTicketBackgroundColor activeItem.ticketServiceName
  , orientation VERTICAL
  , padding $ Padding 16 24 16 24
  , cornerRadius 8.0
  ][ ticketHeaderView state push (getPlaceColor activeItem.ticketServiceName) (getInfoColor activeItem.ticketServiceName)
  ,  ticketImageView state push
  ,  bookingInfoView state push
  ,  shareTicketView state push
  ]

getTicketBackgroundColor :: String -> String
getTicketBackgroundColor ticketServiceName = case ticketServiceName of
  "Entrance" -> Color.black900
  "VideoPhotography" -> Color.yellow800
  "Aquarium" -> Color.blue600
  _ -> Color.grey900

getShareButtonIcon :: String -> String
getShareButtonIcon ticketServiceName = case ticketServiceName of
  "Entrance" -> "ny_ic_share_unfilled_white"
  _ -> "ny_ic_share_unfilled_black"

getShareButtonColor :: String -> String
getShareButtonColor ticketServiceName = case ticketServiceName of
  "Entrance" -> Color.white900
  _ -> Color.black900

getPlaceColor :: String -> String
getPlaceColor ticketServiceName = case ticketServiceName of
  "Entrance" -> Color.white900
  "VideoPhotography" -> Color.black800
  "Aquarium" -> Color.black800
  _ -> Color.grey900

getInfoColor :: String -> String
getInfoColor ticketServiceName = case ticketServiceName of
  "Entrance" -> Color.white900
  "VideoPhotography" -> Color.black900
  "Aquarium" -> Color.black900
  _ -> Color.grey900

ticketHeaderView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> String -> String -> PrestoDOM (Effect Unit) w
ticketHeaderView state push placeColor infoColor  =
  let activeItem = state.props.activeListItem
  in
  linearLayout
  [ width $ MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  ][ imageView
     [ margin $ Margin 0 0 10 0
     , width $ V 24
     , height $ V 24
     , imageWithFallback $ getTicketImage activeItem.ticketServiceName 
     ]
  ,  linearLayout
     [ width $ MATCH_PARENT
     , height WRAP_CONTENT
     , orientation VERTICAL
     ][ tvView state.props.selectedBookingInfo.ticketPlaceName placeColor (MarginBottom 0) (FontStyle.body3 TypoGraphy)
      , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        ][ tvView (convertUTCtoISC state.props.selectedBookingInfo.visitDate "Do MMM, YYYY") infoColor (MarginBottom 0) (FontStyle.subHeading1 TypoGraphy)
        ,  dotView placeColor (Margin 10 10 10 10) 5
        ,  tvView ("Total : ₹ " <>  show state.props.selectedBookingInfo.amount) infoColor (MarginBottom 0) (FontStyle.subHeading1 TypoGraphy)
        ]
     ]
  
  ]

getTicketImage :: String -> String
getTicketImage ticketServiceName = case ticketServiceName of
  "Entrance" -> fetchImage FF_ASSET "ny_ic_ticket"
  _ -> fetchImage FF_ASSET "ny_ic_ticket_black"

carouselDotView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
carouselDotView state push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , margin $ MarginTop 12
  ] (DA.mapWithIndex (\index item -> if index == state.props.activeIndex then (dotView Color.black900 (Margin 2 2 2 2) 6) else (dotView Color.grey900 (Margin 2 2 2 2) 6) ) state.props.selectedBookingInfo.services)

ticketImageView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
ticketImageView state push =
  let activeItem = state.props.activeListItem
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , gravity CENTER
  , margin $ MarginVertical 24 24
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER
      ][  linearLayout
          [ weight 1.0
          , height WRAP_CONTENT
          , gravity CENTER
          ][ imageView
              [ width $ V 24
              , height $ V 24
              , imageWithFallback $ getLeftButtonForSlider state.props.activeListItem.ticketServiceName state.props.leftButtonDisable
              , onClick push $ const DecrementSliderIndex
              , visibility $ if state.props.leftButtonDisable then INVISIBLE else VISIBLE
              , clickable $ if state.props.leftButtonDisable then false else true
              ]
          ]
        , linearLayout
          [ weight 1.0
          , height WRAP_CONTENT
          , gravity CENTER
          ][ imageView
            [ width $ V 192
            , height $ V 192
            , id $ getNewIDWithTag "ticketQRView"
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left_white"
            , afterRender push (const (TicketQRRendered (getNewIDWithTag "ticketQRView") activeItem.ticketServiceShortId ))
            ]
          ]
        , linearLayout
          [ weight 1.0
          , height WRAP_CONTENT
          , gravity CENTER
          ][ imageView
            [ width $ V 24
            , height $ V 24
            , imageWithFallback $ getRightButtonForSlider state.props.activeListItem.ticketServiceName state.props.rightButtonDisable
            , onClick push $ const IncrementSliderIndex
            , visibility $ if state.props.rightButtonDisable then INVISIBLE else VISIBLE
            , clickable $ if state.props.rightButtonDisable then false else true
            ]
          ]
      ]
    , tvView (getTextForQRType activeItem.ticketServiceName) (getInfoColor activeItem.ticketServiceName) (MarginVertical 10 10) (FontStyle.subHeading1 TypoGraphy)
    , pillView state push (getPillBackgroundColor activeItem.ticketServiceName) (getPillInfoColor activeItem.ticketServiceName)
  ]

getTextForQRType :: String -> String
getTextForQRType ticketServiceName = case ticketServiceName of
  "Entrance" -> "Zoo Entry QR"
  "VideoPhotography" -> "Photo / VideoGraphy Entry QR"
  "Aquarium" -> "Aquarium Entry QR"
  _ -> ""
 
getPillBackgroundColor :: String -> String
getPillBackgroundColor ticketServiceName = case ticketServiceName of
  "Entrance" -> Color.black6000
  "VideoPhotography" -> Color.yellow900
  "Aquarium" ->  Color.blue900
  _ -> ""

getPillInfoColor :: String -> String
getPillInfoColor ticketServiceName = case ticketServiceName of
  "Entrance" -> Color.grey900
  "VideoPhotography" -> Color.black800
  "Aquarium" ->  Color.white900
  _ -> ""
  
getLeftButtonForSlider :: String -> Boolean -> String
getLeftButtonForSlider ticketServiceName buttonDisabled = case ticketServiceName of
  "Entrance" -> if buttonDisabled then fetchImage FF_ASSET "ny_ic_chevron" else fetchImage FF_ASSET "ny_ic_chevron_left_white"
  _ -> if buttonDisabled then fetchImage FF_ASSET "ny_ic_chevron" else fetchImage FF_ASSET "ny_ic_chevron_left"

getRightButtonForSlider :: String -> Boolean -> String
getRightButtonForSlider ticketServiceName buttonDisabled = case ticketServiceName of
  "Entrance" -> if buttonDisabled then fetchImage FF_ASSET "ny_ic_chevron" else fetchImage FF_ASSET "ny_ic_chevron_right_white"
  _ -> if buttonDisabled then fetchImage FF_ASSET "ny_ic_chevron" else fetchImage FF_ASSET "ny_ic_chevron_right"

pillView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> String -> String -> PrestoDOM (Effect Unit) w
pillView state push backgroudColor textColor =
  let activeItem = state.props.activeListItem
      itemLength = (DA.length activeItem.prices) - 1
  in
  linearLayout
  [ width $ WRAP_CONTENT
  , height $ WRAP_CONTENT
  , orientation HORIZONTAL
  , background backgroudColor
  , cornerRadius 30.0
  , padding $ Padding 16 6 16 6
  ] (DA.mapWithIndex (\index item ->
      linearLayout
      [ width $ WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      ]([  tvView (show item.numberOfUnits <> " " <> show item.attendeeType) textColor (MarginBottom 0) (FontStyle.subHeading1 TypoGraphy)
      ] <> if index == itemLength then [] else [dotView (getPillInfoColor activeItem.ticketServiceName) (Margin 2 2 2 2) 5] )
   ) activeItem.prices )

dotView :: forall w. String -> Margin -> Int -> PrestoDOM (Effect Unit) w
dotView color layMargin size =
  linearLayout
  [ width $ V size
  , height $ V size
  , background color
  , cornerRadius 30.0
  , margin $ layMargin
  ][]

tvView :: forall w. String -> String -> Margin -> (forall properties. (Array (Prop properties))) -> PrestoDOM (Effect Unit) w
tvView textString textColor textMargin fontSt = 
  textView
  ([ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text textString
  , color textColor
  , margin textMargin
  , gravity CENTER
  ] <>  fontSt )

bookingInfoView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
bookingInfoView state push =
  let activeItem = state.props.activeListItem
  in
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][  bookingInfoListItemView state "Service ID" activeItem.ticketServiceShortId
    , separatorView (getSeparatorColor activeItem.ticketServiceName)
    , bookingInfoListItemView state "Zoo Entry" (show activeItem.amount)
    , separatorView (getSeparatorColor activeItem.ticketServiceName)
    , bookingInfoListItemView state "Valid Until" (convertUTCtoISC (fromMaybe "" activeItem.expiryDate) "Do MMM, YYYY")
  ]

getSeparatorColor :: String -> String
getSeparatorColor ticketServiceName = case ticketServiceName of
  "Entrance" -> Color.black6000
  _ -> Color.grey900

bookingInfoListItemView :: forall w.  ST.TicketBookingScreenState -> String -> String -> PrestoDOM (Effect Unit ) w
bookingInfoListItemView state key value =
  let activeItem = state.props.activeListItem
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginVertical 12 12
  ][  textView
      ([ weight 1.0
      , height WRAP_CONTENT
      , text key
      , color $ getPlaceColor activeItem.ticketServiceName
      ] <> FontStyle.body3 TypoGraphy)
    , textView
      ([ weight 1.0
      , height WRAP_CONTENT
      , text value
      , color $ getPlaceColor activeItem.ticketServiceName
      , gravity RIGHT
      ] <> FontStyle.body3 TypoGraphy)
  ]

shareTicketView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
shareTicketView state push =
  linearLayout
  [ width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  ][imageView
    [ height $ V 16
    , width $ V 16
    , imageWithFallback $ fetchImage FF_ASSET $ getShareButtonIcon state.props.activeListItem.ticketServiceName
    , margin $ MarginRight 8
    ]
  , textView $ 
    [ height $ WRAP_CONTENT
    , width $ WRAP_CONTENT
    , text $ "Share"
    , color $ getShareButtonColor state.props.activeListItem.ticketServiceName
    ] <> FontStyle.tags TypoGraphy
  ]

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

bookingConfirmationActions :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> Common.PaymentStatus -> PrestoDOM (Effect Unit) w
bookingConfirmationActions state push paymentStatus = 
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