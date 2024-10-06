module Screens.TicketBookingFlow.BusTicketBooking.View where

import Prelude

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Common.Resources.Constants (zoomLevel)
import Components.ChooseVehicle.View as ChooseVehicle
import Components.GenericHeader.View as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination.View as SourceToDestinationView
import Components.PopUpModal as PopUpModal
import Constants (languageKey)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Function.Uncurried as DFU
import Data.Maybe (maybe, fromMaybe, Maybe(..), isNothing)
import Data.String as DS
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.CommonView (dummyView)
import Helpers.Utils (FetchImageFrom(..), fetchImage, decodeError, storeCallBackCustomer)
import JBridge as JB
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Locale.Utils (getLanguageLocale)
import Mobility.Prelude (boolToVisibility)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..), Shadow(..), background, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, relativeLayout, scrollView, shimmerFrameLayout, onBackPressed, alignParentBottom, singleLine, accessibilityHint,accessibility,accessibilityHint, Accessiblity(..), id, afterRender, layoutGravity, rippleColor, maxLines, ellipsize, onAnimationEnd, shadow)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.Properties (alpha, cornerRadii, lineHeight, minWidth)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Presto.Core.Types.Language.Flow (Flow, doAff, delay)
import Resources.Localizable.EN (getEN)
import Screens.TicketBookingFlow.BusTicketBooking.Controller (Action(..), ScreenOutput, eval)
import Screens.TicketBookingFlow.BusTicketBooking.ComponentConfig (genericHeaderConfig, sourceToDestinationConfig)
import Screens.HomeScreen.ScreenData (dummyRideBooking)
import Screens.Types as ST
import Services.API
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (GlobalState, defaultGlobalState)

busTicketBookingScreen :: ST.BusTicketBookingState -> Screen Action ST.BusTicketBookingState ScreenOutput
busTicketBookingScreen initialState =
  { initialState
  , view
  , name: "BusTicketBookingScreen"
  , globalEvents: [getTicketBookingListEvent]
  , eval:
      \action state -> do
        let _ = spy "BusTicketBookingScreen action " action
        let _ = spy "BusTicketBookingScreen state " state
        eval action state
  }
  where
    getTicketBookingListEvent push =
      if (isNothing initialState.data.ticketDetailsState) then do
        void $ launchAff_ $ void $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do 
          void $ lift $ lift $ EHU.toggleLoader true
          (GetMetroBookingListResp resp)<- Remote.getMetroBookingStatusListBT
          lift $ lift $ doAff do liftEffect $ push $ BusTicketBookingListRespAC resp
          void $ lift $ lift $ EHU.toggleLoader false
        pure $ pure unit
      else pure $ pure unit

view :: forall w. (Action -> Effect Unit) -> ST.BusTicketBookingState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
    linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push $ const GoBack
    , background Color.white900
    ] $
    [ headerView push state 
    , dummyIllustrationView push state
    , scrollView 
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , visibility GONE
      , visibility $ boolToVisibility $ not $ DA.null $ getAllBusTickets state
      ]
      [ recentTicketsView push state
      -- , recentSearchesView push state -- To be done in v2
      ]
    ]

headerView :: forall w. (Action -> Effect Unit) -> ST.BusTicketBookingState -> PrestoDOM (Effect Unit) w
headerView push state =
  relativeLayout
  [ height $ V 220
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.black900
  , gravity CENTER
  , cornerRadii $ Corners 24.0 false false true true
  ]
  [ linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity RIGHT
    , orientation HORIZONTAL
    ]
    [ imageView
      [ width $ V $ 220
      , height MATCH_PARENT
      , gravity RIGHT
      , alignParentBottom "true,-1"
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_ticket_clip_background"
      ]
    ]
  , linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , padding $ Padding 12 12 12 12
      , gravity CENTER_VERTICAL
      ]
      [ imageView
        [ width $ V 32
        , height $ V 32
        , padding $ Padding 4 4 4 4
        , onClick push $ const GoBack
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left_white"
        , rippleColor Color.rippleShade
        ]
      , linearLayout
        [ weight 1.0
        ] []
      , imageView
        [ width $ V 32
        , height $ V 32
        , gravity RIGHT
        , padding $ Padding 4 4 4 4
        , onClick push $ const TicketIconClicked
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_ticket_icon_yellow"
        , rippleColor Color.rippleShade
        ]
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , margin $ Margin 16 16 0 0
      ]
      [ imageView
        [ width $ V 42
        , height $ V 42
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_bus_icon_light_blue"
        , margin $ MarginRight 16
        ]
      , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation VERTICAL
        ]
        [ textView $
          [ text "Book Bus Ticket"
          , color Color.white900
          , singleLine true
          , maxLines 1
          ] <> FontStyle.body25 TypoGraphy
        , textView $
          [ text "Book a one-way instant bus ticket"
          , color Color.black500
          , singleLine true
          , maxLines 1
          , margin $ MarginTop 2
          ] <> FontStyle.tags TypoGraphy
        ]
      ]
    , searchRouteButton push state
    ]
  ]

dummyIllustrationView :: forall w. (Action -> Effect Unit) -> ST.BusTicketBookingState -> PrestoDOM (Effect Unit) w
dummyIllustrationView push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , layoutGravity "center_vertical"
  , margin $ MarginTop 48
  , visibility $ boolToVisibility $ DA.null $ getAllBusTickets state
  ]
  [ imageView
    [ width $ V $ 280
    , height $ V 280
    , gravity CENTER_HORIZONTAL
    , layoutGravity "center_horizontal"
    , imageWithFallback $ fetchImage FF_ASSET "ny_ic_bus_ticket_illustration"
    ]
  , textView $
    [ text "Experience hassle-free bus bookings with Yatri Sathi"
    , color Color.black800
    , margin $ MarginHorizontal 24 24
    , gravity CENTER_HORIZONTAL
    , padding $ PaddingHorizontal 24 24
    ] <> FontStyle.body25 TypoGraphy
  ]

searchRouteButton :: forall w. (Action -> Effect Unit) -> ST.BusTicketBookingState -> PrestoDOM (Effect Unit) w
searchRouteButton push state =
  let buttonPadding = if EHC.os == "IOS" then Padding 16 16 16 12 else Padding 16 16 16 16
  in 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ Margin 16 24 16 20
    , background Color.white900
    , padding buttonPadding
    , onClick push $ const SearchButtonClick
    , cornerRadius 12.0
    , rippleColor Color.rippleShade
    , gravity CENTER_VERTICAL
    , accessibilityHint "Search Your Bus : Button"
    ]
    [ imageView
      [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_search_grey"
      , height $ V 16
      , width $ V 16
      , gravity CENTER_VERTICAL
      , margin $ MarginRight 12
      ]
    , textView $ 
      [ text "Search Your Bus"
      , color Color.black900
      , singleLine false
      , gravity CENTER_VERTICAL
      ] <> FontStyle.subHeading3 TypoGraphy
    ]

recentTicketsView :: forall w. (Action -> Effect Unit) -> ST.BusTicketBookingState -> PrestoDOM (Effect Unit) w
recentTicketsView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ Margin 16 24 16 16
  , padding $ PaddingBottom 32
  , orientation VERTICAL
  ]
  [ textView $
    [ text "Recent Tickets"
    , color Color.black800
    , singleLine true
    , maxLines 1
    ] <> FontStyle.body6 TypoGraphy
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ] $
    map (\ticketData -> ticketCardView push ticketData) $ getAllBusTickets state
  ]

ticketCardView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketCardData -> PrestoDOM (Effect Unit) w
ticketCardView push ticketData =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , stroke $ "1," <> Color.grey900
  , padding $ Padding 16 16 16 16
  , margin $ MarginTop 12
  , orientation VERTICAL
  , background Color.white900
  , onClick push $ const $ TicketPressed ticketData.metroTicketStatusApiResp
  , rippleColor Color.rippleShade
  -- , shadow $ Shadow 0.1 0.1 10.0 24.0 Color.greyBackDarkColor 0.5 -- Shadow is not having cornerRadius
  , cornerRadius 24.0
  ]
  [ linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    ]
    [ imageView
      [ width $ V 42
      , height $ V 42
      , margin $ MarginRight 12
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_bus_icon_light_blue"
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation VERTICAL
      ]
      [ textView $
        [ text "Bus Ticket"
        , color Color.black800
        , singleLine true
        , maxLines 1
        , margin $ MarginBottom 2
        ] <> FontStyle.body1 TypoGraphy
      , textView $
        [ text $ "Ticket No: " <> extractTicketNumber 
        , color Color.black700
        , singleLine true
        , maxLines 1
        , margin $ MarginBottom 2
        ] <> FontStyle.body3 TypoGraphy
      ]
    , linearLayout [weight 1.0] []
    , textView $
      [ text $ if isActive then "Active" else "Expired"
      , color $ if isActive then Color.white900 else Color.black650
      , background $ if isActive then Color.green900 else Color.grey900
      , padding $ Padding 12 6 12 6
      , cornerRadius 20.0
      , layoutGravity "right"
      ] <> FontStyle.tags TypoGraphy
    ]
  , imageView
    [ height $ V 1
    , width MATCH_PARENT
    , margin $ MarginVertical 18 18
    , imageWithFallback $ fetchImage GLOBAL_COMMON_ASSET "ny_ic_dotted_line"
    ]
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER_VERTICAL
    ]
    [ singleStopView push ticketData true
    , textView $
      [ height $ V 18
      , width $ V 2
      , background Color.grey900
      , margin $ MarginLeft 2 
      ]
    , singleStopView push ticketData false
    ]
  ]
  where
    isActive :: Boolean
    isActive = do
      let (MetroTicketBookingStatus ticketBookingStatusResp) = ticketData.metroTicketStatusApiResp
      (DA.any (_ == ticketData.status) ["CONFIRMED", "CONFIRMING"]) && (not $ isTicketExpired ticketBookingStatusResp.validTill)

    isTicketExpired :: String -> Boolean
    isTicketExpired validTill =  (DFU.runFn2 JB.differenceBetweenTwoUTC validTill (EHC.getCurrentUTC "")) < 0

    extractTicketNumber :: String
    extractTicketNumber = do
      let (MetroTicketBookingStatus ticketBookingStatusResp) = ticketData.metroTicketStatusApiResp
          ticketAPIData = map (\(FRFSTicketAPI tickets) -> tickets.ticketNumber) ticketBookingStatusResp.tickets
      fromMaybe (defaultTicketNumber ticketBookingStatusResp.bookingId) $ DA.head ticketAPIData
    
    defaultTicketNumber :: String -> String 
    defaultTicketNumber = DS.take 8

singleStopView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketCardData -> Boolean -> PrestoDOM (Effect Unit) w
singleStopView push ticketData isSourceView =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  ]
  [ imageView
    [ width $ V 8
    , height $ V 8
    , imageWithFallback $ fetchImage FF_COMMON_ASSET $ if isSourceView then "ny_ic_green_circle" else "ny_ic_red_circle"
    , margin $ MarginTop $ if isSourceView then 0 else 2
    ]
  , textView $
    [ text $ if isSourceView then ticketData.sourceName else ticketData.destinationName
    , margin $ MarginHorizontal 16 15
    , color Color.black650
    , ellipsize true
    , maxLines 1
    , gravity CENTER_VERTICAL
    ] <> FontStyle.body1 TypoGraphy
  ]

getAllBusTickets :: ST.BusTicketBookingState -> Array ST.MetroTicketCardData
getAllBusTickets state = maybe [] (\ticketDetailsState -> ticketDetailsState.data.activeTickets <> ticketDetailsState.data.pastTickets) state.data.ticketDetailsState