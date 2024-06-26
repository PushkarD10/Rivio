module Screens.RideRequestScreen.View where

import Screens.Types as ST
import Prelude
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, frameLayout, orientation, padding, text, textSize, textView, weight, width, onClick, layoutGravity, alpha, scrollView, cornerRadius, onBackPressed, stroke, lineHeight, visibility, afterRender, scrollBarY, imageWithFallback, rippleColor, clickable, relativeLayout, alignParentBottom, id, onAnimationEnd, swipeRefreshLayout, onRefresh, onScroll,nestedScrollView, enableRefresh)
import Screens.RideRequestScreen.Controller
import PrestoDOM.Properties (alpha, cornerRadii, lineHeight, minWidth)
import Engineering.Helpers.Commons (getNewIDWithTag, screenWidth, os, safeMarginBottom,getFutureDate)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Effect (Effect)
import Helpers.Utils (toStringJSON, fetchImage, FetchImageFrom(..))
import Styles.Colors as Color
import Language.Strings (getString)
import Language.Types (STR(..))
import Font.Size as FontSize
import Screens.RideRequestScreen.Controller (Action(..), ScreenOutput)
import Font.Style as FontStyle
import Animation as Anim
import Effect.Aff (launchAff)
import Common.Types.App (LazyCheck(..), CategoryListType)
import Screens.RideRequestScreen.ScreenData
import Engineering.Helpers.Commons (screenWidth)
import Data.Array as DA
import Data.Int (ceil, fromString, round, toNumber)
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import Data.Function.Uncurried (runFn1)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import Debug
import Animation
import PrestoDOM.List
import Debug
import Language.Strings (getString)
import PrestoDOM (BottomSheetState(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), adjustViewWithKeyboard, afterRender, alignParentBottom, alpha, background, bottomSheetLayout, clickable, color, cornerRadius, ellipsize, fontStyle, frameLayout, gravity, halfExpandedRatio, height, id, imageUrl, imageView, imageWithFallback, layoutGravity, lineHeight, linearLayout, lottieAnimationView, margin, onBackPressed, onClick, orientation, padding, peakHeight, relativeLayout, singleLine, stroke, text, textSize, textView, onScrollStateChange,visibility, weight, width, topShift, onAnimationEnd, horizontalScrollView, scrollBarX, setEnable)
import PrestoDOM.Events (globalOnScroll)
import Services.API (ScheduledBookingListResponse(..))
import Types.App (defaultGlobalState)
import Presto.Core.Types.Language.Flow (Flow, doAff)
import Types.App (FlowBT, GlobalState(..))
import Services.Backend as Remote
import Data.Either (Either(..))
import Effect.Class (liftEffect)
import Mobility.Prelude (boolToVisibility)
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple (Tuple(..))



screen :: RideRequestScreenState -> ListItem -> Screen Action RideRequestScreenState ScreenOutput
screen initialState listItem =
  let
    _ = spy "screenName =>" initialState

    _ = spy "screenName =>" listItem
  in
    { initialState
    , view: view listItem
    , name: "RideRequestScreen"
    , globalEvents:
        [ globalOnScroll "RideRequestScreen"
        , ( \push -> do
                void $ launchAff $ EHC.flowRunner defaultGlobalState $ getRideList PastRideApiAC push initialState 
                pure $ pure unit
          )
        ]
    , eval:
        ( \action state -> do
            let
              _ = spy "RideRequestScreen state -----" state
            let
              _ = spy "RideRequestScreen action --------" action
            eval action state
        )
    }

view :: forall w. ListItem -> (Action -> Effect Unit) -> RideRequestScreenState -> PrestoDOM (Effect Unit) w
view listItem push state =
  Anim.screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push $ const BackPressed
        , onAnimationEnd push $ const $ NoAction
        , background Color.grey700
        ]
        ( [ linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , orientation VERTICAL
              ]
              [ headerLayout state push
              , navbarlayout state push
              , dateAndDayLayout state push
              , listLayout push state listItem
              
              ,loadButtonView state push
              ]
          ]
        )

listLayout :: forall w. (Action -> Effect Unit) -> RideRequestScreenState -> ListItem -> PrestoDOM (Effect Unit) w
listLayout push state listItm =
  let (ScheduledBookingListResponse response) = state.data.resp
  
      _= spy "hereeeeeeeeee!!!!!" response.bookings
  
  in 
  swipeRefreshLayout
    ( [ weight 1.0
      , width MATCH_PARENT
      , onRefresh push (const Refresh)
      , enableRefresh state.data.refreshLoader
      ] 
      <> if state.data.scrollEnable then [setEnable $ false] else []
    )
    [ Keyed.relativeLayout
       [
         height MATCH_PARENT
        ,width MATCH_PARENT
        ,orientation VERTICAL
      ]([ Tuple "Rides"
          $list
          [  width MATCH_PARENT
          , visibility VISIBLE
          , listItem listItm
          , visibility $ boolToVisibility $ DA.null response.bookings
          , onScroll "rides" "RideRequestScreen" push (Scroll)
          , onScrollStateChange push (ScrollStateChanged)
          , listDataV2 $ spy "myRideListTransformerProp" (myRideListTransformerProp state.data.filteredArr)
          ]
        , Tuple "NoRides"
        $ linearLayout[
          height $ WRAP_CONTENT
          , width $ MATCH_PARENT
          , gravity CENTER_HORIZONTAL
          , background Color.grey700
          , orientation VERTICAL
          , visibility $ spy "noridessssssssssss---->>>>" (boolToVisibility $ (state.props.receivedResponse  && (DA.null state.data.filteredArr )))
        ][
          imageView [
            width $ V 140
            ,height $ V 155
          , gravity CENTER_HORIZONTAL
          ,imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_no_rides"
          ]
          ,textView $[
            width $ V 290
          , height $ V 20
          , gravity CENTER
          , text "Currently, there are no rides available."
          , textSize FontSize.a_14

          ] <> FontStyle.h3 TypoGraphy
          ,textView $[
            width $ V 290
          , height $ V 20
          , gravity CENTER
          , text "Please try again later"
          , textSize FontSize.a_14

          ] <> FontStyle.h3 TypoGraphy

        ] 
        ,Tuple "LOADER"
        $ PrestoAnim.animationSet
          [ PrestoAnim.Animation
            [ PrestoAnim.duration 1000
            , PrestoAnim.toAlpha $
                case state.data.shimmerLoader of
                  loader | loader `DA.elem` [ST.AnimatingIn, ST.AnimatedIn] -> 1.0
                  loader | loader `DA.elem` [ST.AnimatingOut, ST.AnimatedOut] -> 0.0
                  _ -> 0.0
            , PrestoAnim.fromAlpha $
                case state.data.shimmerLoader of
                  loader | loader `DA.elem` [ST.AnimatingIn, ST.AnimatedOut] -> 0.0
                  loader | loader `DA.elem` [ST.AnimatingOut, ST.AnimatedIn] -> 1.0
                  _ -> 0.0
            , PrestoAnim.tag "Shimmer"
            ] true
          ] $ list
            [ height MATCH_PARENT
            , scrollBarY false
            , background Color.white900
            , width MATCH_PARENT
            , onAnimationEnd push OnFadeComplete
            , listItem listItm
            , listDataV2 $ shimmerData <$> (JB.getArray 5)
            , visibility $  (case state.data.shimmerLoader of
                    ST.AnimatedOut -> GONE
                    _ -> VISIBLE)
            ]
       ])
    
    ]

headerLayout :: RideRequestScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
headerLayout state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , background Color.white900
    ]
    [ headerLeft state push
    ]

headerLeft :: RideRequestScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
headerLeft state push =
  linearLayout
    [ width WRAP_CONTENT
    , height MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , padding $ Padding 5 16 5 16
    , weight 1.0
    ]
    [ imageView
        [ width $ V 30
        , height $ V 30
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
        , onClick push $ const $ BackPressed
        , margin $ MarginLeft 5
        , rippleColor Color.rippleShade
        ]
    , textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ getString MORE_RIDES
          , textSize FontSize.a_18
          , margin $ Margin 5 0 0 2
          , weight 1.0
          , color Color.black900
          ]
        <> FontStyle.h3 TypoGraphy
    ]

headerRight :: RideRequestScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
headerRight state push =
  linearLayout
    [ width WRAP_CONTENT
    , height MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , layoutGravity "center_vertical"
    , padding $ Padding 0 16 12 16
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ getString SORT_BY
          , textSize FontSize.a_14
          , weight 1.0
          , color Color.black900
          ]
        <> FontStyle.tags TypoGraphy
    , imageView
        [ width $ V 15
        , height $ V 15
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_down"
        , onClick push $ const $ DropDown
        , padding $ Padding 2 3 4 2
        , margin $ MarginLeft 5
        , rippleColor Color.rippleShade
        ]
    ]

navbarlayout :: RideRequestScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
navbarlayout state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ Padding 3 4 3 4
    , margin $ Margin 16 16 16 16
    , cornerRadius 18.0
    , background Color.white900
    ]
    ( DA.mapWithIndex
        ( \index item -> navpillView item push index
        )
        state.data.pillViewArray
    )

navpillView :: PillViewConfig -> (Action -> Effect Unit) -> Int -> forall w. PrestoDOM (Effect Unit) w
navpillView config push idx =
  linearLayout
    [ height WRAP_CONTENT
    , gravity CENTER
    , orientation HORIZONTAL
    , padding $ Padding 5 8 5 8
    , weight 1.0
    , cornerRadius 18.0
    , onClick push $ const $ RideTypeSelected config.rideType idx
    , background if config.isSelected then config.activeColor else Color.white900
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text config.pillViewText
          , textSize FontSize.a_13
          , color if config.isSelected then Color.white900 else Color.black900
          ]
        <> FontStyle.tags TypoGraphy
    ]

daypillView :: PillViewConfig -> (Action -> Effect Unit) -> Int -> forall w. PrestoDOM (Effect Unit) w
daypillView config push idx =
  linearLayout
    [ height WRAP_CONTENT
    , gravity CENTER
    , orientation HORIZONTAL
    , padding $ Padding 12 4 12 4
    , weight 1.0
    , cornerRadius 18.0
    , background if config.isSelected then config.activeColor else Color.white900
    , onClick push $ const $ SelectDay idx
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text config.pillViewText
          , color if config.isSelected then Color.white900 else Color.black900
          , textSize FontSize.a_13
          ]
        <> FontStyle.tags TypoGraphy
    ]
dateAndDayLayout :: RideRequestScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
dateAndDayLayout state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , margin $ Margin 16 16 16 16
    , gravity CENTER
    ]
    [ textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ (EHC.convertUTCtoISC (EHC.getCurrentUTC "") "DD MMM,YYYY")
          , color $ Color.black900
          ]
        <> FontStyle.h3 TypoGraphy
    , linearLayout
        [ weight 0.5
        ]
        []
    , dayLayout state push
    ]

dayLayout :: RideRequestScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
dayLayout state push =
  linearLayout
    [ width $ WRAP_CONTENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , layoutGravity "center_vertical"
    , padding $ Padding 5 3 5 3
    , cornerRadius 18.0
    , background Color.white900
    ]
    ( DA.mapWithIndex
        ( \index item -> daypillView item push index
        )
        state.data.dayArray
    )

loadButtonView :: RideRequestScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
loadButtonView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , gravity CENTER
    , padding $ PaddingBottom 5
    , onClick push $ const Loader
    , visibility if (state.data.loaderButtonVisibility && (not state.data.loadMoreDisabled)) then VISIBLE else GONE
    ]
    [ linearLayout
        [ background Color.grey900
        , width MATCH_PARENT
        , height $ V 1
        ]
        []
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation VERTICAL
        , padding $ PaddingVertical 5 5
        ]
        [ textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ getString VIEW_MORE
              , padding (Padding 10 5 10 5)
              , color Color.blue900
              ]
            <> FontStyle.body1 LanguageStyle
        ]
    ]

getRideList :: forall action. (ScheduledBookingListResponse -> String -> action) -> (action -> Effect Unit) -> RideRequestScreenState -> Flow GlobalState Unit
getRideList action push state = do
  (scheduledBookingListResponse) <- Remote.rideBooking "5" (show state.data.offset) "2024-06-03" (getFutureDate state.data.date 1) (state.data.tripCategory)
  case scheduledBookingListResponse of
    Right (ScheduledBookingListResponse listResp) -> do
      doAff do liftEffect $ push $ action (ScheduledBookingListResponse listResp) "success"
      pure unit
    Left (err) -> do
      doAff do liftEffect $ push $ action (ScheduledBookingListResponse dummyListResp) if err.code == 500 then "listCompleted" else "failure"
      pure unit

-- getRideList' :: forall action. (ScheduledBookingListResponse -> String -> action) -> (action -> Effect Unit) -> RideRequestScreenState -> Flow GlobalState Unit
-- getRideList' action push state = do
--   (scheduledBookingListResponse) <- Remote.rideBooking "8" (show state.data.offset)  (getFutureDate state.data.date 1) (state.data.tripCategory)
--   case scheduledBookingListResponse of
--     Right (ScheduledBookingListResponse listResp) -> do
--       doAff do liftEffect $ push $ action (ScheduledBookingListResponse listResp) "success"
--       pure unit
--     Left (err) -> do
--       doAff do liftEffect $ push $ action (ScheduledBookingListResponse dummyListResp) if err.code == 500 then "listCompleted" else "failure"
--       pure unit

dummyListResp :: forall a. { bookings :: Array a }
dummyListResp = { bookings: [] }


shimmerData:: Int -> ST.RideCardItemState
shimmerData i =   {
    date : toPropValue "Today - 13 Apr,2024",
    time : toPropValue "7 pm",
    source : toPropValue "Koramangla",
    distance : toPropValue "120",
    destination : toPropValue "Koramangla",
    totalAmount : toPropValue "2000",
    cardVisibility : toPropValue "visible",
    shimmerVisibility : toPropValue "",
    carImage : toPropValue "",
    rideType : toPropValue "Regular",
    carType : toPropValue "Non-AC-Mini",
    srcLat :  toPropValue 0.0000,
    srcLong :  toPropValue 0.0000,
    desLat :  toPropValue 0.0000,
    desLong : toPropValue  0.0000,
    id : toPropValue "",
    image : toPropValue "",
    visible : toPropValue false,
    pillColor : toPropValue "",
    overlayVisiblity :toPropValue  "gone",
    visiblePill : toPropValue "" , 
    cornerRadius : toPropValue "",
    imageType : toPropValue ""
}
