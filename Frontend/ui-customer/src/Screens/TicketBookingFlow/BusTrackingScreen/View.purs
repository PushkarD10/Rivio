{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TicketBookingFlow.BusTrackingScreen.View where

import Debug
import Prelude
import PrestoDOM
import Screens.TicketBookingFlow.BusTrackingScreen.Controller
import Screens.TicketBookingFlow.BusTrackingScreen.ScreenData
import Accessor (_lat, _lon)
import Animation (screenAnimation)
import Animation as Anim
import Animation.Config (translateFullYAnimWithDurationConfig, translateYAnimConfig, Direction(..), AnimConfig, animConfig)
import CarouselHolder as CarouselHolder
import Common.Resources.Constants as CRC
import Common.Types.App as CTA
import Components.BannerCarousel as BannerCarousel
import Components.BoxContainer as BoxContainer
import Components.DropDownWithHeader as DropDownWithHeader
import Components.GenericHeader as GenericHeader
import Components.InfoBox as InfoBox
import Components.PrimaryButton as PrimaryButton
import Constants.Configs (getPolylineAnimationConfig)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array as DA
import Data.Lens ((^.))
import Data.Maybe as Mb
import Effect (Effect)
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.BackTrack (liftFlowBT)
import Helpers.API as HelpersAPI
import Font.Style as FontStyle
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToInvisibility, boolToVisibility, layoutWithWeight)
import Mobility.Prelude as MP
import Presto.Core.Types.Language.Flow (Flow)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout)
import PrestoDOM.List (ListItem, preComputeListItem)
import Data.Time.Duration (Milliseconds(..))
-- import Effect.Aff (Milliseconds(..), launchAff)
import PrestoDOM.Properties (sheetState, cornerRadii)
import Presto.Core.Types.Language.Flow (doAff, Flow, delay)
import PrestoDOM.Types.DomAttributes (Corners(..))
-- import Screens.TicketBookingFlow.Components.AlertWidget as AlertWidget
-- import Screens.TicketBookingFlow.Components.MetroCard as MetroCard
-- import Screens.TicketBookingFlow.Components.VehicleCard as VehicleCard
-- import Screens.TicketBookingFlow.JourneyTrackingScreen.ComponentConfig (getAlertWidgetConfig)
import Screens.RideBookingFlow.HomeScreen.Config as HSConfig
import Screens.Types as ST
import Services.API as API
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (GlobalState(..), defaultGlobalState)
import Components.SeparatorView.View as SeparatorView
import Types.EndPoint (shareRide)
import Screens.TicketBookingFlow.BusTrackingScreen.ComponentConfig
import Control.Monad.Trans.Class (lift)
import Data.Either
import Storage (getValueToLocalStore, KeyStore(..))
import Data.Traversable (traverse, for)
import Data.Int as DI
import Data.Function.Uncurried (runFn2, runFn3)
import LocalStorage.Cache
import Data.Tuple as DT
import Data.Map as DM

screen :: ST.BusTrackingScreenState -> Screen Action ST.BusTrackingScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "BusTrackingScreen"
  , globalEvents:
      [ ( \push -> do
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
              $ do
                  let
                    city = getValueToLocalStore CUSTOMER_LOCATION
                  if (DA.null initialState.data.stopsList) then do
                    getMetroStationResp <- Remote.getMetroStationBT "BUS" city initialState.data.busRouteCode "" (show initialState.props.srcLat <> "," <> show initialState.props.srcLon)
                    liftFlowBT $ push $ UpdateStops getMetroStationResp
                  else
                    liftFlowBT $ push $ UpdateStops (API.GetMetroStationResponse initialState.data.stopsList)
                  let
                    _ = runFn2 setInCache "BUS_LOCATION_TRACKING" initialState.data.busRouteCode
                  lift $ lift $ busLocationTracking 3000.0 0 initialState.data.busRouteCode push
                  -- lift $ lift $ defaultMockInviteFlow 0 initialState
                  let
                    _ = spy "defaultMockInviteFlow" "defaultMockInviteFlow"
                  pure unit
            -- void $ launchAff $ flowRunner globalState $ updateMockData push initialState tracking.id
            pure $ pure unit
        )
      ]
  , eval:
      ( \action state -> do
          let
            _ = spy "BusTrackingScreen state -----" state

            _ = spy "BusTrackingScreen--------action" action
          eval action state
      )
  }

view :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , onBackPressed push $ const BackPressed
        , background Color.white900
        ]
        [ PrestoAnim.animationSet
            [ Anim.fadeIn true
            ]
            $ linearLayout
                [ width MATCH_PARENT
                , height MATCH_PARENT
                , id $ EHC.getNewIDWithTag "BusTrackingScreenMap"
                , onAnimationEnd
                    ( \action -> do
                        void $ JB.showMap (EHC.getNewIDWithTag "BusTrackingScreenMap") true "satellite" 17.0 state.props.srcLat state.props.srcLon push MapReady
                        push action
                    )
                    (const NoAction)
                ]
                []
        , relativeLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            ]
            [ bottomSheetView push state
            ] --   linearLayout --     [ width WRAP_CONTENT --     , height WRAP_CONTENT --     , orientation HORIZONTAL --     -- , background Color.ba --     , visibility GONE --     ] --     [ -- topLeftIconView push state-- dynamicProgressBarView push state-- , safetyButtonView push state] -- ,  
        -- journeyLegView push state
        , linearLayout
            [ height $ V 48
            , width $ V 48
            , cornerRadius 25.0
            , gravity CENTER
            , background Color.white900
            , margin $ Margin 16 16 0 0
            ]
            [ imageView
                [ width $ V 24
                , height $ V 24
                , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_chevron_left"
                , onClick push $ const BackPressed
                -- , padding $ Padding 7 7 7 7
                , rippleColor Color.rippleShade
                ]
            ]
        ]

journeyLegTitleView :: forall w. Boolean -> String -> ST.BusTrackingScreenState -> PrestoDOM (Effect Unit) w
journeyLegTitleView isSource name state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginVertical 8 (if state.props.showRouteDetailsTab then 0 else 8)
    , background Color.white900
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , cornerRadius 25.0
        , padding $ Padding 5 5 5 5
        , background if isSource then Color.green900 else Color.red
        ]
        [ imageView
            [ height $ V 10
            , width $ V 10
            , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_circle_white"
            ]
        ]
    , textView
        $ [ text name
          , margin $ MarginLeft 16
          ]
        <> FontStyle.body1 CTA.TypoGraphy
    ]

journeyLegDescriptionView :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> PrestoDOM (Effect Unit) w
journeyLegDescriptionView push state =
  linearLayout
    [ height WRAP_CONTENT
    , weight 1.0
    , width WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ textView
        $ [ text "10 min"
          , margin $ MarginLeft 8
          ]
        <> FontStyle.body3 CTA.TypoGraphy
    -- , MetroCard.view (push <<< MetroCardAction) MetroCard.dummyRouteInfo
    -- , VehicleCard.view (push <<< VehicleCardAction) VehicleCard.dummyRouteInfo
    ]

verticalLineView :: forall w. (Action -> Effect Unit) -> String -> Boolean -> Mb.Maybe (Array Number) -> PrestoDOM (Effect Unit) w --ST.BusTrackingScreenState -> Boolean -> PrestoDOM (Effect Unit) w
verticalLineView push idTag showOnlyBullet vehicles =  -- state isDotted =
  -- let
  --   -- ht = JB.getLayoutBounds $ EHC.getNewIDWithTag $ "stopListView" -- <> "false"
  -- in
  -- linearLayout
  --   [ height $ V ht.height 
  --   , width WRAP_CONTENT
  --   ]
  --   [ ]
  relativeLayout
    [ height MATCH_PARENT
    , width $ V 20
    , clipChildren false
    , visibility $ boolToVisibility showOnlyBullet
    , background Color.white900 --if Mb.isJust vehicles then Color.red else Color.white900
    ]
    $ [ linearLayout
          [ height MATCH_PARENT
          , width $ V 2
          , margin $ MarginLeft 9
          , background Color.black900
          , id $ EHC.getNewIDWithTag idTag
          ]
          []
      ]
    <> case vehicles of
        Mb.Just v -> (map (\p -> busMarkerView p) v)
        _ -> []
  where
  lineViewBounds = JB.getLayoutBounds $ EHC.getNewIDWithTag idTag

  busMarkerView :: Number -> PrestoDOM (Effect Unit) w
  busMarkerView percent = do
    let
      lineViewHeight = spy "lineViewHeight" lineViewBounds.height

      totalDistance = 50.0

      currentDistance = 5.0

      _ = spy "busmarginTop1" percent

      _ = spy "busmarginTop2" (DI.toNumber lineViewHeight)

      _ = spy "busmarginTop3" $ DI.round (percent * (DI.toNumber lineViewHeight))

      marginTop = spy "busmarginTop" $ DI.round (percent * (DI.toNumber lineViewHeight) - 8.0)
    imageView
      [ height $ V 20
      , width $ V 20
      , margin $ MarginTop $ marginTop
      , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_bus_marker_with_arrow"
      ]

-- , stopListView push state true
-- ]
knobView :: forall w. PrestoDOM (Effect Unit) w
knobView =
  linearLayout
    [ height $ V 4
    , width $ V 34
    , background Color.transparentGrey
    , margin $ MarginVertical 4 4
    , cornerRadius 4.0
    ]
    []

bottomSheetView :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> PrestoDOM (Effect Unit) w
bottomSheetView push state =  --let
  -- isRideData = isJust state.data.driverInfoCardState
  -- in 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , alignParentBottom "true,-1"
    ]
    [ coordinatorLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , cornerRadii $ Corners 24.0 true true false false
        ]
        [ bottomSheetLayout
            ( [ height WRAP_CONTENT
              , width MATCH_PARENT
              , peakHeight $ 400 --getPeekHeight state
              , enableShift false
              , cornerRadii $ Corners 24.0 true true false false
              ]
                <> Mb.maybe [] (\sheet -> [ sheetState sheet ]) Mb.Nothing
            ) --state.props.sheetState)
            [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation VERTICAL
                    , gravity CENTER
                    , background if state.props.showRouteDetailsTab then Color.white900 else Color.grey700
                    , padding $ PaddingBottom 16
                    , cornerRadii $ Corners 24.0 true true false false
                    ]
                    [ bottomSheetContentView push state
                    ]
                ]
            ]
        ]
    ]

bottomSheetContentView :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> PrestoDOM (Effect Unit) w
bottomSheetContentView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.transparent
    , orientation VERTICAL
    , gravity CENTER
    , cornerRadii $ Corners 24.0 true true false false
    ]
    [ knobView
    , linearLayout
        ( [ height WRAP_CONTENT
          , width MATCH_PARENT
          , background Color.white900
          , orientation VERTICAL
          , gravity CENTER
          , cornerRadii $ Corners 24.0 true true (not state.props.showRouteDetailsTab) (not state.props.showRouteDetailsTab)
          ]
            <> if state.props.showRouteDetailsTab then
                []
              else
                [ margin $ Margin 16 8 16 16 ]
        )
        [ busDetailsView push state
        , busStopsView push state
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , gravity CENTER
            , visibility $ boolToVisibility state.props.showRouteDetailsTab
            ]
            [ separatorView Color.grey900 (MarginTop 10)
            , PrimaryButton.view (push <<< BookTicketButtonAction) (primaryButtonConfig state)
            ]
        ]
    ]

separatorConfig :: SeparatorView.Config
separatorConfig =
  { orientation: VERTICAL
  , count: 50
  , height: V 10
  , width: V 2
  , layoutWidth: V 2
  , layoutHeight: V 100
  , color: Color.black500
  }

actionListVIew :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> PrestoDOM (Effect Unit) w
actionListVIew push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    []

busDetailsView :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> PrestoDOM (Effect Unit) w
busDetailsView push state =
  linearLayout
    ( [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ]
        <> if state.props.showRouteDetailsTab then
            []
          else
            [ padding $ Padding 16 8 16 16 ]
    )
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        ]
        [ linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , weight 1.0
            , gravity if state.props.showRouteDetailsTab then CENTER else LEFT
            ]
            [ textView
                $ [ text $ "Bus No: " <> state.data.routeShortName
                  , margin $ MarginLeft 8
                  ]
                <> FontStyle.h2 CTA.TypoGraphy
            , textView
                $ [ text $ "Towards " <> (Mb.maybe "" (\(API.FRFSStationAPI item) -> item.name) (DA.last state.data.stopsList))
                  , margin $ MarginLeft 8
                  ]
                <> FontStyle.body3 CTA.TypoGraphy
            ]
        , textView
            $ [ text "View Ticket"
              , cornerRadius 8.0
              , color Color.blue800
              , background Color.blue600
              , padding $ Padding 12 8 12 8
              , visibility $ boolToVisibility $ not state.props.showRouteDetailsTab
              , onClick push $ const ViewTicket
              ]
            <> FontStyle.tags CTA.TypoGraphy
        ]
    , separatorView Color.grey900 (MarginTop 10)
    ]

busStopsView :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> PrestoDOM (Effect Unit) w
busStopsView push state = do
  let
    lb = JB.getLayoutBounds $ EHC.getNewIDWithTag "stopListView"

    showScrollView = lb.height > 400
  (if showScrollView then scrollView else linearLayout)
    [ height $ if showScrollView then V 400 else WRAP_CONTENT
    , width MATCH_PARENT
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , margin $ MarginHorizontal 16 16
        ]
        [ journeyLegTitleView true (Mb.maybe "" (\(API.FRFSStationAPI item) -> item.name) (DA.head state.data.stopsList)) state
        , linearLayout [ width MATCH_PARENT, height WRAP_CONTENT ] [ stopListView push state true, stopListView push state false ]
        , journeyLegTitleView false (Mb.maybe "" (\(API.FRFSStationAPI item) -> item.name) (DA.last state.data.stopsList)) state --fromMaybe ""  (state.data.destinationStation <#> ._stationName) --"KSR Bengaluru Railway Station, M.G. Railway Colony, Sevashrama, Bengaluru, Karnataka 560023"
        ]
    ]

stopListView :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> Boolean -> PrestoDOM (Effect Unit) w
stopListView push state showOnlyBullet =
  linearLayout
    ( [ height WRAP_CONTENT
      , orientation VERTICAL
      , id $ EHC.getNewIDWithTag $ "stopListView" -- <> show showOnlyBullet
      , margin $ MarginHorizontal 0 16
      ]
        <> if showOnlyBullet then
            [ width $ V 20 ]
          else
            [ weight 1.0 ]
              <> if state.props.showRouteDetailsTab then
                  []
                else
                  [ cornerRadius 12.0
                  , stroke $ "1," <> Color.grey900
                  , padding $ PaddingBottom $ if state.props.expandStopsView then 12 else 0
                  ]
    )
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        -- , visibility $ boolToVisibility $ state.props.showRouteDetailsTab || state.props.expandStopsView
        ]
        [ verticalLineView push ("verticalLineView2" <> show showOnlyBullet <> "0000") showOnlyBullet Mb.Nothing
        , stopsViewHeader push state showOnlyBullet
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , visibility $ boolToVisibility $ state.props.showRouteDetailsTab || state.props.expandStopsView
        ]
        (DA.mapWithIndex (\index item -> stopView item showOnlyBullet stopMarginTop state push index) stops)
    ]
  where
  stopMarginTop = if state.props.showRouteDetailsTab then 32 else 12

  stops = if DA.length state.data.stopsList <= 2 then [] else DA.slice 1 (DA.length state.data.stopsList - 1) state.data.stopsList

stopsViewHeader :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> Boolean -> PrestoDOM (Effect Unit) w
stopsViewHeader push state showOnlyBullet =
  linearLayout
    [ weight 1.0
    , height $ V 50
    , orientation VERTICAL
    -- , visibility $ boolToInvisibility $ not showOnlyBullet
    , visibility $ boolToVisibility $ not state.props.showRouteDetailsTab
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , visibility $ boolToVisibility $ not state.props.showRouteDetailsTab
        , padding $ Padding 16 16 16 16
        ]
        [ textView
            $ [ text $ show (DA.length state.data.stopsList - 1) <> " stops left"
              -- , margin $ MarginLeft 8
              -- , visibility $ boolToInvisibility isVisible
              ]
            <> FontStyle.tags CTA.TypoGraphy
        , imageView
            [ height $ V 4
            , width $ V 4
            , margin $ MarginHorizontal 4 4
            , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET "ny_ic_dot"
            ]
        , textView
            $ [ text "16 min"
              -- , margin $ MarginLeft 8
              -- , visibility $ boolToInvisibility isVisible
              ]
            <> FontStyle.tags CTA.TypoGraphy
        , layoutWithWeight
        , imageView
            [ imageWithFallback
                $ HU.fetchImage HU.FF_ASSET
                    if state.props.expandStopsView then
                      "ny_ic_chevron_up"
                    else
                      "ny_ic_chevron_down"
            , height $ V 12
            , width $ V 12
            , margin $ MarginLeft 6
            , onClick push $ const ToggleStops
            ]
        ]
    , if state.props.expandStopsView then
        separatorView Color.grey900 (MarginTop 0)
      else
        MP.noView
    ]

-- collapsedStopsView :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> Boolean -> PrestoDOM (Effect Unit) w
-- collapsedStopsView push state =
--   linearLayout
--     [ width MATCH_PARENT
--     , height WRAP_CONTENT
--     , orientation VERTICAL
--     , margin $ MarginHorizontal 16 16
--     , cornerRadius 12.0
--     , stroke "1," <> Color.grey900
--     ]
--     [ 
--     ]
stopView :: forall w. API.FRFSStationAPI -> Boolean -> Int -> ST.BusTrackingScreenState -> (Action -> Effect Unit) -> Int -> PrestoDOM (Effect Unit) w
stopView (API.FRFSStationAPI stop) showOnlyBullet marginTop state push index =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    -- , padding $ Padding 0 marginTop 0 0
    ]
    [ linearLayout
        [ height $ V $ totalHeight / 3
        , width MATCH_PARENT
        ]
        [ verticalLineView push ("verticalLineView1" <> show showOnlyBullet <> show index) showOnlyBullet $ DM.lookup stop.code state.data.vehicleTrackingData
        -- , linearLayout
        --     [ width MATCH_PARENT
        --     , height $ if index == 0 then WRAP_CONTENT else V 32
        --     , padding $ Padding 8 8 8 8
        --     , cornerRadius 12.0
        --     , stroke $ "1," <> Color.grey900
        --     ]
        --     [ textView
        --         $ [ text "Next bus in 10 min"
        --           , margin $ MarginLeft if showOnlyBullet || not state.props.showRouteDetailsTab then 8 else 0
        --           , visibility $ boolToVisibility $ index == 1 && state.props.showRouteDetailsTab
        --           ]
        --         <> FontStyle.body1 CTA.TypoGraphy
        --     , layoutWithWeight
        --     , textView
        --         $ [ text "2:45pm"
        --           , margin $ MarginLeft if showOnlyBullet || not state.props.showRouteDetailsTab then 8 else 0
        --           , visibility $ boolToVisibility $ index == 1 && state.props.showRouteDetailsTab
        --           ]
        --         <> FontStyle.body1 CTA.TypoGraphy
        --     ]
        ]
    , linearLayout
        [ height $ V totalHeight
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        -- , margin $ Margin 0 marginTop 0 0
        -- , visibility $ boolToInvisibility $ not isVisible
        ]
        [ relativeLayout
            [ height MATCH_PARENT
            , gravity CENTER
            ]
            [ verticalLineView push ("verticalLineView2" <> show showOnlyBullet <> show index) showOnlyBullet Mb.Nothing
            , imageView
                [ height $ V 8
                , width $ V 8
                , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_stop_black"
                , margin $ Margin 6 6 0 6
                , visibility $ boolToVisibility showOnlyBullet
                ]
            ]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , id $ EHC.getNewIDWithTag $ "textviewstop" <> show index
          ]
          [ textView
            $ [ text stop.name
              , margin $ MarginLeft if showOnlyBullet || not state.props.showRouteDetailsTab then 8 else 0
              , visibility $ boolToVisibility $ not showOnlyBullet
              , width $ if showOnlyBullet then V 0 else WRAP_CONTENT
              -- , id $ EHC.getNewIDWithTag $ "textviewstop" <> show index
              , singleLine true
              , ellipsize true
              ]
            <> FontStyle.body1 CTA.TypoGraphy
          , showETAView push state index $ API.FRFSStationAPI stop
          ]
        ]
    ]
  where
  b = JB.getLayoutBounds $ EHC.getNewIDWithTag $ "textviewstop" <> show index

  totalHeight = if (Mb.isJust $ findStopInVehicleData (API.FRFSStationAPI stop) state) then 72 else 40

separatorView :: forall w. String -> Margin -> PrestoDOM (Effect Unit) w
separatorView color' margin' =
  linearLayout
    [ height (V 1)
    , width MATCH_PARENT
    , margin margin'
    , background color'
    ]
    []

busLocationTracking :: Number -> Int -> String -> (Action -> Effect Unit) -> Flow GlobalState Unit
busLocationTracking duration id routeCode push = do
  let
    trackingId = runFn3 getFromCache "BUS_LOCATION_TRACKING" Mb.Nothing Mb.Just
  if Mb.isJust trackingId && trackingId /= Mb.Just routeCode then
    pure unit
  else do
    -- void $ delay $ Milliseconds $ 2000.0
    -- let (API.BusTrackingRouteResp dummy) = dummyData
    -- for_ dummy.vehicleTrackingInfo $ \(API.VehicleInfo item) -> do
    --     -- void $ map (\(API.VehicleInfo item) -> do
    --       let (API.LatLong location) = item.location
    --       let markerConfig = JB.defaultMarkerConfig {markerId = item.vehicleId, pointerIcon = "ny_ic_bus_marker"}
    --       EHC.liftFlow $ JB.showMarker markerConfig location.lat location.lon 160 0.5 0.9 (EHC.getNewIDWithTag "BusTrackingScreenMap")
    resp <- HelpersAPI.callApi $ API.BusTrackingRouteReq routeCode
    case resp of
      Right (API.BusTrackingRouteResp resp) -> do
        trackingInfo <-
          for resp.vehicleTrackingInfo
            $ \(API.VehicleInfo item) -> do
                -- void $ map (\(API.VehicleInfo item) -> do
                -- let (API.VehicleInfo l) = item.vehicleInfo
                let
                  (API.VehicleInfoForRoute m) = item.vehicleInfo

                  (API.RouteStopMapping nextStop) = item.nextStop

                  lat = Mb.fromMaybe 0.0 m.latitude

                  lon = Mb.fromMaybe 0.0 m.longitude

                  markerConfig = JB.defaultMarkerConfig { markerId = item.vehicleId, pointerIcon = "ny_ic_bus_marker" }

                  (API.LatLong nextStopPosition) = nextStop.stopPoint
                void $ EHC.liftFlow $ JB.showMarker markerConfig lat lon 160 0.5 0.9 (EHC.getNewIDWithTag "BusTrackingScreenMap")
                pure { vehicleId: item.vehicleId, nextStop: nextStop.stopCode, nextStopDistance: HU.getDistanceBwCordinates lat lon nextStopPosition.lat nextStopPosition.lon, vehicleLat: lat, vehicleLon: lon, nextStopLat: nextStopPosition.lat, nextStopLon: nextStopPosition.lon, nextStopTravelTime : item.nextStopTravelTime}
        let
          _ = spy "trackingInfo-debug" trackingInfo
        EHC.liftFlow $ push $ UpdateTracking trackingInfo
        void $ delay $ Milliseconds $ duration
        busLocationTracking duration id routeCode push
      Left err -> do
        void $ delay $ Milliseconds $ duration
        busLocationTracking duration id routeCode push

-- Example data for BusTrackingRouteResp
-- dummyData :: API.BusTrackingRouteResp
-- dummyData =
--   API.BusTrackingRouteResp
--     { vehicleTrackingInfo:
--         [ API.VehicleInfo
--             { vehicleId: "Bus1234"
--             , vehicleInfo:
--                 API.VehicleInfoForRoute
--                   { startTime: Mb.Just "08:00:00"
--                   , startDate: Mb.Just "2024-10-23"
--                   , scheduleRelationship: Mb.Just "OnSchedule"
--                   , tripId: Mb.Just "Trip123"
--                   , latitude: Mb.Just 12.97619
--                   , longitude: Mb.Just 80.14511000000002
--                   , speed: Mb.Just "45 km/h"
--                   , timestamp: Mb.Just "2024-10-23T10:30:00Z" -- Time of location update
--                   }
--             }
--         , API.VehicleInfo
--             { vehicleId: "Bus5678"
--             , vehicleInfo:
--                 API.VehicleInfoForRoute
--                   { startTime: Mb.Just "08:10:00"
--                   , startDate: Mb.Just "2024-10-23"
--                   , scheduleRelationship: Mb.Just "Delayed"
--                   , tripId: Mb.Just "Trip567"
--                   , latitude: Mb.Just 12.977810000000002
--                   , longitude: Mb.Just 80.13876000000002
--                   , speed: Mb.Just "30 km/h"
--                   , timestamp: Mb.Just "2024-10-23T10:35:00Z"
--                   }
--             }
--         , API.VehicleInfo
--             { vehicleId: "Bus9101"
--             , vehicleInfo:
--                 API.VehicleInfoForRoute
--                   { startTime: Mb.Just "08:15:00"
--                   , startDate: Mb.Just "2024-10-23"
--                   , scheduleRelationship: Mb.Just "AheadOfSchedule"
--                   , tripId: Mb.Just "Trip910"
--                   , latitude: Mb.Just 12.987440000000003
--                   , longitude: Mb.Just 80.14203
--                   , speed: Mb.Just "50 km/h"
--                   , timestamp: Mb.Just "2024-10-23T10:40:00Z"
--                   }
--             }
--         ]
--     }

showETAView :: forall w. (Action -> Effect Unit) -> ST.BusTrackingScreenState -> Int -> API.FRFSStationAPI -> PrestoDOM (Effect Unit) w
showETAView push state index (API.FRFSStationAPI stop) =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , stroke $ "1," <> Color.grey900
  , padding $ Padding 8 8 8 8
  , cornerRadius 12.0
  -- , margin $ MarginHorizontal 8 8
  , visibility $ boolToVisibility $ Mb.isJust $ findStopInVehicleData (API.FRFSStationAPI stop) state
  ]
  [ textView
    $ [ text $ "Next Bus in " <> (Mb.maybe "10" (\item -> Mb.fromMaybe "10" item.nextStopTravelTime) $ findStopInVehicleData (API.FRFSStationAPI stop) state) <> " min"
      , margin $ MarginLeft 8
      ]
    <> FontStyle.body1 CTA.TypoGraphy
  , linearLayout [weight 1.0] []
  , textView
    $ [ text $  extractTimeInHHMMA $ EHC.getCurrentUTC "" -- $ Mb.maybe (EHC.getCurrentUTC "") (\item -> Mb.fromMaybe (EHC.getCurrentUTC "") item.nextStopTravelTime) $ findStopInVehicleData (API.FRFSStationAPI stop) state
      ]
    <> FontStyle.body1 CTA.TypoGraphy
  ]
  where
    extractTimeInHHMMA timeStr = EHC.convertUTCtoISC timeStr "hh" <> ":" <> EHC.convertUTCtoISC timeStr "mm" <> " " <> EHC.convertUTCtoISC timeStr "a"

findStopInVehicleData :: API.FRFSStationAPI -> ST.BusTrackingScreenState -> Mb.Maybe ST.VehicleData
findStopInVehicleData (API.FRFSStationAPI stop) state = DA.find (\item -> item.nextStop == stop.code) state.data.vehicleData -- && Mb.isJust item.nextStopTravelTime