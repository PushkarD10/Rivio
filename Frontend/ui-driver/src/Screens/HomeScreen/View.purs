{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.View where

import Mobility.Prelude
import PrestoDOM.List
import Screens.HomeScreen.ComponentConfig
import Timers

import Animation as Anim
import Animation.Config as AnimConfig
import CarouselHolder as CarouselHolder
import Common.Types.App (LazyCheck(..), Paths(..))
import Components.BannerCarousel as BannerCarousel
import Components.BottomNavBar as BottomNavBar
import Components.BottomNavBar.Controller (navData)
import Components.ChatView as ChatView
import Components.ErrorModal as ErrorModal
import Components.GoToLocationModal as GoToLocationModal
import Components.InAppKeyboardModal as InAppKeyboardModal
import Components.MakePaymentModal as MakePaymentModal
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.RateCard as RateCard
import Components.RatingCard as RatingCard
import Components.RequestInfoCard as RequestInfoCard
import Components.RideActionModal as RideActionModal
import Components.RideCompletedCard as RideCompletedCard
import Components.SelectListModal as SelectListModal
import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Function.Uncurried (runFn1, runFn2)
import Data.Function.Uncurried (runFn3)
import Data.Tuple 
import Data.Int (ceil, toNumber, fromString)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe, isNothing)
import Data.String as DS
import Data.Time.Duration (Milliseconds(..))
import Debug (spy)
import Domain.Payments (APIPaymentStatus(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn1, runEffectFn2, runEffectFn3)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons (flowRunner, getCurrentUTC, getNewIDWithTag, formatCurrencyWithCommas, liftFlow)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Events as Events
import Engineering.Helpers.Utils (toggleLoader)
import Font.Size as FontSize
import Font.Style as FontStyle
import Foreign (unsafeToForeign)
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog)
import MerchantConfig.Utils as MU
import PaymentPage (consumeBP)
import Prelude (Unit, bind, const, discard, not, pure, unit, void, ($), (&&), (*), (-), (/), (<), (<<<), (<>), (==), (>), (>=), (||), (<=), ($>), show, void, (/=), when, map, otherwise, (+), negate)
import Presto.Core.Types.Language.Flow (Flow, delay, doAff)
import PrestoDOM (BottomSheetState(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Shadow(..), adjustViewWithKeyboard, afterRender, alignParentBottom, alpha, background, bottomSheetLayout, clickable, color, cornerRadius, ellipsize, fontStyle, frameLayout, gravity, halfExpandedRatio, height, id, imageUrl, imageView, imageWithFallback, layoutGravity, lineHeight, linearLayout, lottieAnimationView, margin, onBackPressed, onClick, orientation, padding, peakHeight, relativeLayout, singleLine, stroke, text, textSize, textView, visibility, weight, width, topShift, onAnimationEnd, horizontalScrollView, scrollBarX, shadow, clipChildren, textFromHtml)
import PrestoDOM (BottomSheetState(..), alignParentBottom, layoutGravity, Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Prop, afterRender, alpha, background, bottomSheetLayout, clickable, color, cornerRadius, fontStyle, frameLayout, gravity, halfExpandedRatio, height, id, imageUrl, imageView, lineHeight, linearLayout, margin, onBackPressed, onClick, orientation, padding, peakHeight, stroke, text, textSize, textView, visibility, weight, width, imageWithFallback, adjustViewWithKeyboard, lottieAnimationView, relativeLayout, ellipsize, singleLine, scrollView, scrollBarY, rippleColor)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Elements (coordinatorLayout)
import PrestoDOM.Properties as PP
import PrestoDOM.Types.DomAttributes as PTD
import Resource.Constants (constructLocationInfo, getLocationInfoFromStopLocation)
import Resource.Constants as RC
import Screens as ScreenNames
import Screens.HomeScreen.Controller (Action(..), RideRequestPollingData, ScreenOutput, ScreenOutput(GoToHelpAndSupportScreen), checkPermissionAndUpdateDriverMarker, eval, getPeekHeight, getBannerConfigs)
import Screens.Types (HomeScreenStage(..), HomeScreenState, KeyboardModalType(..), DriverStatus(..), DriverStatusResult(..), PillButtonState(..), TimerStatus(..), DisabilityType(..), SavedLocationScreenType(..), LocalStoreSubscriptionInfo, SubscriptionBannerType(..))
import Screens.Types as ST
import Services.API (GetRidesHistoryResp(..), OrderStatusRes(..), Status(..), DriverProfileStatsReq(..), DriverInfoReq(..), BookingTypes(..), RidesInfo(..), StopLocation(..), LocationInfo(..))
import Services.Accessor (_lat, _lon)
import Services.Backend as Remote
import Storage (getValueToLocalStore, KeyStore(..), setValueToLocalStore, getValueToLocalNativeStore, isLocalStageOn, setValueToLocalNativeStore)
import Styles.Colors as Color
import Types.App (GlobalState(..), defaultGlobalState)
import Constants (defaultDensity)
import Resource.Constants (constructLocationInfo,getLocationInfoFromStopLocation)
import Components.ErrorModal as ErrorModal
import Timers
import Components.BannerCarousel as BannerCarousel
import CarouselHolder as CarouselHolder
import PrestoDOM.List
import Mobility.Prelude
import Resource.Constants as RC
import Data.Function.Uncurried (runFn3)
import Screens.HomeScreen.PopUpConfig as PopUpConfig
import Control.Alt ((<|>))
import Effect.Aff (launchAff, makeAff, nonCanceler)
import Common.Resources.Constants(chatService)

screen :: HomeScreenState -> GlobalState -> Screen Action HomeScreenState ScreenOutput
screen initialState (GlobalState globalState) =
  { initialState
  , view
  , name : "HomeScreen"
  , globalEvents : [
        ( \push -> do
          _ <- pure $ JB.checkAndAskNotificationPermission false
          _ <- pure $ spy "initial State" initialState
          _ <- HU.storeCallBackForNotification push Notification
          _ <- HU.storeCallBackTime push TimeUpdate
          _ <- runEffectFn2 JB.storeKeyBoardCallback push KeyboardCallback
          when (initialState.data.activeRide.tripType == ST.Rental) $ void $ JB.storeCallBackImageUpload push CallBackImageUpload
          when (initialState.data.activeRide.tripType == ST.Rental) $ void $ runEffectFn2 JB.storeCallBackUploadMultiPartData push UploadMultiPartDataCallback
                    
          when (getValueToLocalNativeStore IS_RIDE_ACTIVE == "true" && initialState.data.activeRide.status == NOTHING) do
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do              
              (GetRidesHistoryResp activeRideResponse) <- Remote.getRideHistoryReqBT "2" "0" "true" "null" "null"
              case (activeRideResponse.list DA.!! 0) of
                Just ride -> do
                  let advancedRide = (DA.find (\(RidesInfo x) -> x.bookingType == Just ADVANCED) activeRideResponse.list)
                  lift $ lift $ doAff do liftEffect $ push $ RideActiveAction ride advancedRide
                Nothing -> do
                           setValueToLocalStore IS_RIDE_ACTIVE "false"
                           void $ pure $ JB.setCleverTapUserProp [{key : "Driver On-ride", value : unsafeToForeign "No"}]
          if getValueToLocalNativeStore IS_DRIVER_STATS_CALLED == "false"
            then do
              void $ pure $ setValueToLocalStore IS_DRIVER_STATS_CALLED "true"
              void $ launchAff $ EHC.flowRunner defaultGlobalState $ do                
                driverStatsResp <- Remote.getDriverProfileStats (DriverProfileStatsReq (HU.getcurrentdate ""))
                case driverStatsResp of
                  Right driverStats -> liftFlow $ push $ DriverStats driverStats
                  Left _ -> void $ pure $ setValueToLocalStore IS_DRIVER_STATS_CALLED "false"
            else pure unit
          let localStage = getValueToLocalNativeStore LOCAL_STAGE
          if (localStage /= "RideAccepted" && localStage /= "ChatWithCustomer" && initialState.data.activeRide.waitTimerId /= "") then do
            void $ pure $ setValueToLocalStore WAITING_TIME_STATUS (show ST.NoStatus)
            push $ UpdateWaitTime ST.NoStatus
            void $ pure $ clearTimerWithId initialState.data.activeRide.waitTimerId
            pure unit
          else pure unit
          
          void if (DA.any (_ == localStage)["RideRequested", "HomeScreen", "__failed"]) && initialState.data.driverGotoState.isGotoEnabled then 
            void $ startTimer (EHC.getExpiryTime (HU.istToUtcDate initialState.data.driverGotoState.gotoValidTill) false) "goToHomeTimerId" "10" push UpdateGoHomeTimer 
            else if (initialState.data.driverGotoState.timerId /= "") then pure $ clearTimerWithId initialState.data.driverGotoState.timerId
            else pure unit
          when (isNothing initialState.data.bannerData.bannerItem) $ void $ launchAff $ EHC.flowRunner defaultGlobalState $ computeListItem push
          void $ launchAff $ flowRunner defaultGlobalState $ checkBgLocation push BgLocationAC initialState globalState.globalProps.bgLocPopupShown
          case localStage of
            "RideRequested"  -> do
                                if (getValueToLocalStore RIDE_STATUS_POLLING) == "False" then do
                                  _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING_ID (HU.generateUniqueId unit)
                                  _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING "True"
                                  let secondsOver = if EHC.getExpiryTime (getValueToLocalNativeStore RIDE_REQUEST_TIME) true >= (rideRequestPollingData.duration) then (rideRequestPollingData.duration) else EHC.getExpiryTime (getValueToLocalNativeStore RIDE_REQUEST_TIME) true
                                      counts = ceil $ (toNumber (rideRequestPollingData.duration - secondsOver) * 1000.0)/rideRequestPollingData.delay
                                  if counts > 0 then do
                                      void $ launchAff $ EHC.flowRunner defaultGlobalState $ rideRequestPolling (getValueToLocalStore RIDE_STATUS_POLLING_ID) counts rideRequestPollingData.delay initialState push Notification
                                  else
                                      void $ pure $ setValueToLocalStore RIDE_STATUS_POLLING "False"
                                  pure unit
                                  else pure unit
            "RideAccepted"   -> do
                                void $ pure $ setValueToLocalStore RIDE_END_ODOMETER ""
                                void $ pure $ setValueToLocalStore RIDE_START_ODOMETER "" 
                                let waitTime = DS.split (DS.Pattern "<$>") (getValueToLocalStore WAITING_TIME_VAL)
                                    id = fromMaybe "" (waitTime DA.!! 0)
                                    isTimerValid = id == initialState.data.activeRide.id
                                    startingTime = (runFn2 JB.differenceBetweenTwoUTC (HU.getCurrentUTC "") (fromMaybe "" (waitTime DA.!! 1)))    
                                if (getValueToLocalStore WAITING_TIME_STATUS == show ST.Triggered) then do
                                  void $ pure $ setValueToLocalStore WAITING_TIME_STATUS (show ST.PostTriggered)
                                  void $ waitingCountdownTimerV2 startingTime "1" "countUpTimerId" push WaitTimerCallback
                                  push $ UpdateWaitTime ST.PostTriggered
                                  pure unit
                                else if (getValueToLocalStore WAITING_TIME_STATUS == (show ST.PostTriggered) && initialState.data.activeRide.waitTimeSeconds == -1) then do
                                  if isTimerValid then
                                    void $ waitingCountdownTimerV2 startingTime "1" "countUpTimerId" push WaitTimerCallback
                                  else push $ UpdateWaitTime ST.NoStatus
                                  pure unit
                                else if (getValueToLocalStore WAITING_TIME_STATUS == (show ST.Scheduled) && initialState.props.rideStartRemainingTime == -1 && isJust initialState.data.activeRide.tripScheduledAt) then do
                                  void $ startTimer (runFn2 JB.differenceBetweenTwoUTC (fromMaybe (getCurrentUTC "") initialState.data.activeRide.tripScheduledAt) (getCurrentUTC "")) "rideStartRemainingTimeId" "1" push RideStartRemainingTime     
                                  else pure unit
                                if (DA.elem initialState.data.peekHeight [518,470,0]) then void $ push $ RideActionModalAction (RideActionModal.NoAction) else pure unit
                                _ <- pure $ setValueToLocalStore RIDE_G_FREQUENCY "2000"
                                _ <- pure $ setValueToLocalStore DRIVER_MIN_DISPLACEMENT "5.0"
                                if (not initialState.props.chatcallbackInitiated && not initialState.data.activeRide.bookingFromOtherPlatform) then do
                                  _ <- JB.clearChatMessages
                                  _ <- JB.storeCallBackMessageUpdated push initialState.data.activeRide.id "Driver" UpdateMessages AllChatsLoaded
                                  _ <- JB.storeCallBackOpenChatScreen push OpenChatScreen
                                  _ <- JB.startChatListenerService
                                  _ <- JB.scrollOnResume push ScrollToBottom
                                  push InitializeChat
                                  pure unit
                                else pure unit
                                if (not initialState.props.routeVisible) && initialState.props.mapRendered then do
                                  _ <- JB.getCurrentPosition push $ ModifyRoute
                                  pure $ JB.removeMarker "ic_vehicle_side" -- TODO : remove if we dont require "ic_auto" icon on homescreen
                                  pure unit
                                  else pure unit
                                if (getValueToLocalStore RIDE_STATUS_POLLING) == "False" then do
                                  _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING_ID (HU.generateUniqueId unit)
                                  _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING "True"
                                  _ <- launchAff $ EHC.flowRunner defaultGlobalState $ rideStatusPolling (getValueToLocalStore RIDE_STATUS_POLLING_ID) 20000.0 initialState push Notification
                                  pure unit
                                  else pure unit
                                push GetMessages
            "RideStarted"    -> do
                                when (initialState.data.activeRide.tripType == ST.Rental) $ void $ HU.storeCallBackForAddRideStop push CallBackNewStop
                                _ <- pure $ setValueToLocalNativeStore RIDE_START_LAT (HU.toStringJSON initialState.data.activeRide.src_lat)
                                _ <- pure $ setValueToLocalNativeStore RIDE_START_LON (HU.toStringJSON initialState.data.activeRide.src_lon)
                                _ <- pure $ setValueToLocalNativeStore RIDE_END_LAT (HU.toStringJSON initialState.data.activeRide.dest_lat)
                                _ <- pure $ setValueToLocalNativeStore RIDE_END_LON (HU.toStringJSON initialState.data.activeRide.dest_lon)
                                _ <- pure $ setValueToLocalNativeStore WAYPOINT_DEVIATION_COUNT "0"
                                _ <- pure $ setValueToLocalNativeStore TOLERANCE_EARTH "100.0"
                                _ <- pure $ setValueToLocalStore RIDE_G_FREQUENCY 
                                        $ if initialState.data.activeRide.enableFrequentLocationUpdates
                                          then "2000"
                                          else "10000"
                                _ <- pure $ setValueToLocalStore DRIVER_MIN_DISPLACEMENT "20.0"

                                let advancedRideId = case initialState.data.advancedRideData of
                                                        Just advancedRideData -> Just advancedRideData.id
                                                        Nothing -> Nothing
                                                        
                                when (initialState.data.activeRide.tripType /= ST.Rental && isNothing advancedRideId) $ void $ push RemoveChat
                                let rideId = fromMaybe "" (advancedRideId <|> Just initialState.data.activeRide.id)
                                let isChatServiceRunning = runFn1 JB.isServiceRunning chatService
                                let _ = spy "Inside isChatServiceRunning ---------------------------------->" isChatServiceRunning
                                when ((initialState.data.activeRide.tripType == ST.Rental || isJust advancedRideId) && not isChatServiceRunning) $ do
                                  _ <- launchAff $ EHC.flowRunner defaultGlobalState $ checkAndStartChatService push 2 rideId initialState
                                  pure unit
                                
                                -- Launching the Google Map with playing the audio 
                                pushPlayAudioAndLaunchMap <- runEffectFn1 EHC.getValueFromIdMap "PlayAudioAndLaunchMap"
                                when pushPlayAudioAndLaunchMap.shouldPush $ do 
                                  void $ pure $ runFn2  EHC.updatePushInIdMap "PlayAudioAndLaunchMap" false
                                  void $ launchAff $ flowRunner defaultGlobalState $ playAudioAndLaunchMap push TriggerMaps OnAudioCompleted (fromMaybe false initialState.data.activeRide.acRide) initialState.data.activeRide.requestedVehicleVariant
                                
                                if (initialState.data.activeRide.tripType == ST.Rental && getValueToLocalStore RENTAL_RIDE_STATUS_POLLING == "False")
                                  then do
                                    _ <- pure $ spy "global event rentalRideStatusPolling"
                                    void $ pure $ setValueToLocalStore RENTAL_RIDE_STATUS_POLLING_ID (HU.generateUniqueId unit)
                                    void $ pure $  setValueToLocalStore RENTAL_RIDE_STATUS_POLLING "True"
                                    void $ launchAff $ EHC.flowRunner defaultGlobalState $ rentalRideStatusPolling (getValueToLocalStore RENTAL_RIDE_STATUS_POLLING_ID) 60000.0 initialState push NewStopAdded
                                    pure unit
                                else pure unit
                                if (DA.elem initialState.data.peekHeight [518,470,0]) then void $ push $ RideActionModalAction (RideActionModal.NoAction) else pure unit
                                if (not initialState.props.routeVisible) && initialState.props.mapRendered then do
                                  _ <- JB.getCurrentPosition push $ ModifyRoute
                                  pure $ JB.removeMarker "ic_vehicle_side" -- TODO : remove if we dont require "ic_auto" icon on homescreen
                                  pure unit
                                  else pure unit
            _                -> do
                                void $ push RemoveChat
                                _ <- pure $ setValueToLocalStore RENTAL_RIDE_STATUS_POLLING "False"
                                _ <- pure $ setValueToLocalStore RIDE_G_FREQUENCY "50000"
                                _ <- pure $ JB.removeAllPolylines ""
                                _ <- JB.reallocateMapFragment (EHC.getNewIDWithTag "DriverTrackingHomeScreenMap")
                                _ <- pure $ setValueToLocalStore DRIVER_MIN_DISPLACEMENT "25.0"
                                _ <- pure $ setValueToLocalStore SESSION_ID (JB.generateSessionId unit)
                                _ <- checkPermissionAndUpdateDriverMarker initialState true
                                _ <- launchAff $ EHC.flowRunner defaultGlobalState $ checkCurrentRide push Notification initialState
                                _ <- launchAff $ EHC.flowRunner defaultGlobalState $ paymentStatusPooling initialState.data.paymentState.invoiceId 4 5000.0 initialState push PaymentStatusAction
                                
                                pure unit
          runEffectFn1 consumeBP unit
          pure $ pure unit
        )
  ]
  , eval : (\action state -> do
      let _ = spy "HomeScreen state -----" state
      let _ = spy "HomeScreen--------action" action
      eval action state)
  }

checkAndStartChatService :: forall w . (Action -> Effect Unit) -> Int -> String -> HomeScreenState -> Flow GlobalState Unit
checkAndStartChatService push retry rideId state = 
  when (retry > 0) $ do 
    let isChatServiceRunning = runFn1 JB.isServiceRunning chatService
    let _ = spy "IschatServcie " isChatServiceRunning
    if isChatServiceRunning then do
      delay $ Milliseconds 2000.0
      if state.props.chatcallbackInitiated then pure unit
        else doAff do liftEffect $ push InitializeChat
      checkAndStartChatService push (retry-1) rideId state
    else do 
      liftFlow $ JB.clearChatMessages
      liftFlow $ JB.storeCallBackMessageUpdated push rideId "Driver" UpdateMessages AllChatsLoaded
      liftFlow $ JB.storeCallBackOpenChatScreen push OpenChatScreen
      liftFlow $ JB.scrollOnResume push ScrollToBottom
      liftFlow $ JB.startChatListenerService
      liftFlow $ push InitializeChat

view :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
view push state =
  frameLayout 
  [ height MATCH_PARENT
  , width MATCH_PARENT] $ 
  [ relativeLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , background Color.white900
      , weight 1.0
      , afterRender
        (\action -> do
          void $ Events.endMeasuringDuration "onCreateToHomeScreenRenderDuration"
          void $ Events.endMeasuringDuration "initAppToHomeScreenRenderDuration"
          void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ liftFlowBT HU.hideSplash
          _ <- push action
          _ <- Events.measureDuration "JBridge.setFCMToken" $ JB.setFCMToken push $ SetToken
          _ <- Events.measureDuration "JBridge.getCurrentPosition" $ JB.getCurrentPosition push CurrentLocation
          _ <- Events.measureDuration "JBridge.showMap" $ JB.showMap (EHC.getNewIDWithTag "DriverTrackingHomeScreenMap") (enableCurrentLocation state) "satellite" (17.0) 0.0 0.0 push ShowMap
          _ <- push $ UpdateSpecialZoneList
          pure unit
        ) (const AfterRender)
      , onBackPressed push (const BackPressed)
      ][ Anim.screenAnimationFadeInOut $
          driverMapsHeaderView push state
        , rideActionModelView push state
        -- , if state.data.activeRide.bookingFromOtherPlatform then RideActionModal.bottomPlatformInfoBar VISIBLE else dummyTextView
        ]
      -- , if (getValueToLocalNativeStore PROFILE_DEMO) /= "false" then profileDemoView state push else linearLayout[][]       Disabled ProfileDemoView
      , if state.data.paymentState.makePaymentModal && (not $ DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer, RideCompleted]) then makePaymentModal push state else dummyTextView
      , if state.props.goOfflineModal then goOfflineModal push state else dummyTextView
      , if state.props.enterOtpModal || ((state.data.activeRide.tripType == ST.Intercity || state.data.activeRide.tripType == ST.Rental) && state.props.endRideOtpModal) then enterOtpModal push state else dummyTextView
      , if showEnterOdometerReadingModalView then enterOdometerReadingModal push state else dummyTextView
      , if state.props.endRidePopUp then endRidePopView push state else dummyTextView
      , if ((state.props.isMockLocation && (MU.getMerchant FunctionCall == MU.NAMMAYATRI)) && state.props.currentStage == HomeScreen) then (sourceUnserviceableView push state) else dummyTextView
      , if state.props.cancelConfirmationPopup then cancelConfirmation push state else dummyTextView
      , if state.props.cancelRideModalShow then cancelRidePopUpView push state else dummyTextView
      , if state.props.currentStage == ChatWithCustomer then chatView push state else dummyTextView
      , if state.props.showBonusInfo then requestInfoCardView push state else dummyTextView
      , if state.props.specialZoneProps.specialZonePopup then specialZonePopup push state else dummyTextView
      , if state.props.silentPopUpView then popupModelSilentAsk push state else dummyTextView
      , if state.data.activeRide.waitTimeInfo then waitTimeInfoPopUp push state else dummyTextView
      , if state.props.showAccessbilityPopup then accessibilityPopUpView push state else dummyTextView
      , if state.data.paymentState.showRateCard then rateCardView push state else dummyTextView
      , if (state.props.showlinkAadhaarPopup && state.props.showAadharPopUp) then linkAadhaarPopup push state else dummyTextView
      , if state.props.showAdvancedRidePopUp then advancedRidePopUpView push state else dummyTextView
      , if state.props.rcDeactivePopup then PopUpModal.view (push <<< RCDeactivatedAC) (driverRCPopUpConfig state) else dummyTextView
      , if (state.props.subscriptionPopupType == ST.FREE_TRIAL_POPUP) && state.data.config.subscriptionConfig.enableSubscriptionPopups
           then PopUpModal.view (push <<< FreeTrialEndingAC) (freeTrialEndingPopupConfig state) 
           else linearLayout[visibility GONE][]
      , case HU.getPopupObjectFromSharedPrefs SHOW_JOIN_NAMMAYATRI of
          Just configObject -> if (isLocalStageOn HomeScreen) then PopUpModal.view (push <<< OfferPopupAC) (offerPopupConfig true configObject) else linearLayout[visibility GONE][]
          Nothing -> linearLayout[visibility GONE][]
      , if state.props.showOffer && (MU.getMerchant FunctionCall) == MU.NAMMAYATRI && getValueToLocalStore SHOW_SUBSCRIPTIONS == "true" then PopUpModal.view (push <<< OfferPopupAC) (offerPopupConfig false (offerConfigParams state)) else dummyTextView
      , if (DA.any (_ == state.props.subscriptionPopupType)[ST.SOFT_NUDGE_POPUP,  ST.LOW_DUES_CLEAR_POPUP, ST.GO_ONLINE_BLOCKER] && state.data.config.subscriptionConfig.enableSubscriptionPopups)
          then PopUpModal.view (push <<< PaymentPendingPopupAC) (paymentPendingPopupConfig state) 
        else linearLayout[visibility GONE][]
      , if state.props.showGenericAccessibilityPopUp then genericAccessibilityPopUpView push state else dummyTextView
      , if state.props.showCoinsPopup then PopUpModal.view (push <<< CoinsPopupAC) (introducingCoinsPopup state) else dummyTextView
      , if state.props.coinPopupType /= ST.NO_COIN_POPUP then PopUpModal.view (push <<< CoinEarnedPopupAC) (coinEarnedPopup state) else dummyTextView
      , if state.data.driverGotoState.showGoto then gotoListView push state else dummyTextView
      , if state.data.driverGotoState.goToPopUpType /= ST.NO_POPUP_VIEW then gotoRequestPopupView push state else dummyTextView
      , if showPopups then popupModals push state else dummyTextView
      , if (state.props.showChatBlockerPopUp || state.data.paymentState.showBlockingPopup) then blockerPopUpView push state else dummyTextView
      , if state.props.currentStage == RideCompleted then RideCompletedCard.view (getRideCompletedConfig state) (push <<< RideCompletedAC) else dummyTextView -- 
      , if state.props.showRideRating then RatingCard.view (push <<< RatingCardAC) (getRatingCardConfig state) else dummyTextView
      , if state.props.showAcWorkingPopup == Just true && isCar then isAcWorkingPopupView push state else dummyTextView 
      , if state.props.currentStage == RideAccepted && state.data.activeRide.hasToll && state.props.toll.showTollChargePopup then PopUpModal.view (push <<< TollChargesPopUpAC) (PopUpConfig.tollChargesIncluded state) else dummyTextView
      , if state.props.currentStage == RideCompleted && state.data.endRideData.tollAmbigous && state.props.toll.showTollChargeAmbigousPopup then PopUpModal.view (push <<< TollChargesAmbigousPopUpAC) (PopUpConfig.finalFareExcludesToll state) else dummyTextView
      , if state.props.showInterOperablePopUp then interOperableInfoPopUpView push state else dummyTextView
  ]
  where 
    showPopups = (DA.any (_ == true )
      [ state.props.bgLocationPopup,
        state.data.driverGotoState.gotoLocInRange,
        state.data.driverGotoState.goToInfo,
        state.data.driverGotoState.confirmGotoCancel,
        state.props.accountBlockedPopup,
        state.props.vehicleNSPopup && not state.props.rcDeactivePopup,
        state.props.acExplanationPopup && not onRide && isCar,
        state.props.referralEarned
      ])
    onRide = DA.any (_ == state.props.currentStage) [ST.RideAccepted,ST.RideStarted,ST.ChatWithCustomer, ST.RideCompleted]
    showEnterOdometerReadingModalView = state.props.isOdometerReadingsRequired && ( state.props.enterOdometerReadingModal || state.props.endRideOdometerReadingModal )
    isCar = (RC.getCategoryFromVariant state.data.vehicleType) == Just ST.CarCategory

interOperableInfoPopUpView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
interOperableInfoPopUpView push state =
  let config = interOperableInfoPopup state
  in
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , background Color.black800
  ][PopUpModal.view (push <<< PopUpModalInterOperableAction) config{ layout = Just interOperableInfoLayout }]

interOperableInfoLayout :: forall w. PopUpModal.LayoutConfig -> PrestoDOM (Effect Unit) w
interOperableInfoLayout config = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER
  , margin $ Margin 16 0 16 16
  , padding $ PaddingVertical 12 24
  , cornerRadius 12.0
  , background Color.blue600
  , orientation VERTICAL
  ][  interOperableInfo "" "ic_namma_yatri_logo" "Other Apps"
    , interOperableInfo (getString CUSTOMER_CALLING_AND_MESSAGING) "ny_ic_green_check" "-"
    , interOperableInfo (getString WAITING_CHARGES) "ny_ic_green_check" "-"
    , interOperableInfo (getString CUSTOMER_TIPS) "ny_ic_green_check" "-"
    , interOperableInfo (getString CANCELLATION_CHARGES) "ny_ic_green_check" "-"
    , interOperableInfo (getString $ MERCHANT_POINTS (getString $ MERCHANT_NAME "")) "ny_ic_green_check" "-"
   ]

interOperableInfo :: forall w. String -> String -> String -> PrestoDOM (Effect Unit) w
interOperableInfo text1 image1 text2 =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , padding $ Padding 12 12 12 0
  , orientation HORIZONTAL
  , gravity CENTER
  ][  textView $
      [ width $ V ((EHC.screenWidth unit)/3)
      , height WRAP_CONTENT
      , color Color.black800
      , text text1
      , margin $ MarginRight 5
      , padding $ PaddingRight 5
      , gravity RIGHT
      ] <> FontStyle.body1 TypoGraphy
    , imageView
      [ width $ V 20
      , height $ V 20
      , imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET image1
      , margin $ MarginHorizontal 10 10
      , weight 1.0
      ]
    , textView $
      [ width $ V 30
      , height WRAP_CONTENT
      , text text2
      , gravity CENTER
      , weight 1.0
      ] <> FontStyle.body1 TypoGraphy
   ]


blockerPopUpView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
blockerPopUpView push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][PopUpModal.view 
    (push <<< if state.data.paymentState.showBlockingPopup then StartEarningPopupAC else PopUpModalChatBlockerAction ) 
    (if state.data.paymentState.showBlockingPopup then subsBlockerPopUpConfig state else chatBlockerPopUpConfig state)]
  
accessibilityPopUpView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
accessibilityPopUpView push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.blackLessTrans
  ][PopUpModal.view (push <<< PopUpModalAccessibilityAction) (accessibilityPopUpConfig state)]

advancedRidePopUpView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
advancedRidePopUpView push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.blackLessTrans
  ][PopUpModal.view (push <<< PopUpModalAdvancedRideAction) (advancedRidePopUpConfig state)]

driverMapsHeaderView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
driverMapsHeaderView push state =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , weight 1.0
      , orientation VERTICAL
      , background Color.white900
      ,cornerRadius 50.0
      ][ linearLayout
          [ width MATCH_PARENT
          , height $ V 2
          , background Color.greyTextColor
          , visibility if (DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer]) then GONE else VISIBLE
          , alpha 0.1
          ][]
        , frameLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          ]$[  googleMap state
            , if (state.props.driverStatusSet == Offline && not state.data.paymentState.blockedDueToPayment) then offlineView push state else dummyTextView
            , linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              ]$ [ linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , orientation VERTICAL
                  , background $ Color.white900
                  , stroke $ (if (DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer]) then "0," else "1,") <> "#E5E7EB"
                  ][  driverDetail push state
                    , relativeLayout 
                      [ width MATCH_PARENT
                      , height WRAP_CONTENT
                      , visibility if (DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer]) then GONE else VISIBLE
                      , PP.cornerRadii $ PTD.Corners 24.0  false false true true
                      , padding $ PaddingBottom 12
                      ][ statsModel push state
                       , expandedStatsModel push state
                      ]
                  ]
                , offlineNavigationLinks push state
              ] <> [gotoRecenterAndSupport state push]
                <> if state.props.specialZoneProps.nearBySpecialZone then getCarouselView true false else getCarouselView (DA.any (_ == state.props.driverStatusSet) [ST.Online, ST.Silent]) false  --maybe ([]) (\item -> if DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer] && DA.any (_ == state.props.driverStatusSet) [ST.Online, ST.Silent] then [] else [bannersCarousal item state push]) state.data.bannerData.bannerItem
            , linearLayout
              [ width MATCH_PARENT
              , height MATCH_PARENT
              , orientation VERTICAL
              , background Color.transparent
              , gravity BOTTOM
              ] $ [addAadhaarOrOTPView state push]
                <> [specialPickupZone push state]
                <> if state.props.specialZoneProps.nearBySpecialZone then getCarouselView true false else getCarouselView (state.props.driverStatusSet == ST.Offline) true --maybe ([]) (\item -> if DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer] && DA.any (_ == state.props.driverStatusSet) [ST.Offline] then [] else [bannersCarousal item state push]) state.data.bannerData.bannerItem
                <> if state.data.linkedVehicleCategory /= "AUTO_RICKSHAW" && DA.any (_ == state.props.driverStatusSet) [ST.Online, ST.Silent] then [bookingPreferenceNavView push state] else []
            ]
        ]
        , bottomNavBar push state
  ]
  where
    getCarouselView visible bottomMargin = maybe ([]) (\item -> if DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer] || visible then [] else [bannersCarousal item bottomMargin state push]) state.data.bannerData.bannerItem

bookingPreferenceNavView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
bookingPreferenceNavView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , color Color.red
  , clipChildren false
  ]
  [linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , padding $ Padding 12 12 12 12
    , margin $ Margin 16 16 16 16
    , onClick push $ const BookingOptions
    , background Color.white900
    , rippleColor Color.rippleShade
    , stroke $ "1," <> Color.grey900
    , shadow $ Shadow 0.1 2.0 10.0 15.0 Color.greyBackDarkColor 0.5
    , cornerRadius 8.0
    ][
      imageView
      [ imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET "ny_preferences_filter"
      , height $ V 20
      , width $ V 20
      , margin $ MarginRight 8
      ],
      textView
      $ [ text $ getString BOOKING_OPTIONS
        , color Color.black900
        , weight 1.0
        , textSize FontSize.a_16
        , fontStyle $ FontStyle.semiBold LanguageStyle
        ]
      , imageView
        [ imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_arrow_right"
        , height $ V 18
        , width $ V 18
        ]
    ]
  ]

specialPickupZone :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
specialPickupZone push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , padding $ Padding 12 6 12 8
    , margin $ Margin 16 5 16 16
    , cornerRadius 8.0
    , stroke $ "1," <> Color.darkMint
    , gravity BOTTOM
    , background Color.green300
    , visibility $ boolToVisibility $ not (DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer] || not state.props.statusOnline) && state.props.specialZoneProps.nearBySpecialZone
    , orientation HORIZONTAL
    ]
    [ linearLayout 
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER_VERTICAL
      , weight 1.0
      ] [ textView
          $ [ text $ getString SPECIAL_PICKUP_ZONE_NEARBY
            , color Color.green400
            ]
          <> FontStyle.body4 TypoGraphy
        , textView
          $ [ text $ getString SELECT_A_GREEN_AREA_FOR_PRIORITY_RIDES
            , color Color.green400
            ]
          <> FontStyle.body2 TypoGraphy
        , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , cornerRadius 24.0
            , background Color.green400
            , orientation HORIZONTAL
            , margin $ MarginTop 8
            , onClick push $ const $ SpecialZonePopup
            ]
            [ imageView
              [ height $ V 30
              , width $ V 30
              , padding $ PaddingHorizontal 10 5
              , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET "ny_ic_questionmark_white"
              ]
            , textView
              $ [ height MATCH_PARENT
                , width MATCH_PARENT
                , color Color.white900
                , padding $ Padding 0 4 10 0
                , text $ getString LEARN_MORE 
                ]
              <> FontStyle.body1 TypoGraphy
            ] 
      ]
    , imageView
      [ imageWithFallback $ HU.fetchImage HU.COMMON_ASSET "ny_ic_location_unserviceable_green"
      , height $ V 64
      , width $ V 92
      ]  
    ]

bannersCarousal :: forall w. ListItem -> Boolean -> HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
bannersCarousal view bottomMargin state push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin if bottomMargin && state.data.linkedVehicleCategory /= "AUTO_RICKSHAW" then MarginTop 12 else MarginVertical 12 12
  ][CarouselHolder.carouselView push $ getCarouselConfig view state]

getCarouselConfig ∷ forall a. ListItem → HomeScreenState → CarouselHolder.CarouselHolderConfig BannerCarousel.PropConfig Action
getCarouselConfig view state = {
    view
  , items : BannerCarousel.bannerTransformer $ getBannerConfigs state
  , orientation : VERTICAL
  , currentPage : state.data.bannerData.currentPage
  , autoScroll : true
  , autoScrollDelay : 5000.0
  , id : "bannerCarousel"
  , autoScrollAction : Just UpdateBanner
  , onPageSelected : Just BannerChanged
  , onPageScrollStateChanged : Just BannerStateChanged
  , onPageScrolled : Nothing
  , currentIndex : state.data.bannerData.currentBanner
  , showScrollIndicator : true
  , layoutHeight : V 100
  , overlayScrollIndicator : true
}

genericAccessibilityPopUpView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
genericAccessibilityPopUpView push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , onClick push (const BackPressed)
  , gravity CENTER
  , background Color.blackLessTrans
  ][ PopUpModal.view (push <<< GenericAccessibilityPopUpAction) (genericAccessibilityPopUpConfig state)]
  
gotoRecenterAndSupport :: forall w . HomeScreenState -> (Action -> Effect Unit) ->  PrestoDOM (Effect Unit) w
gotoRecenterAndSupport state push =
  horizontalScrollView
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , scrollBarX false
  ][ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ Margin 12 4 12 0
    , padding $ PaddingRight 30
    , gravity if centerView then CENTER_HORIZONTAL else RIGHT
    , visibility $ boolToVisibility $ not (DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer] || not state.props.statusOnline)
    ][ linearLayout
        [ width WRAP_CONTENT
        , height if showReportText then MATCH_PARENT else WRAP_CONTENT
        , gravity CENTER_VERTICAL
        ][ locationUpdateView push state
          , if state.data.driverGotoState.gotoEnabledForMerchant && state.data.config.gotoConfig.enableGoto 
            then gotoButton push state else linearLayout[][]
          , helpAndSupportBtnView push showReportText
          , recenterBtnView state push
        ]
    ]
  ]
  where 
    showReportText = state.props.currentStage == ST.HomeScreen
    centerView = state.data.driverGotoState.gotoEnabledForMerchant && state.props.driverStatusSet /= ST.Offline && state.props.currentStage == ST.HomeScreen && state.data.config.gotoConfig.enableGoto

locationUpdateView :: forall w .(Action -> Effect Unit) -> HomeScreenState  ->  PrestoDOM (Effect Unit) w
locationUpdateView push state =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginRight 10
  , cornerRadius 22.0
  , background Color.white900
  , padding $ Padding 16 12 16 12
  , gravity CENTER
  , stroke $ "1,"<> Color.grey900
  , rippleColor Color.rippleShade
  , onClick push $ const RetryTimeUpdate
  ][ updateButtonIconAndText push state
   , locationLastUpdatedTextAndTimeView push state
  ]

rateCardView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
rateCardView push state =
  PrestoAnim.animationSet [ Anim.fadeIn true ] $
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][ RateCard.view (push <<< RateCardAC) (rateCardState state) ]


addAadhaarOrOTPView :: forall w. HomeScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
addAadhaarOrOTPView state push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , background Color.transparent
  , padding $ Padding 10 0 10 8
  , gravity BOTTOM
  ][  linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity if showAddAadhaar then CENTER else RIGHT
      ][  addAadhaarNumber push state showAddAadhaar
        , if state.data.config.feature.enableOtpRide then otpButtonView state push else dummyTextView
        ]
      ]
  where showAddAadhaar = state.props.showlinkAadhaarPopup && state.props.statusOnline

otpButtonView :: forall w . HomeScreenState -> (Action -> Effect Unit) ->  PrestoDOM (Effect Unit) w
otpButtonView state push =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , stroke $ "1," <> Color.blue900
    , cornerRadius 32.0
    , background Color.white900
    , visibility if state.props.statusOnline then VISIBLE else GONE
    , padding $ Padding 16 14 16 14
    , margin $ MarginLeft 8
    , gravity CENTER_VERTICAL
    , onClick push $ const $ ZoneOtpAction
    ][ imageView
        [ imageWithFallback $ HU.fetchImage HU.FF_ASSET "ic_mode_standby"
        , width $ V 20
        , height $ V 20
        ]
      , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , color Color.blue900
        , padding $ PaddingLeft 8
        , margin $ MarginBottom 2
        , text $ getString OTP_
        ] <> FontStyle.subHeading2 TypoGraphy
    ]


cancelConfirmation :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
cancelConfirmation push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.blackLessTrans
  ][PopUpModal.view (push <<< PopUpModalCancelConfirmationAction) (cancelConfirmationConfig state )]

linkAadhaarPopup :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
linkAadhaarPopup push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][PopUpModal.view (push <<< LinkAadhaarPopupAC) (linkAadhaarPopupConfig state)]

newStopPopup :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
newStopPopup push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][PopUpModal.view (push <<< NewStopPopup) (newStopPopupConfig state )]

googleMap :: forall w . HomeScreenState -> PrestoDOM (Effect Unit) w
googleMap state =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , background Color.white900
  , id (EHC.getNewIDWithTag "DriverTrackingHomeScreenMap")
  ][]

helpAndSupportBtnView :: forall w .(Action -> Effect Unit) -> Boolean ->  PrestoDOM (Effect Unit) w
helpAndSupportBtnView push showReportText =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginLeft 12
  , cornerRadius 22.0
  , onClick push $ const HelpAndSupportScreen
  , background Color.white900
  , padding $ Padding 16 12 16 12
  , gravity CENTER
  , stroke $ "1,"<> Color.grey900
  , rippleColor Color.rippleShade
  ][ imageView
     [ width $ V 15
     , height $ V 15
     , imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_vector"
     ]
   , textView $
     [ weight 1.0
     , text $ getString REPORT_ISSUE
     , gravity CENTER 
     , margin $ MarginLeft 10
     , color Color.black800
     , visibility if showReportText then VISIBLE else GONE
     ] <> FontStyle.tags TypoGraphy  
  ]

recenterBtnView :: forall w . HomeScreenState -> (Action -> Effect Unit) ->  PrestoDOM (Effect Unit) w
recenterBtnView state push =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , stroke $ "1," <> Color.grey900
  , visibility $ boolToVisibility $ not (DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer] || not state.props.statusOnline)
  , cornerRadius 24.0
  , margin $ MarginLeft 12
  , rippleColor Color.rippleShade
  ][ imageView
    [ width ( V 40 )
    , height ( V 40 )
    , imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_recenter_btn"
    , onClick (\action -> do
            _ <- JB.getCurrentPosition push CurrentLocation
            pure unit
          ) (const RecenterButtonAction)
    ]
  ]

sourceUnserviceableView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
sourceUnserviceableView push state =
  PrestoAnim.animationSet [ Anim.fadeIn true ]
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , PP.cornerRadii $ PTD.Corners 24.0 true true false false
        , alignParentBottom "true,-1"
        , gravity BOTTOM
        ]
        [
          ErrorModal.view (push <<< ErrorModalActionController) (sourceUnserviceableConfig state)
        ]

offlineView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
offlineView push state =
  let showGoInYellow = state.data.config.subscriptionConfig.enableSubscriptionPopups && state.data.paymentState.driverBlocked || (state.data.paymentState.totalPendingManualDues > state.data.subsRemoteConfig.high_due_warning_limit) || not state.data.isVehicleSupported
  in
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , gravity BOTTOM
  , background Color.black9000
  ][ frameLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      ][ linearLayout
          [ height $ V if isBookingPreferenceVisible then 340 else 280
          , width MATCH_PARENT
          , gravity CENTER_HORIZONTAL
          ][ lottieAnimationView
              [ id (EHC.getNewIDWithTag "RippleGoOnlineLottie")
              , afterRender (\_-> do
                              void $ pure $ JB.startLottieProcess JB.lottieAnimationConfig{ rawJson = "rippling_online_effect.json", lottieId = (EHC.getNewIDWithTag "RippleGoOnlineLottie"), speed = 1.0 }
                            )(const NoAction)
              , height WRAP_CONTENT
              , width MATCH_PARENT
              ]
          ]
      , linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity BOTTOM
        ][ linearLayout
            [ height $ V if isBookingPreferenceVisible then 205 else 140
            , width MATCH_PARENT
            , gravity BOTTOM
            , orientation VERTICAL
            , background Color.white900
            , PP.cornerRadii $ PTD.Corners 40.0 true true false false
            ][
              textView $
              [
                height WRAP_CONTENT
              , width MATCH_PARENT
              , gravity CENTER_HORIZONTAL
              , text $ getString if state.data.paymentState.driverBlocked && not state.data.paymentState.subscribed then GO_ONLINE_PROMPT_PAYMENT_PENDING
                                 else if state.data.paymentState.driverBlocked then GO_ONLINE_PROMPT_SUBSCRIBE
                                 else GO_ONLINE_PROMPT
              , margin $ MarginBottom if isBookingPreferenceVisible then 0 else 10
              ] <> FontStyle.paragraphText TypoGraphy
              , if isBookingPreferenceVisible then bookingPreferenceNavView push state else dummyTextView
            ]
        ]
    , linearLayout
        [ height $ V 245
        , width MATCH_PARENT
        , gravity CENTER_HORIZONTAL
        ][ linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            ][ frameLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , margin $ MarginTop 72
                ][ linearLayout
                    [ height $ V 132
                    , width $ V 132
                    , cornerRadius 75.0
                    , background if showGoInYellow then Color.yellowText else Color.darkMint
                    , onClick  push  (const $ SwitchDriverStatus Online)
                    , rippleColor Color.rippleShade
                    ][]
                  , textView
                    [ height MATCH_PARENT
                    , width MATCH_PARENT
                    , gravity CENTER
                    , text $ getString GO_ONLINE
                    , textSize FontSize.a_32
                    , fontStyle $ FontStyle.bold LanguageStyle
                    , color Color.white900
                    ]
              ]
          ]
      ]
    ]
  ]
  where
    isBookingPreferenceVisible = state.data.linkedVehicleCategory /= "AUTO_RICKSHAW" && state.props.driverStatusSet == ST.Offline

popupModelSilentAsk :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
popupModelSilentAsk push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.blackLessTrans
    ][PopUpModal.view (push <<< PopUpModalSilentAction) (silentModeConfig state )]

driverDetail :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
driverDetail push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  , background Color.white900
  , clickable true
  , margin (MarginTop 5)
  ] if rideAccStage && state.data.cityConfig.enableAdvancedBooking then [
        tripStageTopBar push state
    ]
    else [ 
      driverProfile push state,
      accessibilityHeaderView push state (getAccessibilityHeaderText state),
      defaultTopBar
    ]

  where
    defaultTopBar = 
      linearLayout[ 
        width MATCH_PARENT
      , height MATCH_PARENT
      , orientation HORIZONTAL
      , gravity CENTER_HORIZONTAL
      , stroke if state.props.driverStatusSet == Offline then ("2," <> Color.red)
              else if (((getValueToLocalStore IS_DEMOMODE_ENABLED) == "true")&& ((state.props.driverStatusSet == Online) || state.props.driverStatusSet == Silent )) then ("2," <> Color.yellow900)
              else if state.props.driverStatusSet == Online then ("2," <> Color.darkMint)
              else ("2," <> Color.blue800)
      , cornerRadius 50.0
      , alpha if rideAccStage then 0.5 else 1.0
      , margin (Margin 0 10 10 10)
      , visibility $ boolToVisibility $ not $ isJust state.data.activeRide.disabilityTag || state.data.activeRide.bookingFromOtherPlatform
      ](DA.mapWithIndex (\index item ->
          driverStatusPill item push state index
        ) driverStatusIndicators
      ) 
    
    rideAccStage =  DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer]

driverProfile :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
driverProfile push state = 
  let driverImage = 
        case state.data.gender of
          "MALE" | (RC.getCategoryFromVariant state.data.vehicleType) == Just ST.AutoCategory -> "ny_ic_new_avatar_profile"
          "MALE" | (RC.getCategoryFromVariant state.data.vehicleType) == Just ST.CarCategory -> "ny_ic_white_avatar_profile"
          "FEMALE" -> "ny_ic_profile_female"
          _ -> "ny_ic_generic_mascot"
  in
   linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER
    , padding $ Padding 16 20 12 16
    ][ linearLayout [
        width $ V 42
      , height $ V 42
      , onClick push $ const GoToProfile
      ][ imageView
          [ width $ V 42
          , height $ V 42
          , imageWithFallback $ HU.fetchImage HU.FF_ASSET driverImage
          ]
        ]
    ]


tripStageTopBar :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
tripStageTopBar push state = 
  let cityConfig = state.data.cityConfig
  in
    horizontalScrollView[
      width MATCH_PARENT,
      height WRAP_CONTENT,
      scrollBarX false,
      background Color.white900
    ][
      linearLayout[
        width MATCH_PARENT,
        height WRAP_CONTENT,
        background Color.white900,
        margin $ MarginRight 16,
        gravity CENTER_VERTICAL
      ] $ [ 
        driverProfile push state
      , advanceBookingSwitch
      ] 
      <> ( 
            map (\(Tuple childs action) -> (tripStageTopBarPill action) childs) [
              -- [
              --   pillIcon  "ny_ic_blue_shield_white_plus",
              --   pillText  "Safety Center"
              -- ],
            ( Tuple [ pillIcon "ny_ic_red_triangle_warning",
                pillText $ getString REPORT_ISSUE
              ] (const HelpAndSupportScreen))
            ]
          )
    ]
  where 
    tripStageTopBarPill action = 
      linearLayout [
        width WRAP_CONTENT,
        height WRAP_CONTENT,
        padding $ Padding 16 16 16 16,
        margin $ Margin 12 12 0 12,
        cornerRadius 32.0,
        background Color.blue600,
        onClick push action
      ] 
    
    pillIcon imgStr = 
      imageView [
        width $ V 20,
        height $ V 20,
        imageWithFallback $ HU.fetchImage HU.FF_ASSET imgStr
      ]
    
    pillText str = 
      textView $ [
        text str,
        color Color.blue900,
        margin $ MarginLeft 8
      ] <> FontStyle.body6 TypoGraphy

    advanceBookingSwitch = 
      linearLayout [
        width WRAP_CONTENT,
        height WRAP_CONTENT,
        margin $ Margin 12 12 0 12,
        cornerRadius 32.0,
        background $ if isNothing state.data.advancedRideData then Color.grey700 else Color.blue600,
        padding $ Padding 4 4 4 4 
      ]$[ swichBtn (getString CURRENT_BUTTON_TEXT) CURRENT false $ state.props.bookingStage /= CURRENT
        , swichBtn (getString ADVANCE) ADVANCED (isNothing state.data.advancedRideData) (state.props.bookingStage /= ADVANCED)
        ]

    swichBtn txt stage isDisabled switchAllowed = 
      textView $ [
        text $ txt,
        color $ 
          case state.props.bookingStage == stage, isDisabled of
            true, _ -> Color.aliceBlueLight
            false, true -> Color.black700
            false, false -> Color.blue900,
        background $ 
          case state.props.bookingStage == stage, isDisabled of
            true, _ -> Color.blue900
            false, true -> Color.grey700
            false, false -> Color.blue600,
        padding $ Padding 12 12 12 12,
        cornerRadius 32.0,
        alpha $ if isDisabled then 0.5 else 1.0
      ] <> FontStyle.body6 TypoGraphy
        <> if isDisabled && switchAllowed then [] else [onClick push $ const $ SwitchBookingStage stage]




accessibilityHeaderView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> ContentConfig -> PrestoDOM (Effect Unit) w
accessibilityHeaderView push state accessibilityHeaderconfig = 
  linearLayout
  [ weight 1.0
  , height MATCH_PARENT
  , gravity LEFT
  , visibility $ boolToVisibility $ (isJust state.data.activeRide.disabilityTag && not (isJust state.data.advancedRideData)) || state.data.activeRide.bookingFromOtherPlatform
  , margin (Margin 10 10 10 10)
  , background accessibilityHeaderconfig.background
  , cornerRadius 50.0
  , clickable accessibilityHeaderconfig.clickable
  , onClick push $ const AccessibilityHeaderAction
  , padding (Padding 14 8 8 8)
  ][
    imageView
    [ width $ V 25
    , imageWithFallback accessibilityHeaderconfig.imageUrl
    , height $ V 22
    , margin $ Margin 13 5 13 0
    ]
  , linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    ][ textView $
        [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , padding $ PaddingRight 4
            , textFromHtml accessibilityHeaderconfig.primaryText
            , color accessibilityHeaderconfig.textColor
        ] <> FontStyle.body1 TypoGraphy
      , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , visibility $ boolToVisibility $ not $ DS.null accessibilityHeaderconfig.secondaryText
        , text accessibilityHeaderconfig.secondaryText
        , color accessibilityHeaderconfig.textColor
        ] <> FontStyle.body4 TypoGraphy
    ]
  ]

driverStatusPill :: forall w . PillButtonState -> (Action -> Effect Unit) -> HomeScreenState -> Int -> PrestoDOM (Effect Unit) w
driverStatusPill pillConfig push state index =
  let isStatusBtnClickable = not (DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer])
  in
  linearLayout
  [ weight 1.0
  , height $ V 35
  , gravity CENTER
  , color Color.greyTextColor
  , margin (Margin 10 10 10 10)
  , background $ case (getDriverStatusResult index state.props.driverStatusSet pillConfig.status) of
                    ACTIVE -> pillConfig.background
                    DEMO_ -> Color.yellow900
                    DEFAULT -> Color.white900
  , cornerRadius 50.0
  ][  linearLayout
      ([ width MATCH_PARENT
      , height MATCH_PARENT
      , gravity CENTER
      , orientation HORIZONTAL
      , onClick push (const $ SwitchDriverStatus pillConfig.status)
      , clickable isStatusBtnClickable
      , cornerRadius 20.0
      ] <> if isStatusBtnClickable then [rippleColor Color.rippleShade] else [])
      [ imageView
        [ width $ V 15
        , height $ V 15
        , margin (Margin 3 0 5 0)
        , visibility $ case (getDriverStatusResult index state.props.driverStatusSet pillConfig.status) of
                    ACTIVE -> VISIBLE
                    DEMO_ -> VISIBLE
                    DEFAULT -> GONE
        , imageWithFallback $ case (getDriverStatusResult index state.props.driverStatusSet pillConfig.status) of
                    ACTIVE -> pillConfig.imageUrl
                    DEMO_ ->   HU.fetchImage HU.FF_ASSET "ic_driver_status_demo"
                    DEFAULT -> "none"
        ]
      , textView(
        [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , padding (Padding 0 0 4 0)
          , text $ case pillConfig.status of
              Online -> if ((getValueToLocalStore IS_DEMOMODE_ENABLED) == "true") then (getString DEMO) else (getString ONLINE_)
              Offline -> (getString OFFLINE)
              Silent -> (getString SILENT)
          , color $ case (getDriverStatusResult index state.props.driverStatusSet pillConfig.status) of
                    ACTIVE -> pillConfig.textColor
                    DEMO_ -> Color.black900
                    DEFAULT -> Color.greyTextColor
        ] <> FontStyle.body1 TypoGraphy
      )
      ]
  ]

statsModel :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
statsModel push state =
  let cityConfig = state.data.cityConfig
  in
    frameLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , visibility $ boolToVisibility showStatsModel
    , gravity CENTER
    , padding $ Padding 16 2 16 6
    ][  linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , background Color.blue600
        , cornerRadius 12.0
        , padding $ Padding 16 2 8 2
        ][ textView $
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , text $ getString TODAYS_EARNINGS_STR
           , color Color.black700
           , weight 1.0
           , onClick push $ const $ if cityConfig.showEarningSection then ToggleStatsModel else NoAction
           , padding $ PaddingVertical 6 6
           ] <> FontStyle.tags TypoGraphy
         , linearLayout
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , gravity CENTER_VERTICAL
           , onClick push $ const $ if cityConfig.showEarningSection then ToggleStatsModel else NoAction
           ][ textView $
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ "₹" <> formatCurrencyWithCommas (show state.data.totalEarningsOfDay)
              , color Color.black800
              , visibility $ boolToVisibility state.data.driverStats
              ] <> FontStyle.h2 TypoGraphy
            , imageView 
              [ width $ V 12
              , height $ V 12
              , visibility $ boolToVisibility cityConfig.showEarningSection
              , margin $ MarginLeft 5
              , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_chevron_down"
              ]
            ]
         , frameLayout
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , gravity CENTER_VERTICAL
           , margin $ MarginLeft 15
           , visibility $ boolToVisibility coinsEnabled
           ][ linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , padding $ Padding 12 8 12 8
              , stroke $ "1,"<> Color.blue400
              , gravity CENTER_VERTICAL
              , onClick push $ const $ GoToEarningsScreen true
              , cornerRadius 6.0
              ][ imageView 
                  [ width $ V 20
                  , height $ V 20
                  , margin $ MarginRight 5
                  , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_yatri_coin"
                  ]
                , textView $
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text $ case state.data.coinBalance == 0 of
                              true -> getString COINS
                              false -> show state.data.coinBalance
                  , color Color.black700
                  , visibility $ boolToVisibility state.data.driverStats
                  ] <> FontStyle.tags TypoGraphy
              ]
            , imageView 
              [ width $ V 24
              , height $ V 24
              , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_new_red_banner"
              , visibility if (fromMaybe 0 $ fromString (getValueToLocalStore VISITED_DRIVER_COINS_PAGE)) >= 3 then GONE else VISIBLE
              ]
           ]
        ]
    ]
    where 
      showStatsModel = not ((DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer]) && not state.props.isStatsModelExpanded)
      coinsEnabled = state.data.config.feature.enableYatriCoins && state.data.cityConfig.enableYatriCoins

expandedStatsModel :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
expandedStatsModel push state =
  let cityConfig = state.data.cityConfig
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , cornerRadius 12.0
  , padding $ Padding 16 16 16 16
  , margin $ MarginHorizontal 16 16
  , background Color.white900
  , stroke $ "1,"<> Color.grey900
  , visibility $ boolToVisibility state.props.isStatsModelExpanded 
  , clickable true
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER_VERTICAL
      ][ commonTV push (getString TODAYS_EARNINGS_STR) Color.black700 FontStyle.tags LEFT 0 ToggleStatsModel true true
      , commonTV push ("₹" <> formatCurrencyWithCommas (show state.data.totalEarningsOfDay)) Color.black800 FontStyle.h2 RIGHT 0 ToggleStatsModel false state.data.driverStats
      , imageView 
        [ width $ V 12
        , height $ V 12
        , margin $ MarginLeft 5
        , onClick push $ const $ ToggleStatsModel
        , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_chevron_up"
        ]    
      ]
    , dashLine
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginTop 10
      , gravity CENTER_VERTICAL
      ][ commonTV push (getString TRIPS) Color.black700 FontStyle.body3 LEFT 0 NoAction true true
        , commonTV push (show state.data.totalRidesOfDay) Color.black800 FontStyle.subHeading1 RIGHT 0 NoAction false state.data.driverStats
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER_VERTICAL
      , margin $ MarginTop 10
      , visibility $ boolToVisibility $ isJust state.data.earningPerKm
      ][ commonTV push (getString EARNINGS_PER_KM) Color.black700 FontStyle.body3 LEFT 0 NoAction false true
        , imageView 
          [ width $ V 12
          , height $ V 12
          , margin $ MarginLeft 5
          , imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_info_grey"
          , onClick push $ const $ ToggleBonusPopup
          ]
        , commonTV push ("₹" <> earningPerKm <> "/km") Color.black800 FontStyle.subHeading1 RIGHT 0 NoAction true state.data.driverStats
      ]
    , separatorView 10
    , textView $
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , text $ getString VIEW_MORE
      , color Color.blue900
      , gravity CENTER
      , onClick push $ const $ GoToEarningsScreen false
      , margin $ MarginTop 10
      ] <> FontStyle.body3 TypoGraphy
  ]
  where earningPerKm = show $ fromMaybe 0 state.data.earningPerKm

commonTV :: forall w .  (Action -> Effect Unit) -> String -> String -> (LazyCheck -> forall properties. (Array (Prop properties))) -> Gravity -> Int -> Action -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w
commonTV push text' color' theme gravity' marginTop action useWeight visible = 
  textView $
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text text'
  , color color'
  , gravity gravity'
  , margin $ MarginTop marginTop
  , onClick push $ const action
  , visibility $ boolToVisibility visible
  ] <> theme TypoGraphy
    <> if useWeight then [weight 1.0] else []

separatorView :: forall w . Int -> PrestoDOM (Effect Unit) w
separatorView marginTop =
  linearLayout
  [ width MATCH_PARENT
  , height $ V 1
  , background Color.grey900
  , margin $ MarginTop marginTop
  ][]

dashLine :: forall w . PrestoDOM (Effect Unit) w
dashLine =
  imageView
  [ width MATCH_PARENT
  , height $ V 2 
  , margin $ MarginTop 10
  , imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET  "ny_ic_horizontal_dash"
  ]

profileDemoView :: forall w . HomeScreenState -> (Action -> Effect Unit) ->  PrestoDOM (Effect Unit) w
profileDemoView state push =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , background Color.black9000
  , clickable true
  ][ linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , margin $ Margin 10 20 0 0
      , orientation VERTICAL
      ][ imageView
          [ width $ V 50
          , height $ V 50
          , margin $ Margin 5 5 5 5
          , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ic_profile_shadow"
          , onClick push $ const $ GoToProfile
          ]
        , imageView
          [ width $ V 40
          , height $ V 40
          , margin $ Margin 15 10 0 15
          , imageWithFallback $ HU.fetchImage HU.FF_ASSET "up_hand_arrow"
          ]
        , clickHereDemoLayout state push
      ]
  ]

clickHereDemoLayout :: forall w . HomeScreenState -> (Action -> Effect Unit) ->  PrestoDOM (Effect Unit) w
clickHereDemoLayout state push =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , background Color.black900
  , stroke $ "1,"<> Color.yellow900
  , margin $ MarginLeft 10
  , cornerRadius 12.0
  ][ textView $
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , text $ getString CLICK_TO_ACCESS_YOUR_ACCOUNT
      , color Color.white900
      , padding $ Padding 20 12 20 15
      ] <> FontStyle.body13 TypoGraphy
  ]

gotoButton :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
gotoButton push state = 
  frameLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , orientation VERTICAL
  , visibility $ boolToVisibility $ not (DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted, ChatWithCustomer] || not state.props.statusOnline)
  , margin $ MarginTop 3
  ] [ linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , margin $ MarginVertical 5 10
      , cornerRadius 22.0
      , gravity CENTER
      , stroke $ "1,"<> Color.grey900
      ][ PrimaryButton.view (push <<< GoToButtonClickAC) (gotoButtonConfig state)]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , layoutGravity "right"
      ][ textView $
          [ height $ V 20
          , width $ V 20
          , cornerRadius 37.0
          , text $ show state.data.driverGotoState.gotoCount 
          , color Color.white900
          , gravity CENTER
          , background Color.black900 
          ] <> FontStyle.body9 TypoGraphy
        ]
    ]

gotoListView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w 
gotoListView push state = 
  PrestoAnim.entryAnimationSetForward [ Anim.translateInXForwardAnim true, Anim.fadeIn true] $
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  , clickable true
  , margin $ MarginTop 15
  ][ gotoHeader state push 
   , linearLayout
     [ height $ V 1
     , width MATCH_PARENT
     , background Color.grey900
     ][]
   , if DA.null state.data.driverGotoState.savedLocationsArray then noGoToLocationView push state else
     linearLayout
      [ width MATCH_PARENT
      , weight 1.0
      , orientation VERTICAL
      ][ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , background Color.blue600
          , padding $ Padding 10 10 10 10
          , orientation VERTICAL
          , gravity CENTER
          ][ textView $
              [ width MATCH_PARENT
              , text $ getString CHOOSE_A_GOTO_LOC
              ] <> FontStyle.body1 TypoGraphy
            ]
        , savedLocationListView push state
        , linearLayout
            [ width MATCH_PARENT
            , orientation VERTICAL
            , gravity BOTTOM
            , padding $ PaddingTop 5
            ] [
              textView
                [ height $ V 45
                , width MATCH_PARENT
                , gravity CENTER
                , background Color.blue600
                , stroke $ "1,"<>Color.blue600
                , margin $ Margin 17 0 17 20 
                , text $ getString GOTO_LOC_LEFT  <> " " <> show state.data.driverGotoState.gotoCount
                , cornerRadius 6.0 
                , color Color.black900
                ]
              , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , gravity CENTER
                , margin $ Margin 16 0 16 20
                ] [ linearLayout
                    [ weight 1.0
                    , gravity CENTER
                    , height WRAP_CONTENT
                    ][ PrimaryButton.view (push <<< CancelBackAC) (cancelButtonConfig state)]
                  , linearLayout
                    [ margin $ MarginLeft 10
                    , weight 1.0
                    , height WRAP_CONTENT
                    ] [ PrimaryButton.view (push <<< EnableGotoTimerAC) (enableButtonConfig state) ]
                  ]
                ]

      ]
   ]



noGoToLocationView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
noGoToLocationView push state =
  linearLayout
    [ width MATCH_PARENT
    , weight 1.0
    , orientation VERTICAL
    , gravity CENTER
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , orientation VERTICAL
        , weight 1.0
        , gravity CENTER
        ]
        [ imageView
            [ height $ V 240
            , width $ V 240
            , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_no_goto_loc"
            , margin $ Margin 10 20 10 0
            ]
        , textView $
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text $ getString NO_GOTO_LOCS_ADDED_YET
            , gravity CENTER
            , color Color.black900
            , margin $ Margin 20 12 20 0
            , fontStyle $ FontStyle.semiBold LanguageStyle
            ] <> FontStyle.h2 TypoGraphy
        , textView $
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text $ getString NO_GOTO_LOCS_ADDED_YET_DESC
            , gravity CENTER
            , color Color.black700
            , margin $ Margin 20 12 20 0
            ] <> FontStyle.paragraphText TypoGraphy
        ]
      , PrimaryButton.view (push <<< AddLocation) (primaryButtonConfig state)
    ]

savedLocationListView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
savedLocationListView push state =
  scrollView
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , weight 1.0
      , scrollBarY true
      ][ linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin $ MarginTop 8
          , orientation VERTICAL
          , padding $ PaddingBottom 10
          ][  linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , margin $ MarginTop 8
              , orientation VERTICAL
              ](map (\item -> 
                GoToLocationModal.view (push <<< GoToLocationModalAC) (locationListItemConfig item state) ) state.data.driverGotoState.savedLocationsArray)
            , linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , padding $ Padding 16 20 16 20
              , margin $ Margin 16 16 16 0
              , stroke $ "1," <> Color.grey900
              , cornerRadius 8.0
              , gravity CENTER_VERTICAL
              , visibility $ boolToVisibility showAddGoto
              , onClick push $ const AddNewLocation
              ][  imageView
                  [ width $ V 24
                  , height $ V 24
                  , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_add_filled"
                  ]
                , textView $
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , text $ getString ADD_A_GOTO_LOC
                  , margin $ MarginLeft 12
                  , color Color.blue900
                  ] <> FontStyle.body1 TypoGraphy
              ]
          ]
        ]
        where showAddGoto = DA.length state.data.driverGotoState.savedLocationsArray < state.data.config.gotoConfig.maxGotoLocations


gotoHeader :: forall w . HomeScreenState ->  (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
gotoHeader state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , padding $ Padding 10 7 10 10
  , background Color.white900 
  , gravity CENTER_VERTICAL
  ] [ imageView 
      [ height $ V 36
      , width $ V 36
      , padding $ Padding 4 4 4 4
      , imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_chevron_left"
      , onClick push $ const BackPressed
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , padding $ PaddingLeft 15
      ] [ textView $
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , text $ getString ENABLE_GOTO
          , gravity LEFT
          , color Color.black900
          ] <> FontStyle.h3 TypoGraphy
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , onClick push $ const ClickInfo
      ] [ textView $
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , text $ getString KNOW_MORE
          , gravity RIGHT
          , color Color.blue900
          ] <> FontStyle.subHeading2 TypoGraphy
        ]  

    ]



driverStatus :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
driverStatus push state =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , gravity RIGHT
  , margin (Margin 0 0 16 0)
  , visibility if (state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted || state.props.currentStage == ChatWithCustomer ) then GONE else VISIBLE
  ][ linearLayout
     [ width WRAP_CONTENT
     , height MATCH_PARENT
     , gravity CENTER
     , orientation VERTICAL
     ][ imageView
        [ width $ V 50
        , height $ V 30
        , imageWithFallback $ HU.fetchImage HU.FF_ASSET if (getValueToLocalStore IS_DEMOMODE_ENABLED == "true") then "ny_ic_demo_mode_switch" else if state.props.statusOnline then "ny_ic_toggle_on" else "ny_ic_toggle_off"
        , margin (MarginTop 10)
        , onClick push (const (ChangeStatus if state.props.statusOnline then false else true))
        , clickable if state.props.rideActionModal then false else true
        ]
      , textView (
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text if (getValueToLocalStore IS_DEMOMODE_ENABLED == "true") then (getString ONLINE_VIA_DEMO_MODE) else if state.props.statusOnline then (getString ONLINE_) else (getString OFFLINE)
        , color Color.greyTextColor
        ]  <> FontStyle.tags TypoGraphy
        )
      ]
   ]

enterOtpModal :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
enterOtpModal push state =
  InAppKeyboardModal.view (push <<< InAppKeyboardModalAction) (enterOtpStateConfig state)

enterOdometerReadingModal :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
enterOdometerReadingModal push state =
  InAppKeyboardModal.view (push <<< InAppKeyboardModalOdometerAction) (enterOdometerReadingConfig state)
  
showOfflineStatus :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
showOfflineStatus push state =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , background Color.black9000
  , gravity BOTTOM
  , visibility $ boolToVisibility $ not state.props.statusOnline
  ][ PrestoAnim.animationSet
    [ Anim.translateYAnim AnimConfig.translateYAnimConfig
    ] $
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , PP.cornerRadii $ PTD.Corners 20.0 true true false false
      , orientation VERTICAL
      , background Color.white900
      , padding (Padding 0 0 0 20)
      , gravity CENTER
      ][
          linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER
          , padding (Padding 0 30 0 25)
          ][ imageView
             [ width (V 65)
             , height (V 65)
             , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_offline_status"
             ]
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin (Margin ((EHC.screenWidth unit)/5) 0 ((EHC.screenWidth unit)/5) 0)
          ][ textView (
             [ width MATCH_PARENT
             , height WRAP_CONTENT
             , text (getString YOU_ARE_OFFLINE)
             , color Color.black
             , gravity CENTER
             ]  <> FontStyle.h2 TypoGraphy
             )
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin (Margin ((EHC.screenWidth unit)/6) 10 ((EHC.screenWidth unit)/6) 10)
          ][ textView (
             [ width MATCH_PARENT
             , height WRAP_CONTENT
             , text (getString YOU_ARE_CURRENTLY_BUSY_GO_ONLINE_TO_RECIEVE_TRIP_REQUESTS)
             , gravity CENTER
             , color Color.black600
             ]  <> FontStyle.body1 TypoGraphy
             )
          ]
      ]
  ]

makePaymentModal :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
makePaymentModal push state = MakePaymentModal.view (push <<< MakePaymentModalAC) (makePaymentState state)


goOfflineModal :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
goOfflineModal push state =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , clickable true
  , background Color.black9000
  , gravity BOTTOM
  ][
   PrestoAnim.animationSet [
      Anim.translateYAnim AnimConfig.translateYAnimConfig
    ] $
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , PP.cornerRadii $ PTD.Corners 20.0 true true false false
      , orientation VERTICAL
      , background Color.white900
      , padding (Padding 0 0 0 20)
      , gravity CENTER
      ][
          linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER
          , padding (Padding 0 30 0 25)
          ][ imageView
             [ width (V 55)
             , height (V 55)
             , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET activeVehicleImage
             ]
           , imageView
             [ width (V 35)
             , height (V 35)
             , margin (Margin 35 0 35 0)
             , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_chevrons_right"
             ]
           , imageView
             [ width (V 55)
             , height (V 55)
             , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET inActiveVehicleImage
             ]
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin (Margin ((EHC.screenWidth unit)/5) 0 ((EHC.screenWidth unit)/5) 0)
          ][ textView $
             [ width MATCH_PARENT
             , height WRAP_CONTENT
             , text (getString GOING_OFFLINE_WILL_NOT_GET_YOU_ANY_RIDE)
             , color Color.black
             , gravity CENTER
             ] <> FontStyle.body14 TypoGraphy
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER
          , margin (MarginTop 25)
          ][ linearLayout
             [ width (V $ ((EHC.screenWidth unit)/7) * 3)
             , height (V $ 55)
             , stroke ("1," <> Color.black900)
             , cornerRadius 8.0
             , gravity CENTER
             , margin (MarginRight 10)
             , onClick push (const CancelGoOffline)
             , rippleColor Color.rippleShade
             ][ textView (
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text (getString CANCEL)
                , color Color.black900
                ]  <> FontStyle.subHeading1 TypoGraphy
                )
             ]
           , linearLayout
             [ width (V $ ((EHC.screenWidth unit)/7) * 3)
             , height (V $ 55)
             , stroke ("1," <> Color.black900)
             , cornerRadius 8.0
             , gravity CENTER
             , margin (MarginRight 10)
             , background Color.black900
             , onClick push (const $ GoOffline if state.props.statusOnline then false else true)
             , rippleColor Color.rippleShade
             ][ textView  (
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , gravity CENTER
                , text (getString GO_OFFLINE)
                , color Color.yellow900
                ]  <> FontStyle.subHeading1 TypoGraphy
                )
             ]
          ]
      ]
  ]
  where activeVehicleImage = 
          case (RC.getCategoryFromVariant state.data.vehicleType) of
            Just ST.AutoCategory -> "ic_vehicle_side_active_auto"
            Just ST.CarCategory -> "ic_vehicle_side_active_car"
            _ -> ""
        inActiveVehicleImage = 
          case (RC.getCategoryFromVariant state.data.vehicleType) of
            Just ST.AutoCategory -> "ic_vehicle_side_inactive_auto"
            Just ST.CarCategory -> "ic_vehicle_side_inactive_car"
            _ -> ""


addAadhaarNumber :: forall w . (Action -> Effect Unit) -> HomeScreenState -> Boolean -> PrestoDOM (Effect Unit) w
addAadhaarNumber push state visibility' =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , background Color.white900
  , orientation HORIZONTAL
  , cornerRadius 32.0
  , stroke $ "1," <> Color.black600
  , padding $ Padding 20 16 20 16
  , gravity CENTER_VERTICAL
  , onClick push $ const ClickAddAlternateButton
  , visibility if visibility' then VISIBLE else GONE
  , rippleColor Color.rippleShade
  ][  imageView
      [ width $ V 20
      , height $ V 15
      , imageWithFallback $ HU.fetchImage HU.FF_ASSET  "ny_ic_aadhaar_logo" 
      , margin (MarginRight 5)
      ]
    , textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , gravity CENTER
      , text $ getString ENTER_AADHAAR_DETAILS 
      , color Color.black900
      ] <> FontStyle.paragraphText TypoGraphy
   ]

offlineNavigationLinks :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
offlineNavigationLinks push state =
  horizontalScrollView
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , scrollBarX false
  , margin $ MarginHorizontal 16 16
  , visibility if state.props.driverStatusSet == ST.Offline then VISIBLE else GONE
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginTop 8
      ](DA.mapWithIndex (\index item -> 
          linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , background Color.white900
            , orientation HORIZONTAL
            , cornerRadius 32.0
            , stroke $ "1," <> Color.black600
            , padding $ Padding 12 12 12 12
            , margin $ MarginLeft if index == 0 then 0 else 5
            , gravity CENTER_VERTICAL
            , onClick push $ const item.action
            , visibility $ itemVisibility item.action
            , rippleColor Color.rippleShade
            ][  imageView
                [ width $ V 16
                , height $ V 16
                , imageWithFallback $ HU.fetchImage HU.FF_ASSET item.icon
                , margin $ MarginRight 5
                ]
              , textView $
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , gravity CENTER
                , text item.title
                , color Color.black900
                , padding $ PaddingBottom 1
                ] <> FontStyle.tags TypoGraphy
            ]
          ) navLinksArray)
    ]
    where
      navLinksArray = [ {title : getString if showAddGoto then ADD_GOTO else GOTO_LOCS , icon : "ny_ic_loc_goto", action : AddGotoAC},
                        {title : getString ADD_ALTERNATE_NUMBER, icon : "ic_call_plus", action : ClickAddAlternateButton},
                        {title : getString REPORT_ISSUE, icon : "ny_ic_vector_black", action : HelpAndSupportScreen},
                        {title : getString ENTER_AADHAAR_DETAILS, icon : "ny_ic_aadhaar_logo", action : LinkAadhaarAC}
                      ]
      itemVisibility action = case action of
                        ClickAddAlternateButton -> if isNothing state.data.driverAlternateMobile then VISIBLE else GONE
                        LinkAadhaarAC -> if state.props.showlinkAadhaarPopup then VISIBLE else GONE
                        AddGotoAC -> if state.data.driverGotoState.gotoEnabledForMerchant && state.data.config.gotoConfig.enableGoto then VISIBLE else GONE
                        _ -> VISIBLE
      showAddGoto = state.data.driverGotoState.savedLocationCount < state.data.config.gotoConfig.maxGotoLocations

locationLastUpdatedTextAndTimeView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
locationLastUpdatedTextAndTimeView push state =
  linearLayout
  [ height WRAP_CONTENT
    , weight 1.0
    , gravity CENTER_VERTICAL
  ][
    textView $
      [  width WRAP_CONTENT
        , height WRAP_CONTENT
        , ellipsize true
        , singleLine true
        , color Color.black800
        , gravity CENTER_VERTICAL
        , text if state.data.locationLastUpdatedTime == "" then (if (getValueToLocalStore LOCATION_UPDATE_TIME) == "__failed" then getString(NO_LOCATION_UPDATE) else (getValueToLocalStore LOCATION_UPDATE_TIME) ) else state.data.locationLastUpdatedTime
      ] <> FontStyle.body4 TypoGraphy
  ]

updateButtonIconAndText :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
updateButtonIconAndText push state =
  linearLayout
  [ width WRAP_CONTENT
  , height MATCH_PARENT
  , margin $ MarginTop 1
  , orientation HORIZONTAL
  , visibility if not state.props.rideActionModal && state.props.statusOnline then VISIBLE else GONE
  ]
  [ PrestoAnim.animationSet [Anim.rotateAnim (AnimConfig.rotateAnimConfig state.props.refreshAnimation)]
    $ imageView
    [ width $ V 17
    , height $ V 17
    , margin $ MarginRight 5
    , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_refresh"
    ]
  ]

waitTimeInfoPopUp :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
waitTimeInfoPopUp push state =
   PrestoAnim.animationSet [ Anim.fadeIn true ]
     $ linearLayout
         [ height MATCH_PARENT
         , width MATCH_PARENT
         ]
         [ RequestInfoCard.view (push <<< RequestInfoCardAction) (waitTimeInfoCardConfig state) ]



bottomNavBar :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
bottomNavBar push state =
    BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.HOME_SCREEN state.data.config.bottomNavConfig)

rideActionModelView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
rideActionModelView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , alignParentBottom "true,-1"
  , visibility if (DA.any (_ == state.props.currentStage) [RideAccepted,RideStarted,ChatWithCustomer]) then VISIBLE else GONE
  ][  coordinatorLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      ][  bottomSheetLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , PP.sheetState COLLAPSED
          , peakHeight $ if (DA.elem state.data.peekHeight [518,470,0]) || state.data.peekHeight < 450 then getPeekHeight state else state.data.peekHeight
          , topShift 0.0
          ][ if (state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted || state.props.currentStage == ChatWithCustomer) then
                RideActionModal.view (push <<< RideActionModalAction) (rideActionModalConfig state)
                else linearLayout[][]
        ]
      ]
    ]


chatView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
chatView push state =
  PrestoAnim.animationSet [ Anim.translateYAnimFromTop $ AnimConfig.translateFullYAnimWithDurationConfig 300 ] $
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , alignParentBottom "true,-1"
  , adjustViewWithKeyboard "true"
  , background Color.transparent
  ][ ChatView.view (push <<< ChatViewActionController) (chatViewConfig state) ]

cancelRidePopUpView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
cancelRidePopUpView push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    ][ SelectListModal.view (push <<< CancelRideModalAction) (cancelRideModalConfig state)
    ]

endRidePopView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
endRidePopView push state =
  linearLayout
    [ width MATCH_PARENT,
      height MATCH_PARENT
    ][ PopUpModal.view (push <<<PopUpModalAction) (endRidePopUp state )]

dummyTextView :: forall w . PrestoDOM (Effect Unit) w
dummyTextView =
  textView
  [ visibility GONE]

requestInfoCardView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
requestInfoCardView push state =
  PrestoAnim.animationSet [ Anim.fadeIn true ]
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        ]
        [ RequestInfoCard.view (push <<< RequestInfoCardAction) (requestInfoCardConfig FunctionCall) ]

specialZonePopup :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
specialZonePopup push state =
  PrestoAnim.animationSet [ Anim.fadeIn true ]
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        ][ RequestInfoCard.view (push <<< SpecialZoneCardAC) (specialZonePopupConfig state) ]

gotoRequestPopupView :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
gotoRequestPopupView push state = 
  PrestoAnim.animationSet [ Anim.fadeIn true ]
    $ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      ][PopUpModal.view (push <<< GotoRequestPopupAction) (gotoRequestPopupConfig state)]

popupModals :: forall w. (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
popupModals push state = 
  PrestoAnim.animationSet [ Anim.fadeIn true ]
    $ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      ][PopUpModal.view (push <<< clickAction popupType) 
        case popupType of 
          ST.LocInRange -> gotoLocInRangeConfig state
          ST.KnowMore -> gotoKnowMoreConfig state
          ST.DisableGotoPopup -> disableGotoConfig state
          ST.AccountBlocked -> accountBlockedPopup state
          ST.VehicleNotSupported -> vehicleNotSupportedPopup state
          ST.BgLocationPopup -> bgLocPopup state
          ST.TopAcDriver -> topAcDriverPopUpConfig state
          ST.ReferralEarned -> referralEarnedConfig state
      ]
  where 
  
    popupType = if state.data.driverGotoState.gotoLocInRange then ST.LocInRange
      else if state.data.driverGotoState.goToInfo then ST.KnowMore
      else if state.data.driverGotoState.confirmGotoCancel then ST.DisableGotoPopup
      else if state.props.accountBlockedPopup then ST.AccountBlocked
      else if state.props.vehicleNSPopup then ST.VehicleNotSupported
      else if state.props.bgLocationPopup then ST.BgLocationPopup
      else if state.props.acExplanationPopup then ST.TopAcDriver
      else if state.props.referralEarned then ST.ReferralEarned
      else ST.KnowMore

    clickAction popupType = case popupType of
          ST.LocInRange -> GotoLocInRangeAction
          ST.KnowMore -> GotoKnowMoreAction
          ST.DisableGotoPopup -> ConfirmDisableGoto
          ST.AccountBlocked -> AccountBlockedAC
          ST.VehicleNotSupported -> VehicleNotSupportedAC
          ST.BgLocationPopup -> BgLocationPopupAC
          ST.TopAcDriver -> ACExpController
          ST.ReferralEarned -> ReferralEarnedAC

enableCurrentLocation :: HomeScreenState -> Boolean
enableCurrentLocation state = if (DA.any (_ == state.props.currentStage) [RideAccepted, RideStarted]) then false else true


rideStatusPolling :: forall action. String -> Number -> HomeScreenState -> (action -> Effect Unit) -> (String -> action) -> Flow GlobalState Unit
rideStatusPolling pollingId duration state push action = do
  if (getValueToLocalStore RIDE_STATUS_POLLING) == "True" && (getValueToLocalStore RIDE_STATUS_POLLING_ID) == pollingId && (DA.any (\stage -> isLocalStageOn stage) [ RideAccepted, ChatWithCustomer]) then do
    activeRideResponse <- Remote.getRideHistoryReq "1" "0" "true" "null" "null" --- needs review here
    _ <- pure $ spy "polling inside rideStatusPolling function" activeRideResponse
    case activeRideResponse of
      Right (GetRidesHistoryResp rideList) -> do
        if not (DA.null rideList.list) then do
          void $ delay $ Milliseconds duration
          rideStatusPolling pollingId duration state push action
          else do
            _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING_ID (HU.generateUniqueId unit)
            _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING "False"
            if isLocalStageOn HomeScreen then pure unit
              else doAff do liftEffect $ push $ action "CANCELLED_PRODUCT"
      Left err -> pure unit
    else pure unit

rentalRideStatusPolling :: forall action. String -> Number -> HomeScreenState -> (action -> Effect Unit) -> (Paths -> Maybe StopLocation -> Maybe StopLocation -> action) -> Flow GlobalState Unit
rentalRideStatusPolling pollingId duration state push action = do
 
  if isRentalRideStatusPollingActive state then do
    activeRideResponse <- Remote.getRideHistoryReq "2" "0" "true" "null" "null"
    case activeRideResponse of
      Right (GetRidesHistoryResp rideList) -> do
        case rideList.list of
          [] -> do
              _ <- pure $ setValueToLocalStore RENTAL_RIDE_STATUS_POLLING_ID (HU.generateUniqueId unit)
              _ <- pure $ setValueToLocalStore RENTAL_RIDE_STATUS_POLLING "False"
              pure unit
          _ ->
            for_ (rideList.list) \(RidesInfo {nextStopLocation,lastStopLocation}) -> do
              if not (checkNextStopLocationIsSame nextStopLocation (constructLocationInfo state.data.activeRide.nextStopLat state.data.activeRide.nextStopLon)) then do
                currentLocation <- doAff do liftEffect JB.getCurrentLatLong 
                doAff do liftEffect $ push $ action currentLocation nextStopLocation lastStopLocation
              else do pure unit

              void $ delay $ Milliseconds duration
              rentalRideStatusPolling pollingId duration state push action
      Left err -> do
            _ <- pure $ setValueToLocalStore RENTAL_RIDE_STATUS_POLLING_ID (HU.generateUniqueId unit)
            _ <- pure $ setValueToLocalStore RENTAL_RIDE_STATUS_POLLING "False"
            pure unit
    else pure unit
  where 
    isRentalRideStatusPollingActive :: HomeScreenState -> Boolean
    isRentalRideStatusPollingActive state = 
      (getValueToLocalStore RENTAL_RIDE_STATUS_POLLING) == "True" 
      && (getValueToLocalStore RENTAL_RIDE_STATUS_POLLING_ID) == pollingId 
      && (state.data.activeRide.tripType == ST.Rental) 
      && isLocalStageOn ST.RideStarted 
    checkNextStopLocationIsSame :: Maybe StopLocation -> Maybe LocationInfo -> Boolean
    checkNextStopLocationIsSame nextStopLocation activeRideNextStopLocation = 
      case nextStopLocation,activeRideNextStopLocation of 
        Just nextStopLocation', Just activeRideNextStopLocation' -> ((nextStopLocation' ^. _lat) == (activeRideNextStopLocation' ^. _lat))  && ((nextStopLocation' ^. _lon) == (activeRideNextStopLocation' ^. _lon))
        Nothing,Nothing  -> true
        _,_ -> false

rideRequestPolling :: forall action. String -> Int -> Number -> HomeScreenState -> (action -> Effect Unit) -> (String -> action) -> Flow GlobalState Unit
rideRequestPolling pollingId count duration state push action = do
  if (getValueToLocalStore RIDE_STATUS_POLLING) == "True" && (getValueToLocalStore RIDE_STATUS_POLLING_ID) == pollingId && isLocalStageOn RideRequested then do
    activeRideResponse <- Remote.getRideHistoryReq "1" "0" "true" "null" "null"
    _ <- pure $ spy "polling inside rideRequestPolling function" activeRideResponse
    case activeRideResponse of
      Right (GetRidesHistoryResp rideList) -> do
        if (DA.null rideList.list) && count > 0 then do
          void $ delay $ Milliseconds duration
          rideRequestPolling pollingId (count - 1) duration state push action
          else do
            _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING_ID (HU.generateUniqueId unit)
            _ <- pure $ setValueToLocalStore RIDE_STATUS_POLLING "False"
            if isLocalStageOn RideAccepted then pure unit
              else doAff do liftEffect $ push $ action "DRIVER_ASSIGNMENT"
      Left err -> pure unit
    else pure unit

paymentStatusPooling :: forall action. String -> Int -> Number -> HomeScreenState -> (action -> Effect Unit) -> (APIPaymentStatus -> action) -> Flow GlobalState Unit
paymentStatusPooling orderId count delayDuration state push action = do
  if (getValueToLocalStore PAYMENT_STATUS_POOLING) == "true" && isLocalStageOn HomeScreen && count > 0 && orderId /= "" then do
    orderStatus <- Remote.paymentOrderStatus orderId
    _ <- pure $ spy "polling inside paymentStatusPooling function" orderStatus
    case orderStatus of
      Right (OrderStatusRes resp) -> do
        if (DA.any (_ == resp.status) [CHARGED, AUTHORIZATION_FAILED, AUTHENTICATION_FAILED, JUSPAY_DECLINED]) then do
            _ <- pure $ setValueToLocalStore PAYMENT_STATUS_POOLING "false"
            doAff do liftEffect $ push $ action resp.status
        else do
            void $ delay $ Milliseconds delayDuration
            paymentStatusPooling orderId (count - 1) delayDuration state push action
      Left err -> pure unit
    else pure unit

checkCurrentRide :: forall action.(action -> Effect Unit) -> (String -> action) -> HomeScreenState -> Flow GlobalState Unit
checkCurrentRide push action initialState = do
  activeRideResponse <- Remote.getRideHistoryReq "1" "0" "true" "null" "null"
  case activeRideResponse of
      Right (GetRidesHistoryResp rideList) -> do
        if (DA.null rideList.list) || isJust initialState.data.advancedRideData then
          pure unit
          else do
            case (rideList.list DA.!! 0) of
              Just ride -> doAff do liftEffect $ push $ action "DRIVER_ASSIGNMENT"
              Nothing -> pure unit
      Left err -> pure unit

rideRequestPollingData :: RideRequestPollingData
rideRequestPollingData = {
  duration : 18,
  delay : 3000.0
}

getDriverStatusResult :: Int -> DriverStatus -> DriverStatus -> DriverStatusResult
getDriverStatusResult index driverStatus currentStatus= case (getValueToLocalStore IS_DEMOMODE_ENABLED) of
                  "true" -> if(index == 2) then DEMO_
                            else DEFAULT
                  _ -> if (driverStatus == currentStatus) then ACTIVE
                       else DEFAULT

playAudioAndLaunchMap :: forall action. (action -> Effect Unit) ->  action -> (String -> action) -> Boolean -> Maybe String -> Flow GlobalState Unit
playAudioAndLaunchMap push action audioCompleted acRide requestedVehicleVariant = do
  let 
    driverLocation = DS.toLower $ getValueToLocalStore DRIVER_LOCATION

  if (driverLocation == "bangalore" && isJust requestedVehicleVariant && (not $ requestedVehicleVariant == Just "AUTO_RICKSHAW") && (getValueToLocalStore TRIGGER_MAPS) == "true" ) then  do
    let 
      language = "kn"
      audioFolder = if acRide  then "ac_background" else "non_ac_background" 
      audioUrl = "https://assets.moving.tech/beckn/audios/" <> audioFolder <> "/" <> language <> ".mp3"
    void $ pure $ runFn3 JB.startAudioPlayer audioUrl push audioCompleted
  else do  
    void $ delay $ Milliseconds 2000.0
    if (getValueToLocalStore TRIGGER_MAPS == "true") then liftFlow $ push $ action  else pure unit
  pure unit


checkBgLocation :: forall action. (action -> Effect Unit) ->  action -> HomeScreenState -> Boolean -> Flow GlobalState Unit
checkBgLocation push action state bgLocPopupShown = do
  andv <- liftFlow $ JB.getAndroidVersion
  bgp <- liftFlow $ JB.isBackgroundLocationEnabled unit
  let onRide = (DA.any (_ == state.props.currentStage) [ST.RideAccepted,ST.RideStarted,ST.ChatWithCustomer])
  if not bgLocPopupShown && andv >= 14 && not bgp && not onRide then doAff do liftEffect $ push $ action
  else pure unit

computeListItem :: (Action -> Effect Unit) -> Flow GlobalState Unit
computeListItem push = do
  bannerItem <- preComputeListItem $ BannerCarousel.view push (BannerCarousel.config BannerCarousal)
  void $ EHC.liftFlow $ push (SetBannerItem bannerItem)

isAcWorkingPopupView :: forall w . (Action -> Effect Unit) -> HomeScreenState -> PrestoDOM (Effect Unit) w
isAcWorkingPopupView push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    ][ PopUpModal.view (push <<< IsAcWorkingPopUpAction) (isAcWorkingPopupConfig state) ]