{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.MessagingView.View where

import Common.Types.App
import Animation (translateInXBackwardAnim, translateInXForwardAnim, translateYAnimFromTop, fadeIn)
import Animation.Config (translateFullYAnimWithDurationConfig)
import Components.MessagingView.Controller (Action(..), Config(..), ChatComponent)
import Components.ChatView as ChatView
import Data.Array (mapWithIndex, (!!), length, null, head)
import Data.Function.Uncurried (runFn1)
import Data.Int as Int
import Data.Maybe (fromMaybe, Maybe(..), maybe)
import Data.Number (pow)
import Data.Number as Num
import Data.String as STR
import Effect (Effect)
import Engineering.Helpers.Commons (safeMarginBottom, getNewIDWithTag, screenWidth, screenHeight, os, safeMarginTop, isPreviousVersion, convertUTCtoISC)
import Engineering.Helpers.Suggestions (getMessageFromKey, chatSuggestion)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge (scrollToEnd, getLayoutBounds, getKeyInSharedPrefKeys, renderBase64Image)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, pure, unit, show, ($), (&&), (-), (/), (<>), (==), (>), (*), (/=), (||), not, negate, (+), (<=), discard, void, (>=), (<), when, (<<<))
import PrestoDOM (Accessiblity(..), BottomSheetState(..), Gravity(..), JustifyContent(..), FlexDirection(..), FlexWrap(..), AlignItems(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), LetterSpacing(..), layoutGravity, accessibility, accessibilityHint, adjustViewWithKeyboard, afterRender, alignParentBottom, background, bottomShift, clickable, color, cornerRadius, editText, ellipsize, fontStyle, gravity, halfExpandedRatio, height, hint, hintColor, horizontalScrollView, id, imageView, imageWithFallback, linearLayout, margin, maxLines, onAnimationEnd, onChange, onClick, onStateChanged, orientation, padding, pattern, peakHeight, relativeLayout, scrollBarX, scrollBarY, scrollView, singleLine, stroke, text, textFromHtml, textSize, textView, topShift, visibility, weight, width, nestedScrollView, flexBoxLayout, justifyContent, flexDirection, flexWrap, alignItems, disableKeyboardAvoidance, letterSpacing, rippleColor)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Elements (bottomSheetLayout, coordinatorLayout)
import PrestoDOM.Events (afterRender)
import PrestoDOM.Properties (alpha, cornerRadii, lineHeight, sheetState)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Storage (KeyStore(..), getValueToLocalStore)
import Mobility.Prelude (boolToVisibility)
import Locale.Utils
import Storage (KeyStore(..))
import LocalStorage.Cache (getValueFromCache)
import Engineering.Helpers.Utils(getFlexBoxCompatibleVersion)
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple (Tuple(..))
import Debug

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
 PrestoAnim.animationSet [ translateYAnimFromTop $ translateFullYAnimWithDurationConfig 300 true] $ 
  relativeLayout
  [ height $ MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , clickable $ os == "IOS"
  , accessibility DISABLE
  ][ PrestoAnim.animationSet [ translateYAnimFromTop $ translateFullYAnimWithDurationConfig 300 true] $ 
      linearLayout
     [ height $ WRAP_CONTENT
     , width $ MATCH_PARENT
     , clickable $ os == "IOS"
     , alignParentBottom "true,-1"
     , adjustViewWithKeyboard "true"
     , margin $ MarginTop safeMarginTop
     , accessibility DISABLE
     ][ linearLayout
        [ height $ V $ config.peekHeight + 125
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.grey700
        , clickable true
        , adjustViewWithKeyboard "false"
        , cornerRadii $ Corners 24.0 true true false false
        , stroke $ config.config.driverInfoConfig.cardStroke
        , accessibility DISABLE
        ][ chatHeaderView config push
         , chatBodyView config push 
         ]
      ]
    , linearLayout
      [ height $ WRAP_CONTENT
      , width $ MATCH_PARENT
      , orientation VERTICAL
      , alignParentBottom "true,-1"
      , adjustViewWithKeyboard "true"
      ][ chatFooterView config push ]
   ] <> if config.fullScreenImage then [ChatView.fullScreenImage ChatView.config (push <<< ChatViewActionController)] else []

    -- fullScreenImageView
        -- else []
--    linearLayout[
--  -- button
--  -- 
--    ] [fullScreenImage config push] else []


-- fullScreenImageView -- ll
  -- dismissButtonView -- ll

  -- fullScreenImage --

-- original one :  <> if config.fullScreenImage then [fullScreenImage config push] else [] use this when not working

-- fullScreenImageView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
-- fullScreenImageView config push = 
  -- linearLayout
  -- [ height WRAP_CONTENT
  -- , width MATCH_PARENT
  -- , gravity LEFT
  -- , accessibility ENABLE
  -- , onClick push $ const OnClickClose
  -- , rippleColor Color.rippleShade
  -- , cornerRadius 20.0

  
  -- ][imageView
  --   [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left_with_bg"
  --   , height $ V 36
  --   , width  $ V 36
  --   , gravity LEFT
  --   ]
  -- ]

--Currently using : 

-- fullScreenImageView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
-- fullScreenImageView config push = 
--     linearLayout
--           [ height WRAP_CONTENT
--           , width MATCH_PARENT
--           , gravity LEFT
--           , accessibility ENABLE
--           , onClick push $ const OnClickClose
--           , cornerRadius 20.0
--           ][ imageView
--             [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left_with_bg"   --ny_ic_chevron_left_with_bg
--             , height $ V 30
--             , width $ V 30
--             , accessibility DISABLE
--             , rippleColor Color.rippleShade
--             , cornerRadius 24.0
--             , margin $ Margin 5 10 0 0
--             ]
--           ]

chatHeaderView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chatHeaderView config push =
  linearLayout
  [ orientation VERTICAL
  , height $ WRAP_CONTENT
  , width MATCH_PARENT
  , clickable true
  , id $ getNewIDWithTag "MessagingHeaderView"
  , accessibility DISABLE
  ][ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , orientation HORIZONTAL
    , margin $ MarginTop 12
    , accessibility DISABLE
    , padding $ PaddingHorizontal 8 16
    ][ linearLayout
        [ height $ V 36
        , width $ V 36
        , gravity CENTER
        , accessibilityHint "Back : Button : Return to ride details"
        , accessibility ENABLE
        , onClick push $ const BackPressed
        , rippleColor Color.rippleShade
        , cornerRadius 20.0
        ][ imageView
          [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
          , height $ V 24
          , width $ V 24
          , accessibility DISABLE
          , cornerRadius 24.0
          ]
        ]
      , headerNameView config push
      , headerActionView config push
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height $ V 1
      , margin $ MarginTop 12
      , background Color.grey900
      ][]
  ]

headerNameView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerNameView config push =
  let lang = getLanguageLocale languageKey
  in
  linearLayout
  [ height WRAP_CONTENT
  , width $ V (((screenWidth unit)/10)* 6)
  , orientation VERTICAL
  , margin $ MarginLeft 8
  ] $ [linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , accessibilityHint $ "Chat With : " <> config.userConfig.userName
    , accessibility ENABLE
    ][ textView $
      [ text $ if lang == "HI_IN" then config.userConfig.userName <> " " else getString CHAT_WITH <> " "
      , color Color.black800
      , ellipsize true
      , singleLine true
      , margin $ MarginBottom if config.feature.showVehicleDetails then 8 else 0
      , accessibility DISABLE
      ] <> if lang == "HI_IN" then FontStyle.body15 TypoGraphy else FontStyle.tags TypoGraphy
    , textView $
      [ text $ if lang == "HI_IN" then getString CHAT_WITH else config.userConfig.userName
      , color Color.black800
      , ellipsize true
      , singleLine true
      , accessibility DISABLE
      , margin $ MarginBottom if config.feature.showVehicleDetails then 8 else 0
      ] <> if lang == "HI_IN" then FontStyle.tags TypoGraphy else FontStyle.body15 TypoGraphy
    ]
  ] <> if config.feature.showVehicleDetails then [vehicleAndOTPAndPriceView config] else []

vehicleAndOTPAndPriceView :: forall w. Config -> PrestoDOM (Effect Unit) w
vehicleAndOTPAndPriceView config = 
  horizontalScrollView
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , scrollBarX false
  , accessibility DISABLE
  , disableKeyboardAvoidance true
  ][ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , accessibility DISABLE
    ][ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , cornerRadius 4.0
        , margin $ MarginRight 6
        , stroke $ "1,"<> Color.black900
        , background Color.yellow900
        , accessibilityHint $ "Vehicle Number : " <> (splitString config.vehicleNo)
        , accessibility ENABLE
        , padding $ Padding 4 2 4 2
        ][ textView $ 
          [ text config.vehicleNo
          , color Color.black800
          , accessibility DISABLE
          ] <> FontStyle.tags TypoGraphy
        ]
      , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , background Color.white900
        , cornerRadius 4.0
        , accessibility ENABLE
        , accessibilityHint $ "O T P : " <> (splitString config.otp)
        , margin $ MarginRight 6
        , padding $ Padding 4 2 4 2
        ][ textView $
          [ text $ "OTP "
          , color Color.black700
          , accessibility DISABLE
          ] <> FontStyle.tags TypoGraphy
        , textView $
          [ text $ config.otp
          , color Color.black700
          , letterSpacing $ PX 1.0
          , accessibility DISABLE
          ] <> FontStyle.body15 TypoGraphy
        ]
      , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , background Color.white900
        , cornerRadius 4.0
        , margin $ MarginRight 6
        , padding $ Padding 4 2 4 2
        , accessibility ENABLE
        , accessibilityHint $ "Fare : " <> config.config.currency <> config.fareAmount
        ][ textView $
          [ text $ "Fare: " <> config.config.currency <> config.fareAmount
          , color Color.black700
          , accessibility DISABLE
          ] <> FontStyle.tags TypoGraphy
        ]
    ]
  ]

headerActionView ::forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerActionView config push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity RIGHT
  ][ linearLayout
     [ height $ V 40
     , width $ V 40
     , gravity CENTER
     , cornerRadius if os == "IOS" then 20.0 else 32.0
     , clickable true
     , background $ Color.green200
     , onClick push $ const $ Call
     , accessibilityHint "Call Driver : Button"
     , accessibility ENABLE
     , rippleColor Color.rippleShade
     ][ imageView
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_call"
        , height $ V 16
        , width $ V 16
        , accessibility DISABLE
        ]
     ]
  ]

chatBodyView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chatBodyView config push =
  if (null config.messages) && not config.feature.showAutoGeneratedText then
    emptyChatView config push
  else 
    chatView config push

chatView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chatView config push =
  let messagesLen = length (spy "testing: " config.messages)
  in
  linearLayout
  [ weight 1.0
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginBottom $ getChatFooterHeight config
  ][scrollView
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , id $ getNewIDWithTag "ChatScrollView"
    , scrollBarY false
    , disableKeyboardAvoidance true
    ]
    [ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][  linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , padding $ PaddingHorizontal 16 16
          , visibility $ boolToVisibility $ config.feature.showAutoGeneratedText
          ][ chatComponent config push {message : config.autoGeneratedText, sentBy : "Driver", timeStamp : config.rideConfirmedAt , type : "" , delay : 0} false "DRIVER" messagesLen ]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , padding (PaddingHorizontal 16 16)
          ](mapWithIndex (\index item -> chatComponent config push item (index == (messagesLen - 1)) (config.userConfig.receiver) index) (config.messages))
      ]
    ]
  ]

chatFooterView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chatFooterView config push =
  linearLayout
  [ height $ WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  , gravity CENTER_VERTICAL
  , accessibility DISABLE
  , alignParentBottom "true,-1"
  , id $ getNewIDWithTag "MessagingFooterView"
  , afterRender push $ const $ NoAction
  , padding $ Padding 16 16 16 16
  ][ suggestionsView config push
   , linearLayout
      [ height $ V 48
      , width MATCH_PARENT
      , padding (PaddingHorizontal 16 8)
      , cornerRadius 24.0
      , accessibility DISABLE
      , gravity CENTER_VERTICAL
      , background Color.blue600
      , orientation HORIZONTAL
      , afterRender push $ const $ NoAction
      ][ editText $
         [ weight 1.0
         , height $ V 48
         , id (getNewIDWithTag "ChatInputEditText")
         , background Color.blue600
         , cornerRadius 24.0
         , accessibility ENABLE
         , accessibilityHint $ "Message Driver : Text Input : Select to send message to driver" 
         , hint $ config.hint <> " " <> fromMaybe "" ((STR.split (STR.Pattern " ") config.userConfig.userName) !! 0) <> "..."
         , singleLine true
         , hintColor Color.black700
         , ellipsize true
         , onChange push $ TextChanged
         , pattern "[^\n]*,255"
         ] <> FontStyle.body1 LanguageStyle
       , linearLayout      --camera image
         [ height $ V 36
         , width $ V 36
         , gravity CENTER
         , onClick push $ const $ CaptureImage
         ][ imageView
            [ imageWithFallback $ ",https://assets.juspay.in/nammayatri/images/driver/ny_ic_camera_white.png"
            , accessibility DISABLE
            , height $ V 20 
            , width $ V 20 
            ] 
         ] 
       , linearLayout     -- send button
         [ height $ V 36
         , width $ V 36
         , gravity CENTER
         , onClick push $ const $ SendMessage
         , accessibilityHint "Send Message : Button"
         , accessibility ENABLE
         ][ imageView
            [ imageWithFallback $ if config.feature.sendMessageActive then fetchImage FF_COMMON_ASSET "ic_send_blue" else fetchImage FF_COMMON_ASSET "ic_send"
            , accessibility DISABLE
            , height $ V 20 
            , width $ V 20 
            ] 
         ]
      ]
  ]

emptyChatView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
emptyChatView config push =
  linearLayout
  [ height $ V 150
  , background Color.transparent
  , width MATCH_PARENT
  , accessibility DISABLE
  ][]

suggestionsView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
suggestionsView config push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , afterRender push $ const $ NoAction
  , visibility $ boolToVisibility $ not $ null config.chatSuggestionsList || not config.feature.canSendSuggestion || not config.feature.enableSuggestions
  , background Color.white900
  , accessibility DISABLE
  , margin $ MarginBottom 16
  ][ textView $
     [ text $ getString QUICK <> "\n" <> getString REPLIES
     , color Color.black700
     , margin $ MarginRight 12
     , accessibility ENABLE
     , accessibilityHint $ "Quick Replies"
     ] <> FontStyle.captions TypoGraphy
   , horizontalScrollView
     [ height WRAP_CONTENT
     , weight 1.0
     , scrollBarX false
     , accessibility DISABLE
     , disableKeyboardAvoidance true
     ][ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity LEFT
        , accessibility DISABLE
        , rippleColor Color.rippleShade
        ] (mapWithIndex (\index item -> quickMessageView config item index push)(config.chatSuggestionsList))
      ]
  ]

quickMessageView :: forall w. Config -> String -> Int -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
quickMessageView config message idx push =
  let value = getMessageFromKey config.suggestionKey message config.languageKey
  in
  linearLayout
  [ height WRAP_CONTENT
  , width $ WRAP_CONTENT
  , onClick push $ const $ SendSuggestion message
  , background Color.blue600
  , visibility $ boolToVisibility $ not $ STR.null value
  , cornerRadius 12.0
  , accessibility ENABLE
  , accessibilityHint $ (getMessageFromKey config.suggestionKey message "EN_US") <> " : Button : Select to send message to driver"
  , margin $ MarginLeft if idx == 0 then 0 else 8
  , padding $ Padding 16 6 16 6
  , rippleColor Color.rippleShade
  ][ textView $
     [ text $ value
     , accessibility DISABLE
     , color Color.black900
     ] <> FontStyle.tags TypoGraphy
  ]
chatComponent :: forall w. Config -> (Action -> Effect Unit) -> ChatComponent -> Boolean -> String -> Int -> PrestoDOM (Effect Unit) w
chatComponent state push config isLastItem receiver index =
  let message = getMessageFromKey state.suggestionKey config.message state.languageKey
      chatConfig = getChatConfig state config.sentBy isLastItem index
      enableFlexBox = not $ (os == "IOS" || (isPreviousVersion (getValueToLocalStore VERSION_NAME) (getFlexBoxCompatibleVersion "")))
      imageId = getNewIDWithTag (getImageId index)
  in
  PrestoAnim.animationSet[
     if config.sentBy == (getCurrentUser (state.userConfig.receiver /= "Driver")) then
         translateInXForwardAnim $ true
      else
          translateInXBackwardAnim $ true
    ]
  $ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , accessibility ENABLE
    , accessibilityHint $ (if config.sentBy == getCurrentUser (state.userConfig.receiver /= "Driver") then "You Sent : " else receiver <> " Sent : ") <> getMessageFromKey state.suggestionKey config.message "EN_US"
    , margin chatConfig.margin
    , gravity chatConfig.gravity
    , orientation VERTICAL
    , visibility $ boolToVisibility $ not $ STR.null message

    ][ (if enableFlexBox then flexBoxLayout else linearLayout) (
        [ height MATCH_PARENT
        , width $ if (os == "IOS" && (STR.length message) > (if state.languageKey == "HI_IN" then 50 else 30) ) then MATCH_PARENT else WRAP_CONTENT
        , accessibility DISABLE_DESCENDANT
        , background chatConfig.background
        , cornerRadii chatConfig.cornerRadii
        ] <> if enableFlexBox 
                then [justifyContent JUSTIFY_END, flexDirection ROW, flexWrap WRAP, alignItems ALIGN_BASELINE, orientation VERTICAL, padding (Padding 10 6 10 6)] 
                else [margin (MarginBottom 4), padding (Padding 12 12 12 12)])
        [ 
        PrestoAnim.animationSet [ if config.sentBy == (getCurrentUser (state.userConfig.receiver /= "Driver")) then translateInXForwardAnim $ true else translateInXBackwardAnim $ true] $
          if config.type == "Image" then 
            linearLayout
              [ height $ V 300
              , width $ V 300
              , id imageId
              , onAnimationEnd (\_ -> renderBase64Image (config.message) (imageId) true "FIT_XY")(const NoAction)
              , padding $ Padding 6 6 6 0
              , onClick push $ const $ FullScreenImage config.message
              ][]
          else 
            textView $
              [ text $ message
              , singleLine false
              , margin $ MarginTop $ if state.languageKey == "KN_IN" then 6 else 0
              , color chatConfig.textColor
              ] <> FontStyle.body20 TypoGraphy
          , textView $ 
              [ text $ convertUTCtoISC config.timeStamp "hh:mm A"
              , height $ MATCH_PARENT
              , visibility $ boolToVisibility enableFlexBox
              , color chatConfig.timeStampColor
              , margin $ MarginLeft 6
              ] <> FontStyle.body21 TypoGraphy
      ]
    , textView $ 
        [ text $ convertUTCtoISC config.timeStamp "hh:mm A"
        , height $ MATCH_PARENT
        , visibility $ boolToVisibility $ not enableFlexBox
        , color Color.black800
        ] <> FontStyle.body21 TypoGraphy
    ]
  where 
    getImageId index = spy "getImageId" $ "inChatImage_" <> show index

getChatConfig :: Config -> String -> Boolean -> Int -> {margin :: Margin, gravity :: Gravity, background :: String, cornerRadii :: Corners, textColor :: String, timeStampColor :: String}
getChatConfig state sentBy isLastItem index =
  if (getCurrentUser (state.userConfig.receiver /= "Driver")) == sentBy then
    {
      margin : (Margin ((screenWidth unit)/5) 8 0 if isLastItem then 8 else 0),
      gravity : RIGHT,
      background : Color.blue800,
      cornerRadii : (Corners 12.0 true true false true),
      textColor :  Color.white900,
      timeStampColor : Color.white900
    }
  else
    { margin : (Margin 0 8 ((screenWidth unit)/5) if isLastItem then 8 else 0),
      gravity :  LEFT,
      background : Color.white900,
      cornerRadii : (Corners 12.0 true true true false ),
      textColor :   Color.black800,
      timeStampColor : Color.black800
    }

getChatFooterHeight :: Config -> Int
getChatFooterHeight config = 
  let height = (runFn1 getLayoutBounds $ getNewIDWithTag "MessagingFooterView").height
  in if height < 125 then (if null config.chatSuggestionsList || not config.feature.canSendSuggestion || not config.feature.enableSuggestions then 80 else 125) else height

splitString :: String -> String
splitString str = (STR.replaceAll (STR.Pattern "") (STR.Replacement " ") str)

getCurrentUser :: Boolean -> String
getCurrentUser isChatWithEM = if isChatWithEM then getValueFromCache (show CUSTOMER_ID) (getKeyInSharedPrefKeys) else "Customer"

-- fullScreenImage :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w 
-- fullScreenImage config push = do
--   -- let message = head config.messages
--   -- let imageId = getNewIDWithTag (getImageId index)
--   let messagesLen = spy "fullscreenimage " config
--   relativeLayout[
--     width MATCH_PARENT
--     , orientation VERTICAL
--     , height MATCH_PARENT
--     , gravity LEFT
--     , background Color.blackLessTrans
--       ][ 
--           PrestoAnim.animationSet [fadeIn config.fullScreenImage] $
--             linearLayout
--             [ width $ V (screenWidth unit)
--             , height $ V (screenHeight unit)
--             , gravity CENTER
--             , background Color.blackLessTrans
--             , padding $ PaddingBottom 25
--             , id $ getNewIDWithTag ("fullScreenChatImage")
--             , onAnimationEnd (\_ ->renderBase64Image  (spy "config.activeImageURL" config.activeImageURL) (getNewIDWithTag ("fullScreenChatImage")) true "FIT_XY")(const NoAction)
--             ][]
--         , fullScreenImageView config push 
--       ] 