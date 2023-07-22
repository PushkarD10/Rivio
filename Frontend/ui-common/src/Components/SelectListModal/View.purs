{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.SelectListModal.View where

import Prelude
import PrestoDOM (Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), Length(..), PrestoDOM, background, clickable, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, orientation, text, textSize, textView, weight, width, padding, visibility, frameLayout, stroke, scrollView, afterRender, editText, onClick, id, onChange, pattern, relativeLayout, alignParentBottom, adjustViewWithKeyboard, singleLine, hint, hintColor, multiLineEditText, disableClickFeedback, imageWithFallback,onBackPressed)
import Effect (Effect)
import PrestoDOM.Properties (lineHeight, cornerRadii)
import PrestoDOM.Types.DomAttributes (Gravity(..), Corners(..))
import Components.SelectListModal.Controller
import Components.PrimaryButton.Controller as PrimaryButtonConfig
import Components.PrimaryButton.View as PrimaryButton
import Common.Styles.Colors as Color
import Font.Style as FontStyle
import Font.Size as FontSize
import Data.Array (mapWithIndex, length)
import Engineering.Helpers.Commons (screenWidth)
import Log (printLog)
import Effect.Aff (launchAff_)
import Engineering.Helpers.Commons (flowRunner, os, setText')
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Control.Monad.Trans.Class (lift)
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Data.Array (length,(!!))
import Data.Maybe
import Common.Types.App
import JBridge(requestKeyboardShow, hideKeyboardOnNavigation)
import Styles.Types (FontStyle)

view :: forall w .  (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  relativeLayout
    [ width MATCH_PARENT
      , height MATCH_PARENT
      , clickable true
      , background Color.black9000
      , disableClickFeedback true
      , gravity BOTTOM
      , onClick push (const OnGoBack)
      , adjustViewWithKeyboard "true"
      ][ linearLayout
          [ width MATCH_PARENT
          , alignParentBottom "true,-1"
          , height WRAP_CONTENT
          , clickable true
          , disableClickFeedback true
          , onClick ( \action -> do
            _ <- pure $ hideKeyboardOnNavigation true
            pure unit
          ) (const NoAction)
          , cornerRadii $ Corners 20.0 true true false false
          , orientation VERTICAL
          , background Color.white900
          , padding (Padding 20 20 20 0)
          ][  headingText config push ,
              optionListView push config
           ]
         ]


headingText :: forall w . Config -> (Action -> Effect Unit) ->  PrestoDOM (Effect Unit) w
headingText config push = 
  linearLayout
  [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  ][
    linearLayout
    [
      height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity LEFT
    , margin $ MarginTop 4
    , visibility if config.topLeftIcon then VISIBLE else GONE
    , onClick push (const OnGoBack)
    , padding (PaddingRight 12)
    ][
      imageView
      [ height $ V 22
      , width $ V 22
      , imageWithFallback "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
      , margin $ MarginTop 4
      , color Color.black900
      , fontStyle $ FontStyle.semiBold LanguageStyle
      ] 
    ]   
    ,linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , clickable true
    , onClick ( \action -> do 
                _ <- pure $ hideKeyboardOnNavigation true
                pure unit 
              ) (const NoAction)
    , disableClickFeedback true
    ][ textView
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , text config.headingTextConfig.text
        , color config.headingTextConfig.color
        , orientation HORIZONTAL
        , gravity if config.topLeftIcon then LEFT else CENTER
        , textSize config.headingTextConfig.size
        , fontStyle $ FontStyle.bold LanguageStyle
        ], 
        textView 
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , visibility if config.subHeadingTextConfig.visibility then VISIBLE else GONE
        , text config.subHeadingTextConfig.text
        , orientation HORIZONTAL
        , padding (PaddingTop 4)
        , gravity CENTER
        , textSize config.subHeadingTextConfig.size
        , color config.subHeadingTextConfig.color
        , fontStyle $ FontStyle.regular LanguageStyle
        ],
        linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , padding (Padding 4 12 4 12)
        , margin (Margin 2 16 0 0)
        , orientation VERTICAL
        , onClick ((\action -> do
            _ <- push action
            _ <- setText' (getNewIDWithTag "OtherReasonEditText") ""
            _ <- setText' (getNewIDWithTag "TechGlitchEditText") ""
            pure unit 
        )) ( const ClearOptions)
        , visibility case config.activeReasonCode of 
                        Just reasonCode -> if ( reasonCode == "OTHER" || reasonCode == "TECHNICAL_GLITCH") then VISIBLE else GONE
                        _               -> GONE
        ][  textView
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text config.showAllOptionsText
            , color Color.blue900
            , textSize FontSize.a_12
            , fontStyle $ FontStyle.semiBold LanguageStyle
            ]
        ]
    ]
  ]
  

optionListView :: forall w .  (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
optionListView push config =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , padding (PaddingVertical 0 24)
    , orientation VERTICAL
    ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ][dataListOptions config push],
          primaryButtons push config
    ]


dataListOptions :: forall w . Config -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
dataListOptions config push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding (PaddingBottom 24)
  ] (mapWithIndex (\index item ->
      linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , orientation VERTICAL
      ][ linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , padding (PaddingTop if index == 0 then 12 else 0)
          , visibility case config.activeReasonCode of
                Just reasonCode -> if (( reasonCode == "TECHNICAL_GLITCH" && item.reasonCode /= "TECHNICAL_GLITCH") || ( reasonCode == "OTHER" && item.reasonCode /= "OTHER")) then GONE else VISIBLE
                _               -> VISIBLE
          , onClick (\action -> do
            _ <- push action
            _ <- case item.reasonCode of
              "OTHER" -> requestKeyboardShow (getNewIDWithTag "OtherReasonEditText")
              "TECHNICAL_GLITCH" -> requestKeyboardShow (getNewIDWithTag "TechGlitchEditText")
              _ -> pure unit
            pure unit
          ) (const (UpdateIndex index))
          ][radioButton config push index item,
            horizontalLine index (fromMaybe (-1) config.activeIndex) config,
            (case config.activeReasonCode of
              Just reasonCode -> if (( reasonCode == "OTHER" && item.reasonCode == "OTHER")) then someOtherReason config push index else dummyTextView
              Nothing         -> dummyTextView),
            (case config.activeReasonCode of
              Just reasonCode -> if (( reasonCode == "TECHNICAL_GLITCH" && item.reasonCode == "TECHNICAL_GLITCH")) then technicalGlitchDescription config push index else dummyTextView
              Nothing         -> dummyTextView)
            -- , technicalGlitchDescription config push index
           ]
          ]
      ) config.selectionOptions)


someOtherReason :: forall w . Config -> (Action  -> Effect Unit) -> Int -> PrestoDOM (Effect Unit) w
someOtherReason config push index =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  -- , margin (MarginLeft 40)
  , visibility case config.activeReasonCode of
                  Just reasonCode -> if (reasonCode == "OTHER") then VISIBLE else GONE
                  _               -> GONE
  ][ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][  linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , stroke ("1," <> (if config.isLimitExceeded then Color.warningRed else Color.grey800))
          , gravity LEFT
          , cornerRadius 4.0
          , background Color.grey800
          , padding (Padding 16 2 16 2)
          ][
            ((if os == "ANDROID" then editText else multiLineEditText)
              $ [ width MATCH_PARENT
              , height ( V 58)
              , color Color.black800
              , textSize FontSize.a_14
              , hint config.hint
              , hintColor Color.black650
              , fontStyle $ FontStyle.medium LanguageStyle
              , cornerRadius 4.0
              , background Color.grey800
              , singleLine false
              , onChange push (TextChanged ( getNewIDWithTag "OtherReasonEditText") )
              , pattern "[A-Za-z0-9 ]*,100"
              ] <> (if os == "ANDROID" then [id (getNewIDWithTag "OtherReasonEditText")] else [] ))
            ]
          , textView
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text if (not config.isMandatoryTextHidden ) then config.strings.mandatory else config.strings.limitReached
            , color Color.warningRed
            , textSize FontSize.a_12
            , fontStyle $ FontStyle.regular LanguageStyle
            , visibility if ((not config.isMandatoryTextHidden )|| config.isLimitExceeded) then VISIBLE else GONE
            ]
          ]
      ]

technicalGlitchDescription :: forall w . Config -> (Action  -> Effect Unit) -> Int -> PrestoDOM (Effect Unit) w
technicalGlitchDescription config push index =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  -- , margin (MarginLeft 40)
  , visibility case config.activeReasonCode of
                  Just reasonCode -> if (reasonCode == "TECHNICAL_GLITCH" )  then VISIBLE else GONE
                  _               -> GONE
  ][ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][  linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , stroke ("1," <> (if config.isLimitExceeded then Color.warningRed else Color.grey800))
          , gravity LEFT
          , cornerRadius 4.0
          , background Color.grey800
          , padding (Padding 16 2 16 2)
          ][
            ((if os == "ANDROID" then editText else multiLineEditText)
              $ [ width MATCH_PARENT
              , height ( V 58)
              , color Color.black800
              , textSize FontSize.a_14
              , hint config.hint
              , hintColor Color.black650
              , background Color.grey800
              , fontStyle $ FontStyle.medium LanguageStyle
              , cornerRadius 4.0
              , singleLine false
              , onChange push (TextChanged ( getNewIDWithTag "TechGlitchEditText") )
              , pattern "[A-Za-z0-9 ]*,100"
              ] <> (if os == "ANDROID" then [id (getNewIDWithTag "TechGlitchEditText")] else []))
            ]
          , textView
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text if (not config.isMandatoryTextHidden ) then config.strings.mandatory else config.strings.limitReached
            , color Color.warningRed
            , textSize FontSize.a_12
            , fontStyle $ FontStyle.regular LanguageStyle
            , visibility if ((not config.isMandatoryTextHidden )|| config.isLimitExceeded) then VISIBLE else GONE
            ]
          ]
      ]
primaryButtons :: forall w . (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
primaryButtons push config =
 linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  ] [ PrimaryButton.view (push <<< Button1) (primaryButtonConfig config)
    , PrimaryButton.view (push <<< Button2) (secondaryButtonConfig config)]


radioButton :: forall w .  Config -> (Action  -> Effect Unit) -> Int -> OptionButtonList -> PrestoDOM (Effect Unit) w
radioButton config push index item =
 linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , padding $ PaddingVertical 12 12
  ][ imageView
      [ width $ V 24
      , height $ V 24
      , imageWithFallback cancelValues.imageString
      ],

      linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation VERTICAL
      , margin $ MarginLeft 10
      ][ textView
          [ text item.description
          , padding $ PaddingBottom 5
          , fontStyle cancelValues.font
          , color Color.black900
          ],
          textView
          [ text $ fromMaybe "" item.subtext
          , width if os == "IOS" then V $ (screenWidth unit) - 80 else WRAP_CONTENT
          , color Color.black650
          , visibility cancelValues.visibility
          , fontStyle $ FontStyle.regular LanguageStyle
          ]
      ]
  ] where cancelValues = getCancelValues config.activeIndex item index

getCancelValues :: Maybe Int -> OptionButtonList -> Int -> {imageString :: String , font :: FontStyle , visibility :: Visibility}
getCancelValues index item index' = if isJust index && index' == (fromMaybe 0 index) 
                                        then {  imageString : "ny_ic_radio_selected,https://assets.juspay.in/nammayatri/images/common/ny_ic_radio_selected.png",
                                                font : FontStyle.bold LanguageStyle,
                                                visibility : if item.subtext == Nothing then GONE else VISIBLE
                                          }
                                        else {  imageString : "ny_ic_radio_unselected,https://assets.juspay.in/nammayatri/images/common/ny_ic_radio_unselected.png",
                                                font : FontStyle.regular LanguageStyle,
                                                visibility : GONE
                                          }


primaryButtonConfig :: Config -> PrimaryButtonConfig.Config
primaryButtonConfig config = let
  config' = PrimaryButtonConfig.config
  primaryButtonConfig' =
    config'
      {textConfig
      { text = config.primaryButtonTextConfig.firstText
      , color = Color.black700}
      , background = Color.white900
      , stroke = "1," <> Color.black500
      , width = if(config.secondaryButtonVisibility) then (V ((screenWidth unit/2)-30)) else config.primaryButtonTextConfig.width
      , id = "Button1"
      , visibility = if config.primaryButtonVisibility then VISIBLE else GONE
      }
  in primaryButtonConfig'

secondaryButtonConfig :: Config -> PrimaryButtonConfig.Config
secondaryButtonConfig config = let
  config' = PrimaryButtonConfig.config
  primaryButtonConfig' =
    config'
       {textConfig
        { text = config.primaryButtonTextConfig.secondText}
        , width = if config.primaryButtonVisibility then (V ((screenWidth unit/2)-30)) else config.primaryButtonTextConfig.width
        , id = "Button2"
        , alpha = if(config.isSelectButtonActive) then 1.0 else 0.5
        , isClickable = config.isSelectButtonActive
        , visibility = if config.secondaryButtonVisibility then VISIBLE else GONE
       }
  in primaryButtonConfig'


horizontalLine :: forall w . Int -> Int -> Config -> PrestoDOM (Effect Unit) w
horizontalLine index activeIndex config =
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , background Color.grey800
  , padding (PaddingVertical 2 2)
  , visibility if(((fromMaybe dummyReason( (config.selectionOptions)!!index)).textBoxRequired == true && index == activeIndex)) then GONE else VISIBLE
  ][]

dummyTextView :: forall w . PrestoDOM (Effect Unit) w
dummyTextView =
 textView
 [ width $ V 0
 , height $ V 0
 ]

dummyReason :: OptionButtonList
dummyReason = {
  reasonCode : ""
, description : ""
, textBoxRequired : false
, subtext : Nothing
}
