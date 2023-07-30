{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.FareBreakUp.View where

import Prelude
import Components.FareBreakUp.Controller
import Effect (Effect)
import Styles.Colors as Color
import Data.Array (length)
import Font.Style as FontStyle
import Font.Size as FontSize
import Language.Types (STR(..))
import Language.Strings (getString)
import Components.SourceToDestination.Controller as SourceToDestinationConfig
import Components.SourceToDestination.View as SourceToDestination
import Helpers.Utils (parseFloat)
import Data.Int (toNumber)
import Storage (getValueToLocalStore, KeyStore(..))
import PrestoDOM (Length(..) , Margin(..), Orientation(..), Padding(..) , Visibility(..), Gravity(..), PrestoDOM, cornerRadius, height, width, margin, padding, linearLayout, gravity, orientation, fontStyle, textSize, textView, text, background, clickable, color, imageView, imageUrl, ellipsize, maxLines, onClick, lineHeight, visibility, textFromHtml, layoutGravity, imageWithFallback)
import Common.Types.App
import Data.Maybe
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))
import MerchantConfig.Utils (getValueFromConfig)

view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity LEFT
  , orientation VERTICAL
  , background Color.white900
  ][  linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][ fareBreakUpView config.totalAmount config.rideDetails.estimatedDistance push
        , if (length config.fareDetails /= 0 ) then horizontalLine (Margin 0 0 0 0) config else textView[height (V 0)]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          ] ( map (\(item) -> fareBreakUpListView (length config.fareDetails) item push ) config.fareDetails)
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity LEFT
      , visibility GONE
      , padding $ Padding 0 20 4 8
      , margin (MarginVertical 0 8)
      , onClick push $ const ShowInvoice
      ][ textView (
        [ text config.headingText
        , color Color.blue900
        ] <> FontStyle.body3 TypoGraphy)]
      , if config.totalAmount.visibility == VISIBLE then horizontalLine (Margin 0 0 0 20) config else textView[height (V 0)]
      , rideStartTimeView config
      , SourceToDestination.view (push <<< SourceToDestinationActionController) (sourceToDestinationConfig config)
      ]


rideStartTimeView :: forall w. Config -> PrestoDOM (Effect Unit) w
rideStartTimeView config =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  , margin (MarginVertical 0 16)
  ][  textView (
      [ text config.rideDetails.rideStartDate
      , color Color.black800
      ] <> FontStyle.body1 TypoGraphy)
    , textView 
      [ height $ V 4 
      , width $ V 4 
      , cornerRadius 2.0 
      , background Color.black600
      , gravity CENTER
      , margin (MarginHorizontal 8 8)
      ]
    , textView (
      [ text config.rideDetails.rideStartTime 
      , color Color.black800
      ] <> FontStyle.body1 TypoGraphy)

  ]

sourceToDestinationConfig :: Config -> SourceToDestinationConfig.Config
sourceToDestinationConfig state = SourceToDestinationConfig.config
    { 
      sourceMargin = Margin 0 0 0 24
    , separatorMargin = 21
    , sourceImageConfig {
        imageUrl = "ny_ic_source_dot," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_source_dot.png"
      , height = V 16
      , width = V 16
      }
    , sourceTextConfig {
        text = state.rideDetails.sourceTitle
      , padding = Padding 2 0 2 2
      , margin = Margin 12 0 15 0
      , textStyle = FontStyle.Body9
      , color = Color.black700
      , ellipsize = true
      }
    , destinationImageConfig {
        imageUrl = "ny_ic_loc_red," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_loc_red.png"
      , height = V 16
      , width = V 16
      }
    , destinationTextConfig {
        text = state.rideDetails.destinationTitle
      , padding = Padding 2 0 2 2
      , margin = Margin 12 0 15 0
      , color = Color.black700
      , textStyle = FontStyle.Body9
      , ellipsize = true
      }
    , rideEndedAtConfig {
        text = state.rideDetails.destination
      , visibility = VISIBLE
      , padding = Padding 1 0 1 1
      , margin = Margin 12 1 0 0
      , maxLines = 1
      , color = Color.black700
      , textStyle = FontStyle.Body9
      , ellipsize = true
      }
    , rideStartedAtConfig {
        text = state.rideDetails.source
      , visibility = VISIBLE
      , textStyle = FontStyle.Body9
      , padding = Padding 1 0 1 1
      , margin = Margin 12 1 0 0
      , color = Color.black700
      , maxLines = 1
      , ellipsize = true
    }
    }
  -- in sourceToDestinationConfig'

----------------------------------- horizontalLine -------------------------------------
horizontalLine :: forall w. Margin ->  Config -> PrestoDOM (Effect Unit) w
horizontalLine marginConfig config =
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , background Color.grey900
  , margin marginConfig
  ,gravity CENTER
  ][]

----------------------------------- fareBreakUpListView -------------------------------------
fareBreakUpListView :: forall w . Int ->  FareDetails -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
fareBreakUpListView numberOfViews state push =
  linearLayout
  [ orientation HORIZONTAL
  , width MATCH_PARENT
  , height WRAP_CONTENT
  , visibility state.visibility
  , margin state.margin
  ][  textView $
      [ text state.text
      , color state.color
      ] <> (FontStyle.getFontStyle state.priceDetails.textStyle LanguageStyle)
    , linearLayout 
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity RIGHT
      ][  textView $ 
          [ text $ (getValueFromConfig "currency") <> (show state.priceDetails.text)
          , color state.color
          ] <> (FontStyle.getFontStyle state.priceDetails.textStyle LanguageStyle)
      ]
  ]


fareBreakUpView :: forall w . FareDetails -> Maybe Int ->(Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
fareBreakUpView state estimatedDistance push =
  linearLayout
  [ orientation VERTICAL
  , width MATCH_PARENT
  , height WRAP_CONTENT
  , visibility state.visibility
  , margin $ MarginBottom 20
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      ][ textView $
          [ text state.text
          , lineHeight "20"
          , color state.color
          ] <> (FontStyle.getFontStyle state.textStyle LanguageStyle)
        , linearLayout 
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , gravity RIGHT
          ][ linearLayout
              [ width WRAP_CONTENT
              , height MATCH_PARENT
              , gravity CENTER
              ][ imageView
                  [ width $ V 16
                  , height $ V 16
                  , imageWithFallback $ "ny_ic_parallel_arrows," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_parallel_arrows.png"
                  , margin $ MarginRight 5
                  , visibility if state.priceDetails.text /= state.priceDetails.offeredFare then VISIBLE else GONE
                  ]
                , textView (
                  [ text $ state.currency <> show state.priceDetails.text
                  , color Color.black800
                  , height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , lineHeight "20"
                  ] <> FontStyle.subHeading2 TypoGraphy)
                , textView (
                  [ textFromHtml $ "<strike> " <> state.currency <> " " <> (show state.priceDetails.offeredFare) <> "</strike>"
                  , height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , color Color.black600
                  , margin $ Margin 5 1 0 0 
                  , visibility if state.priceDetails.text /= state.priceDetails.offeredFare then VISIBLE else GONE
                  ] <> FontStyle.paragraphText TypoGraphy)
              ]
          ]
      ]
    , textView $
      ([ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginTop 10
      , text $ getFareUpdatedString state.priceDetails.distanceDifference
      , visibility if (state.priceDetails.text /= state.priceDetails.offeredFare && estimatedDistance /= Nothing) then VISIBLE else GONE
      , color Color.black700
      ]) <> FontStyle.body3 TypoGraphy
  ]


getFareUpdatedString :: Int -> String
getFareUpdatedString diffInDist = do
  let dist = if diffInDist > 0 then (parseFloat (toNumber diffInDist / 1000.0) 2) else (parseFloat (toNumber (-diffInDist) / 1000.0) 2)
  if diffInDist > 0 then ((getString FARE_UPDATED) <> " - " <> case (getValueToLocalStore LANGUAGE_KEY) of
                                                        "HI_IN" -> "आपकी सवारी  "<> dist <> "किमी कम थी"
                                                        "KN_IN" -> "ನಿಮ್ಮ ಸವಾರಿ " <> dist <> " ಕಿಮೀ ಕಡಿಮೆಯಾಗಿದೆ"
                                                        "ML_IN" -> "താങ്കളുടെ യാത്ര " <> dist <> " Km കുറവായിരുന്നു"
                                                        _       -> "your ride was " <> dist <> " km shorter" )
    else ((getString FARE_UPDATED) <> " - " <> case (getValueToLocalStore LANGUAGE_KEY) of
                                                        "HI_IN" -> "आपकी सवारी  "<> dist <> "किमी लंबी थी"
                                                        "KN_IN" -> "ನಿಮ್ಮ ಸವಾರಿ " <> dist <> " ಕಿಮೀ ಉದ್ದವಾಗಿದೆ"
                                                        "ML_IN" -> "താങ്കളുടെ യാത്ര " <> dist <> " Km കൂടുതലായിരുന്നു"
                                                        _       -> "your ride was " <> dist <> " km longer")
