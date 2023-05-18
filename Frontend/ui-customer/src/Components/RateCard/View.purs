{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RateCard.View where

import Common.Types.App (LazyCheck(..))
import Components.RateCard.Controller (Action(..), Config)
import Data.String as DS
import Data.Int as DI
import Data.Maybe as DM
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Merchant.Utils (Merchant(..), getMerchant)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), const, (<>), (>), (==), (&&))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
view push config = 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , padding (Padding 18 0 18 0)
  , gravity CENTER
  , background Color.black9000
  , onClick push $ const BackPressed
  ][ linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , orientation VERTICAL
     , background Color.white900
     , cornerRadius 16.0
     , onClick push $ const NoAction
     ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background if config.nightCharges then Color.black900 else Color.blue600'
        , orientation HORIZONTAL
        , cornerRadii $ Corners 16.0 true true false false
        ][ linearLayout
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , orientation VERTICAL
           , padding (Padding 20 15 0 10)
           ][ textView $
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , color if config.nightCharges then Color.white900 else Color.black800
              , text (getString RATE_CARD)
              , margin (Margin 0 5 0 5)
              ] <> FontStyle.h0 LanguageStyle
            , textView $
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , color if config.nightCharges then Color.black500 else Color.black700
              , text if config.nightCharges then (getString NIGHT_TIME_CHARGES) else (getString DAY_TIME_CHARGES)
              , margin (MarginBottom 8)
              ] <> FontStyle.paragraphText TypoGraphy
            ]
         , imageView
           [ width MATCH_PARENT
           , height $ V 90
           , imageWithFallback if config.nightCharges then "ny_ic_night," <> (getAssetStoreLink FunctionCall) <> "ny_ic_night.png" else "ny_ic_day," <> (getAssetStoreLink FunctionCall) <> "ny_ic_day.png"
           ]  
         ]
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , padding (Padding 20 20 20 8)
        ][ textView $
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , color Color.black800
           , text $ (getString MIN_FARE_UPTO) <> if config.nightCharges then " 🌙" else ""
           ] <> FontStyle.body5 TypoGraphy
         , textView $
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , color Color.black800
           , text config.baseFare
           , gravity RIGHT
           , weight 1.0
           ] <> FontStyle.body5 TypoGraphy
         ]
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , margin (Margin 0 8 0 8)
        , padding (Padding 20 0 20 0)
        ][ textView $
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , color Color.black800
           , text $ (getString RATE_ABOVE_MIN_FARE) <> if config.nightCharges then " 🌙" else ""
           ] <> FontStyle.body5 TypoGraphy
         , textView $
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , color Color.black800
           , text (config.extraFare <> "/ km")
           , gravity RIGHT
           , weight 1.0
           ] <> FontStyle.body5 TypoGraphy
         ]
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , margin (Margin 0 8 0 8)
        , padding (Padding 20 0 20 0)
        ][ textView $
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , color Color.black800
           , text (getString DRIVER_PICKUP_CHARGES)
           ] <> FontStyle.body5 TypoGraphy
         , textView $
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , color Color.black800
           , text config.pickUpCharges
           , gravity RIGHT
           , weight 1.0
           ] <> FontStyle.body5 TypoGraphy
         ]
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , margin (Margin 0 8 0 8)
        , padding (Padding 20 0 20 0)
        , visibility if (getAdditionalFare config.additionalFare) > 0 then VISIBLE else GONE
        ][ textView $
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , color Color.black800
           , text (getString NOMINAL_FARE)
           ] <> FontStyle.body5 TypoGraphy
         , textView $
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , color Color.black800
           , text (getString PERCENTAGE_OF_NOMINAL_FARE)
           , gravity RIGHT
           , weight 1.0
           ] <> FontStyle.body5 TypoGraphy
         ]
      , imageView
        [ width MATCH_PARENT
        , height $ V 2 
        , imageWithFallback $ "ny_ic_horizontal_dash," <> (getAssetStoreLink FunctionCall) <> "ny_ic_horizontal_dash.png"
        , margin (Margin 20 7 20 10)
        ]
      , textView $
        [ width MATCH_PARENT
        , height WRAP_CONTENT
            , color Color.black700
            , text if config.nightCharges then
                      (getString NIGHT_TIMES_OF) <> config.nightShiftMultiplier <> (getString DAYTIME_CHARGES_APPLIED_AT_NIGHT)
                   else
                      (getString DAY_TIMES_OF) <> config.nightShiftMultiplier <> (getString DAYTIME_CHARGES_APPLICABLE_AT_NIGHT)
        , padding (Padding 20 0 20 0)
        ] <> FontStyle.paragraphText TypoGraphy
      , textView $
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , color Color.black700
        , text (getString TOTAL_FARE_MAY_CHANGE_DUE_TO_CHANGE_IN_ROUTE)
        , margin (Margin 0 8 0 8)
        , padding (Padding 20 0 20 0)
        ] <> FontStyle.paragraphText TypoGraphy
      , textView $
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , color Color.black700
        , text (getString DRIVERS_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC)
        , visibility if (getMerchant FunctionCall) == NAMMAYATRI && ((getAdditionalFare config.additionalFare) > 0) then VISIBLE else GONE
        , margin (MarginBottom 12)
        , padding (Padding 20 0 20 0)
        ] <> FontStyle.paragraphText TypoGraphy
      , textView $
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , color Color.blue800
        , gravity CENTER
        , text (getString GOT_IT)
        , padding (Padding 0 8 0 25)
        , onClick push $ const Close
        ] <> FontStyle.h2 LanguageStyle
      ]
   ]

getAdditionalFare :: String -> Int
getAdditionalFare additionalFare = DM.fromMaybe 0 $ DI.fromString $ DS.drop 1 additionalFare