{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AboutUsScreen.View where

import Prelude (Unit, const, ($), (<>),(==), bind, pure, unit, (<<<))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, relativeLayout, background, color, fontStyle, gravity, height, imageUrl, imageView, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, text, textSize, textView, weight, width, lineHeight, afterRender, scrollView, scrollBarY, imageWithFallback)
import Effect (Effect)
import Screens.AboutUsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Font.Style as FontStyle
import Font.Size as FontSize
import Animation as Anim
import Language.Strings (getString)
import Language.Types(STR(..))
import Storage (KeyStore(..),getValueToLocalStore)
import JBridge as JB
import Components.PopUpModal as PopUpModal
import Common.Types.App
import Screens.AboutUsScreen.ComponentConfig
import Merchant.Utils (getValueFromConfig)
import Components.ComplaintsModel as ComplaintsModel
import Data.Maybe (Maybe(..))
import Merchant.Utils (getMerchant, Merchant(..))

screen :: ST.AboutUsScreenState -> Screen Action ST.AboutUsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "AboutUsScreen"
  , globalEvents : []
  , eval
  }

view
  :: forall w
  . (Action -> Effect Unit)
  -> ST.AboutUsScreenState
  -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  relativeLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      , onBackPressed push (const BackPressed state.props.demoModePopup)
      , afterRender push (const AfterRender)
      ]([ linearLayout[
        height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
      ][  headerLayout  state push
        , scrollView
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , scrollBarY false
          ][ linearLayout
              [ width MATCH_PARENT
              , height MATCH_PARENT
              , orientation VERTICAL
              ][  applicationInformationLayout state push
                , linearLayout
                    [ width MATCH_PARENT
                    , weight 1.0
                    , gravity BOTTOM
                    ][ footerView state ]
              ]
            ]
          ]
      ]<> if state.props.demoModePopup then [demoModePopUpView push state] else [])

demoModePopUpView :: forall w . (Action -> Effect Unit) -> ST.AboutUsScreenState -> PrestoDOM (Effect Unit) w
demoModePopUpView push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][PopUpModal.view (push <<< PopUpModalDemoModeAction) (demoModePopUpConfig state )]

------------------------------------------------------------------- headerLayout ------------------------------------------------
headerLayout :: ST.AboutUsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerLayout state push = 
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , orientation VERTICAL
 ][ linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation HORIZONTAL
    , layoutGravity "center_vertical"
    , padding (Padding 5 5 5 0)
    ][ imageView
        [ width $ V 25
        , height MATCH_PARENT
        , imageWithFallback "ny_ic_back,https://assets.juspay.in/nammayatri/images/driver/ny_ic_back.png"
        , gravity CENTER_VERTICAL
        , onClick push (const $ BackPressed state.props.demoModePopup)
        , padding (Padding 2 2 2 2)
        , margin (MarginLeft 5)
        ]
      , textView
        [ width WRAP_CONTENT
        , height MATCH_PARENT
        , text (getString ABOUT)
        , textSize FontSize.a_19
        , margin (MarginLeft 20)
        , color Color.black
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , weight 1.0
        , gravity CENTER_VERTICAL
        ]
    ]
  , horizontalLine 0 0
 ]

--------------------------------------- footerView ----------------------
footerView :: ST.AboutUsScreenState -> forall w . PrestoDOM (Effect Unit) w
footerView state = 
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , orientation VERTICAL
 , margin (MarginBottom 30)
 ][textView
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , text $ "v" <> (getValueToLocalStore VERSION_NAME) <> "-[" <> (getValueToLocalStore BUNDLE_VERSION) <> "]"
    , textSize FontSize.a_15
    , color Color.black800
    , gravity CENTER
    ]
 ]

--------------------------------- applicationInformationLayout ----------------------------
applicationInformationLayout :: ST.AboutUsScreenState -> (Action -> Effect Unit) ->forall w . PrestoDOM (Effect Unit) w
applicationInformationLayout state push =
 linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin (MarginTop 20)
  , padding (PaddingHorizontal 20 20)
  ][ imageView
    ([ width $ V 150
    , height $ V 100
    , layoutGravity "center_horizontal"
    , imageWithFallback "ny_ic_banner,https://assets.juspay.in/nammayatri/images/driver/ny_ic_banner.png"
    ] <> if getValueToLocalStore DRIVER_STATUS == "true" then [onClick push (const ShowDemoPopUp)] else [])
    , textView
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , text $ getString ABOUT_TEXT
    , textSize FontSize.a_16
    , fontStyle $ FontStyle.regular LanguageStyle
    , color Color.black800
    , gravity LEFT
    , margin (MarginVertical 20 32)
    , lineHeight "20"
    ]
    , ComplaintsModel.view (ComplaintsModel.config{cardData = contactUsData state})
    , underlinedTextView (getString T_C) push
    , underlinedTextView (getString PRIVACY_POLICY) push
  ]

--------------------------------- underlinedTextView ----------------------
underlinedTextView :: String -> (Action -> Effect Unit) ->forall w . PrestoDOM (Effect Unit) w
underlinedTextView value push  = 
 linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , onClick (\action -> do
              _<- push action
              _ <- JB.openUrlInApp if (value == (getString T_C)) then (getValueFromConfig "DOCUMENT_LINK") 
                else (getValueFromConfig "PRIVACY_POLICY_LINK")
              pure unit
              ) (const TermsAndConditionAction)
  , margin (Margin 20 30 0 0)
  ][ textView
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , text value
    , textSize FontSize.a_16
    , fontStyle $ FontStyle.regular LanguageStyle
    , color Color.primaryBlue
    ]
    , linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.primaryBlue
    , margin (Margin 1 0 2 0)
    ][]
  ]

-------------------------------------- horizontalLine ---------------------
horizontalLine :: Int -> Int -> forall w . PrestoDOM (Effect Unit) w
horizontalLine marginLeft marginRight = 
 linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , background Color.greyBackDarkColor
  , margin (Margin marginLeft 0 marginRight 15)
  ][]

contactUsData :: ST.AboutUsScreenState -> Array ComplaintsModel.CardData
contactUsData state =[
  { title : (getString CORPORATE_ADDRESS)
  , subTitle : (getString CORPORATE_ADDRESS_DESCRIPTION)
  , addtionalData : Just (getString (if getMerchant unit == YATRIPARTNER then CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL_YATRI else CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL))
  }
, { title : (getString REGISTERED_ADDRESS)
  , subTitle : (getString REGISTERED_ADDRESS_DESCRIPTION)
  , addtionalData : Just (getString (if getMerchant unit == YATRIPARTNER then REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL_YATRI else REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL_YATRI))
  }
]