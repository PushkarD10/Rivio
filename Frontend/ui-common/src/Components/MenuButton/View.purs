{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.MenuButton.View where

import Prelude (Unit , (/=) ,const,(<>),($),not)
import Effect (Effect)
import Font.Style as FontStyle
import Common.Styles.Colors as Color
import Components.MenuButton.Controller (Action(..), Config)
import PrestoDOM (Gravity(..), Length(..), Orientation(..), PrestoDOM, Visibility(..), Padding(..), clickable, color, cornerRadius, fontStyle, gravity, height, imageView, lineHeight, linearLayout, margin, onClick, orientation, padding, singleLine, stroke, text, textSize, textView, visibility, width, imageWithFallback)
import Common.Types.App

view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
    linearLayout
        [ height config.height
        , width config.width
        , orientation HORIZONTAL
        , padding config.padding
        , gravity CENTER_VERTICAL
        , cornerRadius config.cornerRadius
        , stroke config.stroke
        , onClick push  (const (OnClick config))
        , clickable true
        ][  if config.leftsidebutton then buttonLayout config else linearLayout[width $ V 0][],
            linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            ][  titleView config
                ,subTitleView config
              ]
              , if not config.leftsidebutton then buttonLayout config else linearLayout[width $ V 0][]
            ]
titleView :: forall w . Config -> PrestoDOM (Effect Unit) w
titleView config =
  textView
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , padding $ PaddingBottom 4
    , text config.titleConfig.text
    , color if config.isSelected then Color.black900 else config.titleConfig.color
    , fontStyle if config.isSelected then config.titleConfig.selectedFontStyle else config.titleConfig.unselectedFontStyle
    , gravity LEFT
    , singleLine config.titleConfig.singleLine
    , lineHeight "24"
    , visibility config.titleConfig.visibility
    ]



subTitleView :: forall w . Config -> PrestoDOM (Effect Unit) w
subTitleView config =
  textView
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , textSize config.subTitleConfig.textSize
    , text config.subTitleConfig.text
    , color config.subTitleConfig.color
    , fontStyle config.subTitleConfig.selectedFontStyle
    , gravity LEFT
    , lineHeight "23"
    , singleLine config.subTitleConfig.singleLine
    , visibility if config.subTitleConfig.text /= "" then VISIBLE else GONE
    ]

buttonLayout :: forall w . Config -> PrestoDOM (Effect Unit) w
buttonLayout config =
  linearLayout
    [ height WRAP_CONTENT
    , width if config.leftsidebutton then WRAP_CONTENT  else MATCH_PARENT
    , gravity if config.leftsidebutton then LEFT else  RIGHT
    , margin config.radioButtonConfig.buttonMargin
    , padding config.radioButtonConfig.buttonPadding
    ][  linearLayout
        [ height config.radioButtonConfig.height
        , width config.radioButtonConfig.width
        , stroke if config.isSelected then config.radioButtonConfig.activeStroke else config.radioButtonConfig.inActiveStroke
        , cornerRadius config.radioButtonConfig.cornerRadius
        , gravity CENTER
        ][  imageView
            [ width config.radioButtonConfig.imageWidth
            , height config.radioButtonConfig.imageHeight
            , margin config.radioButtonConfig.imageMargin
            , padding config.radioButtonConfig.imagePadding
            , visibility if config.isSelected then VISIBLE else GONE
            , imageWithFallback config.radioButtonConfig.imageUrl
            ]
          ]
      ]
