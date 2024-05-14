module Components.ServiceTierCard.View where

import Data.Array (mapWithIndex)
import Effect (Effect)
import Font.Style as FontStyle
import Prelude (Unit, const, (<>), bind, ($), pure, unit, show, (==), (||), (&&), (/=), not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Orientation(..), Visibility(..), Accessiblity(..), PrestoDOM, alignParentBottom, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, stroke, text, textSize, textView, weight, width, imageWithFallback, id, afterRender, visibility, background, padding, accessibilityHint, accessibility, rippleColor, cornerRadius)
import Screens.Types (StepsHeaderModelState)
import Styles.Colors as Color
import Components.StepsHeaderModel.Controller (Action(..))
import Data.Array as Array
import Data.String as DS
import Data.Maybe (Maybe(..), isNothing)
import Common.Types.App (LazyCheck(..))
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude as MP
import Engineering.Helpers.Commons as EHC

view :: forall w. Config -> PrestoDOM (Effect Unit) w
view config =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , padding $ Padding 4 4 8 4
    , background Color.blue600
    , gravity CENTER_VERTICAL
    , cornerRadius if EHC.os == "IOS" then 13.0 else 18.0
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , background Color.blue800
        , padding $ Padding 4 bluePillPadding 5 bluePillPadding
        , visibility $ MP.boolToVisibility $ showACDetails config.name config.isAc
        , gravity CENTER_VERTICAL
        , cornerRadius if EHC.os == "IOS" then 11.0 else 18.0
        ]
        [ imageView
            [ height $ V 12
            , width $ V 12
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_ac_white"
            , margin $ MarginRight 3
            ]
        , textView
            $ [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text $ getString AC
              , color Color.white900
              , padding $ PaddingBottom if EHC.os == "IOS" then 0 else 2
              ]
            <> FontStyle.tags TypoGraphy
        ]
    , textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ parseName config.name
          , color Color.black700
          , padding $ PaddingBottom if EHC.os == "IOS" then 0 else 2
          , margin $ MarginLeft 4
          ]
        <> FontStyle.tags TypoGraphy
    , case config.capacity of
        Just capacity ->
          linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , gravity CENTER_VERTICAL
            , margin $ MarginLeft 4
            ]
            [ textView $
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text "•"
                , color Color.black700
                , padding $ PaddingBottom 3
                ] <> FontStyle.paragraphText TypoGraphy
            , imageView
                [ height $ V 12
                , width $ V 12
                , margin $ MarginLeft 3
                , padding $ PaddingBottom if EHC.os == "IOS" then 2 else 0
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_user_filled_dark"
                ]
            , textView $
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , padding $ PaddingBottom if EHC.os == "IOS" then 0 else 1
                , color Color.black700
                , text $ show capacity
                ] <> FontStyle.tags TypoGraphy
            ]
        Nothing -> linearLayout [] []
    ]
  where
    bluePillPadding = if EHC.os == "IOS" then 3 else 2


showACDetails :: String -> Maybe Boolean -> Boolean
showACDetails name isAc =
    case isAc of
        Just val -> val
        Nothing -> (not DS.contains (DS.Pattern "Non-AC") name) && Array.notElem name ["Auto", "Taxi"]

type Config
  = { name :: String
    , capacity :: Maybe Int
    , isAc :: Maybe Boolean
    }

parseName :: String -> String
parseName name =
  if DS.contains (DS.Pattern "AC") name && not DS.contains (DS.Pattern "Non-AC") name then
    DS.replace (DS.Pattern "AC") (DS.Replacement "") name
  else
    name

