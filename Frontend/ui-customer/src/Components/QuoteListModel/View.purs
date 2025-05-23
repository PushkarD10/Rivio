{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.QuoteListModel.View where

import Common.Types.App
import Components.PrimaryButton as PrimaryButton
import Components.QuoteListItem as QuoteListItem
import Components.QuoteListModel.Controller (Action(..), QuoteListModelState)
import Components.SeparatorView.View as SeparatorView
import Components.TipsView as TipsView
import Data.Array (filter, head, null, (!!), mapWithIndex, slice, length, cons, findIndex, elem, sortBy)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag, isPreviousVersion, os, safeMarginBottom, safeMarginTop, screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), getAssetsBaseUrl, getPaymentMethod, getCityFromString, quoteModalVariantImage)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import JBridge (getBtnLoader, startLottieProcess, lottieAnimationConfig)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (class Eq, Unit, show, bind, const, map, pure, unit, not, void, ($), (&&), (+), (/), (/=), (<<<), (<>), (==), (||), discard, (*), negate, (-))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Accessiblity(..), PrestoDOM, Visibility(..), JustifyContent(..), FlexDirection(..), FlexWrap(..), AlignItems(..), afterRender, accessibilityHint ,alignParentBottom, background, clickable, color, cornerRadius, ellipsize, fontStyle, gravity, height, id, imageUrl, imageView, imageWithFallback, lineHeight, linearLayout, lottieAnimationView, margin, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, accessibility, rippleColor, flexBoxLayout, justifyContent, flexDirection, flexWrap, alignItems, fillViewport, alpha)
import PrestoDOM.Animation as PrestoAnim
import Screens.Types (Stage(..), QuoteListItemState(..), City(..))
import Storage 
import Styles.Colors as Color
import Data.String (replaceAll, Pattern(..), Replacement(..))
import Data.String (null) as DS
import Locale.Utils
import Mobility.Prelude
import Engineering.Helpers.Utils(splitIntoEqualParts)
import Debug
import Components.ProviderModel as PM
import Components.ChooseVehicle.Controller as CVC
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Animation.Config as AnimConfig
import Data.Function
import Data.Ord

view :: forall w . (Action  -> Effect Unit) -> QuoteListModelState -> PrestoDOM (Effect Unit) w
view push state =
  PrestoAnim.animationSet [Anim.translateYAnimFromTop $ AnimConfig.translateFullYAnimWithDurationConfig 500 state.showAnim] $
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , accessibility DISABLE
  ][  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
       , accessibility DISABLE
      , clickable true
      ][ quoteListTopSheetView state push
        , providerQuoteList push state
        , offersView push state
        ]
      , paymentView state
    ]

offersView :: forall w . (Action  -> Effect Unit) -> QuoteListModelState -> PrestoDOM (Effect Unit) w
offersView push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility $ boolToVisibility $ not state.providerSelectionStage
  ][  selectRideAndConfirmView state push
    , linearLayout
      [ height $ V 1
      , width MATCH_PARENT
      , background Color.grey900
      , visibility if ( null state.quoteListModel) then GONE else VISIBLE
      ][]
    , quotesView state push
  ]

paymentView :: forall w . QuoteListModelState -> PrestoDOM (Effect Unit) w
paymentView state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , visibility if state.isRentalSearch then VISIBLE else if state.selectedQuote == Nothing && (null state.quoteListModel) && (not isLocalStageOn FindingQuotes) && (not state.findingRidesAgain) then GONE else VISIBLE
  , alignParentBottom "true,-1"
  , background Color.white900
  , accessibility DISABLE
  , orientation VERTICAL
  ][  lottieAnimationView
          [ id (getNewIDWithTag "lottieLoaderAnimProgress")
          , afterRender (\action-> do
                        void $ pure $ startLottieProcess lottieAnimationConfig {rawJson = (getAssetsBaseUrl FunctionCall) <> "lottie/progress_loader_line.json", lottieId = (getNewIDWithTag "lottieLoaderAnimProgress"), minProgress = state.progress, scaleType = "CENTER_CROP"}
                        )(const NoAction)
          , height WRAP_CONTENT
          , width MATCH_PARENT
          , visibility if state.showProgress || state.isRentalSearch then VISIBLE else GONE
          , accessibilityHint "Booking Status: Looking for rides"
          , accessibility ENABLE
          ]
    , linearLayout
        [ background Color.grey900
        , height $ V 1
        , accessibility DISABLE
        , width MATCH_PARENT
        , visibility if ( null state.quoteListModel ) then GONE else VISIBLE
        ][]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , accessibility DISABLE
      , padding $ PaddingVertical 15 (if safeMarginBottom == 0 then 17 else safeMarginBottom)
      , orientation HORIZONTAL
      ][  imageView
          [ imageWithFallback imageData.imageUrl
          , height imageData.height
          , width imageData.width
          , accessibility DISABLE
          , margin $ MarginRight 8
          , accessibility DISABLE 
          ]
        , textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ if (getPaymentMethod unit) == "cash" then (getString PAY_DRIVER_USING_CASH_OR_UPI) else (getString PAY_DRIVER_USING_WALLET)
          , gravity CENTER_HORIZONTAL
          , color Color.black800
          , accessibilityHint "Payment Method Cash or UPI"
          , accessibility ENABLE
          ] <> FontStyle.body1 TypoGraphy
        ]
    ]


imageData :: { height :: Length
, width :: Length
, imageUrl :: String
}
imageData = 
  if os == "IOS" then {imageUrl : fetchImage FF_ASSET "ny_ic_wallet_rect", height : (V 15), width : (V 15)}
    else {imageUrl : fetchImage FF_ASSET "ny_ic_wallet", height : (V 24) , width : (V 24)}
    
---------------------------- sourceDestinationImageView ---------------------------------
sourceDestinationImageView :: forall w . QuoteListModelState -> PrestoDOM (Effect Unit) w
sourceDestinationImageView state =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , margin $ MarginTop 7
    , gravity CENTER
    , accessibility DISABLE
    , orientation VERTICAL
    ][ imageView
        [ height $ V 15
        , width $ V 15
        , accessibility DISABLE
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_pickup"
        ]
      , SeparatorView.view separatorConfig
      , imageView
        [ height $ V 15
        , width $ V 15
        , accessibility DISABLE
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_drop"
        ]
      ]

---------------------------- sourceDestinationEditTextView ---------------------------------
sourceDestinationView :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
sourceDestinationView state push =
  linearLayout
    [ width MATCH_PARENT
    , orientation VERTICAL
    , height WRAP_CONTENT
    , margin $ MarginTop 7
    ][ linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity CENTER_VERTICAL
      ][ 
      imageView
        [ height $ V 15
        , width $ V 15
        , accessibility DISABLE
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_pickup"
        ] 
      , textView $
        [ height WRAP_CONTENT
        , margin $ MarginLeft 12
        , weight 1.0
        , text state.source
        , accessibility ENABLE
        , color state.appConfig.quoteListModel.textColor
        , accessibilityHint $ "Pickup Location is " <> (replaceAll (Pattern ",") (Replacement " : ") state.source)
        , ellipsize true
        , singleLine true
        ] <> FontStyle.paragraphText TypoGraphy
      ]
      , if DS.null state.destination then textView[] else SeparatorView.view separatorConfig
      , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity CENTER_VERTICAL
      , visibility $ boolToVisibility $ not $ DS.null state.destination
      ][ 
        imageView
        [ height $ V 15
        , width $ V 15
        , accessibility DISABLE
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_drop"
        ]
        , textView $
        [ height WRAP_CONTENT
        , weight 1.0
        , text state.destination
        , margin $ MarginLeft 12
        , color state.appConfig.quoteListModel.textColor
        , accessibilityHint $ "Destination Location is " <>  (replaceAll (Pattern ",") (Replacement " : ") state.destination)
        , accessibility ENABLE
        , ellipsize true
        , singleLine true
        ] <> FontStyle.paragraphText TypoGraphy
      ]
    ]   
      
---------------------------- quotesView ---------------------------------
quotesView :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
quotesView state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility VISIBLE
    , accessibility DISABLE
    ][  quoteListView state push
      , noQuotesErrorModel state
      , findingRidesView state push
    ]

findingRidesView :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
findingRidesView state push =
  let lottieRawJson = if (state.appConfig.autoVariantEnabled && getValueToLocalStore SELECTED_VARIANT == "AUTO_RICKSHAW") then (getAssetsBaseUrl FunctionCall) <> getAutoLottie state.city else (getAssetsBaseUrl FunctionCall) <> "lottie/finding_rides_loader_without_text_cab.json"
  in
    linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity CENTER_HORIZONTAL
    , visibility if (null state.quoteListModel && isLocalStageOn FindingQuotes) || state.findingRidesAgain || state.isRentalSearch then VISIBLE else GONE
    , clickable true
    , accessibility DISABLE
    , margin $ if state.tipViewProps.onlyPrimaryText then MarginBottom 80 else if state.tipViewProps.isprimaryButtonVisible then MarginBottom 82 else  MarginBottom 85
    , orientation VERTICAL
    ][
      linearLayout
      [ width MATCH_PARENT
      , orientation VERTICAL
      , weight 1.0
      , gravity CENTER
      , accessibility DISABLE
      ]
      [
        lottieAnimationView
        [ id (getNewIDWithTag "lottieLoaderAnim")
        , afterRender (\action-> do
                      void $ pure $ startLottieProcess lottieAnimationConfig{ rawJson = lottieRawJson, lottieId = (getNewIDWithTag "lottieLoaderAnim") }
                      pure unit)(const NoAction)
        , height $ V state.appConfig.quoteListModel.lottieHeight
        , accessibility DISABLE
        , width $ V state.appConfig.quoteListModel.lottieWidth
        ]
      , textView 
        [ text (getString PLEASE_WAIT)
        , color "#7C7C7C"
        , visibility if state.appConfig.showQuoteFindingText then VISIBLE else GONE
        , textSize FontSize.a_17
        , margin $ MarginTop if state.appConfig.showQuoteFindingText then 22 else 0
        , accessibility DISABLE
        , lineHeight "25"
        , fontStyle $ FontStyle.regular LanguageStyle
        ]
      , textView 
        [ text $ if state.isRentalSearch then "" else getString $  FINDING_QUOTES_TEXT
        , color "#7C7C7C"
        , visibility if state.appConfig.showQuoteFindingText || state.isRentalSearch && (state.vehicleVariant /= "AUTO_RICKSHAW") then VISIBLE else GONE
        , textSize FontSize.a_17
        , accessibility DISABLE
        , gravity CENTER
        , lineHeight "25"
        , fontStyle $ FontStyle.regular LanguageStyle
        ]
      , textView $ [
          text $ getString APPLICABLE_TOLL_CHARGES
        , color Color.black900
        , gravity CENTER
        , height WRAP_CONTENT
        , width MATCH_PARENT
        , visibility $ boolToVisibility $ (state.vehicleVariant /= "AUTO_RICKSHAW") && (getValueToLocalStore HAS_TOLL_CHARGES == "true")
        ] <> FontStyle.subHeading2 TypoGraphy
      ]
    , addTipView state push
    ]

addTipView :: forall w. QuoteListModelState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
addTipView state push =
  linearLayout
  [ width MATCH_PARENT
  , orientation VERTICAL
  , height WRAP_CONTENT
  , accessibility DISABLE
  , visibility $ boolToVisibility $ state.appConfig.tipsEnabled && state.tipViewProps.isVisible && length state.customerTipArray > 0
  ]
  [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , alignParentBottom "true,-1"
      , background Color.ivory
      , margin $ MarginHorizontal 16 16
      , cornerRadius 12.0
      , padding $ Padding 20 16 20 16
      , accessibility DISABLE
      ]
      [ textView
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , text state.tipViewProps.secondaryText
          , color Color.black800
          , gravity CENTER
          , textSize $ FontSize.a_12
          , accessibility ENABLE
          , accessibilityHint state.tipViewProps.secondaryText
          , fontStyle $ FontStyle.regular LanguageStyle
          , visibility if state.tipViewProps.onlyPrimaryText then GONE else VISIBLE
          ]
      , linearLayout
          ( [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER
            ]
              -- []-- <> if state.tipViewProps.onlyPrimaryText then [ clickable true, onClick push $ const $ ChangeTip ] else []
          ) $ 
          [ textView
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text state.tipViewProps.primaryText
            , color Color.black800
            , gravity CENTER
            , accessibility ENABLE
            , accessibilityHint state.tipViewProps.primaryText
            , textSize $ FontSize.a_14
            , fontStyle $ FontStyle.bold LanguageStyle
            ]
          ] <> (if state.tipViewProps.onlyPrimaryText then [textView $ 
                  [ text $ getString CHANGE
                  , margin $ MarginLeft 4
                  , height $ WRAP_CONTENT
                  , width $ WRAP_CONTENT
                  , color Color.blue900
                  , visibility GONE-- $ boolToVisibility state.tipViewProps.onlyPrimaryText
                  ] <> FontStyle.body20 LanguageStyle
                  ] else [])
      , tipsView state push
      , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , visibility if state.tipViewProps.isprimaryButtonVisible && not state.tipViewProps.onlyPrimaryText then VISIBLE else GONE
          ]
          [ PrimaryButton.view (push <<< TipViewPrimaryButtonClick) (continueWithTipButtonConfig state)
          ]
      ]
  ]

tipsView :: forall w. QuoteListModelState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
tipsView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , visibility $ boolToVisibility $ not state.tipViewProps.onlyPrimaryText
  ][TipsView.view (push <<< TipsViewActionController) $ tipsViewConfig state]

selectRideAndConfirmView :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
selectRideAndConfirmView state push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background Color.white900
  , visibility if ( null state.quoteListModel ) then GONE else VISIBLE
  ][ linearLayout[
    height WRAP_CONTENT
  , width WRAP_CONTENT
  , weight 1.0
  , padding (Padding 16 16 0 16)
  -- , background state.appConfig.quoteListModel.backgroundColor
  ][textView (
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , color state.appConfig.quoteListModel.selectRideTextColor
    , text case getValueToLocalStore AUTO_SELECTING of
       "CANCELLED_AUTO_ASSIGN" -> "Select a Ride"
       "false"                 -> "Select a Ride"
       _                       -> case (getLanguageLocale languageKey) of
                                    _ -> "Confirming selected ride in" <> " : " <> (fromMaybe dummyQuoteList ((filter (\item -> item.id == (fromMaybe "" state.selectedQuote)) state.quoteListModel) !! 0)).timer <> "s"
                                    -- _ -> "state.timer" <> "s " <> (getString AUTO_ACCEPTING_SELECTED_RIDE) TODO :: NEED TO UPDATE LANGUAGE
    ] <> FontStyle.subHeading2 TypoGraphy)]
   , linearLayout
    [ height MATCH_PARENT
    , width WRAP_CONTENT
    , gravity CENTER
    , padding (Padding 64 12 16 12)
    , onClick push $ const CancelAutoAssigning
    ][ imageView
      [ height $ V 24
      , width $ V 24
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close"
      , visibility if getValueToLocalStore AUTO_SELECTING == "false" || getValueToLocalStore AUTO_SELECTING == "CANCELLED_AUTO_ASSIGN" then GONE else VISIBLE 
     
      ]
    ]
  ]

paymentMethodView :: forall w.(Action -> Effect Unit) ->  QuoteListModelState -> PrestoDOM (Effect Unit) w
paymentMethodView push state =
  linearLayout[
    orientation HORIZONTAL
  , width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER_VERTICAL
  , visibility $ boolToVisibility (state.selectedQuote == Nothing && (null state.quoteListModel) && (not (isLocalStageOn FindingQuotes))) 
  ][linearLayout
  [ orientation VERTICAL
  , height WRAP_CONTENT
  , padding (Padding 16 6 16 16)
  , width WRAP_CONTENT
  , gravity LEFT
  ][  textView (
      [ text (getString PAYMENT_METHOD)
      , color Color.black700
      ] <> FontStyle.body3 TypoGraphy)
    , linearLayout
      [ orientation HORIZONTAL
      , width WRAP_CONTENT
      , height WRAP_CONTENT
      , margin (MarginTop 7)
      ][  imageView
          [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_wallet"
          , height $ V 20
          , width $ V 20
          , accessibility DISABLE
          ]
        , textView $
          [ text (getString PAYMENT_METHOD_STRING)
          , margin (MarginLeft 8)
          , color Color.black800
          ] <> FontStyle.body1 TypoGraphy
      ]
  ] -- TODO ADD PAYMENT OPTIONS
  -- , linearLayout[
  --   height WRAP_CONTENT
  -- , width MATCH_PARENT
  -- , gravity RIGHT
  -- , padding (PaddingRight 16)
  -- ][imageView
  --   [ imageUrl "ic_chevron_right"
  --   , height $ V 20
  --   , width $ V 20
  --   , gravity RIGHT
  --   ]]
  ]

---------------------------- quoteListTopSheetView ---------------------------------
quoteListTopSheetView :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
quoteListTopSheetView state push =
   linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , background state.appConfig.quoteListModel.backgroundColor
      , accessibility DISABLE
      , padding $ PaddingTop safeMarginTop
      , orientation VERTICAL
      ][  linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation HORIZONTAL
          , gravity CENTER_VERTICAL
          , padding $ Padding 10 10 16 28
          ][ linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation HORIZONTAL
              ][ linearLayout
                  [ height $ V 36
                  , width $ V 36
                  , onClick push $ const GoBack
                  , accessibilityHint "Cancel Search : Button"
                  , accessibility ENABLE
                  , rippleColor Color.rippleShade
                  , cornerRadius 18.0
                  ][  imageView
                      [ height $ V 24
                      , width $ V 24
                      , accessibility DISABLE
                      , imageWithFallback state.appConfig.quoteListModel.closeIcon
                      , margin $ Margin 6 6 6 6
                      ]
                  ]
                , sourceDestinationView state push
                ]
            ]
        , linearLayout
          [ height $ V 1
          , width MATCH_PARENT
          , background state.appConfig.quoteListModel.separatorColor
          , visibility if state.appConfig.quoteListModel.showSeparator then VISIBLE else GONE
          ]
          []
        ]
noQuotesErrorModel :: forall w . QuoteListModelState -> PrestoDOM (Effect Unit) w
noQuotesErrorModel state =
  let city = getCityFromString $ getValueToLocalStore CUSTOMER_LOCATION
      variant = state.vehicleVariant 
  in linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , background Color.white900
    , accessibility DISABLE
    , visibility if state.findingRidesAgain then GONE else if ( null state.quoteListModel) && ( isLocalStageOn QuoteList) then VISIBLE else GONE
    , margin (MarginBottom 100)
    ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER
      ][imageView
        [ height $ V 115
        , width $ V 161
        , accessibility DISABLE
        , imageWithFallback $ fetchImage FF_ASSET (quoteModalVariantImage variant)
        ] 
      , textView $
        [ height WRAP_CONTENT
        , width $ V ((screenWidth unit / 2) + (screenWidth unit /3))
        , color Color.black800
        , text (getString SORRY_WE_COULDNT_FIND_ANY_RIDES)
        , margin $ MarginVertical 20 4
        , gravity CENTER
        ] <> FontStyle.h2 TypoGraphy
      , textView $
        [ height WRAP_CONTENT
        , width $ V ((screenWidth unit / 2) + (screenWidth unit /3))
        , text (getString IT_SEEMS_TO_BE_A_VERY_BUSY_DAY)
        , color Color.black700
        , gravity CENTER
        ] <> FontStyle.paragraphText TypoGraphy
    ]
    ]  
---------------------------- quoteListView ---------------------------------
quoteListView :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
quoteListView state push =
  scrollView
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , scrollBarY false
    , visibility if ( null state.quoteListModel) then GONE else VISIBLE
    ][  linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , accessibility DISABLE
        , orientation VERTICAL
        ](map (\item ->
            QuoteListItem.view (push <<< QuoteListItemActionController) item{appConfig = state.appConfig}) state.quoteListModel)
    ]

---------------------------- primaryButtonView ---------------------------------
primaryButtonView :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
primaryButtonView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , weight 1.0
    , alignParentBottom "true,-1"
    , background Color.white900 -- TODO : change to white900 once shadow is fixed
    -- --, visibility GONE-- $ checkVisibility state
    , padding (Padding 0 16 0 30)
    ][ homeOrTryAgain state push ]

---------------------------- homeOrTryAgainView ---------------------------------
homeOrTryAgain :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
homeOrTryAgain state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , visibility $ boolToVisibility $ state.selectedQuote == Nothing && (null state.quoteListModel) && isLocalStageOn QuoteList
    ][ PrimaryButton.view (push <<< HomeButtonActionController) (homeButtonConfig state)
     , PrimaryButton.view (push <<< TryAgainButtonActionController) (tryAgainButtonConfig state)
    ]

-- buttonView :: forall w. QuoteListModelState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
-- buttonView state push =
--   linearLayout
--   [ height WRAP_CONTENT
--   , width MATCH_PARENT
--   , margin (MarginHorizontal 16 16)
--   , background Color.black900
--   , padding (PaddingHorizontal 14 14)
--   , gravity CENTER
--   , onClick push (const HidePopUp)
--   ][ textView
--       [ text "Got It!"
--       , textSize FontSize.a_16
--       , color Color.yellow900
--       , gravity CENTER
--       ]
--     ]

---------------------------- continueWithTipButtonConfig ---------------------------------
continueWithTipButtonConfig :: QuoteListModelState -> PrimaryButton.Config
continueWithTipButtonConfig state = let
    config = PrimaryButton.config
    continueWithTipButtonConfig' = config
      { textConfig
        { text = state.tipViewProps.primaryButtonText
        ,  color = state.appConfig.primaryTextColor 
        , accessibilityHint = state.tipViewProps.primaryButtonText <> " : Button" 
        }
      , id = "ContinueWithTipButtonQuoteList"
      , margin = MarginTop 12
      , background = state.appConfig.primaryBackground
      , enableRipple = true
      , rippleColor = Color.rippleShade
      }
  in continueWithTipButtonConfig'

---------------------------- homeButtonConfig ---------------------------------
homeButtonConfig :: QuoteListModelState -> PrimaryButton.Config
homeButtonConfig state = let
    config = PrimaryButton.config
    homeButtonConfig' = config
      { textConfig
        { text = (getString HOME)
        , color = Color.black900
        }
      , margin =( Margin 16 0 8 0)
      , width = V $ (screenWidth unit/4)
      , background = Color.white900
      , stroke = ("1," <> Color.black)
      , id = "HomeButtonQuoteList"
      , enableLoader = (getBtnLoader "HomeButtonQuoteList")
      }
  in homeButtonConfig'

---------------------------- tryAgainButtonConfig ---------------------------------
tryAgainButtonConfig :: QuoteListModelState -> PrimaryButton.Config
tryAgainButtonConfig state = let
    config = PrimaryButton.config
    tryAgainButtonConfig' = config
      { textConfig
        { text = (getString TRY_AGAIN)
        ,  color = state.appConfig.primaryTextColor 
        }
      , margin =( Margin 8 0 16 0)
      , width = MATCH_PARENT
      , id = "TryAgainButtonQuoteList"
      , enableLoader = (getBtnLoader "TryAgainButtonQuoteList")
      , background = state.appConfig.primaryBackground
      , enableRipple = true
      , rippleColor = Color.rippleShade
      }
  in tryAgainButtonConfig'


getPrice :: QuoteListModelState -> String
getPrice state =
  let selectQuoteArray = (filter (\x -> state.selectedQuote == Just x.id) state.quoteListModel)
      price = (fromMaybe dummyQuoteList (head selectQuoteArray)).price
    in price



dummyQuoteList :: QuoteListItemState
dummyQuoteList = QuoteListItem.config{
   seconds = 15
  , id = ""  
  , timer = ""
  , timeLeft = 15
  , driverRating = 0.0
  , profile = ""
  , price = ""
  , vehicleType = "auto"
  , driverName = ""
  , selectedQuote = Nothing
  }


checkVisibility :: QuoteListModelState -> Visibility
checkVisibility state =
  case state.selectedQuote ,(null state.quoteListModel) of
    Just _ ,_                           -> VISIBLE
    Nothing , true                      -> VISIBLE
    Nothing , false                     -> GONE

setText :: QuoteListModelState -> String
setText state =
  case state.selectedQuote ,(null state.quoteListModel) of
    Just _ ,_                           -> (getString CONFIRM_FOR) <> state.appConfig.currency <> " " <> getPrice state
    Nothing , true                      -> (getString GO_HOME_)
    _,_                                 -> ""

getSelectedItemTimer :: QuoteListModelState -> String
getSelectedItemTimer state =
  let selectQuoteArray = (filter (\x -> state.selectedQuote == Just x.id) state.quoteListModel)
      timer = (fromMaybe dummyQuoteList (head selectQuoteArray)).timer
    in timer

separatorConfig :: SeparatorView.Config
separatorConfig = 
  {
    orientation : VERTICAL
  , count : 3
  , height : V 4
  , width : V 2
  , layoutWidth : V 15
  , layoutHeight : V 15
  , color : Color.black500
  }


getAutoLottie :: City -> String
getAutoLottie city = 
  case city of 
    Hyderabad -> "lottie/finding_rides_loader_auto_yellow_black.json"
    Kochi     -> "lottie/finding_rides_loader_auto_kochi.json"
    Chennai   -> "lottie/finding_rides_loader_auto_yellow_black.json"
    _         -> "lottie/finding_rides_loader_with_text_auto.json"

tipsViewConfig :: QuoteListModelState -> TipsView.Config
tipsViewConfig state = let  
  config = TipsView.config
  tipsViewConfig' = config {
    activeIndex = state.tipViewProps.activeIndex
  , isVisible = state.tipViewProps.isVisible
  , customerTipArray = state.customerTipArray
  , customerTipArrayWithValues = state.customerTipArrayWithValues
  , enableTips = state.appConfig.tipsEnabled && length state.customerTipArray > 0
  , showTipInfo = false
  }
  in tipsViewConfig'

providerQuoteList :: forall w . (Action  -> Effect Unit) -> QuoteListModelState -> PrestoDOM (Effect Unit) w
providerQuoteList push state =
  scrollView
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , visibility $ boolToVisibility state.providerSelectionStage
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][  animationtimer push state
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin $ Margin 16 16 16 16
          , orientation VERTICAL
          ](map (\element -> PM.view (push <<< ProviderModelAC) $ (providerModelConfig state element) ) filteredWithSelectedVariant)
      ]
  ]
  where
    filteredWithSelectedVariant = makeNYasFirst $ filter (\x -> state.selectedEstimatesObject.vehicleVariant == x.vehicleVariant) state.quoteList
    makeNYasFirst configArray = sortBy (compare `on` \item -> if item.providerType == ONUS then 0 else 1) configArray

animationtimer :: forall w . (Action  -> Effect Unit) -> QuoteListModelState -> PrestoDOM (Effect Unit) w
animationtimer push state = 
  let timerRunning = state.selectProviderTimer /= "0"
  in relativeLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , stroke $ "1," <> Color.grey900
  ][ PrestoAnim.animationSet
        [ Anim.translateOutXBackwardAnimY AnimConfig.animConfig
            { duration = (state.animEndTime * 1000) - 1000
            , toX = 0 
            , fromX = - (screenWidth unit)
            , ifAnim = true
            }
        ] $ linearLayout
            [ height $ V 50
            , width MATCH_PARENT
            , alpha 0.5
            , visibility $ boolToVisibility $ timerRunning
            , background Color.blue700
            ][]
    , linearLayout
      [ width MATCH_PARENT
      , height $ V 50
      , gravity CENTER_VERTICAL
      , padding $ Padding 5 6 5 6
      , background Color.transparent
      ][ textView
          [ weight 1.0
          , height WRAP_CONTENT
          , margin $ MarginLeft 16
          , text if timerRunning then (getString CONFIRMING_SELECTED_PROVIDER <> state.selectProviderTimer <> "s") else (getString SELECT_A_PROVIDER)
          ]
        , imageView
          [ height $ V 30
          , width $ V 30
          , accessibility DISABLE
          , visibility $ boolToVisibility $ timerRunning
          , padding $ Padding 6 6 6 6
          , rippleColor Color.rippleShade
          , onClick push $ const CancelTimer
          , imageWithFallback $ fetchImage FF_ASSET "ny_ic_close"
          ]
      ]

  ]

providerModelConfig :: QuoteListModelState -> CVC.Config -> PM.Config
providerModelConfig config quoteItem = 
  let onus = quoteItem.providerType == ONUS
  in PM.config {
    isActive = quoteItem.id == config.selectedEstimatesObject.id,
    pillsVisibility = boolToVisibility onus,
    id = quoteItem.id,
    name = quoteItem.providerName,
    logo = if onus then "ny_ic_ny_network" else "ny_ic_ondc_network",
    selectButtonVisibility = GONE,
    showExpandAnim = true,
    priceRange = quoteItem.price,
    capacity = quoteItem.capacity,
    vehicleType = quoteItem.vehicleType,
    vehicleImage = quoteItem.vehicleImage
}
