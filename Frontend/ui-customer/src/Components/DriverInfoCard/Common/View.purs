module Components.DriverInfoCard.Common.View where

import Prelude
import Components.DriverInfoCard.Common.Types
import Common.Types.App
import Animation (fadeIn, fadeInWithDelay, scaleYAnimWithDelay)
import Common.Types.App (LazyCheck(..))
import Components.DriverInfoCard.Controller (Action(..), DriverInfoCardState)
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination as SourceToDestination
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String (Pattern(..), split, length, take, drop, replaceAll, Replacement(..), contains, toLower)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String as STR
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag, os, safeMarginBottom, screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), getAssetsBaseUrl, getPaymentMethod, secondsToHms, makeNumber, getVariantRideType, getTitleConfig, getCityNameFromCode)
import Language.Strings (getString)
import Resources.Localizable.EN (getEN)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude (Unit, (<<<), ($), (/), (<>), (==), unit, show, const, map, (>), (<), (-), (*), bind, pure, discard, not, (&&), (||), (/=),(+), (+))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Accessiblity(..), Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), accessibility, accessibilityHint, afterRender, alignParentBottom, alignParentLeft, alignParentRight, alpha, background, clickable, color, cornerRadius, ellipsize, fontSize, fontStyle, frameLayout, gradient, gravity, height, id, imageUrl, imageView, imageWithFallback, letterSpacing, lineHeight, linearLayout, margin, maxLines, onAnimationEnd, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, shimmerFrameLayout, alignParentRight)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (rippleColor, cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (Stage(..), ZoneType(..), SearchResultType(..), SheetState(..),City(..))
import Storage (isLocalStageOn, getValueToLocalStore)
import Styles.Colors as Color
import Common.Styles.Colors as CommonColor
import Storage (KeyStore(..))
import Engineering.Helpers.Utils (showAndHideLoader)
import Types.App (defaultGlobalState)
import JBridge(fromMetersToKm, getLayoutBounds)
import Engineering.Helpers.Suggestions (getMessageFromKey, chatSuggestion)
import Helpers.Utils (parseFloat)
import Data.Int(toNumber, fromString)
import MerchantConfig.Types (DriverInfoConfig)
import Mobility.Prelude (boolToVisibility, spaceSeparatedPascalCase)
import Data.Function.Uncurried(runFn1)
import Components.ServiceTierCard.View as ServiceTierCard
import Resources.Constants (getVehicleCapacity)
 
import Screens.Types (FareProductType(..)) as FPT

---------------------------------- driverDetailsView ---------------------------------------
driverDetailsView :: forall w. DriverDetailsType -> String -> String -> PrestoDOM (Effect Unit) w
driverDetailsView config uid nid =
 linearLayout
  [ orientation HORIZONTAL
  , height WRAP_CONTENT 
  , padding $ Padding 16 16 16 8
  , width MATCH_PARENT
  , id $ getNewIDWithTag uid
  , margin $ Margin 16 (if config.fareProductType == FPT.ONE_WAY_SPECIAL_ZONE then 12 else 0) 16 (if config.enablePaddingBottom then safeMarginBottom else 0)
  , background Color.white900
  , cornerRadius 8.0
  , visibility $  boolToVisibility $ if config.fareProductType == FPT.ONE_WAY_SPECIAL_ZONE then config.rideStarted else true
  ][  linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , weight 1.0
      , gravity CENTER_VERTICAL
      , background Color.white900
      , padding $ PaddingRight 16
      ][linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity LEFT
        ][ frameLayout
          [ orientation VERTICAL
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          , margin $ MarginBottom 12
          ][imageView
            [ height $ V 50
            , width $ V 50
            , padding $ Padding 2 3 2 1
            , accessibility if config.rating == 0.0 then DISABLE else ENABLE
            , accessibilityHint $ "Driver : Rated " <> show config.rating <> " stars"
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_user"
            ]
            , ratingView config
            ]
          ]
        , textView $
          [ text config.driverName 
          , maxLines 2
          , ellipsize true
          , accessibility DISABLE
          , color Color.black800
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity LEFT
          ] <> FontStyle.body27 TypoGraphy
        , textView (
          [ text $ spaceSeparatedPascalCase $ config.vehicleColor <> " " <> config.vehicleModel
          , color Color.black700
          , accessibilityHint $ "Driver : " <> config.driverName <> " : Vehicle : " <>  config.vehicleModel
          , accessibility ENABLE
          , width WRAP_CONTENT
          , maxLines 2
          , ellipsize true
          , height WRAP_CONTENT
          , gravity LEFT
          , margin $ MarginBottom 6
          ] <> FontStyle.captions TypoGraphy)
        , case config.serviceTierName of
            Just name -> ServiceTierCard.view $ serviceTierConfig name
            Nothing -> linearLayout [] []
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation VERTICAL
      , accessibility DISABLE
      ][  frameLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , id $ getNewIDWithTag nid
          , margin $ MarginBottom 16
          , accessibility DISABLE 
          ][linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , gravity CENTER_HORIZONTAL
            , alpha if config.providerType == ONUS then 1.0 else 0.5
            ][ imageView
               [ imageWithFallback $ getVehicleImage config.vehicleVariant config.vehicleDetails config.merchantCity
               , height $ V 125
               , width $ V 125
               , accessibility DISABLE_DESCENDANT
               ]
            ]
            , linearLayout
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , accessibility ENABLE
              , accessibilityHint $ "Vehicle Number " <> (STR.replaceAll (STR.Pattern "") (STR.Replacement " ") config.registrationNumber)
              , accessibility DISABLE_DESCENDANT
              ][linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , margin $ MarginTop 90
                ][ linearLayout
                  [ height $ V 38
                  , width WRAP_CONTENT
                  , background config.config.driverInfoConfig.numberPlateBackground
                  , cornerRadius 4.0
                  , orientation HORIZONTAL
                  , margin $ MarginRight 2
                  , padding $ Padding 2 2 2 2
                  , alignParentBottom "true,-1"
                  ][linearLayout
                    [ height $ V 34
                    , width WRAP_CONTENT
                    , stroke $ "2," <> Color.black
                    , cornerRadius 4.0
                    , orientation HORIZONTAL
                    ][  imageView
                        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_number_plate"
                        , gravity LEFT
                        , visibility if config.config.driverInfoConfig.showNumberPlatePrefix then VISIBLE else GONE
                        , background "#1C4188"
                        , height MATCH_PARENT
                        , width $ V 22
                        ]
                        , textView $
                        [ margin $ Margin 2 2 2 2
                        , weight 1.0
                        , height MATCH_PARENT
                        , text $ (makeNumber config.registrationNumber)
                        , color Color.black800
                        , fontStyle $ FontStyle.feFont LanguageStyle
                        , gravity CENTER
                        , textSize FontSize.a_14
                        ]
                        , imageView
                        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_number_plate_suffix"
                        , gravity RIGHT
                        , visibility if config.config.driverInfoConfig.showNumberPlateSuffix then VISIBLE else GONE
                        , height MATCH_PARENT
                        , width $ V 13
                        ]
                      ]
                    ]
                 ]
              ]
            ]
        ]
    ]
  where getVehicleType = case getMerchant FunctionCall of
                          YATRISATHI -> case config.vehicleVariant of
                                          "TAXI" -> getEN NON_AC_TAXI
                                          "SUV"  -> getEN AC_SUV
                                          _      -> getEN AC_CAB
                          _          -> case config.vehicleVariant of
                                        "TAXI_PLUS" -> (getEN AC_TAXI)
                                        "TAXI" -> (getEN NON_AC_TAXI)
                                        _ -> ""
        vehicleMargin = do 
          let width = (runFn1 getLayoutBounds $ getNewIDWithTag nid).width
          if width > 125 then ((width - 125)/2) else 0

        serviceTierConfig name = {
          name : name,
          capacity : fromString $ getVehicleCapacity config.vehicleVariant,
          isAc : Nothing,
          showACPill : config.showAcView,
          fareProductType : config.fareProductType
        }


---------------------------------- ratingView ---------------------------------------

ratingView :: forall w. DriverDetailsType -> PrestoDOM (Effect Unit) w
ratingView config =
  linearLayout
  [ orientation HORIZONTAL
  , margin $ MarginTop 40
  , height $ V 19
  , width $ V 50
  , padding $ Padding 6 3 6 3
  , background config.config.driverInfoConfig.ratingBackground
  , gravity CENTER_VERTICAL
  , stroke  config.config.driverInfoConfig.ratingStroke
  , cornerRadius config.config.driverInfoConfig.ratingCornerRadius
  , accessibility DISABLE
  ][textView $
    [ text $ if config.rating == 0.0 then (getString NEW_) else show config.rating
    , color config.config.driverInfoConfig.ratingTextColor
    , gravity CENTER_VERTICAL
    , margin $ MarginLeft if os == "IOS" then 0 else 3
    , color Color.black700
    , accessibility DISABLE
    ] <> FontStyle.body16 TypoGraphy
  , imageView
    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_star_active_rounded"
    , height $ V 11
    , width $ V 11
    , margin $ MarginLeft 3
    , accessibility DISABLE
    ]
  ]


getVehicleImage :: String -> String -> City -> String
getVehicleImage variant vehicleDetail city = do
  let details = (toLower vehicleDetail)
  fetchImage FF_ASSET $
    if variant == "AUTO_RICKSHAW" then mkAutoImage city
    else
      if contains (Pattern "ambassador") details then "ic_yellow_ambassador"
      else 
        case (getMerchant FunctionCall) of
          YATRISATHI -> case variant of
                          "SUV" -> "ny_ic_suv_concept"
                          "TAXI" -> "ic_white_taxi"
                          "TAXI_PLUS" -> "ny_ic_sedan_concept"
                          _     -> "ny_ic_sedan_concept"
          _          -> case variant of
                          "TAXI"      -> "ny_ic_hatchback_concept"
                          "TAXI_PLUS" -> "ny_ic_sedan_concept"
                          "SUV"       -> "ny_ic_suv_concept"
                          "SEDAN"     -> "ny_ic_sedan_concept"
                          "HATCHBACK" -> "ny_ic_hatchback_concept"
                          "ECO"       -> "ny_ic_hatchback_concept"
                          "COMFY"     -> "ny_ic_sedan_concept"
                          _           -> "ny_ic_sedan_concept"              
    where 
      mkAutoImage :: City -> String
      mkAutoImage city = 
        case city of 
          Hyderabad -> "ic_auto_rickshaw_black_yellow"
          _ | Array.elem city [Chennai, Vellore, Hosur, Madurai, Thanjavur, Tirunelveli, Salem, Trichy] -> "ic_auto_rickshaw_black_yellow"
          _ | Array.elem city [Kochi, Kozhikode, Thrissur, Trivandrum] -> "ny_ic_black_auto"
          _ -> "ic_auto_rickshaw"


---------------------------------- tripDetailsView ---------------------------------------

sourceDestinationView :: forall action w.(action -> Effect Unit) -> TripDetails action -> PrestoDOM (Effect Unit) w
sourceDestinationView push config = 
  let isNotRentalRide = (config.fareProductType /= FPT.RENTAL)
      bottomMargin = (if os == "IOS" && config.rideStarted && config.enablePaddingBottom then (safeMarginBottom + 36) else 12)
      dropLocationTextWidth = if config.enableEditDestination && isNotRentalRide then if os == "IOS" then V ((screenWidth unit) / 100 * 80) else V ((screenWidth unit) / 100 * 65) else V ((screenWidth unit) / 10 * 8)
  in 
    PrestoAnim.animationSet [ scaleYAnimWithDelay 100] $ 
      linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ Margin 16 0 16 bottomMargin
      , background config.backgroundColor
      , onAnimationEnd push $ const $ config.onAnimationEnd
      , cornerRadius 8.0
      , padding $ Padding 16 12 16 12
      ][linearLayout
        [ orientation VERTICAL
        , height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity LEFT
        , accessibility ENABLE
        , accessibilityHint $ "Pickup : " <> config.source
        ][linearLayout
          [ orientation HORIZONTAL
          , gravity CENTER
          ][imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_pickup"
            , height $ V 8
            , width $ V 8
            ]
            ,textView $
            [ text $ getString PICKUP
            , margin $ MarginLeft 6
            , color Color.black700
            ] <> FontStyle.body3 TypoGraphy
            ]
          , textView $
            [ text config.source
            , maxLines 1
            , ellipsize true
            , width $ V ((screenWidth unit) / 10 * 8)
            , height MATCH_PARENT
            , gravity LEFT
            , color Color.black900
            , margin $ MarginTop 3
            ] <> FontStyle.body1 TypoGraphy
          ]
        , separator (MarginVertical 12 12) (V 1) Color.ghostWhite isNotRentalRide
        , linearLayout
        [
            orientation HORIZONTAL
          , height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER
        ][
          linearLayout
          [ orientation VERTICAL
            , height WRAP_CONTENT
            , width WRAP_CONTENT
            , weight 1.0
            , gravity LEFT
            , accessibility ENABLE
            , accessibilityHint $ "Drop : " <> config.destination
            , visibility $ boolToVisibility $ isNotRentalRide 
          ][linearLayout
            [ orientation HORIZONTAL
            , gravity CENTER
            ][imageView
              [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_drop"
              , height $ V 8
              , width $ V 8
              ]
            , textView $ 
              [ text $ getString DROP
              , margin $ MarginLeft 6
              , color Color.black700
              ] <> FontStyle.body3 TypoGraphy
            ]
            , textView $
              [ text config.destination
              , maxLines 2
              , ellipsize true
              , width $ dropLocationTextWidth
              , height MATCH_PARENT
              , gravity LEFT
              , margin $ MarginTop 3
              , color Color.black900
              ] <> FontStyle.body1 TypoGraphy
          ]
            , textView $
              [
                width WRAP_CONTENT
                , height WRAP_CONTENT
                , text $ getString EDIT
                , cornerRadius if os == "IOS" then 16.0 else 32.0
                , stroke $ "1," <> Color.grey900
                , padding $ Padding 12 8 12 8
                , visibility $ boolToVisibility $ config.enableEditDestination && isNotRentalRide
                , onClick push $ const config.editingDestinationLoc
                , rippleColor Color.rippleShade
              ] <> FontStyle.body1 TypoGraphy
      ] 
    ]

separator :: forall w. Margin -> Length -> String -> Boolean -> PrestoDOM (Effect Unit) w
separator margin' height' color' isVisible =
  linearLayout
  [ height $ height'
  , margin $ margin'
  , width MATCH_PARENT
  , visibility if isVisible then VISIBLE else GONE
  , background color'
  ][]


addressShimmerView :: forall w. PrestoDOM (Effect Unit) w
addressShimmerView = 
  linearLayout
  [ height $ WRAP_CONTENT
  , width $ MATCH_PARENT
  , cornerRadius 8.0
  , stroke $ "1," <> Color.grey900
  , background Color.white900
  , padding $ Padding 16 16 16 16
  , margin $ Margin 16 0 16 12
  , orientation VERTICAL
  ][linearLayout
    [ height $ WRAP_CONTENT
    , width $ MATCH_PARENT
    , gravity CENTER_VERTICAL
    ][linearLayout
      [ height $ V 8
      , width $ V 8
      , cornerRadius 52.0
      , background Color.grey900
      , margin $ MarginRight 6
      ][]
    , customTextView 16 40 4
    ]
  , customTextView 20 ((screenWidth unit) / 10 * 6) 4
  , linearLayout
    [ height $ V 1
    , width $ MATCH_PARENT
    , margin $ MarginVertical 12 12
    , background Color.grey900
    ][]
  , linearLayout
    [ height $ WRAP_CONTENT
    , width $ MATCH_PARENT
    , gravity CENTER_VERTICAL
    ][linearLayout
      [ height $ V 8
      , width $ V 8
      , cornerRadius 52.0
      , background Color.grey900
      , margin $ MarginRight 6
      ][]
    , customTextView 16 40 4
    ]
  , customTextView 20 ((screenWidth unit) / 10 * 6) 4
  ]

driverInfoShimmer :: forall w. PrestoDOM (Effect Unit) w
driverInfoShimmer = 
  linearLayout
  [ height $ WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ MarginVertical 12 12
  , cornerRadius 8.0
  , stroke $ "1," <> Color.grey900
  , background Color.white900
  , margin $ Margin 16 0 16 0
  ][linearLayout
    [ width $ MATCH_PARENT
    , height $ V 130
    , padding $ Padding 16 16 16 20
    , cornerRadius 8.0
    ][linearLayout
      [ width $ MATCH_PARENT
      , height $ MATCH_PARENT
      , gravity LEFT
      ][linearLayout
        [ height $ MATCH_PARENT
        , gravity LEFT
        , width $ V ((screenWidth unit) / 10 * 4)
        , orientation VERTICAL
        , cornerRadius 8.0
        ][ sfl 40 40 52.0
        , customTextView 16 ((screenWidth unit) / 10 * 4) 4
        , customTextView 13 ((screenWidth unit) / 10 * 3) 0
        ]
      , linearLayout
        [ height $ MATCH_PARENT
        , width $ MATCH_PARENT
        , gravity RIGHT
        ][shimmerFrameLayout
          [ height $ V 90
          , width $ V 130
          , cornerRadius 8.0
          ][linearLayout
            [ height $ V 90
            , width $ V 130
            , cornerRadius 8.0
            , background Color.grey900
            ][]
          ]
        ]
      ]
    ]
  ]

customTextView :: forall w. Int -> Int -> Int -> PrestoDOM (Effect Unit) w
customTextView height' width' bottomMargin' =
  shimmerFrameLayout
  [ cornerRadius 8.0
  ][linearLayout 
    [ width $ V width'
    , height $ V height'
    , background Color.grey900
    , margin $ MarginBottom bottomMargin'
    , cornerRadius 8.0
    ][]
  ]


sfl :: forall w. Int -> Int -> Number -> PrestoDOM (Effect Unit) w
sfl height' width' radius' =
  shimmerFrameLayout
  [ cornerRadius radius'
  , stroke $ "1," <> Color.grey900
  , margin $ MarginBottom 12
  ][linearLayout 
    [ width $ V width'
    , height $ V height'
    , cornerRadius radius'
    , background Color.grey900
    ][]
  ]