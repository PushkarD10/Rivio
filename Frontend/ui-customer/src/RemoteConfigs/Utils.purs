module RemoteConfig.Utils where

import Prelude
import DecodeUtil (decodeForeignObject, parseJSON)
import Foreign (Foreign)
import Foreign.Index (readProp)
import Common.RemoteConfig (fetchRemoteConfigString, getCityBasedConfig, defaultRemoteConfig)
import Data.Maybe (Maybe(..), maybe)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Presto.Core.Utils.Encoding (defaultDecode)
import Control.Monad.Except (runExcept)
import Data.Function (on)
import Data.String as DS
import Common.Types.App
import RemoteConfig.Types
import Data.Array as DA
import Locale.Utils(getLanguageLocale)
import Constants (languageKey)

safetyVideoConfigData :: String -> String -> Array SafetyVideoConfig
safetyVideoConfigData city language = do
    let config = fetchRemoteConfigString ("safety_videos_" <> language)
        value = decodeForeignObject (parseJSON config) $ defaultRemoteConfig []
    getCityBasedConfig value city

safetyBannerVideoConfigData :: String -> String -> Array SafetyVideoConfig
safetyBannerVideoConfigData city language = do
    let config = fetchRemoteConfigString ("safety_banner_videos_" <> language)
        value = decodeForeignObject (parseJSON config) $ defaultRemoteConfig []
    getCityBasedConfig value city

pickupInstructions :: String -> String -> String -> Array PickupInstructions
pickupInstructions placeName gateName language = do
    let config = fetchRemoteConfigString ("pickup_instructions_" <> language)
        pickupPlaces = decodeForeignObject (parseJSON config) defPlaces
        compareStrings = on (==) DS.trim
        locationsArr = pickupPlaces.locations
        myMbSpecialLocation = DA.find (\el -> compareStrings el.name placeName) locationsArr
    case myMbSpecialLocation of
        Just specialLocation -> do
            let gates = specialLocation.gates
                myMbgate = DA.find (\el -> compareStrings el.gateName gateName) gates
            case myMbgate of
                Just gate -> gate.images
                Nothing -> []
        Nothing -> []

defPlaces :: SpecialLocationsOb
defPlaces = {
    locations : []
}

getFamousDestinations :: String -> Array FamousDestination
getFamousDestinations city = do
    let langConfig = fetchRemoteConfigString $ "famous_destinations" <> (getLanguage $ getLanguageLocale languageKey)
        config = if not $ DS.null langConfig
                   then langConfig 
                   else fetchRemoteConfigString "famous_destinations_en"
        value = decodeConfig config
        famousDestinationsBasedOnLanguage = getCityBasedConfig value city
    case famousDestinationsBasedOnLanguage of
        [] -> getCityBasedConfig defaultConfigInEnglish city
        _ -> famousDestinationsBasedOnLanguage
  where
    getLanguage lang = 
      let language = DS.toLower $ DS.take 2 lang
      in if not (DS.null language) then "_" <> language else "_en"
    decodeConfig config = decodeForeignObject (parseJSON config) $ defaultRemoteConfig []
    defaultConfigInEnglish = decodeConfig (fetchRemoteConfigString "famous_destinations_en")

getEstimatesOrder :: String -> Array String
getEstimatesOrder city = do
    let config = fetchRemoteConfigString "estimates_order"
        value = decodeForeignObject (parseJSON config) $ defaultRemoteConfig ["AUTO_RICKSHAW", "BOOK_ANY"]
    getCityBasedConfig value city

getPreferredVariant :: String -> String
getPreferredVariant city = do
    let config = fetchRemoteConfigString "preferred_estimate_order"
        value = decodeForeignObject (parseJSON config) $ defaultRemoteConfig ""
    getCityBasedConfig value city

getBookAnyServices :: String -> Array String
getBookAnyServices city = do
    let config = fetchRemoteConfigString "book_any_services"
        value = decodeForeignObject (parseJSON config) $ defaultRemoteConfig ["Auto", "Non-AC Mini", "AC Mini", "Sedan", "XL Cab"]
    getCityBasedConfig value city

getBookAnySelectedServices :: String -> Array String
getBookAnySelectedServices city = do
    let config = fetchRemoteConfigString "book_any_selected_services"
        value = decodeForeignObject (parseJSON config) $ defaultRemoteConfig ["Auto", "Non-AC Mini", "AC Mini"]
    getCityBasedConfig value city

getLocationSuggestionsToExclude :: String -> Array String
getLocationSuggestionsToExclude city = do
    let config = fetchRemoteConfigString "location_suggestions_to_exclude"
        value = decodeForeignObject (parseJSON config) $ defaultRemoteConfig []
    getCityBasedConfig value city

getEnabledServices :: String -> Array String
getEnabledServices city = do
    let config = fetchRemoteConfigString "enabled_services"
        value = decodeForeignObject (parseJSON config) $ defaultRemoteConfig []
    getCityBasedConfig value city

defaultTipConfig :: TipsConfigRC
defaultTipConfig = {
  sedan : [0, 20, 30, 50],
  suv : [0, 20, 30, 50],
  hatchback : [0, 20, 30, 50],
  autoRickshaw : [0, 10, 20, 30],
  taxi : [0, 20, 30, 50],
  taxiPlus : [0, 20, 30, 50],
  bike : [0, 20, 30, 50],
  suvPlus : [0, 20, 30, 50],
  default : [0, 10, 20, 30],
  bookAny : [0, 10, 20, 30]
}

getTipConfigRC :: String -> TipsConfigRC
getTipConfigRC city = do
    let config = fetchRemoteConfigString "tip_config"
        value = decodeForeignObject (parseJSON config) $ defaultRemoteConfig defaultTipConfig
    getCityBasedConfig value city