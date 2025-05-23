{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Common.RemoteConfig.Utils where

import Common.RemoteConfig.Types (RemoteConfig, RCCarousel(..), ForwardBatchConfigData(..), TipsConfig, defaultForwardBatchConfigData)
import DecodeUtil (decodeForeignObject, parseJSON, setAnyInWindow)
import Data.String (null, toLower)
import Data.Maybe (Maybe(..))
import Prelude (not, ($), (==), (||))
import Data.Maybe (fromMaybe)
import Data.Array (elem, filter, uncons)
import Data.Array as DA
import Data.Function.Uncurried (runFn3, runFn2)
import DecodeUtil (getAnyFromWindow)

foreign import fetchRemoteConfigString :: String -> String

foreign import fetchRemoteConfig :: forall a. String -> a

foreign import isWhiteListed :: String -> Array String -> Boolean

defaultRemoteConfig :: forall a. a -> RemoteConfig a
defaultRemoteConfig defaultValue =
  { bangalore : Just defaultValue
  , kolkata : Just defaultValue
  , chennai : Just defaultValue
  , tumakuru : Just defaultValue
  , mysore : Just defaultValue
  , kochi : Just defaultValue
  , delhi : Just defaultValue
  , hyderabad : Just defaultValue
  , mumbai : Just defaultValue
  , coimbatore : Just defaultValue
  , pondicherry : Just defaultValue
  , goa : Just defaultValue
  , pune : Just defaultValue
  , tamilnaducities : Just defaultValue
  , default : defaultValue
  , noida : Just defaultValue
  , gurugram : Just defaultValue
  , vellore : Just defaultValue
  , hosur : Just defaultValue
  , madurai : Just defaultValue
  , thanjavur : Just defaultValue
  , tirunelveli : Just defaultValue
  , salem : Just defaultValue
  , trichy : Just defaultValue 
  , davanagere : Just defaultValue
  , shivamogga : Just defaultValue
  , hubli : Just defaultValue
  , mangalore : Just defaultValue
  , gulbarga : Just defaultValue
  , udupi : Just defaultValue
  , config: Nothing
  }

carouselConfigData :: String -> String -> String -> String -> String -> String -> Array RCCarousel
carouselConfigData city configKey default userId categoryFilter variantFilter =
  let
    remoteConfig = fetchRemoteConfigString configKey

    parseVal = if not null remoteConfig then remoteConfig else fetchRemoteConfigString default

    decodedConfg = decodeForeignObject (parseJSON parseVal) $ defaultRemoteConfig []
  in
    filterWhiteListedConfigs userId $ filterCategoryBasedCarousel categoryFilter variantFilter $ getCityBasedConfig decodedConfg city

-- Each RCCarousel has a category field which is an array of strings, If the array is empty I want to include that RCCarousel in output array, but if it has some values I want to match check `elem` if the categoryFilter is present in the array or not. If it is present then include that RCCarousel in the output array.
filterCategoryBasedCarousel :: String -> String -> Array RCCarousel -> Array RCCarousel
filterCategoryBasedCarousel allowedFilter variantFilter configs =
  let
    filteredConfigs = filter (\x -> validateConfig x) configs
  in
    filteredConfigs
  where
  validateConfig :: RCCarousel -> Boolean
  validateConfig (RCCarousel config) =
    let
      categoryList = fromMaybe [] config.categoryFilter
    in
      if DA.null categoryList then true else elem allowedFilter categoryList || elem variantFilter categoryList

fetchWhiteListedUser :: String -> Array String
fetchWhiteListedUser configKey = fetchRemoteConfig configKey

filterWhiteListedConfigs :: String -> Array RCCarousel -> Array RCCarousel
filterWhiteListedConfigs userId configs =
  let
    whiteListedConfigs = filter (\x -> validateConfig x) configs
  in
    whiteListedConfigs
  where
  validateConfig :: RCCarousel -> Boolean
  validateConfig (RCCarousel config) =
    let
      whiteListedUserListArray = fromMaybe [] config.whitelist
    in
      if DA.null whiteListedUserListArray then true else validateUser whiteListedUserListArray

  validateUser :: Array String -> Boolean
  validateUser parameterList = case uncons parameterList of
    Just { head: x, tail: xs } ->
      let
        userList = fetchWhiteListedUser x
      in
        if isWhiteListed userId userList then true else validateUser xs -- TODO:: Need to check why it's not working within PS and replace with Map for optimisation
    Nothing -> false

forwardBatchConfigData :: String -> ForwardBatchConfigData
forwardBatchConfigData city =
  let
    remoteConfig = fetchRemoteConfigString "Forward_Dispatch_Feature"
    decodedConfg = decodeForeignObject (parseJSON remoteConfig) $ defaultRemoteConfig defaultForwardBatchConfigData
  in 
    getCityBasedConfig decodedConfg $ toLower city

getCityBasedConfig :: forall a. RemoteConfig a -> String -> a
getCityBasedConfig config city = case city of
  "bangalore" -> fromMaybe config.default config.bangalore
  "kolkata" -> fromMaybe config.default config.kolkata
  "chennai" -> fromMaybe config.default config.chennai
  "mysore" -> fromMaybe config.default config.mysore
  "tumakuru" -> fromMaybe config.default config.tumakuru
  "kochi" -> fromMaybe config.default config.kochi
  "delhi" -> fromMaybe config.default config.delhi
  "hyderabad" -> fromMaybe config.default config.hyderabad
  "mumbai" -> fromMaybe config.default config.mumbai
  "coimbatore" -> fromMaybe config.default config.coimbatore
  "pondicherry" -> fromMaybe config.default config.pondicherry
  "goa" -> fromMaybe config.default config.goa
  "pune" -> fromMaybe config.default config.pune
  "tamilnaducities" -> fromMaybe config.default config.tamilnaducities
  "noida" -> fromMaybe config.default config.noida
  "gurugram" -> fromMaybe config.default config.gurugram
  "vellore" -> fromMaybe config.default config.vellore
  "hosur" -> fromMaybe config.default config.hosur
  "madurai" -> fromMaybe config.default config.madurai
  "thanjavur" -> fromMaybe config.default config.thanjavur
  "tirunelveli" -> fromMaybe config.default config.tirunelveli
  "salem" -> fromMaybe config.default config.salem
  "trichy" -> fromMaybe config.default config.trichy
  "davanagere" -> fromMaybe config.default config.davanagere
  "shivamogga" -> fromMaybe config.default config.shivamogga
  "hubli" -> fromMaybe config.default config.hubli
  "mangalore" -> fromMaybe config.default config.mangalore
  "gulbarga" -> fromMaybe config.default config.gulbarga
  "udupi" -> fromMaybe config.default config.udupi
  _ -> config.default

tipConfigData :: String -> String -> Array Int
tipConfigData city variant = do
  let
    tipsConfig = runFn3 getAnyFromWindow "tips_config" Nothing Just
    decodedConfig = case tipsConfig of
          Just (config :: (RemoteConfig TipsConfig)) -> config
          Nothing -> do
            let remoteConfig = fetchRemoteConfigString "tips_config"
                decodedConfg = decodeForeignObject (parseJSON remoteConfig) $ defaultRemoteConfig defaultTipsConfig
                _ = runFn2 setAnyInWindow "tips_config" decodedConfg
            decodedConfg
  getTipForVariant variant $ getCityBasedConfig decodedConfig $ toLower city
  where
    -- if a variant tip is not provided for a particular city we will check for default variant config for that city. If default is not there then tip wont be there.
    getTipForVariant variant config = case getTip config variant of
      Nothing -> fromMaybe [] $ getTip config "default"
      Just tips -> tips

    getTip config variant = 
      case variant of 
        "SEDAN" -> config.sedan
        "SUV" -> config.suv
        "HATCHBACK" -> config.hatchback
        "AUTO_RICKSHAW" -> config.autoRickshaw
        "TAXI" -> config.taxi
        "TAXI_PLUS" -> config.taxiPlus
        "BOOK_ANY" -> config.bookAny
        _ -> config.default

defaultTipsConfig :: TipsConfig
defaultTipsConfig = 
  { sedan: Nothing
  , suv: Nothing
  , hatchback: Nothing
  , autoRickshaw: Nothing
  , taxi: Nothing
  , taxiPlus: Nothing
  , bookAny: Nothing
  , default: Nothing
  }
