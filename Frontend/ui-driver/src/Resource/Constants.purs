{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Resource.Constants where

import Prelude ((==), (&&), (<>))
import Data.String (trim)
import Services.API (LocationInfo(..))
import Data.Maybe (fromMaybe, Maybe(..))

type Language =
    {
        name :: String,
        value :: String
    }

getLanguages :: Array Language
getLanguages = 
    [
        {name:"English",value:"EN_US"},
        {name:"ಕನ್ನಡ",value:"KN_IN"},
        {name:"हिन्दी",value :"HI_IN"},
        {name:"தமிழ்",value :"TA_IN"}
    ]

decodeAddress :: LocationInfo -> Boolean -> String
decodeAddress ( LocationInfo address) fullAddress =
        if fullAddress then 
             if ( trim (fromMaybe "" address.city) == "" && trim (fromMaybe "" address.area) == "" && trim (fromMaybe "" address.street) == "" && trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "" ) then
                    ((fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
            else if ( trim (fromMaybe "" address.area) == "" && trim (fromMaybe "" address.street) == "" && trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "" ) then
                    ((fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
            else if ( trim (fromMaybe "" address.street) == "" && trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "" ) then
                    ((fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
            else if ( trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "") then
                    ((fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
            else if ( trim (fromMaybe "" address.door) == "") then
                    ((fromMaybe "" address.building) <> ", " <> (fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
                    else
                    ((fromMaybe "" address.door) <> ", " <> (fromMaybe "" address.building) <> ", " <> (fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
        else 
            if ( trim (fromMaybe "" address.city) == "" && trim (fromMaybe "" address.area) == "" && trim (fromMaybe "" address.street) == ""  ) then
                    (trim (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
            else if ( trim (fromMaybe "" address.area) == "" && trim (fromMaybe "" address.street) == "" ) then
                    (trim (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
            else if ( trim (fromMaybe "" address.street) == "") then
                    (trim (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
                    else
                    (trim (fromMaybe "" address.street)) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country)

tripDatesCount :: Int
tripDatesCount = 15