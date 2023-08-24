{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.SearchRequestSpecialZone where

import qualified Domain.Types.FareProduct as FareProductD
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import qualified Tools.Maps as Maps

data SearchRequestSpecialZone = SearchRequestSpecialZone
  { id :: Id SearchRequestSpecialZone,
    transactionId :: Text,
    messageId :: Text,
    startTime :: UTCTime,
    validTill :: UTCTime,
    providerId :: Id DM.Merchant,
    fromLocation :: DLoc.SearchReqLocation,
    toLocation :: DLoc.SearchReqLocation,
    area :: Maybe FareProductD.Area,
    bapId :: Text,
    bapUri :: BaseUrl,
    estimatedDistance :: Meters,
    estimatedDuration :: Seconds,
    mapsServices :: SearchRequestSpecialZoneMapsServices,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, PrettyShow, Show)

data SearchRequestSpecialZoneMapsServices = SearchRequestSpecialZoneMapsServices
  { getDistances :: Maybe (Maps.SMapsService 'Maps.GetDistances),
    getPlaceName :: Maybe (Maps.SMapsService 'Maps.GetPlaceName)
  }
  deriving (Generic, PrettyShow, Show)
