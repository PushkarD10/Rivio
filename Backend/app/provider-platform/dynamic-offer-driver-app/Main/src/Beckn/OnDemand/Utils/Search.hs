{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Utils.Search where

import Beckn.ACL.Common (getTagV2)
import Beckn.OnDemand.Utils.Common (firstStop, lastStop)
import qualified BecknV2.OnDemand.Types as Spec
import Control.Lens
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Time
import EulerHS.Prelude hiding (id, view, (^?))
import Kernel.External.Maps as Maps
import Kernel.Types.Common
import Kernel.Utils.Common (fromMaybeM)
import Tools.Error (GenericError (InvalidRequest))

getPickUpTime :: Spec.SearchReqMessage -> Maybe Data.Time.UTCTime
getPickUpTime req =
  req.searchReqMessageIntent
    >>= (.intentFulfillment)
    >>= (.fulfillmentStops)
    >>= firstStop
    >>= (.stopTime)
    >>= (.timeTimestamp)

getPickUpLocation :: MonadFlow m => Spec.SearchReqMessage -> m Spec.Location
getPickUpLocation req =
  req.searchReqMessageIntent
    >>= (.intentFulfillment)
    >>= (.fulfillmentStops)
    >>= firstStop
    >>= (.stopLocation)
    & fromMaybeM (InvalidRequest "Missing Pickup Location")

getDropOffLocation :: Spec.SearchReqMessage -> Maybe Spec.Location
getDropOffLocation req = do
  req.searchReqMessageIntent
    >>= (.intentFulfillment)
    >>= (.fulfillmentStops)
    >>= lastStop
    >>= (.stopLocation)

getPickUpLocationGps :: MonadFlow m => Spec.SearchReqMessage -> m Text
getPickUpLocationGps req =
  req.searchReqMessageIntent
    >>= (.intentFulfillment)
    >>= (.fulfillmentStops)
    >>= firstStop
    >>= (.stopLocation)
    >>= (.locationGps)
    & fromMaybeM (InvalidRequest "Missing Pickup Location GPS")

getDropOffLocationGps :: Spec.SearchReqMessage -> Maybe Text
getDropOffLocationGps req = do
  req.searchReqMessageIntent
    >>= (.intentFulfillment)
    >>= (.fulfillmentStops)
    >>= lastStop
    >>= (.stopLocation)
    >>= (.locationGps)

getDistance :: MonadFlow m => Spec.SearchReqMessage -> m (Maybe Meters)
getDistance req = do
  tagGroups <-
    req.searchReqMessageIntent
      >>= (.intentFulfillment)
      >>= (.fulfillmentTags)
      & fromMaybeM (InvalidRequest "Missing Tags")
  let tagValue = getTagV2 "route_info" "distance_info_in_m" tagGroups
  return $ tagValue >>= readMaybe . T.unpack >>= Just . Meters

getDuration :: MonadFlow m => Spec.SearchReqMessage -> m (Maybe Seconds)
getDuration req = do
  tagGroups <-
    req.searchReqMessageIntent
      >>= (.intentFulfillment)
      >>= (.fulfillmentTags)
      & fromMaybeM (InvalidRequest "Missing Tags")
  let tagValue = getTagV2 "route_info" "duration_info_in_s" tagGroups
  return $ tagValue >>= readMaybe . T.unpack >>= Just . Seconds

buildCustomerLanguage :: MonadFlow m => Spec.SearchReqMessage -> m (Maybe Language)
buildCustomerLanguage req = do
  tagGroups <-
    req.searchReqMessageIntent
      >>= (.intentFulfillment)
      >>= (.fulfillmentCustomer)
      >>= (.customerPerson)
      >>= (.personTags)
      & fromMaybeM (InvalidRequest "Missing Tags")
  let tagValue = getTagV2 "customer_info" "customer_language" tagGroups
  return $ tagValue >>= readMaybe . T.unpack >>= Just

buildDisabilityTag :: MonadFlow m => Spec.SearchReqMessage -> m (Maybe Text)
buildDisabilityTag req = do
  tagGroups <-
    req.searchReqMessageIntent
      >>= (.intentFulfillment)
      >>= (.fulfillmentCustomer)
      >>= (.customerPerson)
      >>= (.personTags)
      & fromMaybeM (InvalidRequest "Missing Tags")
  let tagValue = getTagV2 "customer_info" "customer_disability" tagGroups
  return tagValue

buildCustomerPhoneNumber :: MonadFlow m => Spec.SearchReqMessage -> m (Maybe Text)
buildCustomerPhoneNumber req = do
  tagGroups <-
    req.searchReqMessageIntent
      >>= (.intentFulfillment)
      >>= (.fulfillmentCustomer)
      >>= (.customerPerson)
      >>= (.personTags)
      & fromMaybeM (InvalidRequest "Missing Tags")
  let tagValue = getTagV2 "customer_info" "customer_phone_number" tagGroups
  return tagValue

-- customerPerson <- req ^? (ix "searchReqMessageIntent" . key "intentFulfillment" . key "fulfillmentCustomer" . key "customerPerson" . key "tags") & fromMaybeM (InvalidRequest "Missing Fields")

getIsReallocationEnabled :: MonadFlow m => Spec.SearchReqMessage -> m (Maybe Bool)
getIsReallocationEnabled req = do
  tagGroups <-
    req.searchReqMessageIntent
      >>= (.intentFulfillment)
      >>= (.fulfillmentTags)
      & fromMaybeM (InvalidRequest "Missing Tags")
  let tagValue = getTagV2 "reallocation_info" "is_reallocation_enabled" tagGroups
  return $ tagValue >>= readMaybe . T.unpack >>= Just

buildRoutePoints :: MonadFlow m => Spec.SearchReqMessage -> m (Maybe [Maps.LatLong])
buildRoutePoints req = do
  tagGroups <-
    req.searchReqMessageIntent
      >>= (.intentFulfillment)
      >>= (.fulfillmentTags)
      & fromMaybeM (InvalidRequest "Missing Tags")
  return $ getTagV2 "route_info" "route_points" tagGroups >>= decode . encodeUtf8 >>= Just

buildMultipleRoutesTag :: MonadFlow m => Spec.SearchReqMessage -> m (Maybe [Maps.RouteInfo])
buildMultipleRoutesTag req = do
  tagGroups <-
    req.searchReqMessageIntent
      >>= (.intentFulfillment)
      >>= (.fulfillmentTags)
      & fromMaybeM (InvalidRequest "Missing Tags")
  return $ getTagV2 "route_info" "multiple_routes" tagGroups >>= decode . encodeUtf8 >>= Just
