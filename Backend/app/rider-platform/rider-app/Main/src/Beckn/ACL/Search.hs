{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Search (buildRentalSearchReq, buildOneWaySearchReq) where

import qualified Beckn.Types.Core.Taxi.Search as Search
import qualified Domain.Action.UI.Search.Common as DSearchCommon
import qualified Domain.Action.UI.Search.OneWay as DOneWaySearch
import qualified Domain.Action.UI.Search.Rental as DRentalSearch
import qualified Domain.Types.SearchRequest as DSearchReq
import Environment
import EulerHS.Prelude hiding (state)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Tools.Maps as Maps

buildOneWaySearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DOneWaySearch.OneWaySearchRes ->
  Maybe Maps.RouteInfo ->
  m (BecknReq Search.SearchMessage)
buildOneWaySearchReq DOneWaySearch.OneWaySearchRes {..} routeInto =
  buildSearchReq
    origin
    (Just destination)
    searchId
    now
    routeInto
    city
    autoAssignEnabled
    customerLanguage

buildRentalSearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DRentalSearch.RentalSearchRes ->
  m (BecknReq Search.SearchMessage)
buildRentalSearchReq DRentalSearch.RentalSearchRes {..} =
  buildSearchReq
    origin
    Nothing
    searchId
    startTime
    Nothing
    city
    False
    Nothing

buildSearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DSearchCommon.SearchReqLocation ->
  Maybe DSearchCommon.SearchReqLocation ->
  Id DSearchReq.SearchRequest ->
  UTCTime ->
  Maybe Maps.RouteInfo ->
  Text ->
  Bool ->
  Maybe Maps.Language ->
  m (BecknReq Search.SearchMessage)
buildSearchReq origin mbDestination searchId startTime mbRouteInfo city autoAssignEnabled customerLanguage = do
  let transactionId = getId searchId
      messageId = transactionId
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  context <- buildTaxiContext Context.SEARCH messageId (Just transactionId) bapIDs.cabs bapURIs.cabs Nothing Nothing city
  let intent = mkIntent origin mbDestination startTime autoAssignEnabled customerLanguage
  let searchMessage = Search.SearchMessage intent mbRouteInfo
  pure $ BecknReq context searchMessage

mkIntent ::
  DSearchCommon.SearchReqLocation ->
  Maybe DSearchCommon.SearchReqLocation ->
  UTCTime ->
  Bool ->
  Maybe Maps.Language ->
  Search.Intent
mkIntent origin mbDestination startTime autoAssignEnabled customerLanguage = do
  let startLocation =
        Search.StartInfo
          { location = mkLocation origin,
            time = Search.TimeTimestamp startTime
          }
      mkStopInfo destination =
        Search.StopInfo
          { location = mkLocation destination
          }
      mbEndLocation = mkStopInfo <$> mbDestination

      fulfillment =
        Search.FulfillmentInfo
          { start = startLocation,
            end = mbEndLocation,
            tags =
              Search.Tags
                { auto_assign_enabled = autoAssignEnabled,
                  customer_language = customerLanguage
                }
          }
  Search.Intent
    { ..
    }
  where
    mkLocation info =
      Search.Location
        { gps =
            Search.Gps
              { lat = info.gps.lat,
                lon = info.gps.lon
              },
          address = Nothing
        }
