{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Maps
  ( module Reexport,
    autoComplete,
    getDistance,
    getDistances,
    getPlaceDetails,
    getPlaceName,
    getRoutes,
    snapToRoad,
    getPickupRoutes,
    getTripRoutes,
    getSimulatedRoutes,
    getSimulatedDistance,
  )
where

import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Domain.Types.Merchant.MerchantServiceUsageConfig (MerchantServiceUsageConfig)
import Kernel.External.Maps as Reexport hiding
  ( autoComplete,
    getDistance,
    getDistances,
    getPlaceDetails,
    getPlaceName,
    getRoutes,
    snapToRoad,
  )
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import Tools.Error
import Tools.Metrics

getDistance ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getDistance = runWithServiceConfig Maps.getDistance (.getDistances)

getSimulatedDistance ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  GetDistanceReq a b ->
  m (GetDistanceResp a b)
getSimulatedDistance = runWithServiceConfig Maps.getDistance (.getSimulatedDistance)

getDistances ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  Id Merchant ->
  GetDistancesReq a b ->
  m (GetDistancesResp a b)
getDistances = runWithServiceConfig Maps.getDistances (.getDistances)

getRoutes :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => Id Merchant -> GetRoutesReq -> m GetRoutesResp
getRoutes = runWithServiceConfig Maps.getRoutes (.getRoutes)

getSimulatedRoutes :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => Id Merchant -> GetRoutesReq -> m GetRoutesResp
getSimulatedRoutes = runWithServiceConfig Maps.getRoutes (.getSimulatedRoutes)

getPickupRoutes :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id Merchant -> GetRoutesReq -> m GetRoutesResp
getPickupRoutes = runWithServiceConfig Maps.getRoutes (.getPickupRoutes)

getTripRoutes :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id Merchant -> GetRoutesReq -> m GetRoutesResp
getTripRoutes = runWithServiceConfig Maps.getRoutes (.getTripRoutes)

snapToRoad ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["snapToRoadSnippetThreshold" ::: HighPrecMeters]
  ) =>
  Id Merchant ->
  SnapToRoadReq ->
  m SnapToRoadResp
snapToRoad = runWithServiceConfig Maps.snapToRoad (.snapToRoad)

autoComplete :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => Id Merchant -> AutoCompleteReq -> m AutoCompleteResp
autoComplete = runWithServiceConfig Maps.autoComplete (.autoComplete)

getPlaceName :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => Id Merchant -> GetPlaceNameReq -> m GetPlaceNameResp
getPlaceName = runWithServiceConfig Maps.getPlaceName (.getPlaceName)

getPlaceDetails :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => Id Merchant -> GetPlaceDetailsReq -> m GetPlaceDetailsResp
getPlaceDetails = runWithServiceConfig Maps.getPlaceDetails (.getPlaceDetails)

runWithServiceConfig ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) =>
  (MapsServiceConfig -> req -> m resp) ->
  (MerchantServiceUsageConfig -> MapsService) ->
  Id Merchant ->
  req ->
  m resp
runWithServiceConfig func getCfg merchantId req = do
  merchantConfig <- QMSUC.findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  merchantMapsServiceConfig <-
    QMSC.findByMerchantIdAndService merchantId (DMSC.MapsService $ getCfg merchantConfig)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Maps" (show $ getCfg merchantConfig))
  case merchantMapsServiceConfig.serviceConfig of
    DMSC.MapsServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"
