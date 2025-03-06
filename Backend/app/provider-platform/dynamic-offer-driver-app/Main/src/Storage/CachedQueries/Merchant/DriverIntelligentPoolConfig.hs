{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.DriverIntelligentPoolConfig
  {-# WARNING
    "This module contains direct calls to the table and redis. \
  \ But most likely you need a version from Cac with inMem results feature."
    #-}
  ( create,
    clearCache,
    update,
    getDriverIntelligentPoolConfigFromDB,
    getDriverIntelligentPoolConfigFromDBInRideFlow,
  )
where

import Domain.Types.DriverIntelligentPoolConfig
import Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.DriverIntelligentPoolConfig as Queries
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Tools.DynamicLogic as DynamicLogic

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverIntelligentPoolConfig -> m ()
create = Queries.create

getDriverIntelligentPoolConfigFromDBInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> [LYT.ConfigVersionMap] -> Maybe Value -> m (Maybe DriverIntelligentPoolConfig)
getDriverIntelligentPoolConfigFromDBInRideFlow id configVersionMap extraDimensions = getDriverIntelligentPoolConfigFromDB id (Just configVersionMap) extraDimensions

getDriverIntelligentPoolConfigFromDB :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> Maybe Value -> m (Maybe DriverIntelligentPoolConfig)
getDriverIntelligentPoolConfigFromDB id mbConfigVersionMap extraDimensions =
  DynamicLogic.findOneConfig
    (cast id)
    (LYT.DRIVER_CONFIG LYT.DriverIntelligentPoolConfig)
    mbConfigVersionMap
    extraDimensions
    (Queries.findByMerchantOpCityId id)

-- Call it after any update
clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m ()
clearCache merchantOpCityId = DynamicLogic.clearConfigCache
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.DriverIntelligentPoolConfig)
    Nothing

update :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverIntelligentPoolConfig -> m ()
update = Queries.update
