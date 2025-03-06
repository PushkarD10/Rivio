{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.GoHomeConfig where

import Domain.Types.GoHomeConfig
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
-- import Kernel.Types.Error
import Kernel.Types.Id
-- import Kernel.Utils.Error.Throwing
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.GoHomeConfig as Queries
import qualified Tools.DynamicLogic as DynamicLogic
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => GoHomeConfig -> m ()
create = Queries.create

findByMerchantOpCityId :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m (Maybe GoHomeConfig)
findByMerchantOpCityId id mbConfigVersionMap = 
  DynamicLogic.findOneConfig
    (cast id)
    (LYT.DRIVER_CONFIG LYT.GoHomeConfig)
    mbConfigVersionMap
    Nothing
    (Queries.findByMerchantOpCityId id)

-- Call it after any update
clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m ()
clearCache merchantOpCityId =
  DynamicLogic.clearConfigCache
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.GoHomeConfig)
    Nothing

