{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.DriverPoolConfig
  ( clearCache,
    create,
    findAllByMerchantOpCityId,
    findByMerchantOpCityIdAndTripDistance,
    findByMerchantOpCityIdAndTripDistanceAndDVeh,
    update,
  )
where

import Domain.Types.DriverPoolConfig
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DriverPoolConfig as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverPoolConfig -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m [DriverPoolConfig]
findAllByMerchantOpCityId id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id) >>= \case
    Just a -> return a
    Nothing -> cacheDriverPoolConfigs id /=<< Queries.findAllByMerchantOpCityId Nothing Nothing id

findByMerchantOpCityIdAndTripDistance :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Meters -> m (Maybe DriverPoolConfig)
findByMerchantOpCityIdAndTripDistance merchantOpCityId tripDistance = find (\config -> config.tripDistance == tripDistance) <$> findAllByMerchantOpCityId merchantOpCityId

findByMerchantOpCityIdAndTripDistanceAndDVeh :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Meters -> Maybe Variant.Variant -> Text -> m (Maybe DriverPoolConfig)
findByMerchantOpCityIdAndTripDistanceAndDVeh merchantOpCityId tripDistance variant tripCategory = find (\config -> config.tripDistance == tripDistance && config.vehicleVariant == variant && config.tripCategory == tripCategory) <$> findAllByMerchantOpCityId merchantOpCityId

cacheDriverPoolConfigs :: (CacheFlow m r) => Id MerchantOperatingCity -> [DriverPoolConfig] -> m ()
cacheDriverPoolConfigs merchantOpCityId cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantOpCityIdKey merchantOpCityId
  Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey cfg expTime

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdKey id = "driver-offer:CachedQueries:DriverPoolConfig:MerchantOperatingCityId-" <> id.getId

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache = Hedis.withCrossAppRedis . Hedis.del . makeMerchantOpCityIdKey

update :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverPoolConfig -> m ()
update = Queries.updateByPrimaryKey
