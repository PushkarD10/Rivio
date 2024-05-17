{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantOperatingCity
  ( create,
    findById,
    findByMerchantIdAndCity,
    findByMerchantShortIdAndCity,
    findAllByMerchantIdAndState,
  )
where

import Domain.Types.Merchant (Merchant)
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.MerchantOperatingCity as Queries

create :: KvDbFlow m r => MerchantOperatingCity -> m ()
create = Queries.create

findById :: KvDbFlow m r => Id MerchantOperatingCity -> m (Maybe MerchantOperatingCity)
findById id =
  Hedis.safeGet (makeMerchantOperatingCityIdKey id) >>= \case
    Just a -> return a
    Nothing -> flip whenJust cachedMerchantOperatingCityId /=<< Queries.findById id

findByMerchantIdAndCity :: KvDbFlow m r => Id Merchant -> Context.City -> m (Maybe MerchantOperatingCity)
findByMerchantIdAndCity merchantId city =
  Hedis.safeGet (makeMerchantIdAndCityKey merchantId city) >>= \case
    Just a -> return a
    Nothing -> flip whenJust cachedMerchantIdAndCity /=<< Queries.findByMerchantIdAndCity merchantId city

findByMerchantShortIdAndCity :: KvDbFlow m r => ShortId Merchant -> Context.City -> m (Maybe MerchantOperatingCity)
findByMerchantShortIdAndCity merchantShortId city =
  Hedis.safeGet (makeMerchantShortIdAndCityKey merchantShortId city) >>= \case
    Just a -> return a
    Nothing -> flip whenJust cachedMerchantShortIdAndCity /=<< Queries.findByMerchantShortIdAndCity merchantShortId city

findAllByMerchantIdAndState :: KvDbFlow m r => Id Merchant -> Context.IndianState -> m [MerchantOperatingCity]
findAllByMerchantIdAndState merchantId state =
  Hedis.safeGet (makeMerchantIdAndStateKey merchantId state) >>= \case
    Just a -> return a
    Nothing -> cacheMerchantIdAndState merchantId state /=<< Queries.findAllByMerchantIdAndState merchantId state

cachedMerchantOperatingCityId :: CacheFlow m r => MerchantOperatingCity -> m ()
cachedMerchantOperatingCityId merchantOperatingCity = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantOperatingCityIdKey = makeMerchantOperatingCityIdKey merchantOperatingCity.id
  Hedis.setExp merchantOperatingCityIdKey merchantOperatingCity expTime

cachedMerchantIdAndCity :: CacheFlow m r => MerchantOperatingCity -> m ()
cachedMerchantIdAndCity merchantOperatingCity = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdAndCityKey = makeMerchantIdAndCityKey merchantOperatingCity.merchantId merchantOperatingCity.city
  Hedis.setExp merchantIdAndCityKey merchantOperatingCity expTime

cachedMerchantShortIdAndCity :: CacheFlow m r => MerchantOperatingCity -> m ()
cachedMerchantShortIdAndCity merchantOperatingCity = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantShortIdAndCityKey = makeMerchantShortIdAndCityKey merchantOperatingCity.merchantShortId merchantOperatingCity.city
  Hedis.setExp merchantShortIdAndCityKey merchantOperatingCity expTime

cacheMerchantIdAndState :: CacheFlow m r => Id Merchant -> Context.IndianState -> [MerchantOperatingCity] -> m ()
cacheMerchantIdAndState merchantId state merchantOperatingCities = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdAndStateKey = makeMerchantIdAndStateKey merchantId state
  Hedis.setExp merchantIdAndStateKey merchantOperatingCities expTime

makeMerchantOperatingCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOperatingCityIdKey merchantOperatingCityId = "CachedQueries:MerchantOperatingCity:Id-" <> merchantOperatingCityId.getId

makeMerchantIdAndCityKey :: Id Merchant -> Context.City -> Text
makeMerchantIdAndCityKey merchantId city = "CachedQueries:MerchantOperatingCity:MerchantId-" <> merchantId.getId <> ":City-" <> show city

makeMerchantShortIdAndCityKey :: ShortId Merchant -> Context.City -> Text
makeMerchantShortIdAndCityKey merchantShortId city = "CachedQueries:MerchantOperatingCity:MerchantShortId-" <> merchantShortId.getShortId <> ":City-" <> show city

makeMerchantIdAndStateKey :: Id Merchant -> Context.IndianState -> Text
makeMerchantIdAndStateKey merchantId state = "CachedQueries:MerchantOperatingCity:MerchantId-" <> merchantId.getId <> ":State-" <> show state
