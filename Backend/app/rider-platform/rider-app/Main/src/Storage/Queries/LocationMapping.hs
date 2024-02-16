{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.LocationMapping where

import qualified Data.Text as T
import Domain.Types.LocationMapping
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.LocationMapping as BeamLM

latestTag :: Text
latestTag = "LATEST"

create :: MonadFlow m => LocationMapping -> m ()
create = createWithKV

countOrders :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m Int
countOrders entityId = findAllWithKVAndConditionalDB [Se.Is BeamLM.entityId $ Se.Eq entityId] Nothing <&> length

findByEntityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [LocationMapping]
findByEntityId entityId =
  findAllWithKVAndConditionalDB
    [ Se.Is BeamLM.entityId $ Se.Eq entityId
    ]
    (Just (Se.Desc BeamLM.createdAt))

getLatestStartByEntityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m (Maybe LocationMapping)
getLatestStartByEntityId entityId =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamLM.entityId $ Se.Eq entityId,
          Se.Is BeamLM.order $ Se.Eq 0,
          Se.Is BeamLM.version $ Se.Eq latestTag
        ]
    ]

findAllByEntityIdAndOrder :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Int -> m [LocationMapping]
findAllByEntityIdAndOrder entityId order =
  findAllWithKVAndConditionalDB
    [Se.And [Se.Is BeamLM.entityId $ Se.Eq entityId, Se.Is BeamLM.order $ Se.Eq order]]
    Nothing

maxOrderByEntity :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m Int
maxOrderByEntity entityId = do
  lms <- findAllWithKVAndConditionalDB [Se.Is BeamLM.entityId $ Se.Eq entityId] Nothing
  let orders = map order lms
  case orders of
    [] -> pure 0
    _ -> pure $ maximum orders

updatePastMappingVersions :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Int -> m ()
updatePastMappingVersions entityId order = do
  mappings <- findAllByEntityIdAndOrder entityId order
  traverse_ incrementVersion mappings

-- This function is not correct, need to correct it later
incrementVersion :: MonadFlow m => LocationMapping -> m ()
incrementVersion mapping = do
  newVersion <- getNewVersion mapping
  updateVersion mapping.entityId mapping.order newVersion

getNewVersion :: MonadFlow m => LocationMapping -> m Text
getNewVersion mapping =
  case T.splitOn "-" mapping.version of
    ["v", versionNum] -> do
      oldVersionInt <-
        fromEitherM (InternalError . (("Location mapping version parse failed: id: " <> mapping.id.getId <> "; err: ") <>)) $
          readEither @String @Integer (T.unpack versionNum)
      pure $ "v-" <> T.pack (show (oldVersionInt + 1))
    _ -> pure "v-1"

updateVersion :: MonadFlow m => Text -> Int -> Text -> m ()
updateVersion entityId order version = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamLM.version version,
      Se.Set BeamLM.updatedAt now
    ]
    [Se.Is BeamLM.entityId $ Se.Eq entityId, Se.Is BeamLM.order $ Se.Eq order]

findLastMapping :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Int -> m (Maybe LocationMapping)
findLastMapping entityId order = do
  findOneWithKV
    [ Se.And
        [ Se.Is BeamLM.entityId $ Se.Eq entityId,
          Se.Is BeamLM.order $ Se.Eq order,
          Se.Is BeamLM.version $ Se.Eq latestTag
        ]
    ]

instance FromTType' BeamLM.LocationMapping LocationMapping where
  fromTType' BeamLM.LocationMappingT {..} = do
    pure $
      Just
        LocationMapping
          { id = Id id,
            tag = tag,
            locationId = Id locationId,
            entityId = entityId,
            order = order,
            version = version,
            createdAt = createdAt,
            updatedAt = updatedAt,
            merchantId = Id <$> merchantId,
            merchantOperatingCityId = Id <$> merchantOperatingCityId
          }

instance ToTType' BeamLM.LocationMapping LocationMapping where
  toTType' LocationMapping {..} = do
    BeamLM.LocationMappingT
      { BeamLM.id = getId id,
        BeamLM.tag = tag,
        BeamLM.locationId = getId locationId,
        BeamLM.entityId = entityId,
        BeamLM.order = order,
        BeamLM.version = version,
        BeamLM.createdAt = createdAt,
        BeamLM.updatedAt = updatedAt,
        BeamLM.merchantId = getId <$> merchantId,
        BeamLM.merchantOperatingCityId = getId <$> merchantOperatingCityId
      }
