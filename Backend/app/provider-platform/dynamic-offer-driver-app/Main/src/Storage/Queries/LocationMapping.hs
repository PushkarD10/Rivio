{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Storage.Queries.LocationMapping where

import qualified Data.Text as T
import Domain.Types.LocationMapping
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.LocationMapping as BeamLM

latestTag :: Text
latestTag = "LATEST"

create :: KvDbFlow m r => LocationMapping -> m ()
create = createWithKV

findById :: KvDbFlow m r => Id LocationMapping -> m (Maybe LocationMapping)
findById (Id locationMapping) = findOneWithKV [Se.Is BeamLM.id $ Se.Eq locationMapping]

countOrders :: KvDbFlow m r => Text -> m Int
countOrders entityId = findAllWithKVAndConditionalDB [Se.Is BeamLM.entityId $ Se.Eq entityId] Nothing <&> length

maxOrderByEntity :: KvDbFlow m r => Text -> m Int
maxOrderByEntity entityId = do
  lms <- findAllWithKVAndConditionalDB [Se.Is BeamLM.entityId $ Se.Eq entityId] Nothing
  let orders = map order lms
  case orders of
    [] -> pure 0
    _ -> pure $ maximum orders

findByEntityIdOrderAndVersion :: KvDbFlow m r => Text -> Int -> Text -> m [LocationMapping]
findByEntityIdOrderAndVersion entityId order version =
  findAllWithKVAndConditionalDB
    [Se.And [Se.Is BeamLM.entityId $ Se.Eq entityId, Se.Is BeamLM.order $ Se.Eq order, Se.Is BeamLM.version $ Se.Eq version]]
    Nothing

findByEntityId :: KvDbFlow m r => Text -> m [LocationMapping]
findByEntityId entityId =
  findAllWithKVAndConditionalDB
    [ Se.Is BeamLM.entityId $ Se.Eq entityId
    ]
    (Just (Se.Desc BeamLM.createdAt))

getLatestStartByEntityId :: KvDbFlow m r => Text -> m (Maybe LocationMapping)
getLatestStartByEntityId entityId =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamLM.entityId $ Se.Eq entityId,
          Se.Is BeamLM.order $ Se.Eq 0,
          Se.Is BeamLM.version $ Se.Eq latestTag
        ]
    ]

getLatestEndByEntityId :: KvDbFlow m r => Text -> m (Maybe LocationMapping)
getLatestEndByEntityId entityId =
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is BeamLM.entityId $ Se.Eq entityId,
          Se.Is BeamLM.order $ Se.Not $ Se.Eq 0,
          Se.Is BeamLM.version $ Se.Eq latestTag
        ]
    ]
    (Just (Se.Desc BeamLM.createdAt))
    <&> listToMaybe

findAllByEntityIdAndOrder :: KvDbFlow m r => Text -> Int -> m [LocationMapping]
findAllByEntityIdAndOrder entityId order =
  findAllWithKVAndConditionalDB
    [Se.And [Se.Is BeamLM.entityId $ Se.Eq entityId, Se.Is BeamLM.order $ Se.Eq order]]
    Nothing

updatePastMappingVersions :: KvDbFlow m r => Text -> Int -> m ()
updatePastMappingVersions entityId order = do
  mappings <- findAllByEntityIdAndOrder entityId order
  traverse_ incrementVersion mappings

incrementVersion :: KvDbFlow m r => LocationMapping -> m ()
incrementVersion mapping = do
  let newVersion = getNewVersion mapping.version
  updateVersion mapping.id newVersion

getNewVersion :: Text -> Text
getNewVersion oldVersion =
  case T.splitOn "-" oldVersion of
    ["v", versionNum] -> "v-" <> T.pack (show (read (T.unpack versionNum) + 1))
    _ -> "v-1"

updateVersion :: KvDbFlow m r => Id LocationMapping -> Text -> m ()
updateVersion id version = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamLM.version version,
      Se.Set BeamLM.updatedAt now
    ]
    [Se.Is BeamLM.id $ Se.Eq id.getId]

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
