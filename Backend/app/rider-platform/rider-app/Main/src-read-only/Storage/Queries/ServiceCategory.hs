{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ServiceCategory where

import qualified Domain.Types.ServiceCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ServiceCategory as Beam

create :: KvDbFlow m r => (Domain.Types.ServiceCategory.ServiceCategory -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.ServiceCategory.ServiceCategory] -> m ())
createMany = traverse_ create

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> m (Maybe Domain.Types.ServiceCategory.ServiceCategory))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> m (Maybe Domain.Types.ServiceCategory.ServiceCategory))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.ServiceCategory.ServiceCategory -> m ())
updateByPrimaryKey (Domain.Types.ServiceCategory.ServiceCategory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.allowedSeats allowedSeats,
      Se.Set Beam.availableSeats availableSeats,
      Se.Set Beam.description description,
      Se.Set Beam.name name,
      Se.Set Beam.peopleCategory (Kernel.Types.Id.getId <$> peopleCategory),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.ServiceCategory Domain.Types.ServiceCategory.ServiceCategory where
  fromTType' (Beam.ServiceCategoryT {..}) = do
    pure $
      Just
        Domain.Types.ServiceCategory.ServiceCategory
          { allowedSeats = allowedSeats,
            availableSeats = availableSeats,
            description = description,
            id = Kernel.Types.Id.Id id,
            name = name,
            peopleCategory = Kernel.Types.Id.Id <$> peopleCategory,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ServiceCategory Domain.Types.ServiceCategory.ServiceCategory where
  toTType' (Domain.Types.ServiceCategory.ServiceCategory {..}) = do
    Beam.ServiceCategoryT
      { Beam.allowedSeats = allowedSeats,
        Beam.availableSeats = availableSeats,
        Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.peopleCategory = Kernel.Types.Id.getId <$> peopleCategory,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
