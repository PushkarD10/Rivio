{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketPlace where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketPlace
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketPlace as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TicketPlace.TicketPlace -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.TicketPlace.TicketPlace] -> m ()
createMany = traverse_ createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> m (Maybe (Domain.Types.TicketPlace.TicketPlace))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

getTicketPlaces :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.TicketPlace.TicketPlace])
getTicketPlaces (Kernel.Types.Id.Id merchantOperatingCityId) = do
  findAllWithKV
    [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> m (Maybe (Domain.Types.TicketPlace.TicketPlace))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.TicketPlace.TicketPlace -> m ()
updateByPrimaryKey Domain.Types.TicketPlace.TicketPlace {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.closeTimings $ closeTimings,
      Se.Set Beam.description $ description,
      Se.Set Beam.gallery $ gallery,
      Se.Set Beam.iconUrl $ iconUrl,
      Se.Set Beam.lat $ lat,
      Se.Set Beam.lon $ lon,
      Se.Set Beam.mapImageUrl $ mapImageUrl,
      Se.Set Beam.merchantOperatingCityId $ (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name $ name,
      Se.Set Beam.openTimings $ openTimings,
      Se.Set Beam.placeType $ placeType,
      Se.Set Beam.shortDesc $ shortDesc,
      Se.Set Beam.termsAndConditions $ termsAndConditions,
      Se.Set Beam.merchantId $ (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.TicketPlace Domain.Types.TicketPlace.TicketPlace where
  fromTType' Beam.TicketPlaceT {..} = do
    pure $
      Just
        Domain.Types.TicketPlace.TicketPlace
          { closeTimings = closeTimings,
            description = description,
            gallery = gallery,
            iconUrl = iconUrl,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            mapImageUrl = mapImageUrl,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            openTimings = openTimings,
            placeType = placeType,
            shortDesc = shortDesc,
            termsAndConditions = termsAndConditions,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TicketPlace Domain.Types.TicketPlace.TicketPlace where
  toTType' Domain.Types.TicketPlace.TicketPlace {..} = do
    Beam.TicketPlaceT
      { Beam.closeTimings = closeTimings,
        Beam.description = description,
        Beam.gallery = gallery,
        Beam.iconUrl = iconUrl,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.mapImageUrl = mapImageUrl,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.openTimings = openTimings,
        Beam.placeType = placeType,
        Beam.shortDesc = shortDesc,
        Beam.termsAndConditions = termsAndConditions,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
