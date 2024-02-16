{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FleetDriverAssociation where

import Domain.Types.FleetDriverAssociation
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.FleetDriverAssociation as BeamFDVA

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FleetDriverAssociation -> m ()
create = createWithKV

findByDriverIdAndFleetOwnerId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Text -> m (Maybe FleetDriverAssociation)
findByDriverIdAndFleetOwnerId driverId fleetOwnerId =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamFDVA.driverId $ Se.Eq driverId.getId,
          Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId
        ]
    ]

upsert :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FleetDriverAssociation -> m ()
upsert a@FleetDriverAssociation {..} = do
  res <- findOneWithKV [Se.And [Se.Is BeamFDVA.driverId $ Se.Eq (a.driverId.getId), Se.Is BeamFDVA.fleetOwnerId $ Se.Eq a.fleetOwnerId]]
  if isJust res
    then
      updateOneWithKV
        [ Se.Set BeamFDVA.isActive isActive,
          Se.Set BeamFDVA.updatedAt updatedAt
        ]
        [Se.And [Se.Is BeamFDVA.driverId $ Se.Eq (a.driverId.getId), Se.Is BeamFDVA.fleetOwnerId $ Se.Eq a.fleetOwnerId]]
    else createWithKV a

findAllActiveDriverByFleetOwnerId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Int -> Int -> m [FleetDriverAssociation]
findAllActiveDriverByFleetOwnerId fleetOwnerId limit offset = do
  findAllWithOptionsKV
    [Se.And [Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId, Se.Is BeamFDVA.isActive $ Se.Eq True]]
    (Se.Desc BeamFDVA.updatedAt)
    (Just limit)
    (Just offset)

findAllDriverByFleetOwnerId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Int -> Int -> m [FleetDriverAssociation]
findAllDriverByFleetOwnerId fleetOwnerId limit offset = do
  findAllWithOptionsKV
    [Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId]
    (Se.Desc BeamFDVA.updatedAt)
    (Just limit)
    (Just offset)

findAllDriverByFleetOwnerId' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [FleetDriverAssociation]
findAllDriverByFleetOwnerId' fleetOwnerId = do
  findAllWithKV
    [Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId]

updateFleetDriverActiveStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Id Person -> Bool -> m ()
updateFleetDriverActiveStatus fleetOwnerId driverId isActive = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamFDVA.isActive isActive,
      Se.Set BeamFDVA.updatedAt now
    ]
    [Se.And [Se.Is BeamFDVA.driverId (Se.Eq driverId.getId), Se.Is BeamFDVA.fleetOwnerId (Se.Eq fleetOwnerId)]]

deleteByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m ()
deleteByDriverId driverId = do
  deleteWithKV [Se.Is BeamFDVA.driverId $ Se.Eq driverId.getId]

instance FromTType' BeamFDVA.FleetDriverAssociation FleetDriverAssociation where
  fromTType' BeamFDVA.FleetDriverAssociationT {..} = do
    pure $
      Just
        FleetDriverAssociation
          { id = Id id,
            driverId = Id driverId,
            ..
          }

instance ToTType' BeamFDVA.FleetDriverAssociation FleetDriverAssociation where
  toTType' FleetDriverAssociation {..} = do
    BeamFDVA.FleetDriverAssociationT
      { BeamFDVA.id = getId id,
        BeamFDVA.driverId = getId driverId,
        BeamFDVA.fleetOwnerId = fleetOwnerId,
        BeamFDVA.isActive = isActive,
        BeamFDVA.createdAt = createdAt,
        BeamFDVA.updatedAt = updatedAt
      }
