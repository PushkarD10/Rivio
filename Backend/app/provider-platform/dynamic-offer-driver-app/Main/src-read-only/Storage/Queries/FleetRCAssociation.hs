{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetRCAssociation (module Storage.Queries.FleetRCAssociation, module ReExport) where

import qualified Domain.Types.FleetRCAssociation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetRCAssociation as Beam
import Storage.Queries.FleetRCAssociationExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.FleetRCAssociation.FleetRCAssociation -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.FleetRCAssociation.FleetRCAssociation] -> m ())
createMany = traverse_ create

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.FleetRCAssociation.FleetRCAssociation -> m (Maybe Domain.Types.FleetRCAssociation.FleetRCAssociation))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.FleetRCAssociation.FleetRCAssociation -> m (Maybe Domain.Types.FleetRCAssociation.FleetRCAssociation))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.FleetRCAssociation.FleetRCAssociation -> m ())
updateByPrimaryKey (Domain.Types.FleetRCAssociation.FleetRCAssociation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.associatedOn associatedOn,
      Se.Set Beam.associatedTill associatedTill,
      Se.Set Beam.fleetOwnerId (Kernel.Types.Id.getId fleetOwnerId),
      Se.Set Beam.rcId (Kernel.Types.Id.getId rcId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
