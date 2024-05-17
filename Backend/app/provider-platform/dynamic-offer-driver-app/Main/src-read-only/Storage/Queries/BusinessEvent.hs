{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BusinessEvent (module Storage.Queries.BusinessEvent, module ReExport) where

import qualified Domain.Types.BusinessEvent
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BusinessEvent as Beam
import Storage.Queries.BusinessEventExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.BusinessEvent.BusinessEvent -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.BusinessEvent.BusinessEvent] -> m ())
createMany = traverse_ create

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.BusinessEvent.BusinessEvent -> m (Maybe Domain.Types.BusinessEvent.BusinessEvent))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.BusinessEvent.BusinessEvent -> m ())
updateByPrimaryKey (Domain.Types.BusinessEvent.BusinessEvent {..}) = do
  updateWithKV
    [ Se.Set Beam.bookingId (Kernel.Types.Id.getId <$> bookingId),
      Se.Set Beam.distance (Kernel.Types.Common.getMeters <$> distance),
      Se.Set Beam.driverId (Kernel.Types.Id.getId <$> driverId),
      Se.Set Beam.duration (Kernel.Types.Common.getSeconds <$> duration),
      Se.Set Beam.eventType eventType,
      Se.Set Beam.rideId (Kernel.Types.Id.getId <$> rideId),
      Se.Set Beam.timeStamp timeStamp,
      Se.Set Beam.vehicleVariant vehicleVariant,
      Se.Set Beam.whenPoolWasComputed whenPoolWasComputed
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
