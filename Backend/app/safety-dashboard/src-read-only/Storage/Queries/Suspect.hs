{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Suspect (module Storage.Queries.Suspect, module ReExport) where

import qualified Domain.Types.Suspect
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Suspect as Beam
import Storage.Queries.SuspectExtra as ReExport
import Storage.Queries.Transformers.Suspect

create :: KvDbFlow m r => (Domain.Types.Suspect.Suspect -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.Suspect.Suspect] -> m ())
createMany = traverse_ create

findByDl :: KvDbFlow m r => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.Suspect.Suspect))
findByDl dl = do findOneWithKV [Se.Is Beam.dl $ Se.Eq dl]

findByVoterId :: KvDbFlow m r => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.Suspect.Suspect))
findByVoterId voterId = do findOneWithKV [Se.Is Beam.voterId $ Se.Eq voterId]

updateFlaggedCounterByDl :: KvDbFlow m r => (Kernel.Prelude.Int -> Domain.Types.Suspect.FlaggedStatus -> [Domain.Types.Suspect.FlaggedBy] -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ())
updateFlaggedCounterByDl flaggedCounter flaggedStatus flaggedBy dl = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.flaggedCounter flaggedCounter,
      Se.Set Beam.flaggedStatus flaggedStatus,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.flaggedBy (convertFlaggedByToTable flaggedBy)
    ]
    [Se.Is Beam.dl $ Se.Eq dl]

updateFlaggedCounterByVoterId :: KvDbFlow m r => (Kernel.Prelude.Int -> Domain.Types.Suspect.FlaggedStatus -> [Domain.Types.Suspect.FlaggedBy] -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ())
updateFlaggedCounterByVoterId flaggedCounter flaggedStatus flaggedBy voterId = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.flaggedCounter flaggedCounter,
      Se.Set Beam.flaggedStatus flaggedStatus,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.flaggedBy (convertFlaggedByToTable flaggedBy)
    ]
    [Se.Is Beam.voterId $ Se.Eq voterId]

updateFlaggedStatusById :: KvDbFlow m r => (Domain.Types.Suspect.FlaggedStatus -> Kernel.Types.Id.Id Domain.Types.Suspect.Suspect -> m ())
updateFlaggedStatusById flaggedStatus (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.flaggedStatus flaggedStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Suspect.Suspect -> m (Maybe Domain.Types.Suspect.Suspect))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.Suspect.Suspect -> m ())
updateByPrimaryKey (Domain.Types.Suspect.Suspect {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.dl dl,
      Se.Set Beam.firstName firstName,
      Se.Set Beam.flagUpdatedAt flagUpdatedAt,
      Se.Set Beam.flaggedBy (convertFlaggedByToTable flaggedBy),
      Se.Set Beam.flaggedCounter flaggedCounter,
      Se.Set Beam.flaggedStatus flaggedStatus,
      Se.Set Beam.lastName lastName,
      Se.Set Beam.statusChangedReason statusChangedReason,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.voterId voterId
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
