{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PurchaseHistory (module Storage.Queries.PurchaseHistory, module ReExport) where

import qualified Domain.Types.PurchaseHistory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PurchaseHistory as Beam
import Storage.Queries.PurchaseHistoryExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.PurchaseHistory.PurchaseHistory -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.PurchaseHistory.PurchaseHistory] -> m ())
createMany = traverse_ create

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.PurchaseHistory.PurchaseHistory -> m (Maybe Domain.Types.PurchaseHistory.PurchaseHistory))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.PurchaseHistory.PurchaseHistory -> m ())
updateByPrimaryKey (Domain.Types.PurchaseHistory.PurchaseHistory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.cash cash,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.driverId driverId,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOptCityId merchantOptCityId,
      Se.Set Beam.numCoins numCoins,
      Se.Set Beam.title title,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
