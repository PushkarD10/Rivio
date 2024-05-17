{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PurchaseHistory where

import qualified Domain.Types.PurchaseHistory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PurchaseHistory as Beam

instance FromTType' Beam.PurchaseHistory Domain.Types.PurchaseHistory.PurchaseHistory where
  fromTType' (Beam.PurchaseHistoryT {..}) = do
    pure $
      Just
        Domain.Types.PurchaseHistory.PurchaseHistory
          { cash = cash,
            createdAt = createdAt,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            driverId = driverId,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOptCityId = merchantOptCityId,
            numCoins = numCoins,
            title = title,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PurchaseHistory Domain.Types.PurchaseHistory.PurchaseHistory where
  toTType' (Domain.Types.PurchaseHistory.PurchaseHistory {..}) = do
    Beam.PurchaseHistoryT
      { Beam.cash = cash,
        Beam.createdAt = createdAt,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.driverId = driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOptCityId = merchantOptCityId,
        Beam.numCoins = numCoins,
        Beam.title = title,
        Beam.updatedAt = updatedAt
      }
