{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverStats where

import qualified Domain.Types.DriverStats
import qualified GHC.Float
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverStats as Beam
import Storage.Queries.Transformers.DriverStats

instance FromTType' Beam.DriverStats Domain.Types.DriverStats.DriverStats where
  fromTType' (Beam.DriverStatsT {..}) = do
    pure $
      Just
        Domain.Types.DriverStats.DriverStats
          { bonusEarned = Kernel.Types.Common.mkAmountWithDefault bonusEarnedAmount bonusEarned,
            coinCovertedToCashLeft = Kernel.Prelude.fromMaybe 0 coinCovertedToCashLeft,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverId = Kernel.Types.Id.Id driverId,
            earningsMissed = Kernel.Types.Common.mkAmountWithDefault earningsMissedAmount earningsMissed,
            favRiderCount = favRiderCount,
            favRiderList = favRiderList,
            idleSince = idleSince,
            lateNightTrips = lateNightTrips,
            ridesCancelled = ridesCancelled,
            totalCoinsConvertedCash = Kernel.Prelude.fromMaybe 0 totalCoinsConvertedCash,
            totalDistance = Kernel.Types.Common.Meters $ GHC.Float.double2Int totalDistance,
            totalEarnings = Kernel.Types.Common.mkAmountWithDefault totalEarningsAmount totalEarnings,
            totalRides = totalRides,
            totalRidesAssigned = totalRidesAssigned,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverStats Domain.Types.DriverStats.DriverStats where
  toTType' (Domain.Types.DriverStats.DriverStats {..}) = do
    Beam.DriverStatsT
      { Beam.bonusEarned = Kernel.Prelude.roundToIntegral bonusEarned,
        Beam.bonusEarnedAmount = Kernel.Prelude.Just bonusEarned,
        Beam.coinCovertedToCashLeft = Kernel.Prelude.Just coinCovertedToCashLeft,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.earningsMissed = Kernel.Prelude.roundToIntegral earningsMissed,
        Beam.earningsMissedAmount = Kernel.Prelude.Just earningsMissed,
        Beam.favRiderCount = favRiderCount,
        Beam.favRiderList = favRiderList,
        Beam.idleSince = idleSince,
        Beam.lateNightTrips = lateNightTrips,
        Beam.ridesCancelled = ridesCancelled,
        Beam.totalCoinsConvertedCash = Kernel.Prelude.Just totalCoinsConvertedCash,
        Beam.totalDistance = getTotalDistance totalDistance,
        Beam.totalEarnings = Kernel.Prelude.roundToIntegral totalEarnings,
        Beam.totalEarningsAmount = Kernel.Prelude.Just totalEarnings,
        Beam.totalRides = totalRides,
        Beam.totalRidesAssigned = totalRidesAssigned,
        Beam.updatedAt = updatedAt
      }
