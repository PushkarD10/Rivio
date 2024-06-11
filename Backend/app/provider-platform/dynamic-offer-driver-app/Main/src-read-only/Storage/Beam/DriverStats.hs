{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverStats where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data DriverStatsT f = DriverStatsT
  { bonusEarned :: B.C f Kernel.Types.Common.Money,
    bonusEarnedAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    coinCovertedToCashLeft :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    driverId :: B.C f Kernel.Prelude.Text,
    earningsMissed :: B.C f Kernel.Types.Common.Money,
    earningsMissedAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    favRiderCount :: B.C f Kernel.Prelude.Int,
    favRiderList :: B.C f [Kernel.Prelude.Text],
    idleSince :: B.C f Kernel.Prelude.UTCTime,
    lateNightTrips :: B.C f Kernel.Prelude.Int,
    ridesCancelled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    totalCoinsConvertedCash :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    totalDistance :: B.C f Kernel.Prelude.Double,
    totalEarnings :: B.C f Kernel.Types.Common.Money,
    totalEarningsAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    totalRides :: B.C f Kernel.Prelude.Int,
    totalRidesAssigned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverStatsT where
  data PrimaryKey DriverStatsT f = DriverStatsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverStatsId . driverId

type DriverStats = DriverStatsT Identity

$(enableKVPG ''DriverStatsT ['driverId] [])

$(mkTableInstances ''DriverStatsT "driver_stats")
