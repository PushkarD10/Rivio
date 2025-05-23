{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RiderDetails where

import qualified Database.Beam as B
import qualified Domain.Types.RiderDetails
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data RiderDetailsT f = RiderDetailsT
  { cancellationDues :: B.C f Kernel.Types.Common.HighPrecMoney,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    disputeChancesUsed :: B.C f Kernel.Prelude.Int,
    firstRideId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    hasTakenValidRide :: B.C f Kernel.Prelude.Bool,
    hasTakenValidRideAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    id :: B.C f Kernel.Prelude.Text,
    isDeviceIdExists :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f Kernel.Prelude.Text,
    mobileCountryCode :: B.C f Kernel.Prelude.Text,
    mobileNumberEncrypted :: B.C f Kernel.Prelude.Text,
    mobileNumberHash :: B.C f Kernel.External.Encryption.DbHash,
    nightSafetyChecks :: B.C f Kernel.Prelude.Bool,
    otpCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    payoutFlagReason :: B.C f (Kernel.Prelude.Maybe Domain.Types.RiderDetails.PayoutFlagReason),
    referralCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    referredAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    referredByDriver :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RiderDetailsT where
  data PrimaryKey RiderDetailsT f = RiderDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RiderDetailsId . id

type RiderDetails = RiderDetailsT Identity

$(enableKVPG ''RiderDetailsT ['id] [['mobileNumberHash]])

$(mkTableInstances ''RiderDetailsT "rider_details")
