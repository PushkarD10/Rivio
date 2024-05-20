{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Booking where

import qualified BecknV2.OnDemand.Enums
import qualified Database.Beam as B
import qualified Domain.Types.Extra.Booking
import qualified Domain.Types.FarePolicy.FareProductType
import qualified Domain.Types.VehicleServiceTier
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Version
import qualified Kernel.Utils.Common
import Tools.Beam.UtilsTH

data BookingT f = BookingT
  { backendAppVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    backendConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    distance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    fareProductType :: B.C f Domain.Types.FarePolicy.FareProductType.FareProductType,
    otpCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    stopLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    bppBookingId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    itemId :: B.C f Kernel.Prelude.Text,
    clientBundleVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientOsType :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType),
    clientOsVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientSdkVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    discount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    distanceValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    estimatedDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    estimatedDistanceValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    estimatedDuration :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Utils.Common.Currency),
    estimatedFare :: B.C f Kernel.Types.Common.HighPrecMoney,
    estimatedTotalFare :: B.C f Kernel.Types.Common.HighPrecMoney,
    fromLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fulfillmentId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    isScheduled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    paymentMethodId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    paymentStatus :: B.C f (Kernel.Prelude.Maybe BecknV2.OnDemand.Enums.PaymentStatus),
    paymentUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    primaryExophone :: B.C f Kernel.Prelude.Text,
    providerId :: B.C f Kernel.Prelude.Text,
    providerUrl :: B.C f Kernel.Prelude.Text,
    quoteId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    riderId :: B.C f Kernel.Prelude.Text,
    serviceTierName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    serviceTierShortDesc :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    specialLocationTag :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    startTime :: B.C f Kernel.Prelude.UTCTime,
    status :: B.C f Domain.Types.Extra.Booking.BookingStatus,
    riderTransactionId :: B.C f Kernel.Prelude.Text,
    tripTermsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vehicleVariant :: B.C f Domain.Types.VehicleServiceTier.VehicleServiceTierType
  }
  deriving (Generic, B.Beamable)

instance B.Table BookingT where
  data PrimaryKey BookingT f = BookingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BookingId . id

type Booking = BookingT Identity

$(enableKVPG ''BookingT ['id] [['bppBookingId], ['quoteId], ['riderId], ['riderTransactionId]])

$(mkTableInstancesWithTModifier ''BookingT "booking" [("bppBookingId", "bpp_ride_booking_id"), ("riderTransactionId", "transaction_id")])
