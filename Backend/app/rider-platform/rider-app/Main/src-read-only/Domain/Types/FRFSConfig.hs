{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSConfig = FRFSConfig
  { bookingEndTime :: Kernel.Prelude.UTCTime,
    bookingStartTime :: Kernel.Prelude.UTCTime,
    customDates :: [Kernel.Prelude.Text],
    customEndTime :: Kernel.Prelude.Text,
    discount :: Kernel.Prelude.Int,
    freeTicketInterval :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    isEventOngoing :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    maxFreeTicketCashback :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    metroStationTtl :: Kernel.Prelude.Int,
    oneWayTicketLimit :: Kernel.Prelude.Int,
    roundTripTicketLimit :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON)
