{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.TrackRoute where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.RouteStopMapping
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data TrackingResp = TrackingResp {vehicleTrackingInfo :: [API.Types.UI.TrackRoute.VehicleInfo]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleInfo = VehicleInfo {nextStop :: Domain.Types.RouteStopMapping.RouteStopMapping, vehicleId :: Kernel.Prelude.Text, vehicleInfo :: API.Types.UI.TrackRoute.VehicleInfoForRoute}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleInfoForRoute = VehicleInfoForRoute
  { latitude :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    longitude :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    scheduleRelationship :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    speed :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startDate :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    timestamp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tripId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
