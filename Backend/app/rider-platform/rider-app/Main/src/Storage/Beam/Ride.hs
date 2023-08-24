{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Ride where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Ride as Domain
import qualified Domain.Types.VehicleVariant as VehVar (VehicleVariant (..))
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize
import qualified Tools.Maps as Maps

instance FromField Domain.RideStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.RideStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.RideStatus

instance FromBackendRow Postgres Domain.RideStatus

instance IsString Domain.RideStatus where
  fromString = show

data RideT f = RideT
  { id :: B.C f Text,
    bppRideId :: B.C f Text,
    bookingId :: B.C f Text,
    shortId :: B.C f Text,
    merchantId :: B.C f (Maybe Text),
    status :: B.C f Domain.RideStatus,
    driverName :: B.C f Text,
    driverRating :: B.C f (Maybe Centesimal),
    driverMobileNumber :: B.C f Text,
    driverMobileCountryCode :: B.C f (Maybe Text),
    driverRegisteredAt :: B.C f Time.UTCTime,
    vehicleNumber :: B.C f Text,
    vehicleModel :: B.C f Text,
    vehicleColor :: B.C f Text,
    vehicleVariant :: B.C f VehVar.VehicleVariant,
    otp :: B.C f Text,
    trackingUrl :: B.C f (Maybe Text),
    fare :: B.C f (Maybe HighPrecMoney),
    totalFare :: B.C f (Maybe HighPrecMoney),
    chargeableDistance :: B.C f (Maybe HighPrecMeters),
    traveledDistance :: B.C f (Maybe HighPrecMeters),
    mapsServiceGetDistancesForCancelRide :: B.C f (Maybe (Maps.SMapsService 'Maps.GetDistancesForCancelRide)),
    mapsServiceGetTripRoutes :: B.C f (Maybe (Maps.SMapsService 'Maps.GetTripRoutes)),
    driverArrivalTime :: B.C f (Maybe Time.UTCTime),
    rideStartTime :: B.C f (Maybe Time.UTCTime),
    rideEndTime :: B.C f (Maybe Time.UTCTime),
    rideRating :: B.C f (Maybe Int),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RideT where
  data PrimaryKey RideT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Ride = RideT Identity

rideTMod :: RideT (B.FieldModification (B.TableField RideT))
rideTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      bppRideId = B.fieldNamed "bpp_ride_id",
      bookingId = B.fieldNamed "booking_id",
      shortId = B.fieldNamed "short_id",
      merchantId = B.fieldNamed "merchant_id",
      status = B.fieldNamed "status",
      driverName = B.fieldNamed "driver_name",
      driverRating = B.fieldNamed "driver_rating",
      driverMobileNumber = B.fieldNamed "driver_mobile_number",
      driverRegisteredAt = B.fieldNamed "driver_registered_at",
      vehicleNumber = B.fieldNamed "vehicle_number",
      vehicleModel = B.fieldNamed "vehicle_model",
      vehicleColor = B.fieldNamed "vehicle_color",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      otp = B.fieldNamed "otp",
      trackingUrl = B.fieldNamed "tracking_url",
      fare = B.fieldNamed "fare",
      totalFare = B.fieldNamed "total_fare",
      chargeableDistance = B.fieldNamed "chargeable_distance",
      traveledDistance = B.fieldNamed "traveled_distance",
      mapsServiceGetDistancesForCancelRide = B.fieldNamed "maps_service_get_distances_for_cancel_ride",
      mapsServiceGetTripRoutes = B.fieldNamed "maps_service_get_trip_routes",
      driverArrivalTime = B.fieldNamed "driver_arrival_time",
      rideStartTime = B.fieldNamed "ride_start_time",
      rideEndTime = B.fieldNamed "ride_end_time",
      rideRating = B.fieldNamed "ride_rating",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at",
      driverMobileCountryCode = B.fieldNamed "driver_mobile_country_code"
    }

$(enableKVPG ''RideT ['id] [['bppRideId], ['bookingId]])

$(mkTableInstances ''RideT "ride" "atlas_app")
