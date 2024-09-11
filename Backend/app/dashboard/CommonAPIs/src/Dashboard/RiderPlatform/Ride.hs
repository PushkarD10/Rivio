{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.RiderPlatform.Ride
  ( module Dashboard.RiderPlatform.Ride,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import qualified Dashboard.Common as DP
import Dashboard.Common.Ride as Reexport
import Data.Aeson
import Domain.Types
import Kernel.External.Maps
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.Centesimal
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.Price
import Kernel.Utils.Common
import Kernel.Utils.JSON (constructorsWithLowerCase)
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Kernel.Utils.Validation
import Servant hiding (Summary)

---------------------------------------------------------
-- share ride info--------------------------------------

type ShareRideInfoAPI =
  Capture "rideId" (Id DP.Ride)
    :> "info"
    :> Get '[JSON] ShareRideInfoRes

type ShareRideInfoByShortIdAPI =
  Capture "rideShortId" (ShortId DP.Ride)
    :> "rideInfo"
    :> Get '[JSON] ShareRideInfoRes

type RideInfoAPI =
  "rideinfo"
    :> Capture "rideId" (Id DP.Ride)
    :> Get '[JSON] RideInfoRes

data ShareRideInfoRes = ShareRideInfoRes
  { id :: Id Ride,
    bookingId :: Id Booking,
    status :: RideStatus,
    driverName :: Text,
    driverNumber :: Maybe Text,
    driverRating :: Maybe Centesimal,
    vehicleNumber :: Text,
    vehicleModel :: Text,
    vehicleColor :: Text,
    trackingUrl :: Maybe BaseUrl,
    estimatedDistance :: Maybe HighPrecMeters,
    estimatedDistanceWithUnit :: Maybe Distance,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    userFirstName :: Maybe Text,
    userLastName :: Maybe Text,
    fromLocation :: Location,
    toLocation :: Maybe Location,
    sosStatus :: Maybe SosStatus,
    vehicleVariant :: VehicleVariant,
    nextStopLocation :: Maybe Location,
    rideScheduledAt :: UTCTime,
    fareProductType :: FareProductType, -- TODO :: For backward compatibility, please do not maintain this in future. `fareProductType` is replaced with `tripCategory`.
    tripCategory :: TripCategory,
    estimatedEndTimeRange :: Maybe (UTCTime, UTCTime),
    destinationReachedAt :: Maybe UTCTime
  }
  deriving (Generic, Show, ToSchema, FromJSON, ToJSON)

data RideInfoRes = RideInfoRes
  { rideId :: Id Ride,
    bookingId :: Id Booking,
    rideStatus :: RideStatus,
    customerName :: Maybe Text,
    customerPhoneNo :: Maybe Text,
    rideOtp :: Text,
    customerPickupLocation :: Location,
    customerDropLocation :: Maybe Location,
    driverName :: Text,
    driverPhoneNo :: Maybe Text,
    driverRegisteredAt :: Maybe UTCTime,
    vehicleNo :: Text,
    vehicleModel :: Text,
    vehicleVariant :: VehicleVariant,
    vehicleServiceTierName :: Maybe Text,
    rideBookingTime :: UTCTime,
    actualDriverArrivalTime :: Maybe UTCTime,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    rideDistanceEstimated :: Maybe HighPrecMeters,
    rideDistanceActual :: Maybe HighPrecMeters,
    chargeableDistance :: Maybe HighPrecMeters,
    rideDistanceEstimatedWithUnit :: Maybe Distance,
    rideDistanceActualWithUnit :: Maybe Distance,
    chargeableDistanceWithUnit :: Maybe Distance,
    estimatedFare :: Money,
    actualFare :: Maybe Money,
    estimatedFareWithCurrency :: PriceAPIEntity,
    actualFareWithCurrency :: Maybe PriceAPIEntity,
    estimatedRideDuration :: Maybe Seconds,
    rideDuration :: Maybe Seconds,
    cancelledTime :: Maybe UTCTime,
    cancelledBy :: Maybe CancellationSource,
    nextStopLocation :: Maybe Location,
    rideScheduledAt :: UTCTime,
    fareProductType :: FareProductType, -- TODO :: Deprecated, please do not maintain this in future. `fareProductType` is replaced with `tripCategory`.
    tripCategory :: TripCategory,
    endOtp :: Maybe Text,
    estimateFareBP :: Maybe [EstimateBreakup],
    merchantOperatingCityId :: Maybe Text,
    estimatedDistance :: Maybe HighPrecMeters,
    computedPrice :: Maybe HighPrecMoney,
    fareBreakup :: [FareBreakup],
    rideCreatedAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data CancellationSource
  = ByUser
  | ByDriver
  | ByMerchant
  | ByAllocator
  | ByApplication
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideStatus
  = UPCOMING_RIDE
  | NEW
  | INPROGRESS
  | COMPLETED
  | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data SosStatus
  = NotResolved
  | Pending
  | Resolved
  | MockPending
  | MockResolved
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data EstimateBreakup = EstimateBreakup
  { title :: Text,
    price :: EstimateBreakupPrice
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype EstimateBreakupPrice = EstimateBreakupPrice
  { value :: PriceAPIEntity
  }
  deriving (Generic, Show, ToSchema, ToJSON, FromJSON)

data FareBreakup = FareBreakup
  { amount :: Price,
    description :: Text,
    entityId :: Text,
    entityType :: FareBreakupEntityType
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data FareBreakupEntityType = BOOKING_UPDATE_REQUEST | BOOKING | RIDE | INITIAL_BOOKING
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data Location = Location
  { id :: Id Location,
    lat :: Double,
    lon :: Double,
    address :: LocationAddress,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, HasCoordinates, ToSchema, FromJSON, ToJSON)

data LocationAddress = LocationAddress
  { street :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToSchema, FromJSON, ToJSON)

---------------------------------------------------------
-- ride list --------------------------------------------

type RideListAPI =
  "list"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "bookingStatus" BookingStatus
    :> QueryParam "rideShortId" (ShortId Ride)
    :> QueryParam "customerPhoneNo" Text
    :> QueryParam "driverPhoneNo" Text
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> Get '[JSON] RideListRes

data RideListRes = RideListRes
  { totalItems :: Int, -- for backward compatibility
    summary :: Summary,
    rides :: [RideListItem]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideListItem = RideListItem
  { rideShortId :: ShortId Ride,
    rideCreatedAt :: UTCTime,
    rideId :: Id Ride,
    customerName :: Maybe Text,
    customerPhoneNo :: Maybe Text,
    driverName :: Text,
    driverPhoneNo :: Text,
    vehicleNo :: Text,
    bookingStatus :: BookingStatus,
    nextStopLocation :: Maybe Location,
    rideScheduledAt :: UTCTime,
    fareProductType :: FareProductType, -- TODO :: For backward compatibility, please do not maintain this in future. `fareProductType` is replaced with `tripCategory`.
    tripCategory :: TripCategory,
    endOtp :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BookingStatus = UPCOMING | UPCOMING_6HRS | ONGOING | ONGOING_6HRS | RCOMPLETED | RCANCELLED
  deriving stock (Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

derivePersistField "BookingStatus"

$(mkHttpInstancesForEnum ''BookingStatus)

---------------------------------------------------------
-- Trip Route--------------------------------------

type TripRouteAPI =
  "trip"
    :> "route"
    :> Capture "rideId" (Id DP.Ride)
    :> MandatoryQueryParam "lat" Double
    :> MandatoryQueryParam "lon" Double
    :> Get '[JSON] Maps.GetRoutesResp

type PickupRouteAPI =
  "pickup"
    :> "route"
    :> Capture "rideId" (Id DP.Ride)
    :> MandatoryQueryParam "lat" Double
    :> MandatoryQueryParam "lon" Double
    :> Get '[JSON] Maps.GetRoutesResp

---------------------------------------------------------
-- multiple ride sync -----------------------------

type MultipleRideSyncAPI =
  "sync"
    :> ReqBody '[JSON] MultipleRideSyncReq
    :> Post '[JSON] MultipleRideSyncResp

newtype MultipleRideSyncReq = MultipleRideSyncReq
  { rides :: [MultipleRideItem]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype MultipleRideItem = MultipleRideItem
  { rideId :: Id Ride
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets MultipleRideSyncReq where
  hideSecrets = identity

validateMultipleRideSyncReq :: Validate MultipleRideSyncReq
validateMultipleRideSyncReq MultipleRideSyncReq {..} = do
  validateField "rides" rides $ UniqueField @"rideId"

---------------------------------------------------------
-- Ticket Ride List--------------------------------------

type TicketRideListAPI =
  "kapture"
    :> "list"
    :> QueryParam "rideShortId" (ShortId Ride)
    :> QueryParam "countryCode" Text
    :> QueryParam "phoneNumber" Text
    :> QueryParam "supportPhoneNumber" Text
    :> Get '[JSON] TicketRideListRes

newtype TicketRideListRes = TicketRideListRes
  { rides :: [RideInfo]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance HideSecrets TicketRideListRes where
  hideSecrets = identity

data RideInfo = RideInfo
  { rideShortId :: ShortId Ride,
    customerName :: Maybe Text,
    customerPhoneNo :: Maybe Text,
    driverName :: Text,
    driverPhoneNo :: Maybe Text,
    vehicleNo :: Text,
    status :: BookingStatus,
    rideCreatedAt :: UTCTime,
    pickupLocationLat :: Maybe Double,
    pickupLocationLon :: Maybe Double,
    pickupLocationStreet :: Maybe Text,
    pickupLocationCity :: Maybe Text,
    pickupLocationState :: Maybe Text,
    pickupLocationCountry :: Maybe Text,
    pickupLocationBuilding :: Maybe Text,
    pickupLocationAreaCode :: Maybe Text,
    pickupLocationArea :: Maybe Text,
    dropLocationLat :: Maybe Double,
    dropLocationLon :: Maybe Double,
    dropLocationStreet :: Maybe Text,
    dropLocationCity :: Maybe Text,
    dropLocationState :: Maybe Text,
    dropLocationCountry :: Maybe Text,
    dropLocationBuilding :: Maybe Text,
    dropLocationAreaCode :: Maybe Text,
    dropLocationArea :: Maybe Text,
    fare :: Maybe Money,
    fareWithCurrency :: Maybe PriceAPIEntity,
    personId :: Id Customer,
    nextStopLocation :: Maybe Location,
    rideScheduledAt :: UTCTime,
    fareProductType :: FareProductType, -- TODO :: For backward compatibility, please do not maintain this in future. `fareProductType` is replaced with `tripCategory`.
    tripCategory :: TripCategory,
    endOtp :: Maybe Text,
    classification :: Ticket.Classification
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

instance FromJSON RideInfo where
  parseJSON = genericParseJSON constructorsWithLowerCase

instance ToJSON RideInfo where
  toJSON = genericToJSON constructorsWithLowerCase
