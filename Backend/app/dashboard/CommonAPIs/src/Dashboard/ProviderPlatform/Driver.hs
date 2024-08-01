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

module Dashboard.ProviderPlatform.Driver
  ( module Dashboard.ProviderPlatform.Driver,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import qualified Dashboard.ProviderPlatform.Driver.Registration as Registration
import Data.Aeson
import Kernel.External.Maps.Types
import Kernel.External.Notification.FCM.Types (FCMRecipientToken)
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.City as City
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common (Centesimal, HighPrecMoney, MandatoryQueryParam, Money, PriceAPIEntity)
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.Version
import Kernel.Utils.GenericPretty
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Kernel.Utils.Validation
import Servant hiding (Summary, throwError)
import qualified Text.Show

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data DriverEndpoint
  = EnableDriverEndpoint
  | DisableDriverEndpoint
  | BlockDriverWithReasonEndpoint
  | BlockDriverEndpoint
  | UnblockDriverEndpoint
  | DeleteDriverEndpoint
  | UnlinkVehicleEndpoint
  | UnlinkDLEndpoint
  | UnlinkAadhaarEndpoint
  | EndRCAssociationEndpoint
  | UpdatePhoneNumberEndpoint
  | AddVehicleEndpoint
  | UpdateDriverNameEndpoint
  | CollectCashEndpoint
  | ExemptCashEndpoint
  | SetRCStatusEndpoint
  | DeleteRCEndpoint
  | UpdateDriverHomeLocationEndpoint
  | IncrementDriverGoToCountEndPoint
  | UpdateSubscriptionDriverFeeAndInvoiceEndpoint
  | SetVehicleDriverRcStatusForFleetEndpoint
  | FleetUnlinkVehicleEndpoint
  | SendMessageToDriverViaDashboardEndPoint
  | SendDummyRideRequestToDriverViaDashboardEndPoint
  | ChangeOperatingCityEndpoint
  | PauseOrResumeServiceChargesEndPoint
  | UpdateRCInvalidStatusEndPoint
  | UpdateVehicleVariantEndPoint
  | BulkReviewRCVariantEndPoint
  | RemoveACUsageRestrictionEndpoint
  | UpdateDriverTagEndPoint
  | UpdateFleetOwnerEndPoint
  | SendFleetJoiningOtpEndPoint
  | PostDriverClearFeeEndpoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord, ToSchema)

derivePersistField "DriverEndpoint"

newtype DriverIds = EnableDriversRequest
  { driverIds :: [Id Driver]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- driver list ------------------------------------------

type DriverListAPI =
  "list"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "verified" Bool
    :> QueryParam "enabled" Bool
    :> QueryParam "blocked" Bool
    :> QueryParam "subscribed" Bool
    :> QueryParam "phone" Text
    :> QueryParam "vehicleNumberSearchString" Text
    :> Get '[JSON] DriverListRes

data DriverListRes = DriverListRes
  { totalItems :: Int, -- for backward compatibility
    summary :: Summary,
    drivers :: [DriverListItem]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

----------------------------------------- go to home-----------------------------------------

data DriverHomeLocationAPIEntity = DriverHomeLocationAPIEntity
  { id :: Id DriverHomeLocation,
    lat :: Double,
    lon :: Double,
    address :: Text,
    tag :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets DriverHomeLocationAPIEntity where
  hideSecrets = identity

type GetHomeLocationsRes = [DriverHomeLocationAPIEntity]

type UpdateDriverHomeLocationReq = DriverHomeLocationAPIEntity

data CachedGoHomeRequestInfoRes = CachedGoHomeRequestInfoRes
  { status :: Maybe String,
    cnt :: Int,
    validTill :: Maybe UTCTime,
    driverGoHomeRequestId :: Maybe (Id DriverGoHomeRequest),
    isOnRide :: Bool,
    goHomeReferenceTime :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets CachedGoHomeRequestInfoRes where
  hideSecrets = identity

------------------------------------------------------------------------

data DriverListItem = DriverListItem
  { driverId :: Id Driver,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    vehicleNo :: Maybe Text,
    phoneNo :: Maybe Text,
    enabled :: Bool,
    blocked :: Bool,
    subscribed :: Bool,
    verified :: Bool,
    onRide :: Bool,
    active :: Bool,
    onboardingDate :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LicDetails = LicDetails
  { licExpiry :: UTCTime,
    vehicleClass :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RCDetails = RCDetails
  { vehicleClass :: Text,
    fitnessExpiry :: UTCTime,
    insuranceExpiry :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- documents info ---------------------------------------

type DriverDocumentsInfoAPI =
  "documents"
    :> "info"
    :> Get '[JSON] DriverDocumentsInfoRes

data DriverDocumentsInfoRes = DriverDocumentsInfoRes
  { registered :: !Int,
    verified :: !Int,
    enabled :: !Int,
    blocked :: !Int,
    subscribed :: !Int,
    validDocuments :: !DocumentsByStateInfo,
    invalidDocuments :: !DocumentsByStateInfo,
    verificationPending :: !DocumentsByStateInfo,
    verificationFailed :: !DocumentsByStateInfo,
    verificationLimitExceeded :: !DocumentsByStateInfo,
    docsExpiringInMonth :: !DocumentsByStateInfo,
    onboardingDate :: !(Maybe UTCTime)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentsByStateInfo = DocumentsByStateInfo
  { driverLicense :: !Int,
    vehicleRegistrationCertificate :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

emptyDocumentsByStateInfo :: DocumentsByStateInfo
emptyDocumentsByStateInfo = DocumentsByStateInfo 0 0

emptyInfo :: DriverDocumentsInfoRes
emptyInfo =
  DriverDocumentsInfoRes
    { registered = 0,
      verified = 0,
      enabled = 0,
      blocked = 0,
      subscribed = 0,
      validDocuments = emptyDocumentsByStateInfo,
      invalidDocuments = emptyDocumentsByStateInfo,
      verificationPending = emptyDocumentsByStateInfo,
      verificationFailed = emptyDocumentsByStateInfo,
      verificationLimitExceeded = emptyDocumentsByStateInfo,
      docsExpiringInMonth = emptyDocumentsByStateInfo,
      onboardingDate = Nothing
    }

---------------------------------------------------------
-- driver aadhaar Info api ----------------------------------------

type DriverAadhaarInfoAPI =
  Capture "driverId" (Id Driver)
    :> "aadhaarInfo"
    :> Get '[JSON] DriverAadhaarInfoRes

data DriverAadhaarInfoRes = DriverAadhaarInfoRes
  { driverName :: Text,
    driverGender :: Text,
    driverDob :: Text,
    driverImage :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- driver aadhaar Info api by mobile Number ----------------------------------------

type DriverAadhaarInfoByPhoneAPI =
  Capture "mobileNo" Text
    :> "aadhaarInfobyMobileNumber"
    :> Get '[JSON] DriverAadhaarInfoByPhoneReq

type DriverAadhaarInfoByPhoneReq = DriverAadhaarInfoRes

---------------------------------------------------------

---------------------------------------------------------
-- driver outstanding balance api ----------------------------------------

data DriverOutstandingBalanceResp = DriverOutstandingBalanceResp
  { driverFeeId :: Id DriverOutstandingBalanceResp,
    driverId :: Id Driver,
    govtCharges :: Money,
    govtChargesWithCurrency :: PriceAPIEntity,
    platformFee :: PlatformFee,
    numRides :: Int,
    payBy :: UTCTime,
    totalFee :: Money,
    totalEarnings :: Money,
    totalFeeWithCurrency :: PriceAPIEntity,
    totalEarningsWithCurrency :: PriceAPIEntity,
    startTime :: UTCTime,
    endTime :: UTCTime,
    status :: DriverFeeStatus
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

data PlatformFee = PlatformFee
  { fee :: HighPrecMoney,
    cgst :: HighPrecMoney,
    sgst :: HighPrecMoney,
    feeWithCurrency :: PriceAPIEntity,
    cgstWithCurrency :: PriceAPIEntity,
    sgstWithCurrency :: PriceAPIEntity
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

data DriverFeeStatus = ONGOING | PAYMENT_PENDING | PAYMENT_OVERDUE | CLEARED | EXEMPTED | COLLECTED_CASH | INACTIVE | CLEARED_BY_YATRI_COINS | MANUAL_REVIEW_NEEDED deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema)

instance CH.ClickhouseValue DriverFeeStatus

type DriverOutstandingBalanceAPI =
  "paymentDue"
    :> QueryParam "countryCode" Text
    :> MandatoryQueryParam "phone" Text
    :> Get '[JSON] [DriverOutstandingBalanceResp]

---------------------------------------------------------
-- driver cash collection api ----------------------------------------

type DriverCashCollectionAPIV2 =
  Capture "driverId" (Id Driver)
    :> "v2"
    :> "collectCash"
    :> Capture "serviceName" ServiceNames
    :> Post '[JSON] APISuccess

type DriverCashCollectionAPI =
  Capture "driverId" (Id Driver)
    :> "collectCash"
    :> Post '[JSON] APISuccess

-------------------------------------

-- driver cash exemption api ----------------------------------------

type DriverCashExemptionAPIV2 =
  Capture "driverId" (Id Driver)
    :> "v2"
    :> "exemptCash"
    :> Capture "serviceName" ServiceNames
    :> Post '[JSON] APISuccess

type DriverCashExemptionAPI =
  Capture "driverId" (Id Driver)
    :> "exemptCash"
    :> Post '[JSON] APISuccess

-------------------------------------

---------------------------------------------------------
-- driver activity --------------------------------------

type DriverActivityAPI =
  "activity"
    :> Get '[JSON] DriverActivityRes

data DriverActivityRes = DriverActivityRes
  { activeDriversInApp :: !Int,
    --    activeDriversInLastHour :: !Int,
    inactiveDrivers :: !Int
    --    inactiveDriversSinceTwoDays :: !Int,
    --    trendFrequency :: !Seconds,
    --    trend :: ![ActivityTrendItem]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

{-
data ActivityTrendItem = ActivityTrendItem
  { timestamp :: UTCTime,
    active :: Int,
    inactive :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
oneHour :: Seconds
oneHour = 3600
-}

mkDriverActivityRes :: (Int, Int) -> DriverActivityRes
mkDriverActivityRes (active, inactive) =
  DriverActivityRes
    { activeDriversInApp = active,
      --      activeDriversInLastHour = 0,
      inactiveDrivers = inactive
      --      inactiveDriversSinceTwoDays = 0,
      --      trendFrequency = oneHour,
      --      trend = []
    }

---------------------------------------------------------
-- enable driver ----------------------------------------

type EnableDriverAPI =
  Capture "driverId" (Id Driver)
    :> "enable"
    :> Post '[JSON] APISuccess

---------------------------------------------------------
-- disable driver ---------------------------------------

type DisableDriverAPI =
  Capture "driverId" (Id Driver)
    :> "disable"
    :> Post '[JSON] APISuccess

---------------------------------------------------------
-- remove driver ac restriction ----------------------------------------

type UpdateACUsageRestrictionAPI =
  Capture "driverId" (Id Driver)
    :> "acRestriction"
    :> "update"
    :> ReqBody '[JSON] UpdateACUsageRestrictionReq
    :> Post '[JSON] APISuccess

newtype UpdateACUsageRestrictionReq = UpdateACUsageRestrictionReq
  { isWorking :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets UpdateACUsageRestrictionReq where
  hideSecrets = identity

---------------------------------------------------------
-- block driver with reason ----------------------------------------

type BlockDriverWithReasonAPI =
  Capture "driverId" (Id Driver)
    :> "blockWithReason"
    :> ReqBody '[JSON] BlockDriverWithReasonReq
    :> Post '[JSON] APISuccess

data BlockDriverWithReasonReq = BlockDriverWithReasonReq
  { reasonCode :: Text,
    blockReason :: Maybe Text,
    blockTimeInHours :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

----------------------------------------------------------
-- block driver ----------------------------------------

type BlockDriverAPI =
  Capture "driverId" (Id Driver)
    :> "block"
    :> Post '[JSON] APISuccess

----------------------------------------------------------
-- block driver reason list ----------------------------------------

type DriverBlockReasonListAPI =
  "blockReasonList"
    :> Get '[JSON] [BlockReason]

data BlockReason = BlockReason
  { reasonCode :: Id BlockReason,
    blockReason :: Maybe Text,
    blockTimeInHours :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- unblock driver ---------------------------------------

type UnblockDriverAPI =
  Capture "driverId" (Id Driver)
    :> "unblock"
    :> Post '[JSON] APISuccess

---------------------------------------------------------
-- driver location --------------------------------------

type DriverLocationAPI =
  "location"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> ReqBody '[JSON] DriverIds
    :> Get '[JSON] DriverLocationRes

data DriverLocationRes = DriverLocationRes
  { driverLocationsNotFound :: Maybe (NonEmpty (Id Driver)),
    driverLocations :: [DriverLocationItem]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverLocationItem = DriverLocationItem
  { driverId :: Id Driver,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    vehicleNo :: Text,
    phoneNo :: Text,
    active :: Bool,
    onRide :: Bool,
    location :: LatLong,
    lastLocationTimestamp :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- driver info ------------------------------------------

type DriverInfoAPI =
  "info"
    :> QueryParam "mobileNumber" Text
    :> QueryParam "mobileCountryCode" Text
    :> QueryParam "vehicleNumber" Text
    :> QueryParam "dlNumber" Text
    :> QueryParam "rcNumber" Text
    :> QueryParam "email" Text
    :> Get '[JSON] DriverInfoRes

data DriverInfoRes = DriverInfoRes
  { driverId :: Id Driver,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    numberOfRides :: Int,
    mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text,
    enabled :: Bool,
    blocked :: Bool,
    blockedReason :: Maybe Text,
    verified :: Bool,
    subscribed :: Bool,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchback :: Bool,
    canDowngradeToTaxi :: Bool,
    canSwitchToRental :: Bool,
    canSwitchToInterCity :: Bool,
    vehicleNumber :: Maybe Text,
    selectedServiceTiers :: [Text],
    driverLicenseDetails :: Maybe DriverLicenseAPIEntity,
    vehicleRegistrationDetails :: [DriverRCAssociationAPIEntity],
    onboardingDate :: Maybe UTCTime,
    bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version,
    alternateNumber :: Maybe Text,
    rating :: Maybe Centesimal,
    availableMerchants :: [Text],
    merchantOperatingCity :: Maybe Context.City,
    currentAcOffReportCount :: Int,
    totalAcRestrictionUnblockCount :: Int,
    lastACStatusCheckedAt :: Maybe UTCTime,
    currentACStatus :: Bool,
    blockedDueToRiderComplains :: Bool,
    blockStateModifier :: Maybe Text,
    driverTag :: Maybe [Text],
    email :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverLicense

data VehicleRegistrationCertificate

data DriverLicenseAPIEntity = DriverLicenseAPIEntity
  { driverLicenseId :: Id DriverLicense,
    documentImageId1 :: Id Image,
    documentImageId2 :: Maybe (Id Image),
    driverDob :: Maybe UTCTime,
    driverName :: Maybe Text,
    licenseNumber :: Text,
    licenseExpiry :: UTCTime,
    classOfVehicles :: [Text],
    failedRules :: [Text],
    verificationStatus :: VerificationStatus,
    consent :: Bool,
    consentTimestamp :: UTCTime
    -- createdAt :: UTCTime, -- do we need it?
    -- updatedAt UTCTime,
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverRCAssociationAPIEntity = DriverRCAssociationAPIEntity
  { associatedOn :: UTCTime,
    associatedTill :: Maybe UTCTime,
    isRcActive :: Bool,
    details :: VehicleRegistrationCertificateAPIEntity
    -- consent :: Bool, -- do we need it?
    -- consentTimestamp :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleRegistrationCertificateAPIEntity = VehicleRegistrationCertificateAPIEntity
  { registrationCertificateId :: Id VehicleRegistrationCertificate,
    documentImageId :: Id Image,
    certificateNumber :: Text,
    fitnessExpiry :: UTCTime,
    permitExpiry :: Maybe UTCTime,
    pucExpiry :: Maybe UTCTime,
    insuranceValidity :: Maybe UTCTime,
    vehicleClass :: Maybe Text,
    failedRules :: [Text],
    vehicleManufacturer :: Maybe Text,
    vehicleCapacity :: Maybe Int,
    vehicleModel :: Maybe Text,
    vehicleColor :: Maybe Text,
    vehicleEnergyType :: Maybe Text,
    reviewRequired :: Maybe Bool,
    reviewedAt :: Maybe UTCTime,
    manufacturerModel :: Maybe Text,
    verificationStatus :: VerificationStatus,
    fleetOwnerId :: Maybe Text,
    vehicleVariant :: Maybe Variant
    -- createdAt :: UTCTime, -- do we need it?
    -- updatedAt UTCTime,
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RCStatusReq = RCStatusReq
  { rcNo :: Text,
    isActivate :: Bool,
    serviceName :: Maybe ServiceNames,
    planToAssociate :: Maybe Text
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

instance HideSecrets RCStatusReq where
  hideSecrets = identity

newtype DeleteRCReq = DeleteRCReq
  { rcNo :: Text
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

instance HideSecrets DeleteRCReq where
  hideSecrets = identity

---------------------------------------------------------
-- delete driver ----------------------------------------

type DeleteDriverAPI =
  Capture "driverId" (Id Driver)
    :> "permanentlyDelete"
    :> Delete '[JSON] APISuccess

---------------------------------------------------------
-- unlink vehicle ---------------------------------------

type UnlinkVehicleAPI =
  Capture "driverId" (Id Driver)
    :> "unlinkVehicle"
    :> Post '[JSON] APISuccess

---------------------------------------------------------
-- unlink dl ---------------------------------------

type UnlinkDLAPI =
  Capture "driverId" (Id Driver)
    :> "unlinkDL"
    :> Post '[JSON] APISuccess

---------------------------------------------------------
-- unlink Aadhaar -----------------------------------------

type UnlinkAadhaarAPI =
  Capture "driverId" (Id Driver)
    :> "unlinkAadhaar"
    :> Post '[JSON] APISuccess

---------------------------------------------------------
-- end rc association -----------------------------------

type EndRCAssociationAPI =
  Capture "driverId" (Id Driver)
    :> "endRCAssociation"
    :> Post '[JSON] APISuccess

---------------------------------------------------------
-- set rc status -----------------------------------

type SetRCStatusAPI =
  Capture "driverId" (Id Driver)
    :> "setRCStatus"
    :> ReqBody '[JSON] RCStatusReq
    :> Post '[JSON] APISuccess

---------------------------------------------------------
-- delete rc -----------------------------------

type DeleteRCAPI =
  Capture "driverId" (Id Driver)
    :> "deleteRC"
    :> ReqBody '[JSON] DeleteRCReq
    :> Post '[JSON] APISuccess

---------------------------------------------------------
-- update phone number ----------------------------------

type UpdatePhoneNumberAPI =
  Capture "driverId" (Id Driver)
    :> "updatePhoneNumber"
    :> ReqBody '[JSON] UpdatePhoneNumberReq
    :> Post '[JSON] APISuccess

data UpdatePhoneNumberReq = UpdatePhoneNumberReq
  { newPhoneNumber :: Text,
    newCountryCode :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

validateUpdatePhoneNumberReq :: Validate UpdatePhoneNumberReq
validateUpdatePhoneNumberReq UpdatePhoneNumberReq {..} =
  sequenceA_
    [ validateField "newPhoneNumber" newPhoneNumber P.mobileNumber,
      validateField "newCountryCode" newCountryCode P.mobileCountryCode
    ]

instance HideSecrets UpdatePhoneNumberReq where
  hideSecrets = identity

-- update driver aadhaar  api ----------------------------------------
type UpdateDriverAadhaarAPI =
  Capture "mobileNo" Text
    :> "updateByPhoneNumber"
    :> ReqBody '[JSON] UpdateDriverDataReq
    :> Post '[JSON] APISuccess

data UpdateDriverDataReq = UpdateDriverDataReq
  { driverName :: Text,
    driverGender :: Text,
    driverDob :: Text,
    driverAadhaarNumber :: Text,
    isVerified :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- add vehicle ------------------------------------------

type AddVehicleAPI =
  Capture "driverId" (Id Driver)
    :> "addVehicle"
    :> ReqBody '[JSON] AddVehicleReq
    :> Post '[JSON] APISuccess

data AddVehicleReq = AddVehicleReq
  { registrationNo :: Text,
    vehicleClass :: Text,
    capacity :: Maybe Int,
    colour :: Text,
    energyType :: Maybe Text,
    model :: Text,
    make :: Text,
    airConditioned :: Maybe Bool,
    driverName :: Text,
    imageId :: Maybe (Id Image),
    vehicleCategory :: Maybe Category,
    oxygen :: Maybe Bool,
    ventilator :: Maybe Bool,
    dateOfRegistration :: Maybe UTCTime,
    vehicleModelYear :: Maybe Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

validateAddVehicleReq :: Validate AddVehicleReq
validateAddVehicleReq AddVehicleReq {..} =
  sequenceA_
    [ validateField "color" colour $ NotEmpty `And` P.name,
      validateField "registrationNo" registrationNo $
        LengthInRange 1 11 `And` star (P.latinUC \/ P.digit)
    ]

instance HideSecrets AddVehicleReq where
  hideSecrets = identity

---------------------------------------------------------

-- add vehicle for fleet ------------------------------------------

type AddVehicleForFleetAPI =
  Capture "mobileNo" Text
    :> QueryParam "countryCode" Text
    :> "fleet"
    :> "addVehicle"
    :> ReqBody '[JSON] AddVehicleReq
    :> Post '[JSON] APISuccess

---------------------------------------------------------

-- add vehicle for fleet withoutDriver ------------------------------------------

type RegisterRCForFleetWithoutDriverAPI =
  "fleet"
    :> "addRC"
    :> "withoutDriver"
    :> ReqBody '[JSON] Registration.RegisterRCReq
    :> Post '[JSON] APISuccess

---------------------------------------------------------

-- get vehicle for fleet  ------------------------------------------

type GetAllVehicleForFleetAPI =
  "fleet"
    :> "getAllVehicle"
    :> QueryParam "mblimit" Int
    :> QueryParam "mboffset" Int
    :> Get '[JSON] ListVehicleRes

newtype ListVehicleRes = ListVehicleRes
  {vehicles :: [VehicleAPIEntity]}
  deriving (Generic, ToJSON, ToSchema, FromJSON)

data VehicleAPIEntity = VehicleAPIEntity
  { variant :: Maybe Reexport.Variant,
    model :: Maybe Text,
    color :: Maybe Text,
    registrationNo :: Text
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)

-- get All driver for fleet  ------------------------------------------

type GetAllDriverForFleetAPI =
  "fleet"
    :> "getAllDriver"
    :> QueryParam "mblimit" Int
    :> QueryParam "mboffset" Int
    :> Get '[JSON] FleetListDriverRes

newtype FleetListDriverRes = FleetListDriverRes
  {fleetDriversInfos :: [FleetDriversAPIEntity]}
  deriving (Generic, ToJSON, ToSchema, FromJSON, Show)

data FleetDriversAPIEntity = FleetDriversAPIEntity
  { driverId :: Id Driver,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON, Show)

-------------------------------------------------------

-- unlink vehicle ---------------------------------------

type FleetUnlinkVehicleAPI =
  Capture "driverId" (Id Driver)
    :> Capture "vehicleNo" Text
    :> "fleet"
    :> "unlink"
    :> Post '[JSON] APISuccess

---------------------------------------------------------
-- remove fleet vehicle ---------------------------------------

type FleetRemoveVehicleAPI =
  Capture "vehicleNo" Text
    :> "fleet"
    :> "remove"
    :> "vehicle"
    :> Post '[JSON] APISuccess

-- remove fleet vehicle ---------------------------------------

type FleetRemoveDriverAPI =
  Capture "driverId" (Id Driver)
    :> "fleet"
    :> "remove"
    :> "driver"
    :> Post '[JSON] APISuccess

---------------------------------------------------------
data DriverMode
  = ONLINE
  | OFFLINE
  | SILENT
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
  deriving (PrettyShow) via Showable DriverMode

type FleetTotalEarningAPI =
  "fleet"
    :> "totalEarning"
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> Get '[JSON] FleetTotalEarningResponse

type FleetVehicleEarningAPI =
  "fleet"
    :> "vehicleEarning"
    :> QueryParam "vehicleNo" Text
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> Get '[JSON] FleetEarningListRes

data FleetEarningRes = FleetEarningRes
  { totalRides :: Int,
    totalEarning :: Int,
    vehicleNo :: Maybe Text,
    driverId :: Maybe (Id Driver),
    driverName :: Maybe Text,
    status :: Maybe DriverMode,
    vehicleType :: Maybe Variant,
    totalDuration :: TotalDuration,
    distanceTravelled :: Double,
    driverPhoneNo :: Maybe Text,
    cancelledRides :: Int
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)

data TotalDuration = TotalDuration
  { hours :: Int,
    minutes :: Int
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)

data FleetTotalEarningResponse = FleetTotalEarningResponse
  { totalRides :: Int,
    totalEarning :: Int,
    totalVehicle :: Int,
    conversionRate :: Double,
    cancellationRate :: Double,
    cancelledRides :: Int,
    totalDistanceTravelled :: Double
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)

data FleetEarningListRes = FleetEarningListRes
  { fleetEarningRes :: [FleetEarningRes],
    summary :: Summary
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)

type FleetDriverEarningAPI =
  "fleet"
    :> "driverEarning"
    :> QueryParam "mobileCountryCode" Text
    :> QueryParam "mobileNo" Text
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> Get '[JSON] FleetEarningListRes

---------------------------------------------------------
-- update driver name -----------------------------------

type UpdateDriverNameAPI =
  Capture "driverId" (Id Driver)
    :> "updateName"
    :> ReqBody '[JSON] UpdateDriverNameReq
    :> Post '[JSON] APISuccess

data UpdateDriverNameReq = UpdateDriverNameReq
  { firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

validateUpdateDriverNameReq :: Validate UpdateDriverNameReq
validateUpdateDriverNameReq UpdateDriverNameReq {..} =
  sequenceA_
    [ validateField "firstName" firstName $ MinLength 3 `And` P.name,
      validateField "middleName" middleName $ InMaybe P.name,
      validateField "lastName" lastName $ InMaybe P.name
    ]

instance HideSecrets UpdateDriverNameReq where
  hideSecrets = identity

---------------------------------------------------------
-- Get Route driver ids ---------------------------------------

type ClearOnRideStuckDriversAPI =
  "clearStuck"
    :> "onRide"
    :> QueryParam "dbSyncTime" Int
    :> Get '[JSON] ClearOnRideStuckDriversRes

newtype ClearOnRideStuckDriversRes = ClearOnRideStuckDriversRes
  { driverIds :: [Id Driver]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

----------------- Go To Home ----------------------------------
type GetDriverHomeLocationAPI =
  Capture "driverId" (Id Driver)
    :> "getHomeLocation"
    :> Get '[JSON] GetHomeLocationsRes

type UpdateDriverHomeLocationAPI =
  Capture "driverId" (Id Driver)
    :> "updateHomeLocation"
    :> ReqBody '[JSON] UpdateDriverHomeLocationReq
    :> Post '[JSON] APISuccess

type IncrementDriverGoToCountAPI =
  Capture "driverId" (Id Driver)
    :> "incrementGoToCount"
    :> Post '[JSON] APISuccess

type GetDriverGoHomeInfoAPI =
  Capture "driverId" (Id Driver)
    :> "getGoHomeInfo"
    :> Get '[JSON] CachedGoHomeRequestInfoRes

---------------------------------------------------------
-- Get Route driver ids ---------------------------------------

----------- update driver fees ---------------

type UpdateSubscriptionDriverFeeAndInvoiceAPI =
  Capture "driverId" (Id Driver)
    :> "update"
    :> "driverFeeAndInvoiceInfo"
    :> Capture "serviceName" ServiceNames
    :> ReqBody '[JSON] SubscriptionDriverFeesAndInvoicesToUpdate
    :> Post '[JSON] SubscriptionDriverFeesAndInvoicesToUpdate

data SubscriptionDriverFeesAndInvoicesToUpdate = SubscriptionDriverFeesAndInvoicesToUpdate
  { driverFees :: Maybe [DriverFeeInfoToUpdate],
    invoices :: Maybe [InvoiceInfoToUpdate],
    mkDuesToAmount :: Maybe HighPrecMoney,
    mkDuesToAmountWithCurrency :: Maybe PriceAPIEntity,
    subscribed :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverFeeInfoToUpdate = DriverFeeInfoToUpdate
  { driverFeeId :: Text,
    mkManualDue :: Maybe Bool,
    mkAutoPayDue :: Maybe Bool,
    mkCleared :: Maybe Bool,
    platformFee :: Maybe HighPrecMoney,
    sgst :: Maybe HighPrecMoney,
    cgst :: Maybe HighPrecMoney,
    platformFeeWithCurrency :: Maybe PriceAPIEntity,
    sgstWithCurrency :: Maybe PriceAPIEntity,
    cgstWithCurrency :: Maybe PriceAPIEntity
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data InvoiceInfoToUpdate = InvoiceInfoToUpdate
  { invoiceId :: Text,
    driverFeeId :: Maybe Text,
    invoiceStatus :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets SubscriptionDriverFeesAndInvoicesToUpdate where
  hideSecrets = identity

type SetVehicleDriverRcStatusForFleetAPI =
  Capture "driverId" (Id Driver)
    :> "fleet"
    :> "vehicleDriverRCstatus"
    :> ReqBody '[JSON] RCStatusReq
    :> Post '[JSON] APISuccess

type GetFleetDriverVehicleAssociationAPI =
  "fleet"
    :> "driverVehicleAssociation"
    :> QueryParam "Limit" Int
    :> QueryParam "Offset" Int
    :> QueryParam "countryCode" Text
    :> QueryParam "phoneNo" Text
    :> QueryParam "vehicleNo" Text
    :> QueryParam "includeStats" Bool
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> Get '[JSON] DrivertoVehicleAssociationRes

data DriveVehicleAssociationListItem = DriveVehicleAssociationListItem
  { driverId :: Maybe Text,
    vehicleNo :: Maybe Text,
    driverName :: Maybe Text,
    status :: Maybe DriverMode,
    driverPhoneNo :: Maybe Text,
    completedRides :: Int, ----- will remove this
    vehicleType :: Maybe Variant,
    earning :: Int, ------ this also
    isDriverActive :: Bool,
    isRcAssociated :: Bool,
    verificationDocsStatus :: Maybe VerificationDocsStatus
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)

data VerificationDocsStatus = VerificationDocsStatus
  { vehicleRegistrationCertificate :: Maybe VerificationStatus,
    vehiclePermit :: Maybe VerificationStatus,
    vehicleFitness :: Maybe VerificationStatus,
    vehicleInsurance :: Maybe VerificationStatus,
    driverLicense :: Maybe VerificationStatus
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)

data DrivertoVehicleAssociationRes = DrivertoVehicleAssociationRes
  { fleetOwnerId :: Text,
    listItem :: [DriveVehicleAssociationListItem],
    summary :: Summary
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)

type GetFleetDriverAssociationAPI =
  "fleet"
    :> "driverAssociation"
    :> QueryParam "isActive" Bool
    :> QueryParam "Limit" Int
    :> QueryParam "Offset" Int
    :> QueryParam "countryCode" Text
    :> QueryParam "phoneNo" Text
    :> QueryParam "includeStats" Bool
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> QueryParam "status" DriverMode
    :> Get '[JSON] DrivertoVehicleAssociationRes

type GetFleetVehicleAssociationAPI =
  "fleet"
    :> "vehicleAssociation"
    :> QueryParam "Limit" Int
    :> QueryParam "Offset" Int
    :> QueryParam "vehicleNo" Text
    :> QueryParam "includeStats" Bool
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> QueryParam "status" FleetVehicleStatus
    :> Get '[JSON] DrivertoVehicleAssociationRes

data FleetVehicleStatus = Active | InActive | Pending | Invalid
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema, ToParamSchema)

-- data DriverMode
--   = ONLINE
--   | OFFLINE
--   | SILENT
--   deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
--   deriving (PrettyShow) via Showable DriverMode

---------------------------------------------------------
-- Send dummy ride request to driver ---------------------

type SendDummyRideRequestToDriverAPI =
  Capture "driverId" (Id Driver)
    :> "sendDummyNotification" -- TODO: refactor to sendDummyRideRequestToDriver
    :> Post '[JSON] APISuccess

-- change operating city Api ------------------------
-------------------------------------------

type ChangeOperatingCityAPI =
  Capture "driverId" (Id Driver)
    :> "changeOperatingCity"
    :> ReqBody '[JSON] ChangeOperatingCityReq
    :> Post '[JSON] APISuccess

newtype ChangeOperatingCityReq = ChangeOperatingCityReq
  { operatingCity :: City.City
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance HideSecrets ChangeOperatingCityReq where
  hideSecrets = identity

-- get operating city Api ------------------------
-------------------------------------------

type GetOperatingCityAPI =
  "getOperatingCity"
    :> QueryParam "mobileCountryCode" Text
    :> QueryParam "mobileNumber" Text
    :> QueryParam "rideId" (Id Ride)
    :> Get '[JSON] GetOperatingCityResp

newtype GetOperatingCityResp = GetOperatingCityResp
  { operatingCity :: City.City
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance HideSecrets GetOperatingCityResp where
  hideSecrets = identity

-- pause or resume service charges via dashboard Api ------------------------

type PauseOrResumeServiceChargesAPI =
  Capture "driverId" (Id Driver)
    :> "pauseOrResumeServiceCharges"
    :> ReqBody '[JSON] PauseOrResumeServiceChargesReq
    :> Post '[JSON] APISuccess

data PauseOrResumeServiceChargesReq = PauseOrResumeServiceChargesReq
  { serviceChargeEligibility :: Bool,
    vehicleId :: Text,
    serviceName :: ServiceNames,
    reason :: Maybe ReasonForDisablingServiceCharge
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance HideSecrets PauseOrResumeServiceChargesReq where
  hideSecrets = identity

data ReasonForDisablingServiceCharge = OUT_SICK | VEHICLE_UNDER_MAINTENANCE | EXITED_INITIATIVE | SWITCH_VEHICLE | PROMOTIIONAL_ACTIVITY | NOTIFIED_LEAVE | OTHER
  deriving (Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Read)

instance Show ReasonForDisablingServiceCharge where
  show OUT_SICK = "sickness"
  show VEHICLE_UNDER_MAINTENANCE = "vehicle under maintainence"
  show EXITED_INITIATIVE = "initiative exited from intiative"
  show SWITCH_VEHICLE = "switching of vehicle"
  show PROMOTIIONAL_ACTIVITY = "attending promotional activity"
  show NOTIFIED_LEAVE = "notified leave"
  show OTHER = "miscellaneous"

data ServiceNames = YATRI_SUBSCRIPTION | YATRI_RENTAL
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''ServiceNames)
$(mkHttpInstancesForEnum ''ReasonForDisablingServiceCharge)
$(mkHttpInstancesForEnum ''FleetVehicleStatus)
$(mkHttpInstancesForEnum ''DriverMode)

-- change RC INVALID status  Api ------------------------
-------------------------------------------

type UpdateRCInvalidStatusAPI =
  "updateRCInvalidStatus"
    :> Capture "driverId" (Id Driver)
    :> ReqBody '[JSON] UpdateRCInvalidStatusReq
    :> Post '[JSON] APISuccess

data UpdateRCInvalidStatusReq = UpdateRCInvalidStatusReq
  { rcId :: Text,
    vehicleVariant :: Variant
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance HideSecrets UpdateRCInvalidStatusReq where
  hideSecrets = identity

-- change RC vehicle varient Api ------------------------
-------------------------------------------

type UpdateVehicleVariantAPI =
  "updateVehicleVariant"
    :> Capture "driverId" (Id Driver)
    :> ReqBody '[JSON] UpdateVehicleVariantReq
    :> Post '[JSON] APISuccess

data UpdateVehicleVariantReq = UpdateVehicleVariantReq
  { rcId :: Text,
    vehicleVariant :: Variant
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance HideSecrets UpdateVehicleVariantReq where
  hideSecrets = identity

type BulkReviewRCVariantAPI =
  "bulkReviewRCVariant"
    :> ReqBody '[JSON] [ReviewRCVariantReq]
    :> Post '[JSON] [ReviewRCVariantRes]

data ReviewRCVariantReq = ReviewRCVariantReq
  { rcId :: Text,
    vehicleVariant :: Maybe Variant,
    markReviewed :: Maybe Bool
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data ReviewRCVariantRes = ReviewRCVariantRes
  { rcId :: Text,
    status :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance HideSecrets [ReviewRCVariantReq] where
  hideSecrets = identity

----------------UPDATE DRIVER TAG---------------------------------------------
type UpdateDriverTagAPI =
  Capture "driverId" (Id Driver)
    :> "updateDriverTag"
    :> ReqBody '[JSON] UpdateDriverTagReq
    :> Post '[JSON] APISuccess

data UpdateDriverTagReq = UpdateDriverTagReq
  { driverTag :: Text,
    isAddingTag :: Bool
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance HideSecrets UpdateDriverTagReq where
  hideSecrets = identity

----------------  UPDATE FLEET OWNER INFO ---------------------------------------------

type UpdateFleetOwnerInfoAPI =
  Capture "driverId" (Id Driver)
    :> "updateFleetOwnerInfo"
    :> ReqBody '[JSON] UpdateFleetOwnerInfoReq
    :> Post '[JSON] APISuccess

data UpdateFleetOwnerInfoReq = UpdateFleetOwnerInfoReq
  { firstName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNo :: Maybe Text,
    mobileCountryCode :: Maybe Text,
    email :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance HideSecrets UpdateFleetOwnerInfoReq where
  hideSecrets = identity

---------------- Get GST and Pan Details For Fleet Owner Id ---------------------------------------------

type GetFleetOwnerInfoAPI =
  Capture "driverId" (Id Driver)
    :> "fleetOwnerInfo"
    :> Get '[JSON] FleetOwnerInfoRes

data FleetOwnerInfoRes = FleetOwnerInfoRes
  { blocked :: Bool,
    enabled :: Bool,
    fleetType :: Text,
    verified :: Bool,
    gstNumber :: Maybe Text,
    gstImageId :: Maybe Text,
    panNumber :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance HideSecrets FleetOwnerInfoRes where
  hideSecrets = identity

---------------- Fleet Driver Joining API  ------------------------------------------------

type SendFleetJoiningOtpAPI =
  "fleet"
    :> "sendJoiningOtp"
    :> ReqBody '[JSON] Registration.AuthReq
    :> Post '[JSON] Registration.AuthRes

type VerifyFleetJoiningOtpAPI =
  "fleet"
    :> "verifyJoiningOtp"
    :> QueryParam "authId" Text
    :> ReqBody '[JSON] VerifyFleetJoiningOtpReq
    :> Post '[JSON] APISuccess

data VerifyFleetJoiningOtpReq = VerifyFleetJoiningOtpReq
  { mobileCountryCode :: Text,
    mobileNumber :: Text,
    otp :: Text,
    deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance HideSecrets VerifyFleetJoiningOtpReq where
  hideSecrets = identity

--------- Link RC With Driver For Fleet ------------

type LinkRCWithDriverForFleetAPI =
  "fleet"
    :> "linkRCWithDriver"
    :> ReqBody '[JSON] LinkRCWithDriverForFleetReq
    :> Post '[JSON] APISuccess

data LinkRCWithDriverForFleetReq = LinkRCWithDriverForFleetReq
  { driverMobileCountryCode :: Maybe Text,
    driverMobileNumber :: Text,
    vehicleRegistrationNumber :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance HideSecrets LinkRCWithDriverForFleetReq where
  hideSecrets = identity
