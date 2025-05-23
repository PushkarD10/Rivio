{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Driver where

import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data BlockDriverWithReasonReq = BlockDriverWithReasonReq {reasonCode :: Kernel.Prelude.Text, blockReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text, blockTimeInHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BlockReason = BlockReason
  { reasonCode :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Management.Driver.BlockReason,
    blockReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    blockTimeInHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype ChangeOperatingCityReq = ChangeOperatingCityReq {operatingCity :: Kernel.Types.Beckn.Context.City}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ChangeOperatingCityReq where
  hideSecrets = Kernel.Prelude.identity

data ClearDriverFeeReq = ClearDriverFeeReq
  { serviceName :: Dashboard.Common.Driver.ServiceNames,
    feeType :: API.Types.ProviderPlatform.Management.Driver.DriverFeeType,
    platformFee :: Kernel.Types.Common.HighPrecMoney,
    sgstPercentage :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    cgstPercentage :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    sendManualLink :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype ClearOnRideStuckDriversRes = ClearOnRideStuckDriversRes {driverIds :: [Kernel.Types.Id.Id Dashboard.Common.Driver]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype DeleteRCReq = DeleteRCReq {rcNo :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DeleteRCReq where
  hideSecrets = Kernel.Prelude.identity

type DriverAadhaarInfoByPhoneReq = API.Types.ProviderPlatform.Management.Driver.DriverAadhaarInfoRes

data DriverAadhaarInfoRes = DriverAadhaarInfoRes {driverName :: Kernel.Prelude.Text, driverGender :: Kernel.Prelude.Text, driverDob :: Kernel.Prelude.Text, driverImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverFeeType
  = PAYOUT_REGISTRATION
  | ONE_TIME_SECURITY_DEPOSIT
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data DriverListItem = DriverListItem
  { driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    firstName :: Kernel.Prelude.Text,
    middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    phoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    blocked :: Kernel.Prelude.Bool,
    subscribed :: Kernel.Prelude.Bool,
    verified :: Kernel.Prelude.Bool,
    onRide :: Kernel.Prelude.Bool,
    active :: Kernel.Prelude.Bool,
    onboardingDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverListRes = DriverListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, drivers :: [API.Types.ProviderPlatform.Management.Driver.DriverListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverLocationItem = DriverLocationItem
  { driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    firstName :: Kernel.Prelude.Text,
    middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNo :: Kernel.Prelude.Text,
    phoneNo :: Kernel.Prelude.Text,
    active :: Kernel.Prelude.Bool,
    onRide :: Kernel.Prelude.Bool,
    location :: Kernel.External.Maps.Types.LatLong,
    lastLocationTimestamp :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverLocationRes = DriverLocationRes
  { driverLocationsNotFound :: Kernel.Prelude.Maybe (Kernel.Prelude.NonEmpty (Kernel.Types.Id.Id Dashboard.Common.Driver)),
    driverLocations :: [API.Types.ProviderPlatform.Management.Driver.DriverLocationItem]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype GetOperatingCityResp = GetOperatingCityResp {operatingCity :: Kernel.Types.Beckn.Context.City}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets GetOperatingCityResp where
  hideSecrets = Kernel.Prelude.identity

data LicDetails = LicDetails {licExpiry :: Kernel.Prelude.UTCTime, vehicleClass :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PauseOrResumeServiceChargesReq = PauseOrResumeServiceChargesReq
  { serviceChargeEligibility :: Kernel.Prelude.Bool,
    vehicleId :: Kernel.Prelude.Text,
    serviceName :: Dashboard.Common.Driver.ServiceNames,
    reason :: Kernel.Prelude.Maybe Dashboard.Common.Driver.ReasonForDisablingServiceCharge
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PauseOrResumeServiceChargesReq where
  hideSecrets = Kernel.Prelude.identity

data RCDetails = RCDetails {vehicleClass :: Kernel.Prelude.Text, fitnessExpiry :: Kernel.Prelude.UTCTime, insuranceExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReviewRCVariantReq = ReviewRCVariantReq {rcId :: Kernel.Prelude.Text, vehicleVariant :: Kernel.Prelude.Maybe Dashboard.Common.Variant, markReviewed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReviewRCVariantRes = ReviewRCVariantRes {rcId :: Kernel.Prelude.Text, status :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype UpdateACUsageRestrictionReq = UpdateACUsageRestrictionReq {isWorking :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateACUsageRestrictionReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateDriverDataReq = UpdateDriverDataReq
  { driverName :: Kernel.Prelude.Text,
    driverGender :: Kernel.Prelude.Text,
    driverDob :: Kernel.Prelude.Text,
    driverAadhaarNumber :: Kernel.Prelude.Text,
    isVerified :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateDriverNameReq = UpdateDriverNameReq {firstName :: Kernel.Prelude.Text, middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text, lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateDriverNameReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateDriverTagReq = UpdateDriverTagReq {driverTag :: Kernel.Prelude.Text, isAddingTag :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateDriverTagReq where
  hideSecrets = Kernel.Prelude.identity

data UpdatePhoneNumberReq = UpdatePhoneNumberReq {newPhoneNumber :: Kernel.Prelude.Text, newCountryCode :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdatePhoneNumberReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateRCInvalidStatusReq = UpdateRCInvalidStatusReq {rcId :: Kernel.Prelude.Text, vehicleVariant :: Dashboard.Common.Variant}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateRCInvalidStatusReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateVehicleVariantReq = UpdateVehicleVariantReq {rcId :: Kernel.Prelude.Text, vehicleVariant :: Dashboard.Common.Variant}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateVehicleVariantReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("driver" :> (GetDriverDocumentsInfo :<|> PostDriverPersonNumbers :<|> GetDriverAadhaarInfo :<|> GetDriverAadhaarInfobyMobileNumber :<|> GetDriverList :<|> GetDriverActivity :<|> PostDriverDisable :<|> PostDriverAcRestrictionUpdate :<|> PostDriverBlockWithReasonHelper :<|> PostDriverBlock :<|> GetDriverBlockReasonList :<|> PostDriverUnblockHelper :<|> GetDriverLocation :<|> DeleteDriverPermanentlyDelete :<|> PostDriverUnlinkDL :<|> PostDriverUnlinkAadhaar :<|> PostDriverUpdatePhoneNumber :<|> PostDriverUpdateByPhoneNumber :<|> PostDriverUpdateName :<|> PostDriverDeleteRC :<|> GetDriverClearStuckOnRide :<|> PostDriverSendDummyNotification :<|> PostDriverChangeOperatingCity :<|> GetDriverGetOperatingCity :<|> PostDriverPauseOrResumeServiceCharges :<|> PostDriverUpdateRCInvalidStatus :<|> PostDriverUpdateVehicleVariant :<|> PostDriverBulkReviewRCVariant :<|> PostDriverUpdateDriverTag :<|> PostDriverClearFee))

type GetDriverDocumentsInfo = ("documents" :> "info" :> Get '[JSON] Dashboard.Common.Driver.DriverDocumentsInfoRes)

type PostDriverPersonNumbers = ("personNumbers" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp Dashboard.Common.PersonIdsReq :> Post '[JSON] [Dashboard.Common.PersonRes])

type GetDriverAadhaarInfo = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "aadhaarInfo" :> Get '[JSON] API.Types.ProviderPlatform.Management.Driver.DriverAadhaarInfoRes)

type GetDriverAadhaarInfobyMobileNumber =
  ( Capture "mobileNo" Kernel.Prelude.Text :> "aadhaarInfobyMobileNumber"
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.DriverAadhaarInfoByPhoneReq
  )

type GetDriverList =
  ( "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "verified" Kernel.Prelude.Bool
      :> QueryParam
           "enabled"
           Kernel.Prelude.Bool
      :> QueryParam "blocked" Kernel.Prelude.Bool
      :> QueryParam
           "subscribed"
           Kernel.Prelude.Bool
      :> QueryParam
           "phone"
           Kernel.Prelude.Text
      :> QueryParam
           "vehicleNumberSearchString"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.DriverListRes
  )

type GetDriverActivity = ("activity" :> Get '[JSON] Dashboard.Common.Driver.DriverActivityRes)

type PostDriverDisable = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "disable" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverAcRestrictionUpdate =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "acRestriction" :> "update"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.UpdateACUsageRestrictionReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverBlockWithReason =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "blockWithReason"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.BlockDriverWithReasonReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverBlockWithReasonHelper =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "blockWithReason" :> Capture "dashboardUserName" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.BlockDriverWithReasonReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverBlock = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "block" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetDriverBlockReasonList = ("blockReasonList" :> Get '[JSON] [API.Types.ProviderPlatform.Management.Driver.BlockReason])

type PostDriverUnblock = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "unblock" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverUnblockHelper =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "unblock" :> Capture "dashboardUserName" Kernel.Prelude.Text
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetDriverLocation =
  ( "location" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> ReqBody '[JSON] Dashboard.Common.Driver.DriverIds
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.DriverLocationRes
  )

type DeleteDriverPermanentlyDelete = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "permanentlyDelete" :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverUnlinkDL = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "unlinkDL" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverUnlinkAadhaar = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "unlinkAadhaar" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverUpdatePhoneNumber =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "updatePhoneNumber"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.UpdatePhoneNumberReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverUpdateByPhoneNumber =
  ( Capture "mobileNo" Kernel.Prelude.Text :> "updateByPhoneNumber"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.UpdateDriverDataReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverUpdateName =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "updateName"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.UpdateDriverNameReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverDeleteRC =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "deleteRC"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.DeleteRCReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type GetDriverClearStuckOnRide = ("clearStuck" :> "onRide" :> QueryParam "dbSyncTime" Kernel.Prelude.Int :> Get '[JSON] API.Types.ProviderPlatform.Management.Driver.ClearOnRideStuckDriversRes)

type PostDriverSendDummyNotification = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "sendDummyNotification" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverChangeOperatingCity =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "changeOperatingCity"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.ChangeOperatingCityReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type GetDriverGetOperatingCity =
  ( "getOperatingCity" :> QueryParam "mobileCountryCode" Kernel.Prelude.Text :> QueryParam "mobileNumber" Kernel.Prelude.Text
      :> QueryParam
           "rideId"
           (Kernel.Types.Id.Id Dashboard.Common.Ride)
      :> Get '[JSON] API.Types.ProviderPlatform.Management.Driver.GetOperatingCityResp
  )

type PostDriverPauseOrResumeServiceCharges =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "pauseOrResumeServiceCharges"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.PauseOrResumeServiceChargesReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverUpdateRCInvalidStatus =
  ( "updateRCInvalidStatus" :> Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.UpdateRCInvalidStatusReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverUpdateVehicleVariant =
  ( "updateVehicleVariant" :> Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.UpdateVehicleVariantReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverBulkReviewRCVariant =
  ( "bulkReviewRCVariant" :> ReqBody '[JSON] [API.Types.ProviderPlatform.Management.Driver.ReviewRCVariantReq]
      :> Post
           '[JSON]
           [API.Types.ProviderPlatform.Management.Driver.ReviewRCVariantRes]
  )

type PostDriverUpdateDriverTag =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "updateDriverTag"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.UpdateDriverTagReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverClearFee =
  ( "clearFee" :> MandatoryQueryParam "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.ClearDriverFeeReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

data DriverAPIs = DriverAPIs
  { getDriverDocumentsInfo :: EulerHS.Types.EulerClient Dashboard.Common.Driver.DriverDocumentsInfoRes,
    postDriverPersonNumbers :: (Data.ByteString.Lazy.ByteString, Dashboard.Common.PersonIdsReq) -> EulerHS.Types.EulerClient [Dashboard.Common.PersonRes],
    getDriverAadhaarInfo :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Driver.DriverAadhaarInfoRes,
    getDriverAadhaarInfobyMobileNumber :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Driver.DriverAadhaarInfoByPhoneReq,
    getDriverList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Driver.DriverListRes,
    getDriverActivity :: EulerHS.Types.EulerClient Dashboard.Common.Driver.DriverActivityRes,
    postDriverDisable :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverAcRestrictionUpdate :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateACUsageRestrictionReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverBlockWithReason :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Driver.BlockDriverWithReasonReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverBlock :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverBlockReasonList :: EulerHS.Types.EulerClient [API.Types.ProviderPlatform.Management.Driver.BlockReason],
    postDriverUnblock :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverLocation :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Dashboard.Common.Driver.DriverIds -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Driver.DriverLocationRes,
    deleteDriverPermanentlyDelete :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUnlinkDL :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUnlinkAadhaar :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdatePhoneNumber :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdatePhoneNumberReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdateByPhoneNumber :: Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverDataReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdateName :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverNameReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverDeleteRC :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.DeleteRCReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverClearStuckOnRide :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Driver.ClearOnRideStuckDriversRes,
    postDriverSendDummyNotification :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverChangeOperatingCity :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.ChangeOperatingCityReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverGetOperatingCity :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Ride) -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Driver.GetOperatingCityResp,
    postDriverPauseOrResumeServiceCharges :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.PauseOrResumeServiceChargesReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdateRCInvalidStatus :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateRCInvalidStatusReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdateVehicleVariant :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateVehicleVariantReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverBulkReviewRCVariant :: [API.Types.ProviderPlatform.Management.Driver.ReviewRCVariantReq] -> EulerHS.Types.EulerClient [API.Types.ProviderPlatform.Management.Driver.ReviewRCVariantRes],
    postDriverUpdateDriverTag :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverTagReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverClearFee :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.ClearDriverFeeReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkDriverAPIs :: (Client EulerHS.Types.EulerClient API -> DriverAPIs)
mkDriverAPIs driverClient = (DriverAPIs {..})
  where
    getDriverDocumentsInfo :<|> postDriverPersonNumbers :<|> getDriverAadhaarInfo :<|> getDriverAadhaarInfobyMobileNumber :<|> getDriverList :<|> getDriverActivity :<|> postDriverDisable :<|> postDriverAcRestrictionUpdate :<|> postDriverBlockWithReason :<|> postDriverBlock :<|> getDriverBlockReasonList :<|> postDriverUnblock :<|> getDriverLocation :<|> deleteDriverPermanentlyDelete :<|> postDriverUnlinkDL :<|> postDriverUnlinkAadhaar :<|> postDriverUpdatePhoneNumber :<|> postDriverUpdateByPhoneNumber :<|> postDriverUpdateName :<|> postDriverDeleteRC :<|> getDriverClearStuckOnRide :<|> postDriverSendDummyNotification :<|> postDriverChangeOperatingCity :<|> getDriverGetOperatingCity :<|> postDriverPauseOrResumeServiceCharges :<|> postDriverUpdateRCInvalidStatus :<|> postDriverUpdateVehicleVariant :<|> postDriverBulkReviewRCVariant :<|> postDriverUpdateDriverTag :<|> postDriverClearFee = driverClient
