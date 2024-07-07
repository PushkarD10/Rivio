{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.ProviderPlatform.Driver.Registration
  ( module Dashboard.ProviderPlatform.Driver.Registration,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Kernel.External.Notification.FCM.Types (FCMRecipientToken)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Servant hiding (Summary)

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data DriverRegistrationEndpoint
  = UploadDocumentEndpoint
  | RegisterDLEndpoint
  | RegisterRCEndpoint
  | GenerateAadhaarOtpEndpoint
  | VerifyAadhaarOtpEndpoint
  | UpdateDocumentEndpoint
  | AuthEndpoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord, ToSchema)

derivePersistField "DriverRegistrationEndpoint"

-- Under review drivers list --------------
type UnderReviewDriversListAPI =
  "underReviewDrivers"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] UnderReviewDriversListResponse

data UnderReviewDriversListResponse = UnderReviewDriversListResponse
  { drivers :: [UnderReviewDriver],
    summary :: Summary
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data UnderReviewDriver = UnderReviewDriver
  { driverId :: Id Driver,
    driverName :: Text,
    driverMobileNumber :: Text,
    driverMobileCountryCode :: Text,
    reviewedBy :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- Driver documents info --------------
type DriverDocumentInfoAPI =
  Capture "driverId" (Id Driver)
    :> "documents"
    :> "info"
    :> Get '[JSON] [DriverDocument]

newtype DocumentInfo = DocumentInfo
  { documentNumber :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data DriverDocument = DriverDocument
  { documentType :: DocumentType,
    documentStatus :: VerificationStatus,
    documentImageId :: Maybe (Id Image),
    documentImages :: [Id Image],
    documentId :: Maybe (Id Document),
    documentInfo :: Maybe DocumentInfo
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- Approve/Reject Document --------------
type UpdateDocumentAPI =
  "documents"
    :> "update"
    :> ReqBody '[JSON] UpdateDocumentRequest
    :> Post '[JSON] APISuccess

data UpdateDocumentRequest = Approve ApproveDetails | Reject RejectDetails
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets UpdateDocumentRequest where
  hideSecrets = identity

data ImageDocumentsRejectDetails = ImageDocumentsRejectDetails
  { reason :: Text,
    documentImageId :: Id Image
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data SSNRejectDetails = SSNRejectDetails
  { reason :: Text,
    ssn :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data RejectDetails
  = SSNReject SSNRejectDetails
  | ImageDocuments ImageDocumentsRejectDetails
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data FitnessApproveDetails = FitnessApproveDetails
  { applicationNumber :: Text,
    categoryOfVehicle :: Maybe Text,
    documentImageId :: Id Image,
    fitnessExpiry :: UTCTime,
    inspectingAuthority :: Maybe Text,
    inspectingOn :: Maybe UTCTime,
    nextInspectionDate :: Maybe UTCTime,
    rcNumber :: Text,
    receiptDate :: Maybe UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data VInsuranceApproveDetails = VInsuranceApproveDetails
  { documentImageId :: Id Image,
    insuredName :: Maybe Text,
    issueDate :: Maybe UTCTime,
    limitsOfLiability :: Maybe Text,
    policyExpiry :: Maybe UTCTime,
    policyNumber :: Maybe Text,
    policyProvider :: Maybe Text,
    rcNumber :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data VPermitApproveDetails = VPermitApproveDetails
  { documentImageId :: Id Image,
    issueDate :: Maybe UTCTime,
    nameOfPermitHolder :: Maybe Text,
    permitExpiry :: Maybe UTCTime,
    permitNumber :: Maybe Text,
    purposeOfJourney :: Maybe Text,
    rcNumber :: Maybe Text,
    regionCovered :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data VPUCApproveDetails = VPUCApproveDetails
  { documentImageId :: Id Image,
    pucExpiry :: Maybe UTCTime,
    rcNumber :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RCApproveDetails = RCApproveDetails
  { documentImageId :: Id Image,
    vehicleVariant :: Maybe Variant,
    vehicleNumberPlate :: Maybe Text,
    vehicleManufacturer :: Maybe Text,
    vehicleModel :: Maybe Text,
    vehicleModelYear :: Maybe Int,
    vehicleColor :: Maybe Text,
    vehicleDoors :: Maybe Int,
    vehicleSeatBelts :: Maybe Int,
    fitnessExpiry :: Maybe UTCTime,
    permitExpiry :: Maybe UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data VInspectionApproveDetails = VInspectionApproveDetails
  { documentImageId :: Id Image,
    dateOfExpiry :: Maybe UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DLApproveDetails = DLApproveDetails
  { documentImageId :: Id Image,
    driverLicenseNumber :: Maybe Text,
    driverDateOfBirth :: Maybe UTCTime,
    dateOfExpiry :: Maybe UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ApproveDetails
  = RC RCApproveDetails
  | DL DLApproveDetails
  | UploadProfile (Id Image)
  | SSNApprove Text
  | ProfilePhoto (Id Image)
  | VehiclePermit VPermitApproveDetails
  | VehiclePUC VPUCApproveDetails
  | VehicleInsurance VInsuranceApproveDetails
  | VehicleFitnessCertificate FitnessApproveDetails
  | VehicleInspectionForm VInspectionApproveDetails
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- driver documents list API --------------
-- ----------------------------------------

type DocumentsListAPI =
  Capture "driverId" (Id Driver)
    :> "documents"
    :> "list"
    :> Get '[JSON] DocumentsListResponse

data DLDetails = DLDetails
  { driverLicenseNumber :: Text,
    operatingCity :: Text,
    driverDateOfBirth :: Maybe UTCTime,
    classOfVehicles :: [Text],
    imageId1 :: Text,
    imageId2 :: Maybe (Text),
    dateOfIssue :: Maybe UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data RCDetails = RCDetails
  { vehicleRegistrationCertNumber :: Text,
    imageId :: Text,
    operatingCity :: Text,
    dateOfRegistration :: Maybe UTCTime,
    vehicleCategory :: Maybe Text,
    airConditioned :: Maybe Bool,
    vehicleManufacturer :: Maybe Text,
    vehicleModel :: Maybe Text,
    vehicleColor :: Maybe Text,
    vehicleDoors :: Maybe Int,
    vehicleSeatBelts :: Maybe Int,
    vehicleModelYear :: Maybe Int,
    oxygen :: Maybe Bool,
    ventilator :: Maybe Bool,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DocumentsListResponse = DocumentsListResponse
  { driverLicense :: [Text],
    vehicleRegistrationCertificate :: [Text],
    driverLicenseDetails :: [DLDetails],
    vehicleRegistrationCertificateDetails :: [RCDetails],
    vehicleInsurance :: [Text],
    uploadProfile :: [Text],
    ssn :: Maybe Text,
    vehicleFitnessCertificate :: [Text],
    profilePhoto :: [Text],
    vehicleInspectionForm :: [Text]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- driver documents list API --------------
-- ----------------------------------------

type GetDocumentAPI =
  "getDocument"
    :> Capture "imageId" (Id Image)
    :> Get '[JSON] GetDocumentResponse

newtype GetDocumentResponse = GetDocumentResponse
  { imageBase64 :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- driver documents list API --------------
-- ----------------------------------------

type UploadDocumentAPI =
  Capture "driverId" (Id Driver)
    :> "document"
    :> "upload"
    :> ReqBody '[JSON] UploadDocumentReq
    :> Post '[JSON] UploadDocumentResp

data DocumentType
  = DriverLicense
  | VehicleRegistrationCertificate
  | PanCard
  | VehiclePermitImage
  | VehiclePUCImage
  | VehicleInsuranceImage
  | VehicleFitnessCertificateImage
  | VehicleInspectionImage
  | ProfilePhotoImage
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data UploadDocumentReq = UploadDocumentReq
  { imageBase64 :: Text,
    imageType :: DocumentType,
    rcNumber :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data UploadDocumentTReq = UploadDocumentTReq
  { imageType :: DocumentType,
    rcNumber :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON)

newtype UploadDocumentResp = UploadDocumentResp
  { imageId :: Id Image
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets UploadDocumentReq where
  type ReqWithoutSecrets UploadDocumentReq = UploadDocumentTReq
  hideSecrets UploadDocumentReq {..} = UploadDocumentTReq {..}

instance HideSecrets UploadDocumentResp where
  hideSecrets = identity

-- register DL API ------------------------
-- ----------------------------------------

type RegisterDLAPI =
  Capture "driverId" (Id Driver)
    :> "register"
    :> "dl"
    :> ReqBody '[JSON] RegisterDLReq
    :> Post '[JSON] APISuccess

data RegisterDLReq = RegisterDLReq
  { driverLicenseNumber :: Text,
    operatingCity :: Text,
    driverDateOfBirth :: UTCTime,
    imageId1 :: Id Image,
    imageId2 :: Maybe (Id Image),
    dateOfIssue :: Maybe UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets RegisterDLReq where
  hideSecrets = identity

-- register RC API ------------------------
-- ----------------------------------------

type RegisterRCAPI =
  Capture "driverId" (Id Driver)
    :> "register"
    :> "rc"
    :> ReqBody '[JSON] RegisterRCReq
    :> Post '[JSON] APISuccess

data RegisterRCReq = RegisterRCReq
  { vehicleRegistrationCertNumber :: Text,
    imageId :: Id Image,
    operatingCity :: Text,
    dateOfRegistration :: Maybe UTCTime,
    airConditioned :: Maybe Bool,
    multipleRC :: Maybe Bool,
    oxygen :: Maybe Bool,
    ventilator :: Maybe Bool,
    vehicleDetails :: Maybe DriverVehicleDetails
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets RegisterRCReq where
  hideSecrets = identity

-- generate AadhaarOtp  API ------------------------
-- ----------------------------------------

type GenerateAadhaarOtpAPI =
  Capture "driverId" (Id Driver)
    :> "register"
    :> "generateAadhaarOtp"
    :> ReqBody '[JSON] GenerateAadhaarOtpReq
    :> Post '[JSON] GenerateAadhaarOtpRes

data GenerateAadhaarOtpReq = GenerateAadhaarOtpReq
  { aadhaarNumber :: Text,
    consent :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets GenerateAadhaarOtpReq where
  hideSecrets = identity

data GenerateAadhaarOtpRes = GenerateAadhaarOtpRes
  { message :: Text,
    transactionId :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets GenerateAadhaarOtpRes where
  hideSecrets = identity

-- verify AadhaarOtp  API ------------------------
-- ----------------------------------------

type VerifyAadhaarOtpAPI =
  Capture "driverId" (Id Driver)
    :> "register"
    :> "verifyAadhaarOtp"
    :> ReqBody '[JSON] VerifyAadhaarOtpReq
    :> Post '[JSON] VerifyAadhaarOtpRes

data VerifyAadhaarOtpReq = VerifyAadhaarOtpReq
  { otp :: Int,
    shareCode :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets VerifyAadhaarOtpReq where
  hideSecrets = identity

data VerifyAadhaarOtpRes = VerifyAadhaarOtpRes
  { message :: Text,
    code :: Text,
    name :: Text,
    gender :: Text,
    date_of_birth :: Text,
    share_code :: Text,
    image :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets VerifyAadhaarOtpRes where
  hideSecrets = identity

-- auth  API ------------------------
-- ----------------------------------------

type AuthAPI =
  "auth"
    :> ReqBody '[JSON] AuthReq
    :> Post '[JSON] AuthRes

data AuthReq = AuthReq
  { mobileNumber :: Text,
    mobileCountryCode :: Text
  }
  deriving (Generic, FromJSON, ToSchema, ToJSON)

data AuthRes = AuthRes
  { authId :: Text,
    attempts :: Int
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)

-- verify  API ------------------------
-- ----------------------------------------

type VerifyAPI =
  Capture "authId" Text
    :> "verify"
    :> ReqBody '[JSON] AuthVerifyReq
    :> Post '[JSON] APISuccess

---------- Verify Login --------
data AuthVerifyReq = AuthVerifyReq
  { otp :: Text,
    deviceToken :: FCMRecipientToken
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
