{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Driver.Registration
  ( documentsList,
    getDocument,
    uploadDocument,
    registerDL,
    registerRC,
    generateAadhaarOtp,
    verifyAadhaarOtp,
    auth,
    verify,
    underReviewDriversList,
    driverDocumentInfo,
    updateDocument,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Registration as Common
import Domain.Action.Dashboard.Driver (castVehicleVariant)
import qualified Domain.Action.UI.DriverOnboarding.AadhaarVerification as AV
import Domain.Action.UI.DriverOnboarding.DriverLicense
import Domain.Action.UI.DriverOnboarding.Image
import Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate
import qualified Domain.Action.UI.FleetDriverAssociation as FDV
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Types.DocumentVerificationConfig as Domain
import qualified Domain.Types.DriverLicense as DDL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import qualified Domain.Types.VehicleFitnessCertificate as DFC
import qualified Domain.Types.VehicleInsurance as DVI
import qualified Domain.Types.VehicleRegistrationCertificate as DRC
import Environment
import EulerHS.Prelude hiding (map, whenJust)
import Kernel.Beam.Functions
import Kernel.External.AadhaarVerification.Interface.Types
import Kernel.External.Encryption (decrypt, encrypt, hash)
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverOnboarding as DomainRC
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.DriverSSN as QSSN
import qualified Storage.Queries.FleetDriverAssociation as QFDV
import Storage.Queries.Image as QImage
import qualified Storage.Queries.Person as QDriver
import qualified Storage.Queries.VehicleFitnessCertificate as QFC
import qualified Storage.Queries.VehicleInsurance as QVI
import qualified Storage.Queries.VehicleRegistrationCertificate as QRC
import qualified Tools.AadhaarVerification as AadhaarVerification
import Tools.Error
import Tools.Notifications as Notify

documentsList :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow Common.DocumentsListResponse
documentsList merchantShortId city driverId = do
  merchant <- findMerchantByShortId merchantShortId
  dlImgs <- map (.id.getId) <$> runInReplica (findImagesByPersonAndType merchant.id (cast driverId) Domain.DriverLicense)
  vInspectionImgs <- map (.id.getId) <$> runInReplica (findImagesByPersonAndType merchant.id (cast driverId) Domain.VehicleInspectionForm)
  vehRegImgs <- map (.id.getId) <$> runInReplica (findImagesByPersonAndType merchant.id (cast driverId) Domain.VehicleRegistrationCertificate)
  uploadProfImgs <- map (.id.getId) <$> runInReplica (findImagesByPersonAndType merchant.id (cast driverId) Domain.UploadProfile)
  vehicleFitnessCertImgs <- map (.id.getId) <$> runInReplica (findImagesByPersonAndType merchant.id (cast driverId) Domain.VehicleFitnessCertificate)
  vehicleInsImgs <- map (.id.getId) <$> runInReplica (findImagesByPersonAndType merchant.id (cast driverId) Domain.VehicleInsurance)
  profilePics <- map (.id.getId) <$> runInReplica (findImagesByPersonAndType merchant.id (cast driverId) Domain.ProfilePhoto)
  allDlImgs <- runInReplica (QDL.findAllByImageId (map (Id) dlImgs))
  allRCImgs <- runInReplica (QRC.findAllByImageId (map (Id) vehRegImgs))
  allDLDetails <- mapM convertDLToDLDetails allDlImgs
  allRCDetails <- mapM convertRCToRCDetails allRCImgs
  ssnEntry <- QSSN.findByDriverId (cast driverId)
  ssnUnenc <- case ssnEntry of
    Just ssnRecord -> do
      ssnDecrypt <- decrypt ssnRecord.ssn
      return $ Just ssnDecrypt
    _ -> return Nothing
  pure
    Common.DocumentsListResponse
      { driverLicense = dlImgs,
        vehicleRegistrationCertificate = vehRegImgs,
        vehicleInsurance = vehicleInsImgs,
        uploadProfile = uploadProfImgs,
        ssn = ssnUnenc,
        vehicleFitnessCertificate = vehicleFitnessCertImgs,
        profilePhoto = profilePics,
        driverLicenseDetails = allDLDetails,
        vehicleRegistrationCertificateDetails = allRCDetails,
        vehicleInspectionForm = vInspectionImgs
      }
  where
    convertDLToDLDetails dl = do
      driverLicenseNumberDec <- decrypt dl.licenseNumber
      pure $
        Common.DLDetails
          { driverLicenseNumber = driverLicenseNumberDec,
            operatingCity = show city,
            driverDateOfBirth = dl.driverDob,
            classOfVehicles = dl.classOfVehicles,
            imageId1 = dl.documentImageId1.getId,
            imageId2 = getId <$> dl.documentImageId2,
            createdAt = dl.createdAt,
            dateOfIssue = dl.dateOfIssue
          }
    convertRCToRCDetails rc = do
      certificateNumberDec <- decrypt rc.certificateNumber
      pure $
        Common.RCDetails
          { vehicleRegistrationCertNumber = certificateNumberDec,
            imageId = rc.documentImageId.getId,
            operatingCity = show city,
            vehicleCategory = show <$> rc.userPassedVehicleCategory,
            airConditioned = rc.airConditioned,
            oxygen = rc.oxygen,
            ventilator = rc.ventilator,
            vehicleManufacturer = rc.vehicleManufacturer,
            vehicleModel = rc.vehicleModel,
            vehicleColor = rc.vehicleColor,
            vehicleDoors = rc.vehicleDoors,
            vehicleSeatBelts = rc.vehicleSeatBelts,
            createdAt = rc.createdAt,
            dateOfRegistration = rc.dateOfRegistration,
            vehicleModelYear = rc.vehicleModelYear
          }

getDocument :: ShortId DM.Merchant -> Context.City -> Id Common.Image -> Flow Common.GetDocumentResponse
getDocument merchantShortId _ imageId = do
  merchant <- findMerchantByShortId merchantShortId
  img <- getImage merchant.id (cast imageId)
  pure Common.GetDocumentResponse {imageBase64 = img}

mapDocumentType :: Common.DocumentType -> Domain.DocumentType
mapDocumentType Common.DriverLicense = Domain.DriverLicense
mapDocumentType Common.VehicleRegistrationCertificate = Domain.VehicleRegistrationCertificate
mapDocumentType Common.VehiclePUCImage = Domain.VehiclePUC
mapDocumentType Common.VehiclePermitImage = Domain.VehiclePermit
mapDocumentType Common.VehicleInsuranceImage = Domain.VehicleInsurance
mapDocumentType Common.VehicleFitnessCertificateImage = Domain.VehicleFitnessCertificate
mapDocumentType Common.VehicleInspectionImage = Domain.VehicleInspectionForm
mapDocumentType Common.ProfilePhotoImage = Domain.ProfilePhoto
mapDocumentType Common.PanCard = Domain.PanCard

uploadDocument :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UploadDocumentReq -> Flow Common.UploadDocumentResp
uploadDocument merchantShortId opCity driverId_ req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  res <-
    validateImage
      True
      (cast driverId_, cast merchant.id, merchantOpCityId)
      ImageValidateRequest
        { image = req.imageBase64,
          imageType = mapDocumentType req.imageType,
          rcNumber = req.rcNumber,
          validationStatus = Nothing,
          workflowTransactionId = Nothing,
          vehicleCategory = Nothing
        }
  pure $ Common.UploadDocumentResp {imageId = cast res.imageId}

registerDL :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.RegisterDLReq -> Flow APISuccess
registerDL merchantShortId opCity driverId_ Common.RegisterDLReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  verifyDL
    True
    (Just merchant)
    (cast driverId_, cast merchant.id, merchantOpCityId)
    DriverDLReq
      { imageId1 = cast imageId1,
        imageId2 = fmap cast imageId2,
        vehicleCategory = Nothing,
        ..
      }

registerRC :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.RegisterRCReq -> Flow APISuccess
registerRC merchantShortId opCity driverId_ Common.RegisterRCReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  verifyRC
    True
    (Just merchant)
    (cast driverId_, cast merchant.id, merchantOpCityId)
    ( DriverRCReq
        { imageId = cast imageId,
          vehicleCategory = Nothing,
          vehicleDetails = Nothing,
          ..
        }
    )

--make a separate function casting the driverVehiclereq

generateAadhaarOtp :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.GenerateAadhaarOtpReq -> Flow Common.GenerateAadhaarOtpRes
generateAadhaarOtp merchantShortId opCity driverId_ req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  res <-
    AV.generateAadhaarOtp
      True
      (Just merchant)
      (cast driverId_)
      merchantOpCityId
      AadhaarVerification.AadhaarOtpReq
        { aadhaarNumber = req.aadhaarNumber,
          consent = req.consent
        }
  pure (convertVerifyOtp res)

verifyAadhaarOtp :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.VerifyAadhaarOtpReq -> Flow Common.VerifyAadhaarOtpRes
verifyAadhaarOtp merchantShortId opCity driverId_ req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  res <-
    AV.verifyAadhaarOtp
      (Just merchant)
      (cast driverId_)
      merchantOpCityId
      AV.VerifyAadhaarOtpReq
        { otp = req.otp,
          shareCode = req.shareCode
        }
  pure (convertSubmitOtp res)

auth :: ShortId DM.Merchant -> Context.City -> Common.AuthReq -> Flow Common.AuthRes
auth merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  res <-
    DReg.auth
      True
      DReg.AuthReq
        { mobileNumber = Just req.mobileNumber,
          mobileCountryCode = Just req.mobileCountryCode,
          merchantId = merchant.id.getId,
          merchantOperatingCity = Just opCity,
          registrationLat = Nothing,
          registrationLon = Nothing,
          name = Nothing,
          email = Nothing,
          identifierType = Just SP.MOBILENUMBER
        }
      Nothing
      Nothing
      Nothing
      Nothing
  pure $ Common.AuthRes {authId = res.authId.getId, attempts = res.attempts}

verify :: Text -> Bool -> Text -> Common.AuthVerifyReq -> Flow APISuccess
verify authId mbFleet fleetOwnerId req = do
  let regId = Id authId :: Id SR.RegistrationToken
  res <-
    DReg.verify
      regId
      DReg.AuthVerifyReq
        { otp = req.otp,
          deviceToken = req.deviceToken,
          whatsappNotificationEnroll = Nothing
        }
  when mbFleet $ do
    checkAssoc <- runInReplica $ QFDV.findByDriverIdAndFleetOwnerId res.person.id fleetOwnerId True
    when (isJust checkAssoc) $ throwError (InvalidRequest "Driver already associated with fleet")
    assoc <- FDV.makeFleetDriverAssociation res.person.id fleetOwnerId (DomainRC.convertTextToUTC (Just "2099-12-12"))
    QFDV.create assoc
  pure Success

underReviewDriversList :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Flow Common.UnderReviewDriversListResponse
underReviewDriversList _merchantShortId _opCity _limit _offset = throwError (InternalError "Not Implemented")

driverDocumentInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow [Common.DriverDocument]
driverDocumentInfo _merchantShortId _opCity _driverId = throwError (InternalError "Not Implemented")

approveAndUpdateRC :: Common.RCApproveDetails -> Flow ()
approveAndUpdateRC req = do
  let imageId = Id req.documentImageId.getId
  rc <- QRC.findByImageId imageId >>= fromMaybeM (InternalError "RC not found by image id")
  certificateNumber <- mapM encrypt req.vehicleNumberPlate
  let udpatedRC =
        rc
          { DRC.vehicleVariant = (castVehicleVariant <$> req.vehicleVariant) <|> rc.vehicleVariant,
            DRC.verificationStatus = VALID,
            DRC.certificateNumber = fromMaybe rc.certificateNumber certificateNumber,
            DRC.vehicleManufacturer = req.vehicleManufacturer <|> rc.vehicleManufacturer,
            DRC.vehicleModel = req.vehicleModel <|> rc.vehicleModel,
            DRC.vehicleModelYear = req.vehicleModelYear <|> rc.vehicleModelYear,
            DRC.vehicleColor = req.vehicleColor <|> rc.vehicleColor,
            DRC.vehicleDoors = req.vehicleDoors <|> rc.vehicleDoors,
            DRC.vehicleSeatBelts = req.vehicleSeatBelts <|> rc.vehicleSeatBelts,
            DRC.fitnessExpiry = if isJust req.fitnessExpiry then fromJust req.fitnessExpiry else rc.fitnessExpiry,
            DRC.permitExpiry = req.permitExpiry <|> rc.permitExpiry
          }
  QRC.updateByPrimaryKey udpatedRC
  QImage.updateVerificationStatusByIdAndType VALID imageId Domain.VehicleRegistrationCertificate

approveAndUpdateInsurance :: Common.VInsuranceApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdateInsurance req@Common.VInsuranceApproveDetails {..} mId mOpCityId = do
  let imageId = Id req.documentImageId.getId
  QImage.updateVerificationStatusByIdAndType VALID imageId Domain.VehicleInsurance
  QImage.updateDocumentExpiry req.policyExpiry imageId
  vinsurance <- QVI.findByImageId imageId
  now <- getCurrentTime
  uuid <- generateGUID
  case vinsurance of
    Just insurance -> do
      policyNo <- mapM encrypt req.policyNumber
      let updatedInsurance =
            insurance
              { DVI.insuredName = req.insuredName <|> insurance.insuredName,
                DVI.issueDate = req.issueDate <|> insurance.issueDate,
                DVI.limitsOfLiability = req.limitsOfLiability <|> insurance.limitsOfLiability,
                DVI.policyNumber = fromMaybe insurance.policyNumber policyNo,
                DVI.policyProvider = fromMaybe insurance.policyProvider req.policyProvider,
                DVI.verificationStatus = VALID,
                DVI.policyExpiry = fromMaybe insurance.policyExpiry req.policyExpiry
              }
      QVI.updateByPrimaryKey updatedInsurance
    Nothing -> do
      case (req.policyNumber, req.policyExpiry, req.policyProvider, req.rcNumber) of
        (Just policyNum, Just policyExp, Just provider, Just rcNo) -> do
          insuranceImage <- QImage.findById imageId >>= fromMaybeM (InternalError "Image not found by image id")
          rcNoEnc <- encrypt rcNo
          rc <- QRC.findByCertificateNumberHash (rcNoEnc & hash) >>= fromMaybeM (InternalError "RC not found by RC number")
          policyNo <- encrypt policyNum
          let insurance =
                DVI.VehicleInsurance
                  { documentImageId = imageId,
                    driverId = insuranceImage.personId,
                    id = uuid,
                    policyNumber = policyNo,
                    rcId = rc.id,
                    verificationStatus = VALID,
                    createdAt = now,
                    updatedAt = now,
                    merchantId = Just mId,
                    merchantOperatingCityId = Just mOpCityId,
                    policyExpiry = policyExp,
                    policyProvider = provider,
                    rejectReason = Nothing,
                    ..
                  }
          QVI.create insurance
        _ -> pure ()

approveAndUpdateFitnessCertificate :: Common.FitnessApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdateFitnessCertificate req@Common.FitnessApproveDetails {..} mId mOpCityId = do
  let imageId = Id req.documentImageId.getId
  QImage.updateVerificationStatusByIdAndType VALID imageId Domain.VehicleFitnessCertificate
  mbFitnessCert <- QFC.findByImageId imageId
  applicationNo <- encrypt req.applicationNumber
  now <- getCurrentTime
  uuid <- generateGUID
  case mbFitnessCert of
    Just certificate -> do
      let updatedFitnessCert =
            certificate
              { DFC.applicationNumber = applicationNo,
                DFC.categoryOfVehicle = req.categoryOfVehicle <|> certificate.categoryOfVehicle,
                DFC.fitnessExpiry = req.fitnessExpiry,
                DFC.inspectingAuthority = req.inspectingAuthority <|> certificate.inspectingAuthority,
                DFC.inspectingOn = req.inspectingOn <|> certificate.inspectingOn,
                DFC.nextInspectionDate = req.nextInspectionDate <|> certificate.nextInspectionDate,
                DFC.receiptDate = req.receiptDate <|> certificate.receiptDate,
                DFC.verificationStatus = VALID
              }
      QFC.updateByPrimaryKey updatedFitnessCert
    Nothing -> do
      certificateImage <- QImage.findById imageId >>= fromMaybeM (InternalError "Image not found by image id")
      rcNoEnc <- encrypt rcNumber
      rc <- QRC.findByCertificateNumberHash (rcNoEnc & hash) >>= fromMaybeM (InternalError "RC not found by rc number")
      let fitnessCert =
            DFC.VehicleFitnessCertificate
              { applicationNumber = applicationNo,
                documentImageId = imageId,
                driverId = certificateImage.personId,
                id = uuid,
                rcId = rc.id,
                verificationStatus = VALID,
                createdAt = now,
                updatedAt = now,
                merchantId = Just mId,
                merchantOperatingCityId = Just mOpCityId,
                ..
              }
      QFC.create fitnessCert

approveAndUpdateDL :: Common.DLApproveDetails -> Flow ()
approveAndUpdateDL req = do
  let imageId = Id req.documentImageId.getId
  dl <- QDL.findByImageId imageId >>= fromMaybeM (InternalError "DL not found by image id")
  licenseNumber <- mapM encrypt req.driverLicenseNumber
  let updatedDL =
        dl
          { DDL.licenseNumber = fromMaybe dl.licenseNumber licenseNumber,
            DDL.driverDob = req.driverDateOfBirth <|> dl.driverDob,
            DDL.licenseExpiry = fromMaybe dl.licenseExpiry req.dateOfExpiry,
            DDL.verificationStatus = VALID
          }
  QDL.updateByPrimaryKey updatedDL
  QImage.updateVerificationStatusByIdAndType VALID imageId Domain.DriverLicense

handleApproveRequest :: Common.ApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
handleApproveRequest approveReq merchantId merchantOperatingCityId = do
  case approveReq of
    Common.DL dlReq -> approveAndUpdateDL dlReq
    Common.RC rcApproveReq -> approveAndUpdateRC rcApproveReq
    Common.VehicleInsurance vInsuranceReq -> approveAndUpdateInsurance vInsuranceReq merchantId merchantOperatingCityId
    Common.VehicleFitnessCertificate fitnessReq -> approveAndUpdateFitnessCertificate fitnessReq merchantId merchantOperatingCityId
    Common.UploadProfile imageId -> QImage.updateVerificationStatusByIdAndType VALID (Id imageId.getId) Domain.UploadProfile
    Common.ProfilePhoto imageId -> QImage.updateVerificationStatusByIdAndType VALID (Id imageId.getId) Domain.ProfilePhoto
    Common.VehicleInspectionForm req -> do
      QImage.updateVerificationStatusByIdAndType VALID (Id req.documentImageId.getId) Domain.VehicleInspectionForm
      QImage.updateDocumentExpiry req.dateOfExpiry (Id req.documentImageId.getId)
    Common.SSNApprove ssnNum -> do
      ssnEnc <- encrypt ssnNum
      QSSN.updateVerificationStatusAndReasonBySSN VALID Nothing (ssnEnc & hash)
    _ -> throwError (InternalError "Unknown Config in approve update document")

handleRejectRequest :: Common.RejectDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
handleRejectRequest rejectReq _ merchantOperatingCityId = do
  case rejectReq of
    Common.SSNReject ssnRejectReq -> rejectSSNAndSendNotification ssnRejectReq merchantOperatingCityId
    Common.ImageDocuments imageRejectReq -> do
      let imageId = Id imageRejectReq.documentImageId.getId
      image <- QImage.findById imageId >>= fromMaybeM (InternalError "Image not found by image id")
      case image.imageType of
        Domain.ProfilePhoto -> QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid imageRejectReq.reason) imageId
        Domain.UploadProfile -> QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid imageRejectReq.reason) imageId
        Domain.VehicleInspectionForm -> QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid imageRejectReq.reason) imageId
        Domain.VehicleFitnessCertificate -> QFC.updateVerificationStatus INVALID imageId --At this point the vehicle fitness cert doesn not even exist in this table
        Domain.VehicleInsurance -> do
          QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid imageRejectReq.reason) imageId
          vInsurance <- QVI.findByImageId imageId
          whenJust vInsurance $ \_ -> do
            QVI.updateVerificationStatusAndRejectReason INVALID imageRejectReq.reason imageId
        Domain.DriverLicense -> do
          QDL.updateVerificationStatusAndRejectReason INVALID imageRejectReq.reason imageId
          QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid imageRejectReq.reason) imageId
        Domain.VehicleRegistrationCertificate -> do
          QRC.updateVerificationStatusAndRejectReason INVALID imageRejectReq.reason imageId
          QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid imageRejectReq.reason) imageId
        _ -> throwError (InternalError "Unknown Config in reject update document")
      driver <- QDriver.findById image.personId >>= fromMaybeM (PersonNotFound image.personId.getId)
      Notify.notifyDriver merchantOperatingCityId notificationType (notificationTitle (show image.imageType)) (message (show image.imageType)) driver driver.deviceToken
  where
    notificationType = FCM.DOCUMENT_INVALID
    notificationTitle obj = "Attention: Your " <> obj <> " is invalid."
    message obj = "Kindly reapply or reupload your " <> obj
    rejectSSNAndSendNotification req merchantOpCityId = do
      ssnEnc <- encrypt req.ssn
      QSSN.updateVerificationStatusAndReasonBySSN INVALID (Just req.reason) (ssnEnc & hash)
      ssnEntry <- QSSN.findBySSN (ssnEnc & hash) >>= fromMaybeM (InternalError "SSN not found by ssn no")
      driver <- QDriver.findById ssnEntry.driverId >>= fromMaybeM (PersonNotFound ssnEntry.driverId.getId)
      Notify.notifyDriver merchantOpCityId notificationType (notificationTitle "SSN") (message "SSN") driver driver.deviceToken

updateDocument :: ShortId DM.Merchant -> Context.City -> Common.UpdateDocumentRequest -> Flow APISuccess
updateDocument _merchantShortId _opCity _req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  case _req of
    Common.Approve approveReq -> handleApproveRequest approveReq merchant.id merchantOpCityId
    Common.Reject rejectReq -> handleRejectRequest rejectReq merchant.id merchantOpCityId
  pure Success

convertVerifyOtp :: AadhaarVerificationResp -> Common.GenerateAadhaarOtpRes
convertVerifyOtp AadhaarVerificationResp {..} = Common.GenerateAadhaarOtpRes {..}

convertSubmitOtp :: AadhaarOtpVerifyRes -> Common.VerifyAadhaarOtpRes
convertSubmitOtp AadhaarOtpVerifyRes {..} = Common.VerifyAadhaarOtpRes {..}
