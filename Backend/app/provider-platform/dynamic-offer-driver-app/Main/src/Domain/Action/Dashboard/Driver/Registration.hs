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
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Registration as Common
import qualified Domain.Action.UI.DriverOnboarding.AadhaarVerification as AV
import Domain.Action.UI.DriverOnboarding.DriverLicense
import Domain.Action.UI.DriverOnboarding.Image
import Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate
import Domain.Types.DriverOnboarding.Image
import qualified Domain.Types.DriverOnboarding.Image as Domain
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.External.AadhaarVerification.Interface.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as SMOC
import Storage.Queries.DriverOnboarding.Image as QImage
import qualified Tools.AadhaarVerification as AadhaarVerification

documentsList :: ShortId DM.Merchant -> Id Common.Driver -> Flow Common.DocumentsListResponse
documentsList merchantShortId driverId = do
  merchant <- findMerchantByShortId merchantShortId
  licImgs <- map (.id.getId) <$> runInReplica (findImagesByPersonAndType merchant.id (cast driverId) DriverLicense)
  vehRegImgs <- map (.id.getId) <$> runInReplica (findImagesByPersonAndType merchant.id (cast driverId) VehicleRegistrationCertificate)
  pure
    Common.DocumentsListResponse
      { driverLicense = licImgs,
        vehicleRegistrationCertificate = vehRegImgs
      }

getDocument :: ShortId DM.Merchant -> Id Common.Image -> Flow Common.GetDocumentResponse
getDocument merchantShortId imageId = do
  merchant <- findMerchantByShortId merchantShortId
  img <- getImage merchant.id (cast imageId)
  pure Common.GetDocumentResponse {imageBase64 = img}

mapImageType :: Common.DocumentType -> Domain.ImageType
mapImageType Common.DriverLicense = Domain.DriverLicense
mapImageType Common.VehicleRegistrationCertificate = Domain.VehicleRegistrationCertificate

uploadDocument :: ShortId DM.Merchant -> Id Common.Driver -> Common.UploadDocumentReq -> Flow Common.UploadDocumentResp
uploadDocument merchantShortId driverId_ req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- SMOC.findByMerchantId merchant.id >>= fromMaybeM (MerchantOperatingCityNotFound merchant.id.getId)
  res <-
    validateImage
      True
      (cast driverId_, cast merchant.id, cast merchantOperatingCity.id)
      ImageValidateRequest
        { image = req.imageBase64,
          imageType = mapImageType req.imageType
        }
  pure $ Common.UploadDocumentResp {imageId = cast res.imageId}

registerDL :: ShortId DM.Merchant -> Id Common.Driver -> Common.RegisterDLReq -> Flow APISuccess
registerDL merchantShortId driverId_ Common.RegisterDLReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- SMOC.findByMerchantId merchant.id >>= fromMaybeM (MerchantOperatingCityNotFound merchant.id.getId)
  verifyDL
    True
    (Just merchant)
    (cast driverId_, cast merchant.id, cast merchantOperatingCity.id)
    DriverDLReq
      { imageId1 = cast imageId1,
        imageId2 = fmap cast imageId2,
        ..
      }

registerRC :: ShortId DM.Merchant -> Id Common.Driver -> Common.RegisterRCReq -> Flow APISuccess
registerRC merchantShortId driverId_ Common.RegisterRCReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- SMOC.findByMerchantId merchant.id >>= fromMaybeM (MerchantOperatingCityNotFound merchant.id.getId)
  verifyRC
    True
    (Just merchant)
    (cast driverId_, cast merchant.id, cast merchantOperatingCity.id)
    DriverRCReq
      { imageId = cast imageId,
        ..
      }

generateAadhaarOtp :: ShortId DM.Merchant -> Id Common.Driver -> Common.GenerateAadhaarOtpReq -> Flow Common.GenerateAadhaarOtpRes
generateAadhaarOtp merchantShortId driverId_ req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- SMOC.findByMerchantId merchant.id >>= fromMaybeM (MerchantOperatingCityNotFound merchant.id.getId)
  res <-
    AV.generateAadhaarOtp
      True
      (Just merchant)
      (cast driverId_)
      (cast merchantOperatingCity.id)
      AadhaarVerification.AadhaarOtpReq
        { aadhaarNumber = req.aadhaarNumber,
          consent = req.consent
        }
  pure (convertVerifyOtp res)

verifyAadhaarOtp :: ShortId DM.Merchant -> Id Common.Driver -> Common.VerifyAadhaarOtpReq -> Flow Common.VerifyAadhaarOtpRes
verifyAadhaarOtp merchantShortId driverId_ req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- SMOC.findByMerchantId merchant.id >>= fromMaybeM (MerchantOperatingCityNotFound merchant.id.getId)
  res <-
    AV.verifyAadhaarOtp
      (Just merchant)
      (cast driverId_)
      (cast merchantOperatingCity.id)
      AV.VerifyAadhaarOtpReq
        { otp = req.otp,
          shareCode = req.shareCode
        }
  pure (convertSubmitOtp res)

convertVerifyOtp :: AadhaarVerificationResp -> Common.GenerateAadhaarOtpRes
convertVerifyOtp AadhaarVerificationResp {..} = Common.GenerateAadhaarOtpRes {..}

convertSubmitOtp :: AadhaarOtpVerifyRes -> Common.VerifyAadhaarOtpRes
convertSubmitOtp AadhaarOtpVerifyRes {..} = Common.VerifyAadhaarOtpRes {..}
