{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.AadhaarVerification where

import Data.Text (pack)
import qualified Domain.Action.UI.DriverOnboarding.Status as Status
import qualified Domain.Types.DriverOnboarding.AadhaarOtp as Domain
import qualified Domain.Types.DriverOnboarding.AadhaarVerification as VDomain
import Domain.Types.DriverOnboarding.Error
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.DriverInformation as CQDriverInfo
import qualified Storage.CachedQueries.DriverInformation as DriverInfo
import Storage.CachedQueries.Merchant.TransporterConfig as CTC
import qualified Storage.Queries.DriverOnboarding.AadhaarOtp as Query
import qualified Storage.Queries.DriverOnboarding.AadhaarVerification as Q
import qualified Storage.Queries.Person as Person
import qualified Tools.AadhaarVerification as AadhaarVerification
import Tools.Error

data VerifyAadhaarOtpReq = VerifyAadhaarOtpReq
  { otp :: Int,
    shareCode :: Text
  }
  deriving (Show, Generic, ToSchema, ToJSON, FromJSON)

generateAadhaarOtp ::
  Bool ->
  Maybe DM.Merchant ->
  Id Person.Person ->
  Id DMOC.MerchantOperatingCity ->
  AadhaarVerification.AadhaarOtpReq ->
  Flow AadhaarVerification.AadhaarVerificationResp
generateAadhaarOtp isDashboard mbMerchant personId merchantOperatingCityId req = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverInfo <- DriverInfo.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  when driverInfo.blocked $ throwError DriverAccountBlocked
  when (driverInfo.aadhaarVerified) $ throwError AadhaarAlreadyVerified
  whenJust mbMerchant $ \merchant -> do
    -- merchant access checking
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  let tryKey = makeGenerateOtpTryKey person.id
  numberOfTries :: Maybe Int <- Redis.safeGet tryKey
  let tried = fromMaybe 0 numberOfTries
  transporterConfig <- CTC.findByMerchantId person.merchantId >>= fromMaybeM (TransporterConfigNotFound person.merchantId.getId)
  unless (isDashboard || tried < transporterConfig.onboardingTryLimit) $ throwError (GenerateAadhaarOtpExceedLimit personId.getId)
  res <- AadhaarVerification.generateAadhaarOtp merchantOperatingCityId $ req
  aadhaarOtpEntity <- mkAadhaarOtp personId res
  Esq.runNoTransaction $ Query.createForGenerate aadhaarOtpEntity
  cacheAadhaarVerifyTries personId tried res.transactionId isDashboard
  pure res

cacheAadhaarVerifyTries :: Id Person.Person -> Int -> Maybe Text -> Bool -> Flow ()
cacheAadhaarVerifyTries _ _ Nothing _ = return ()
cacheAadhaarVerifyTries personId tried transactionId isDashboard = do
  let key = makeTransactionNumberKey personId
  let tryKey = makeGenerateOtpTryKey personId
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Redis.setExp key transactionId expTime
  unless isDashboard $ Redis.setExp tryKey (tried + 1) expTime

verifyAadhaarOtp ::
  Maybe DM.Merchant ->
  Id Person.Person ->
  Id DMOC.MerchantOperatingCity ->
  VerifyAadhaarOtpReq ->
  Flow AadhaarVerification.AadhaarOtpVerifyRes
verifyAadhaarOtp mbMerchant personId merchantOperatingCityId req = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound (getId personId))
  driverInfo <- DriverInfo.findById (cast personId) >>= fromMaybeM (PersonNotFound (getId personId))
  when (driverInfo.blocked) $ throwError DriverAccountBlocked
  when (driverInfo.aadhaarVerified) $ throwError AadhaarAlreadyVerified
  whenJust mbMerchant $ \merchant -> do
    -- merchant access checking
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound (getId personId))
  let key = makeTransactionNumberKey personId
  transactionId <- Redis.safeGet key
  case transactionId of
    Just tId -> do
      let aadhaarVerifyReq =
            AadhaarVerification.AadhaarOtpVerifyReq
              { otp = req.otp,
                shareCode = req.shareCode,
                transactionId = tId
              }
      res <- AadhaarVerification.verifyAadhaarOtp merchantOperatingCityId aadhaarVerifyReq
      aadhaarVerifyEntity <- mkAadhaarVerify personId tId res
      Esq.runTransaction $ Query.createForVerify aadhaarVerifyEntity
      if res.code == pack "1002"
        then do
          Redis.del key
          aadhaarEntity <- mkAadhaar personId res
          Esq.runNoTransaction $ Q.create aadhaarEntity
          _ <- Status.statusHandler (person.id, person.merchantId, merchantOperatingCityId)
          void $ CQDriverInfo.updateAadhaarVerifiedState (cast personId) True
        else throwError $ InternalError "Aadhaar Verification failed, Please try again"
      pure res
    Nothing -> throwError TransactionIdNotFound

makeTransactionNumberKey :: Id Person.Person -> Text
makeTransactionNumberKey id = "AadhaarVerificationTransactionId:PersonId-" <> id.getId

makeGenerateOtpTryKey :: Id Person.Person -> Text
makeGenerateOtpTryKey id = "GenerateOtpTryKeyId:PersonId-" <> id.getId

mkAadhaarOtp ::
  (MonadGuid m, MonadTime m) =>
  Id Person.Person ->
  AadhaarVerification.AadhaarVerificationResp ->
  m Domain.AadhaarOtpReq
mkAadhaarOtp personId res = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    Domain.AadhaarOtpReq
      { id,
        driverId = personId,
        requestId = res.requestId,
        statusCode = res.statusCode,
        transactionId = res.transactionId,
        requestMessage = res.message,
        createdAt = now
      }

mkAadhaarVerify ::
  (MonadGuid m, MonadTime m) =>
  Id Person.Person ->
  Text ->
  AadhaarVerification.AadhaarOtpVerifyRes ->
  m Domain.AadhaarOtpVerify
mkAadhaarVerify personId tId res = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    Domain.AadhaarOtpVerify
      { id,
        driverId = personId,
        requestId = res.request_id,
        statusCode = res.code,
        transactionId = tId,
        requestMessage = res.message,
        createdAt = now
      }

mkAadhaar ::
  (MonadGuid m, MonadTime m) =>
  Id Person.Person ->
  AadhaarVerification.AadhaarOtpVerifyRes ->
  m VDomain.AadhaarVerification
mkAadhaar personId res = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    VDomain.AadhaarVerification
      { id,
        driverId = personId,
        driverName = res.name,
        driverGender = res.gender,
        driverDob = res.date_of_birth,
        driverImage = res.image,
        createdAt = now
      }
