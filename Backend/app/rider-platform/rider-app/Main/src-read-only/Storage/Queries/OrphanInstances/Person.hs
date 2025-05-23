{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Person where

import qualified Data.Text
import qualified Data.Time
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified Storage.Beam.Person as Beam
import qualified Storage.Queries.Transformers.Person

instance FromTType' Beam.Person Domain.Types.Person.Person where
  fromTType' (Beam.PersonT {..}) = do
    updateMerchantOpIdAndCity <- Storage.Queries.Transformers.Person.backfillCityAndMOCId currentCity merchantOperatingCityId merchantId
    clientBundleVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientBundleVersion)
    clientConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientConfigVersion)
    clientSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientSdkVersion)
    pure $
      Just
        Domain.Types.Person.Person
          { aadhaarVerified = aadhaarVerified,
            androidId = androidId,
            backendAppVersion = backendAppVersion,
            blocked = blocked,
            blockedAt = Data.Time.localTimeToUTC Data.Time.utc <$> blockedAt,
            blockedByRuleId = Kernel.Types.Id.Id <$> blockedByRuleId,
            blockedCount = blockedCount,
            clientBundleVersion = clientBundleVersion',
            clientConfigVersion = clientConfigVersion',
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion clientModelName clientManufacturer,
            clientSdkVersion = clientSdkVersion',
            createdAt = createdAt,
            currentCity = Kernel.Prelude.snd updateMerchantOpIdAndCity,
            customerPaymentId = customerPaymentId,
            customerReferralCode = customerReferralCode,
            defaultPaymentMethodId = defaultPaymentMethodId,
            description = description,
            deviceId = deviceId,
            deviceToken = deviceToken,
            email = EncryptedHashed <$> (Encrypted <$> emailEncrypted) <*> emailHash,
            enableOtpLessRide = enableOtpLessRide,
            enabled = enabled,
            falseSafetyAlarmCount = fromMaybe 0 falseSafetyAlarmCount,
            firstName = firstName,
            followsRide = followsRide,
            gender = gender,
            hasCompletedMockSafetyDrill = hasCompletedMockSafetyDrill,
            hasCompletedSafetySetup = hasCompletedSafetySetup,
            hasDisability = hasDisability,
            hasTakenValidRide = hasTakenValidRide,
            id = Kernel.Types.Id.Id id,
            identifier = identifier,
            identifierType = identifierType,
            informPoliceSos = fromMaybe False informPoliceSos,
            isNew = isNew,
            isValidRating = isValidRating,
            language = language,
            lastName = lastName,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Prelude.fst updateMerchantOpIdAndCity,
            middleName = middleName,
            mobileCountryCode = mobileCountryCode,
            mobileNumber = EncryptedHashed <$> (Encrypted <$> mobileNumberEncrypted) <*> mobileNumberHash,
            nightSafetyChecks = nightSafetyChecks,
            notificationToken = notificationToken,
            passwordHash = passwordHash,
            rating = Just $ fromIntegral totalRatingScore / fromIntegral totalRatings,
            referralCode = referralCode,
            referredAt = referredAt,
            referredByCustomer = referredByCustomer,
            registeredViaPartnerOrgId = Kernel.Types.Id.Id <$> registeredViaPartnerOrgId,
            registrationLat = registrationLat,
            registrationLon = registrationLon,
            role = role,
            safetyCenterDisabledOnDate = safetyCenterDisabledOnDate,
            shareEmergencyContacts = shareEmergencyContacts,
            shareTripWithEmergencyContactOption = shareTripWithEmergencyContactOption,
            totalRatingScore = totalRatingScore,
            totalRatings = totalRatings,
            totalRidesCount = totalRidesCount,
            updatedAt = updatedAt,
            useFakeOtp = useFakeOtp,
            whatsappNotificationEnrollStatus = whatsappNotificationEnrollStatus
          }

instance ToTType' Beam.Person Domain.Types.Person.Person where
  toTType' (Domain.Types.Person.Person {..}) = do
    Beam.PersonT
      { Beam.aadhaarVerified = aadhaarVerified,
        Beam.androidId = androidId,
        Beam.backendAppVersion = backendAppVersion,
        Beam.blocked = blocked,
        Beam.blockedAt = Data.Time.utcToLocalTime Data.Time.utc <$> blockedAt,
        Beam.blockedByRuleId = Kernel.Types.Id.getId <$> blockedByRuleId,
        Beam.blockedCount = blockedCount,
        Beam.clientBundleVersion = fmap Kernel.Utils.Version.versionToText clientBundleVersion,
        Beam.clientConfigVersion = fmap Kernel.Utils.Version.versionToText clientConfigVersion,
        Beam.clientManufacturer = clientDevice >>= (.deviceManufacturer),
        Beam.clientModelName = clientDevice <&> (.deviceModel),
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.clientSdkVersion = fmap Kernel.Utils.Version.versionToText clientSdkVersion,
        Beam.createdAt = createdAt,
        Beam.currentCity = Kernel.Prelude.Just currentCity,
        Beam.customerPaymentId = customerPaymentId,
        Beam.customerReferralCode = customerReferralCode,
        Beam.defaultPaymentMethodId = defaultPaymentMethodId,
        Beam.description = description,
        Beam.deviceId = deviceId,
        Beam.deviceToken = deviceToken,
        Beam.emailEncrypted = email <&> unEncrypted . (.encrypted),
        Beam.emailHash = email <&> (.hash),
        Beam.enableOtpLessRide = enableOtpLessRide,
        Beam.enabled = enabled,
        Beam.falseSafetyAlarmCount = Just falseSafetyAlarmCount,
        Beam.firstName = firstName,
        Beam.followsRide = followsRide,
        Beam.gender = gender,
        Beam.hasCompletedMockSafetyDrill = hasCompletedMockSafetyDrill,
        Beam.hasCompletedSafetySetup = hasCompletedSafetySetup,
        Beam.hasDisability = hasDisability,
        Beam.hasTakenValidRide = hasTakenValidRide,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.identifier = identifier,
        Beam.identifierType = identifierType,
        Beam.informPoliceSos = Just informPoliceSos,
        Beam.isNew = isNew,
        Beam.isValidRating = isValidRating,
        Beam.language = language,
        Beam.lastName = lastName,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = (Kernel.Prelude.Just . Kernel.Types.Id.getId) merchantOperatingCityId,
        Beam.middleName = middleName,
        Beam.mobileCountryCode = mobileCountryCode,
        Beam.mobileNumberEncrypted = mobileNumber <&> unEncrypted . (.encrypted),
        Beam.mobileNumberHash = mobileNumber <&> (.hash),
        Beam.nightSafetyChecks = nightSafetyChecks,
        Beam.notificationToken = notificationToken,
        Beam.passwordHash = passwordHash,
        Beam.referralCode = referralCode,
        Beam.referredAt = referredAt,
        Beam.referredByCustomer = referredByCustomer,
        Beam.registeredViaPartnerOrgId = Kernel.Types.Id.getId <$> registeredViaPartnerOrgId,
        Beam.registrationLat = registrationLat,
        Beam.registrationLon = registrationLon,
        Beam.role = role,
        Beam.safetyCenterDisabledOnDate = safetyCenterDisabledOnDate,
        Beam.shareEmergencyContacts = shareEmergencyContacts,
        Beam.shareTripWithEmergencyContactOption = shareTripWithEmergencyContactOption,
        Beam.totalRatingScore = totalRatingScore,
        Beam.totalRatings = totalRatings,
        Beam.totalRidesCount = totalRidesCount,
        Beam.updatedAt = updatedAt,
        Beam.useFakeOtp = useFakeOtp,
        Beam.whatsappNotificationEnrollStatus = whatsappNotificationEnrollStatus
      }
