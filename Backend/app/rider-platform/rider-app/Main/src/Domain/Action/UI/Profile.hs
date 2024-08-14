{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Profile
  ( ProfileRes,
    UpdateProfileReq (..),
    UpdateProfileResp,
    UpdateProfileDefaultEmergencyNumbersReq (..),
    PersonDefaultEmergencyNumber (..),
    UpdateProfileDefaultEmergencyNumbersResp,
    GetProfileDefaultEmergencyNumbersResp (..),
    UpdateEmergencySettingsReq (..),
    UpdateEmergencySettingsResp,
    EmergencySettingsRes,
    getPersonDetails,
    updatePerson,
    updateDefaultEmergencyNumbers,
    getDefaultEmergencyNumbers,
    updateEmergencySettings,
    getEmergencySettings,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson as DA
import qualified Data.Aeson.KeyMap as DAKM
import Data.Digest.Pure.MD5 as MD5
import qualified Data.HashMap.Strict as HM
import Data.List (nubBy)
import qualified Data.Text as T
import qualified Domain.Action.UI.PersonDefaultEmergencyNumber as DPDEN
import qualified Domain.Action.UI.Registration as DR
import qualified Domain.Types.BecknConfig as BecknConfig
import Domain.Types.Booking as DBooking
import qualified Domain.Types.ClientPersonInfo as DCP
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.Person (RideShareOptions)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.PersonDefaultEmergencyNumber as DPDEN
import qualified Domain.Types.PersonDisability as PersonDisability
import Domain.Types.SafetySettings
import Environment
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import qualified Kernel.Beam.Functions as B
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Notification as Notification
import qualified Kernel.External.Whatsapp.Interface.Types as Whatsapp (OptApiMethods)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.Validation
import Kernel.Types.Version
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import qualified Kernel.Utils.Text as TU
import Kernel.Utils.Validation
import Kernel.Utils.Version
import SharedLogic.Cac
import SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.MessageBuilder as MessageBuilder
import SharedLogic.Person as SLP
import SharedLogic.PersonDefaultEmergencyNumber as SPDEN
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import Storage.Queries.Booking as QBooking
import qualified Storage.Queries.ClientPersonInfo as QCP
import qualified Storage.Queries.Disability as QD
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonDefaultEmergencyNumber as QPersonDEN
import qualified Storage.Queries.PersonDisability as PDisability
import qualified Storage.Queries.PersonStats as QPS
import qualified Storage.Queries.SafetySettings as QSafety
import Tools.Error

data ProfileRes = ProfileRes
  { id :: Id Person.Person,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    maskedMobileNumber :: Maybe Text,
    maskedDeviceToken :: Maybe Text,
    hasTakenRide :: Bool,
    hasTakenValidRide :: Bool,
    hasTakenValidAutoRide :: Bool,
    hasTakenValidCabRide :: Bool,
    hasTakenValidBikeRide :: Bool,
    hasTakenValidAmbulanceRide :: Bool,
    referralCode :: Maybe Text,
    whatsappNotificationEnrollStatus :: Maybe Whatsapp.OptApiMethods,
    language :: Maybe Maps.Language,
    hasDisability :: Maybe Bool,
    disability :: Maybe Text,
    gender :: Person.Gender,
    hasCompletedSafetySetup :: Bool,
    hasCompletedMockSafetyDrill :: Maybe Bool,
    bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version,
    followsRide :: Bool,
    frontendConfigHash :: Maybe Text,
    isSafetyCenterDisabled :: Bool,
    customerReferralCode :: Maybe Text,
    deviceId :: Maybe Text,
    androidId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data UpdateProfileReq = UpdateProfileReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    deviceToken :: Maybe Text,
    notificationToken :: Maybe Text,
    referralCode :: Maybe Text,
    language :: Maybe Maps.Language,
    gender :: Maybe Person.Gender,
    bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version,
    disability :: Maybe Disability,
    hasDisability :: Maybe Bool,
    enableOtpLessRide :: Maybe Bool,
    deviceId :: Maybe Text,
    androidId :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type UpdateProfileResp = APISuccess.APISuccess

data Disability = Disability
  { id :: Id Disability,
    description :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToSchema, ToJSON, FromJSON)

newtype UpdateProfileDefaultEmergencyNumbersReq = UpdateProfileDefaultEmergencyNumbersReq
  { defaultEmergencyNumbers :: [PersonDefaultEmergencyNumber]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

validateUpdateProfileDefaultEmergencyNumbersReq :: Int -> Validate UpdateProfileDefaultEmergencyNumbersReq
validateUpdateProfileDefaultEmergencyNumbersReq maxEmergencyNumberCount UpdateProfileDefaultEmergencyNumbersReq {..} =
  sequenceA_
    [ validateList "defaultEmergencyNumbers" defaultEmergencyNumbers validatePersonDefaultEmergencyNumber,
      validateField "defaultEmergencyNumbers" defaultEmergencyNumbers $ MaxLength maxEmergencyNumberCount
    ]

data PersonDefaultEmergencyNumber = PersonDefaultEmergencyNumber
  { name :: Text,
    mobileCountryCode :: Text,
    mobileNumber :: Text,
    priority :: Maybe Int,
    enableForFollowing :: Maybe Bool,
    shareTripWithEmergencyContactOption :: Maybe RideShareOptions
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

validatePersonDefaultEmergencyNumber :: Validate PersonDefaultEmergencyNumber
validatePersonDefaultEmergencyNumber PersonDefaultEmergencyNumber {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

type UpdateProfileDefaultEmergencyNumbersResp = APISuccess.APISuccess

newtype GetProfileDefaultEmergencyNumbersResp = GetProfileDefaultEmergencyNumbersResp
  { defaultEmergencyNumbers :: [DPDEN.PersonDefaultEmergencyNumberAPIEntity]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getPersonDetails :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => (Id Person.Person, Id Merchant.Merchant) -> Maybe Int -> m ProfileRes
getPersonDetails (personId, _) mbToss = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  tag <- case person.hasDisability of
    Just True -> B.runInReplica $ fmap (.tag) <$> PDisability.findByPersonId personId
    _ -> return Nothing
  decPerson <- decrypt person
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe False (.useCACForFrontend) systemConfigs
  frntndfgs <- if useCACConfig then getFrontendConfigs person mbToss else return $ Just DAKM.empty
  let mbMd5Digest = T.pack . show . MD5.md5 . DA.encode <$> frntndfgs
  isSafetyCenterDisabled_ <- SLP.checkSafetyCenterDisabled person
  hasTakenValidRide <- QCP.findAllByPersonId personId
  let hasTakenValidFirstCabRide = validRideCount hasTakenValidRide BecknConfig.CAB
      hasTakenValidFirstAutoRide = validRideCount hasTakenValidRide BecknConfig.AUTO_RICKSHAW
      hasTakenValidFirstBikeRide = validRideCount hasTakenValidRide BecknConfig.MOTORCYCLE
      hasTakenValidAmbulanceRide = validRideCount hasTakenValidRide BecknConfig.AMBULANCE
  newCustomerReferralCode <-
    if (isNothing person.customerReferralCode)
      then do
        newCustomerReferralCode <- DR.generateCustomerReferralCode
        checkIfReferralCodeExists <- QPerson.findPersonByCustomerReferralCode (Just newCustomerReferralCode)
        if (isNothing checkIfReferralCodeExists)
          then do
            void $ QPerson.updateCustomerReferralCode personId newCustomerReferralCode
            pure $ Just newCustomerReferralCode
          else pure Nothing
      else pure person.customerReferralCode
  return $ makeProfileRes decPerson tag mbMd5Digest isSafetyCenterDisabled_ newCustomerReferralCode hasTakenValidFirstCabRide hasTakenValidFirstAutoRide hasTakenValidFirstBikeRide hasTakenValidAmbulanceRide
  where
    makeProfileRes Person.Person {..} disability md5DigestHash isSafetyCenterDisabled_ newCustomerReferralCode hasTakenCabRide hasTakenAutoRide hasTakenValidFirstBikeRide hasTakenValidAmbulanceRide = do
      ProfileRes
        { maskedMobileNumber = maskText <$> mobileNumber,
          maskedDeviceToken = maskText <$> deviceToken,
          hasTakenRide = hasTakenValidRide,
          frontendConfigHash = md5DigestHash,
          hasTakenValidAutoRide = hasTakenAutoRide,
          hasTakenValidCabRide = hasTakenCabRide,
          hasTakenValidBikeRide = hasTakenValidFirstBikeRide,
          hasTakenValidAmbulanceRide = hasTakenValidAmbulanceRide,
          isSafetyCenterDisabled = isSafetyCenterDisabled_,
          customerReferralCode = newCustomerReferralCode,
          bundleVersion = clientBundleVersion,
          clientVersion = clientSdkVersion,
          deviceId = maskText <$> deviceId,
          androidId = maskText <$> androidId,
          ..
        }

validRideCount :: [DCP.ClientPersonInfo] -> BecknConfig.VehicleCategory -> Bool
validRideCount hasTakenValidRide vehicleCategory =
  case find (\info -> info.vehicleCategory == Just vehicleCategory) hasTakenValidRide of
    Just info -> info.rideCount == 1
    Nothing -> False

updatePerson :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl, "version" ::: DeploymentVersion]) => Id Person.Person -> Id Merchant.Merchant -> UpdateProfileReq -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> m APISuccess.APISuccess
updatePerson personId merchantId req mbBundleVersion mbClientVersion mbClientConfigVersion mbDevice = do
  mPerson <- join <$> QPerson.findByEmailAndMerchantId merchantId `mapM` req.email
  whenJust mPerson (\_ -> throwError PersonEmailExists)
  mbEncEmail <- encrypt `mapM` req.email
  refCode <- join <$> validateRefferalCode personId `mapM` req.referralCode
  deploymentVersion <- asks (.version)
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  void $
    QPerson.updatePersonalInfo
      personId
      req.firstName
      req.middleName
      req.lastName
      refCode
      mbEncEmail
      req.deviceToken
      req.notificationToken
      req.language
      req.gender
      (mbClientVersion <|> req.clientVersion)
      (mbBundleVersion <|> req.bundleVersion)
      mbClientConfigVersion
      (getDeviceFromText mbDevice)
      deploymentVersion.getDeploymentVersion
      req.enableOtpLessRide
      (if isJust person.deviceId then Nothing else req.deviceId)
      (if isJust person.androidId then Nothing else req.androidId)
      person
  updateDisability req.hasDisability req.disability personId

updateDisability :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Maybe Bool -> Maybe Disability -> Id Person.Person -> m APISuccess.APISuccess
updateDisability hasDisability mbDisability personId = do
  case (hasDisability, mbDisability) of
    (Nothing, _) -> logDebug "No Disability"
    (Just False, _) -> do
      QPerson.updateHasDisability (Just False) personId
      PDisability.deleteByPersonId personId
    (Just True, Nothing) -> throwError $ InvalidRequest "Field disability can't be null if hasDisability is True"
    (Just True, Just selectedDisability) -> do
      customerDisability <- B.runInReplica $ PDisability.findByPersonId personId
      QPerson.updateHasDisability (Just True) personId
      let disabilityId = getId $ selectedDisability.id
      disability <- runInReplica $ QD.findByDisabilityId disabilityId >>= fromMaybeM (DisabilityDoesNotExist disabilityId)
      let mbDescription = (selectedDisability.description) <|> (Just disability.description)
      when (isNothing customerDisability) $ do
        newDisability <- makeDisability selectedDisability disability.tag mbDescription
        PDisability.create newDisability
      when (isJust customerDisability) $ do
        PDisability.updateDisabilityByPersonId disabilityId disability.tag mbDescription personId
      where
        makeDisability personDisability tag mbDescription = do
          now <- getCurrentTime
          return $
            PersonDisability.PersonDisability
              { personId = personId,
                disabilityId = getId $ personDisability.id,
                tag = tag,
                description = mbDescription,
                createdAt = now,
                updatedAt = now
              }
  pure APISuccess.Success

validateRefferalCode :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => Id Person.Person -> Text -> m (Maybe Text)
validateRefferalCode personId refCode = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId) >>= decrypt
  when (person.hasTakenValidRide) do
    throwError (InvalidRequest "You have been already referred by someone")
  let isCustomerReferralCode = T.isPrefixOf "C" refCode
  if isCustomerReferralCode
    then do
      logDebug $ "Came inside Customer Referral Code" <> show personId <> " " <> show refCode
      unless (TU.validateAlphaNumericWithLength refCode 6) (throwError $ InvalidRequest "Referral Code must have 6 digits and must be Alphanumeric")
      referredByPerson <- QPerson.findPersonByCustomerReferralCode (Just refCode) >>= fromMaybeM (InvalidRequest "Invalid ReferralCode")
      when (personId == referredByPerson.id) (throwError $ InvalidRequest "Cannot refer yourself")
      stats <- QPS.findByPersonId referredByPerson.id >>= fromMaybeM (PersonStatsNotFound personId.getId)
      void $ QPS.updateReferralCount (stats.referralCount + 1) referredByPerson.id
      void $ QPerson.updateReferredByCustomer personId referredByPerson.id.getId
      return $ Just refCode
    else do
      unless (TU.validateAllDigitWithMinLength 6 refCode) (throwError $ InvalidRequest "Referral Code must have 6 digits")
      case person.referralCode of
        Just code ->
          if code /= refCode
            then throwError (InvalidRequest "Referral Code is not same")
            else return Nothing -- idempotent behaviour
        Nothing -> do
          merchant <- QMerchant.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
          isMultipleDeviceIdExist <-
            maybe
              (return Nothing)
              ( \deviceId -> do
                  personsWithSameDeviceId <- QPerson.findAllByDeviceId (Just deviceId)
                  return $ Just (length personsWithSameDeviceId > 1)
              )
              person.deviceId
          case (person.mobileNumber, person.mobileCountryCode) of
            (Just mobileNumber, Just countryCode) -> do
              void $ CallBPPInternal.linkReferee merchant.driverOfferApiKey merchant.driverOfferBaseUrl merchant.driverOfferMerchantId refCode mobileNumber countryCode isMultipleDeviceIdExist
              return $ Just refCode
            _ -> throwError (PersonMobileNumberIsNULL person.id.getId)

updateDefaultEmergencyNumbers ::
  Id Person.Person ->
  Id Merchant.Merchant ->
  UpdateProfileDefaultEmergencyNumbersReq ->
  Flow UpdateProfileDefaultEmergencyNumbersResp
updateDefaultEmergencyNumbers personId merchantId req = do
  maxEmergencyNumberCount <- asks (.maxEmergencyNumberCount)
  oldPersonDENList <- runInReplica $ QPersonDEN.findAllByPersonId personId
  runRequestValidation (validateUpdateProfileDefaultEmergencyNumbersReq maxEmergencyNumberCount) req
  now <- getCurrentTime
  let uniqueRecords = getUniquePersonByMobileNumber req
  newPersonDENList <- buildPersonDefaultEmergencyNumber now `mapM` uniqueRecords
  fork "Send Emergency Contact Added Message" $ do
    sendEmergencyContactAddedMessage personId newPersonDENList oldPersonDENList
  QPersonDEN.replaceAll personId newPersonDENList
  pure APISuccess.Success
  where
    buildPersonDefaultEmergencyNumber now defEmNum = do
      encMobNum <- encrypt defEmNum.mobileNumber
      dbHash <- getDbHash defEmNum.mobileNumber
      mbEmNumPerson <- QPerson.findByMobileNumberAndMerchantId defEmNum.mobileCountryCode dbHash merchantId
      return $
        DPDEN.PersonDefaultEmergencyNumber
          { mobileNumber = encMobNum,
            name = defEmNum.name,
            mobileCountryCode = defEmNum.mobileCountryCode,
            createdAt = now,
            contactPersonId = (.id) <$> mbEmNumPerson,
            enableForFollowing = fromMaybe False defEmNum.enableForFollowing,
            enableForShareRide = False,
            priority = fromMaybe 1 defEmNum.priority,
            merchantId = Just merchantId,
            shareTripWithEmergencyContactOption = defEmNum.shareTripWithEmergencyContactOption,
            ..
          }

sendEmergencyContactAddedMessage :: Id Person.Person -> [DPDEN.PersonDefaultEmergencyNumber] -> [DPDEN.PersonDefaultEmergencyNumber] -> Flow ()
sendEmergencyContactAddedMessage personId newPersonDENList oldPersonDENList = do
  decNew <- decrypt `mapM` newPersonDENList
  decOld <- decrypt `mapM` oldPersonDENList
  let oldList = (.mobileNumber) <$> decOld
      newList = DPDEN.makePersonDefaultEmergencyNumberAPIEntity False <$> decNew
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  buildSmsReq <-
    MessageBuilder.buildAddedAsEmergencyContactMessage person.merchantOperatingCityId $
      MessageBuilder.BuildAddedAsEmergencyContactMessageReq
        { userName = SLP.getName person,
          appUrl = riderConfig.appUrl
        }
  let filterNewContacts personDEN = return $ not $ personDEN.mobileNumber `elem` oldList
  newlyAddedContacts <- filterM filterNewContacts newList
  SPDEN.notifyEmergencyContacts person (notificationMessage person) "Emergency Contact Added" Notification.EMERGENCY_CONTACT_ADDED (Just buildSmsReq) riderConfig.enableEmergencyContactAddedMessage newlyAddedContacts
  where
    notificationMessage person = SLP.getName person <> " has added you as the emergency contact."

getDefaultEmergencyNumbers :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => (Id Person.Person, Id Merchant.Merchant) -> m GetProfileDefaultEmergencyNumbersResp
getDefaultEmergencyNumbers (personId, _) = do
  personENList <- runInReplica $ QPersonDEN.findAllByPersonId personId
  decPersonENList <- decrypt `mapM` personENList
  emergencyContactsEntity <- mapM makeAPIEntityAndCheckOnRide decPersonENList
  return $ GetProfileDefaultEmergencyNumbersResp emergencyContactsEntity
  where
    makeAPIEntityAndCheckOnRide :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => DPDEN.DecryptedPersonDefaultEmergencyNumber -> m DPDEN.PersonDefaultEmergencyNumberAPIEntity
    makeAPIEntityAndCheckOnRide personEN = do
      onRide <- case (personEN.contactPersonId, personEN.priority == 0) of
        (Just id, True) -> do
          activeBookings <- runInReplica $ QBooking.findByRiderIdAndStatus id DBooking.activeBookingStatus
          return $ not $ null activeBookings
        _ -> return False
      return $ DPDEN.makePersonDefaultEmergencyNumberAPIEntity onRide personEN

getUniquePersonByMobileNumber :: UpdateProfileDefaultEmergencyNumbersReq -> [PersonDefaultEmergencyNumber]
getUniquePersonByMobileNumber req =
  nubBy ((==) `on` mobileNumber) req.defaultEmergencyNumbers

data UpdateEmergencySettingsReq = UpdateEmergencySettingsReq
  { shareEmergencyContacts :: Maybe Bool,
    shareTripWithEmergencyContacts :: Maybe Bool,
    shareTripWithEmergencyContactOption :: Maybe Person.RideShareOptions,
    nightSafetyChecks :: Maybe Bool,
    hasCompletedSafetySetup :: Maybe Bool,
    autoCallDefaultContact :: Maybe Bool,
    enablePostRideSafetyCheck :: Maybe Person.RideShareOptions,
    enableUnexpectedEventsCheck :: Maybe Person.RideShareOptions,
    hasCompletedMockSafetyDrill :: Maybe Bool,
    informPoliceSos :: Maybe Bool,
    notifySafetyTeamForSafetyCheckFailure :: Maybe Bool,
    notifySosWithEmergencyContacts :: Maybe Bool,
    shakeToActivate :: Maybe Bool,
    safetyCenterDisabledOnDate :: Maybe UTCTime,
    enableOtpLessRide :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type UpdateEmergencySettingsResp = APISuccess.APISuccess

updateEmergencySettings :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Id Person.Person -> UpdateEmergencySettingsReq -> m UpdateEmergencySettingsResp
updateEmergencySettings personId req = do
  personENList <- QPersonDEN.findAllByPersonId personId
  let shareTripOptions = case req.shareTripWithEmergencyContactOption of
        Nothing ->
          case req.shareTripWithEmergencyContacts of
            Just True -> Just Person.SHARE_WITH_TIME_CONSTRAINTS
            Just False -> Just Person.NEVER_SHARE
            Nothing -> Just Person.ALWAYS_SHARE
        _ -> req.shareTripWithEmergencyContactOption
  when (fromMaybe False req.shareEmergencyContacts && null personENList) do
    throwError (InvalidRequest "Add atleast one emergency contact.")
  void $ updateSafetySettings req
  QPersonDEN.updateShareTripWithEmergencyContactOptions personId shareTripOptions
  pure APISuccess.Success
  where
    updateSafetySettings :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => UpdateEmergencySettingsReq -> m ()
    updateSafetySettings UpdateEmergencySettingsReq {..} = do
      let shareContacts = fromMaybe False shareEmergencyContacts
          setContactField field = bool (Just shareContacts) field (isJust field)
          emergencyInfo =
            QSafety.UpdateEmergencyInfo
              { autoCallDefaultContact = setContactField autoCallDefaultContact,
                notifySosWithEmergencyContacts = setContactField notifySosWithEmergencyContacts,
                ..
              }
      void $ QSafety.upsert personId emergencyInfo

data EmergencySettingsRes = EmergencySettingsRes
  { shareEmergencyContacts :: Bool,
    shareTripWithEmergencyContacts :: Bool,
    shareTripWithEmergencyContactOption :: Person.RideShareOptions,
    hasCompletedMockSafetyDrill :: Bool,
    nightSafetyChecks :: Bool,
    hasCompletedSafetySetup :: Bool,
    defaultEmergencyNumbers :: [DPDEN.PersonDefaultEmergencyNumberAPIEntity],
    enablePoliceSupport :: Bool,
    localPoliceNumber :: Maybe Text,
    autoCallDefaultContact :: Bool,
    notifySosWithEmergencyContacts :: Bool,
    enablePostRideSafetyCheck :: Person.RideShareOptions,
    enableUnexpectedEventsCheck :: Person.RideShareOptions,
    informPoliceSos :: Bool,
    notifySafetyTeamForSafetyCheckFailure :: Bool,
    shakeToActivate :: Bool,
    safetyCenterDisabledOnDate :: Maybe UTCTime,
    enableOtpLessRide :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getEmergencySettings :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Id Person.Person -> m EmergencySettingsRes
getEmergencySettings personId = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  personENList <- QPersonDEN.findpersonENListWithFallBack personId (Just person)
  safetySettings <- QSafety.findSafetySettingsWithFallback personId (Just person)
  let SafetySettings {personId = _id, ..} = safetySettings
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  decPersonENList <- decrypt `mapM` personENList
  return $
    EmergencySettingsRes
      { shareTripWithEmergencyContacts = any (\x -> (x.shareTripWithEmergencyContactOption /= Just Person.NEVER_SHARE) && isJust x.shareTripWithEmergencyContactOption) personENList,
        shareTripWithEmergencyContactOption = fromMaybe Person.NEVER_SHARE (listToMaybe personENList >>= (.shareTripWithEmergencyContactOption)),
        defaultEmergencyNumbers = DPDEN.makePersonDefaultEmergencyNumberAPIEntity False <$> decPersonENList,
        enablePoliceSupport = riderConfig.enableLocalPoliceSupport,
        localPoliceNumber = riderConfig.localPoliceNumber,
        hasCompletedMockSafetyDrill = fromMaybe False safetySettings.hasCompletedMockSafetyDrill,
        shareEmergencyContacts = safetySettings.notifySosWithEmergencyContacts,
        ..
      }
