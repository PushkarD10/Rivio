{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.UI.Driver
  ( DriverInformationRes (..),
    ListDriverRes (..),
    DriverEntityRes (..),
    OnboardDriverReq (..),
    OnboardDriverRes (..),
    CreatePerson (..),
    CreateVehicle (..),
    UpdateDriverReq (..),
    UpdateDriverRes,
    GetNearbySearchRequestsRes (..),
    DriverOfferReq (..),
    DriverRespondReq (..),
    DriverStatsRes (..),
    DriverAlternateNumberReq (..),
    DriverAlternateNumberRes (..),
    DriverAlternateNumberOtpReq (..),
    DriverPhotoUploadReq (..),
    ResendAuth (..),
    DriverPaymentHistoryResp,
    DriverDuesResp (..),
    MetaDataReq (..),
    getInformation,
    setActivity,
    listDriver,
    changeDriverEnableState,
    createDriver,
    deleteDriver,
    updateDriver,
    getNearbySearchRequests,
    offerQuote,
    respondQuote,
    offerQuoteLockKey,
    getStats,
    driverPhotoUpload,
    validate,
    verifyAuth,
    resendOtp,
    remove,
    getDriverPayments,
    getDriverDues,
    DriverInfo.DriverMode,
    updateMetaData,
  )
where

import AWS.S3 as S3
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Message as Common
import qualified Data.ByteString as BS
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Data.Time (Day, UTCTime (UTCTime, utctDay), fromGregorian)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.DriverFee as DDF
import Domain.Types.DriverInformation (DriverInformation)
import qualified Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.DriverQuote as DDrQuote
import qualified Domain.Types.DriverReferral as DR
import Domain.Types.DriverStats
import qualified Domain.Types.Estimate as DEstimate
import Domain.Types.FareParameters
import qualified Domain.Types.FareParameters as Fare
import Domain.Types.FarePolicy (DriverExtraFeeBounds (..))
import qualified Domain.Types.FarePolicy as DFarePolicy
import qualified Domain.Types.MediaFile as Domain
import qualified Domain.Types.Merchant as DM
import Domain.Types.Merchant.TransporterConfig
import qualified Domain.Types.MetaData as MD
import Domain.Types.Person (Person, PersonAPIEntity)
import qualified Domain.Types.Person as SP
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.SearchRequestForDriver
import qualified Domain.Types.SearchTry as DST
import Domain.Types.Vehicle (VehicleAPIEntity)
import qualified Domain.Types.Vehicle as SV
import qualified Domain.Types.Vehicle as Veh
import qualified Domain.Types.Vehicle.Variant as Variant
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id, state)
import EulerHS.Types (base64Encode)
import qualified GHC.List as GHCL
import GHC.Records.Extra
-- import Kernel.Storage.Esqueleto.Transactionable (runInLocationDB, runInReplica)

import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.External.Notification.FCM.Types (FCMRecipientToken)
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Payment.Interface
import qualified Kernel.External.SMS.MyValueFirst.Flow as SF
import qualified Kernel.External.SMS.MyValueFirst.Types as SMS
import qualified Kernel.External.Verification.Interface.InternalScripts as IF
import Kernel.Prelude (NominalDiffTime)
import Kernel.ServantMultipart
import Kernel.Sms.Config
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow, EsqLocDBFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.SlidingWindowLimiter
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty (PrettyShow)
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.SlidingWindowLimiter
import Kernel.Utils.Validation
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import Lib.Payment.Domain.Types.PaymentTransaction
import Lib.Payment.Storage.Queries.PaymentTransaction
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.CallBAP (sendDriverOffer)
import qualified SharedLogic.DeleteDriver as DeleteDriverOnCheck
import SharedLogic.DriverMode as DMode
import SharedLogic.DriverPool as DP
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.SearchTryLocker as CS
import qualified Storage.CachedQueries.BapMetadata as CQSM
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.DriverInformation as QDriverInformation
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.TransporterConfig as CQTC
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverLocation as QDriverLocation
import qualified Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.DriverQuote as QDrQt
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FareParameters as QFP
import qualified Storage.Queries.MediaFile as MFQuery
import qualified Storage.Queries.MetaData as QMeta
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Plan as QPD
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.RegistrationToken as QRegister
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import qualified Storage.Queries.SearchTry as QST
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Text.Read as Read
import qualified Tools.Auth as Auth
import Tools.Error
import Tools.Event
import Tools.Metrics
import qualified Tools.Notifications as Notify
import Tools.SMS as Sms hiding (Success)
import Tools.Verification

data DriverInformationRes = DriverInformationRes
  { id :: Id Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    numberOfRides :: Int,
    mobileNumber :: Maybe Text,
    linkedVehicle :: Maybe VehicleAPIEntity,
    rating :: Maybe Int,
    active :: Bool,
    onRide :: Bool,
    verified :: Bool,
    enabled :: Bool,
    blocked :: Bool,
    subscribed :: Bool,
    paymentPending :: Bool,
    referralCode :: Maybe Text,
    organization :: DM.MerchantAPIEntity,
    language :: Maybe Maps.Language,
    alternateNumber :: Maybe Text,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchback :: Bool,
    canDowngradeToTaxi :: Bool,
    mode :: Maybe DriverInfo.DriverMode,
    autoPayStatus :: Maybe DriverInfo.DriverAutoPayStatus,
    clientVersion :: Maybe Version,
    bundleVersion :: Maybe Version,
    gender :: Maybe SP.Gender,
    mediaUrl :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

newtype ListDriverRes = ListDriverRes
  {list :: [DriverEntityRes]}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DriverEntityRes = DriverEntityRes
  { id :: Id Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text,
    linkedVehicle :: Maybe VehicleAPIEntity,
    rating :: Maybe Int,
    active :: Bool,
    onRide :: Bool,
    enabled :: Bool,
    blocked :: Bool,
    subscribed :: Bool,
    paymentPending :: Bool,
    verified :: Bool,
    registeredAt :: UTCTime,
    language :: Maybe Maps.Language,
    alternateNumber :: Maybe Text,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchback :: Bool,
    canDowngradeToTaxi :: Bool,
    mode :: Maybe DriverInfo.DriverMode,
    autoPayStatus :: Maybe DriverInfo.DriverAutoPayStatus,
    clientVersion :: Maybe Version,
    bundleVersion :: Maybe Version,
    gender :: Maybe SP.Gender,
    mediaUrl :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- Create Person request and response
data OnboardDriverReq = OnboardDriverReq
  { person :: CreatePerson,
    vehicle :: CreateVehicle,
    metaData :: MetaDataReq
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

validateOnboardDriverReq :: Validate OnboardDriverReq
validateOnboardDriverReq OnboardDriverReq {..} =
  sequenceA_
    [ validateObject "person" person validateCreatePerson,
      validateObject "vehicle" vehicle validateCreateVehicle
    ]

data CreatePerson = CreatePerson
  { firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

validateCreatePerson :: Validate CreatePerson
validateCreatePerson CreatePerson {..} =
  sequenceA_
    [ validateField "firstName" firstName $ MinLength 3 `And` P.name,
      validateField "middleName" middleName $ InMaybe $ NotEmpty `And` P.name,
      validateField "lastName" lastName $ InMaybe $ NotEmpty `And` P.name,
      validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

data CreateVehicle = CreateVehicle
  { category :: Veh.Category,
    model :: Text,
    variant :: Variant.Variant,
    color :: Text,
    registrationNo :: Text,
    capacity :: Int
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

validateCreateVehicle :: Validate CreateVehicle
validateCreateVehicle CreateVehicle {..} =
  sequenceA_
    [ validateField "registrationNo" registrationNo $
        LengthInRange 1 11 `And` star (P.latinUC \/ P.digit),
      validateField "model" model $
        NotEmpty `And` star P.latinOrSpace,
      validateField "color" color $ NotEmpty `And` P.name
    ]

newtype OnboardDriverRes = OnboardDriverRes
  {driver :: PersonAPIEntity}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data UpdateDriverReq = UpdateDriverReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    deviceToken :: Maybe FCMRecipientToken,
    language :: Maybe Maps.Language,
    canDowngradeToSedan :: Maybe Bool,
    canDowngradeToHatchback :: Maybe Bool,
    canDowngradeToTaxi :: Maybe Bool,
    clientVersion :: Maybe Version,
    bundleVersion :: Maybe Version,
    gender :: Maybe SP.Gender,
    languagesSpoken :: Maybe [Text],
    hometown :: Maybe Text,
    vehicleName :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

validateUpdateDriverReq :: Validate UpdateDriverReq
validateUpdateDriverReq UpdateDriverReq {..} =
  sequenceA_
    [ validateField "firstName" firstName $ InMaybe $ MinLength 3 `And` P.name,
      validateField "middleName" middleName $ InMaybe $ NotEmpty `And` P.name,
      validateField "lastName" lastName $ InMaybe $ NotEmpty `And` P.name
    ]

type UpdateDriverRes = DriverInformationRes

newtype GetNearbySearchRequestsRes = GetNearbySearchRequestsRes
  { searchRequestsForDriver :: [SearchRequestForDriverAPIEntity]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema, PrettyShow)

data DriverOfferReq = DriverOfferReq
  { offeredFare :: Maybe Money,
    searchRequestId :: Id DST.SearchTry
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data DriverRespondReq = DriverRespondReq
  { offeredFare :: Maybe Money,
    searchRequestId :: Maybe (Id DST.SearchTry), -- TODO: Deprecated, to be removed
    searchTryId :: Maybe (Id DST.SearchTry),
    response :: SearchRequestForDriverResponse
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data DriverStatsRes = DriverStatsRes
  { totalRidesOfDay :: Int,
    totalEarningsOfDay :: Money,
    bonusEarning :: Money
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DriverPhotoUploadReq = DriverPhotoUploadReq
  { image :: FilePath,
    fileType :: Common.FileType,
    reqContentType :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp DriverPhotoUploadReq where
  fromMultipart form = do
    DriverPhotoUploadReq
      <$> fmap fdPayload (lookupFile "image" form)
      <*> (lookupInput "fileType" form >>= (Read.readEither . T.unpack))
      <*> fmap fdFileCType (lookupFile "image" form)

instance ToMultipart Tmp DriverPhotoUploadReq where
  toMultipart driverPhotoUploadReq =
    MultipartData
      [Input "fileType" (show driverPhotoUploadReq.fileType)]
      [FileData "image" (T.pack driverPhotoUploadReq.image) "" (driverPhotoUploadReq.image)]

data DriverAlternateNumberReq = DriverAlternateNumberReq
  { mobileCountryCode :: Text,
    alternateNumber :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype DriverAlternateNumberOtpReq = DriverAlternateNumberOtpReq
  { otp :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data ResendAuth = ResendAuth
  { auth :: Text,
    attemptsLeft :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype DriverAlternateNumberRes = DriverAlternateNumberRes
  { attempts :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DriverPaymentHistoryResp = DriverPaymentHistoryResp
  { date :: Day, -- window start day
    driverFeeId :: Id DDF.DriverFee,
    status :: DDF.DriverFeeStatus,
    totalRides :: Int,
    totalEarnings :: Money,
    charges :: Money,
    chargesBreakup :: [DriverPaymentBreakup],
    txnInfo :: [DriverTxnInfo]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DriverPaymentBreakup = DriverPaymentBreakup
  { component :: Text,
    amount :: HighPrecMoney
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DriverTxnInfo = DriverTxnInfo
  { id :: Id PaymentTransaction,
    status :: TransactionStatus
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data MetaDataReq = MetaDataReq
  { device :: Maybe Text,
    deviceOS :: Maybe Text,
    deviceDateTime :: Maybe UTCTime,
    appPermissions :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

createDriver ::
  ( HasCacheConfig r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    EsqDBFlow m r,
    EsqLocDBFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m
  ) =>
  SP.Person ->
  OnboardDriverReq ->
  m OnboardDriverRes
createDriver admin req = do
  let merchantId = admin.merchantId
  runRequestValidation validateOnboardDriverReq req
  let personEntity = req.person
  mobileNumberHash <- getDbHash personEntity.mobileNumber
  duplicateCheck
    (QVehicle.findByRegistrationNo req.vehicle.registrationNo)
    "Vehicle with this registration number already exists."
  duplicateCheck
    (QPerson.findByMobileNumberAndMerchant personEntity.mobileCountryCode mobileNumberHash merchantId)
    "Person with this mobile number already exists"
  person <- buildDriver req.person merchantId
  vehicle <- buildVehicle req.vehicle person.id merchantId
  metaData <- buildMetaData req.metaData person.id
  -- Esq.runTransaction $ do
  _ <- QPerson.create person
  _ <- QDFS.create $ makeIdleDriverFlowStatus person
  createDriverDetails person.id admin.id merchantId
  _ <- QVehicle.create vehicle
  -- Esq.runTransaction $ QMeta.create metaData
  _ <- QMeta.create metaData
  now <- getCurrentTime
  -- runInLocationDB $ QDriverLocation.create person.id initLatLong now admin.merchantId
  QDriverLocation.create person.id initLatLong now admin.merchantId
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> createDriver : ") (show person.id)
  org <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  decPerson <- decrypt person
  let mobNum = personEntity.mobileNumber
      mobCounCode = personEntity.mobileCountryCode
  smsCfg <- asks (.smsCfg)
  message <-
    MessageBuilder.buildWelcomeToPlatformMessage person.merchantId $
      MessageBuilder.WelcomeToPlatformMessageReq
        { orgName = org.name
        }
  sendInviteSms smsCfg (mobCounCode <> mobNum) message
    >>= SF.checkSmsResult
  let personAPIEntity = SP.makePersonAPIEntity decPerson
  return $ OnboardDriverRes personAPIEntity
  where
    duplicateCheck cond err = whenM (isJust <$> cond) $ throwError $ InvalidRequest err
    initLatLong = LatLong 0 0
    makeIdleDriverFlowStatus person =
      DDFS.DriverFlowStatus
        { personId = person.id,
          flowStatus = DDFS.IDLE,
          updatedAt = person.updatedAt
        }

createDriverDetails ::
  ( HasCacheConfig r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    EsqDBFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m
  ) =>
  Id SP.Person ->
  Id SP.Person ->
  Id DM.Merchant ->
  m ()
createDriverDetails personId adminId merchantId = do
  now <- getCurrentTime
  let driverInfo =
        DriverInfo.DriverInformation
          { driverId = personId,
            adminId = Just adminId,
            merchantId = Just merchantId,
            active = False,
            onRide = False,
            enabled = True,
            blocked = False,
            numOfLocks = 0,
            verified = False,
            subscribed = True,
            paymentPending = False,
            aadhaarVerified = False,
            referralCode = Nothing,
            canDowngradeToSedan = False,
            canDowngradeToHatchback = False,
            canDowngradeToTaxi = False,
            mode = Just DriverInfo.OFFLINE,
            lastEnabledOn = Just now,
            blockedReason = Nothing,
            blockExpiryTime = Nothing,
            createdAt = now,
            updatedAt = now,
            autoPayStatus = Nothing
          }
  _ <- QDriverStats.createInitialDriverStats driverId
  QDriverInformation.create driverInfo
  pure ()
  where
    driverId = cast personId

getInformation ::
  ( HasCacheConfig r,
    Redis.HedisFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r
  ) =>
  (Id SP.Person, Id DM.Merchant) ->
  m DriverInformationRes
getInformation (personId, merchantId) = do
  let driverId = cast personId
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  -- person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverStats <- runInReplica $ QDriverStats.findById driverId >>= fromMaybeM DriverInfoNotFound
  -- driverStats <- QDriverStats.findById driverId >>= fromMaybeM DriverInfoNotFound
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  driverReferralCode <- fmap (.referralCode) <$> QDR.findById (cast driverId)
  driverEntity <- buildDriverEntityRes (person, driverInfo)
  logDebug $ "alternateNumber-" <> show driverEntity.alternateNumber
  organization <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  pure $ makeDriverInformationRes driverEntity organization driverReferralCode driverStats

setActivity :: (CacheFlow m r, EsqDBFlow m r) => (Id SP.Person, Id DM.Merchant) -> Bool -> Maybe DriverInfo.DriverMode -> m APISuccess.APISuccess
setActivity (personId, _) isActive mode = do
  _ <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let driverId = cast personId
  when (isActive || (isJust mode && (mode == Just DriverInfo.SILENT || mode == Just DriverInfo.ONLINE))) $ do
    driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
    unless (driverInfo.enabled) $ throwError DriverAccountDisabled
    unless (driverInfo.subscribed) $ throwError DriverUnsubscribed
    unless (not driverInfo.blocked) $ throwError DriverAccountBlocked
  _ <- QDriverInformation.updateActivity driverId isActive mode
  driverStatus <- QDFS.getStatus personId >>= fromMaybeM (PersonNotFound personId.getId)
  logInfo $ "driverStatus " <> show driverStatus
  unless (driverStatus `notElem` [DDFS.IDLE, DDFS.ACTIVE, DDFS.SILENT]) $ do
    -- Esq.runTransaction $
    QDFS.updateStatus personId $
      DMode.getDriverStatus mode isActive
  pure APISuccess.Success

listDriver :: (EsqDBFlow m r, EncFlow m r, EsqDBReplicaFlow m r) => SP.Person -> Maybe Text -> Maybe Integer -> Maybe Integer -> m ListDriverRes
listDriver admin mbSearchString mbLimit mbOffset = do
  mbSearchStrDBHash <- getDbHash `traverse` mbSearchString
  personList <- QDriverInformation.findAllWithLimitOffsetByMerchantId mbSearchString mbSearchStrDBHash mbLimit mbOffset admin.merchantId
  respPersonList <- traverse buildDriverEntityRes personList
  return $ ListDriverRes respPersonList

buildDriverEntityRes :: (EsqDBReplicaFlow m r, EncFlow m r) => (SP.Person, DriverInformation) -> m DriverEntityRes
buildDriverEntityRes (person, driverInfo) = do
  vehicleMB <- QVehicle.findById person.id
  decMobNum <- mapM decrypt person.mobileNumber
  decaltMobNum <- mapM decrypt person.alternateMobileNumber
  mediaUrl <- forM person.faceImageId $ \mediaId -> do
    mediaEntry <- runInReplica $ MFQuery.findById mediaId >>= fromMaybeM (FileDoNotExist person.id.getId)
    -- mediaEntry <- MFQuery.findById mediaId >>= fromMaybeM (FileDoNotExist person.id.getId)
    return mediaEntry.url
  return $
    DriverEntityRes
      { id = person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        mobileNumber = decMobNum,
        rating = round <$> person.rating,
        linkedVehicle = SV.makeVehicleAPIEntity <$> vehicleMB,
        active = driverInfo.active,
        onRide = driverInfo.onRide,
        enabled = driverInfo.enabled,
        blocked = driverInfo.blocked,
        verified = driverInfo.verified,
        subscribed = driverInfo.subscribed,
        paymentPending = driverInfo.paymentPending,
        registeredAt = person.createdAt,
        language = person.language,
        alternateNumber = decaltMobNum,
        canDowngradeToSedan = driverInfo.canDowngradeToSedan,
        canDowngradeToHatchback = driverInfo.canDowngradeToHatchback,
        canDowngradeToTaxi = driverInfo.canDowngradeToTaxi,
        mode = driverInfo.mode,
        autoPayStatus = driverInfo.autoPayStatus,
        clientVersion = person.clientVersion,
        bundleVersion = person.bundleVersion,
        gender = Just person.gender,
        mediaUrl = mediaUrl
      }

changeDriverEnableState ::
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m,
    HasCacheConfig r
  ) =>
  SP.Person ->
  Id SP.Person ->
  Bool ->
  m APISuccess
changeDriverEnableState admin personId isEnabled = do
  person <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (person.merchantId == admin.merchantId) $ throwError Unauthorized
  _ <- QDriverInformation.updateEnabledState driverId isEnabled
  unless isEnabled $ void (QDriverInformation.updateActivity driverId False (Just DriverInfo.OFFLINE))
  unless isEnabled $ do
    Notify.notifyDriver person.merchantId FCM.ACCOUNT_DISABLED notificationTitle notificationMessage person.id person.deviceToken
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> changeDriverEnableState : ") (show (driverId, isEnabled))
  return Success
  where
    driverId = cast personId
    notificationTitle = "Account is disabled."
    notificationMessage = "Your account has been disabled. Contact support for more info."

deleteDriver :: (CacheFlow m r, EsqDBFlow m r, Redis.HedisFlow m r, EsqLocDBFlow m r) => SP.Person -> Id SP.Person -> m APISuccess
deleteDriver admin driverId = do
  driver <-
    QPerson.findById driverId
      >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  unless (driver.merchantId == admin.merchantId || driver.role == SP.DRIVER) $ throwError Unauthorized
  -- this function uses tokens from db, so should be called before transaction
  Auth.clearDriverSession driverId
  QDriverInformation.deleteById (cast driverId)
  -- Esq.runTransaction $ do
  QDriverStats.deleteById (cast driverId)
  QR.deleteByPersonId driverId
  QVehicle.deleteById driverId
  QDFS.deleteById driverId
  QPerson.deleteById driverId
  -- runInLocationDB $ QDriverLocation.deleteById driverId
  QDriverLocation.deleteById driverId
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> deleteDriver : ") (show driverId)
  return Success

updateDriver ::
  ( HasCacheConfig r,
    Redis.HedisFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r
  ) =>
  (Id SP.Person, Id DM.Merchant) ->
  UpdateDriverReq ->
  m UpdateDriverRes
updateDriver (personId, _) req = do
  runRequestValidation validateUpdateDriverReq req
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let updPerson =
        person{firstName = fromMaybe person.firstName req.firstName,
               middleName = req.middleName <|> person.middleName,
               lastName = req.lastName <|> person.lastName,
               deviceToken = req.deviceToken <|> person.deviceToken,
               language = req.language <|> person.language,
               clientVersion = req.clientVersion <|> person.clientVersion,
               bundleVersion = req.bundleVersion <|> person.bundleVersion,
               gender = fromMaybe person.gender req.gender,
               hometown = req.hometown <|> person.hometown,
               languagesSpoken = req.languagesSpoken <|> person.languagesSpoken
              }
  mVehicle <- QVehicle.findById personId
  checkIfCanDowngrade mVehicle
  driverInfo <- QDriverInformation.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  let updDriverInfo =
        driverInfo{canDowngradeToSedan = fromMaybe driverInfo.canDowngradeToSedan req.canDowngradeToSedan,
                   canDowngradeToHatchback = fromMaybe driverInfo.canDowngradeToHatchback req.canDowngradeToHatchback,
                   canDowngradeToTaxi = fromMaybe driverInfo.canDowngradeToTaxi req.canDowngradeToTaxi
                  }

  -- Esq.runTransaction $ do
  when (isJust req.vehicleName) $ QVehicle.updateVehicleName req.vehicleName personId
  _ <- QPerson.updatePersonRec personId updPerson
  _ <- QDriverInformation.updateDowngradingOptions person.id updDriverInfo.canDowngradeToSedan updDriverInfo.canDowngradeToHatchback updDriverInfo.canDowngradeToTaxi
  QDriverInformation.clearDriverInfoCache (cast personId)
  driverStats <- runInReplica $ QDriverStats.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  -- driverStats <- QDriverStats.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  driverEntity <- buildDriverEntityRes (updPerson, driverInfo)
  driverReferralCode <- fmap (.referralCode) <$> QDR.findById personId
  let merchantId = person.merchantId
  org <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  return $ makeDriverInformationRes driverEntity org driverReferralCode driverStats
  where
    checkIfCanDowngrade mVehicle = do
      case mVehicle of
        Just vehicle -> do
          when
            ( (vehicle.variant == SV.AUTO_RICKSHAW || vehicle.variant == SV.TAXI || vehicle.variant == SV.HATCHBACK)
                && (req.canDowngradeToSedan == Just True || req.canDowngradeToHatchback == Just True || req.canDowngradeToTaxi == Just True)
            )
            $ throwError $ InvalidRequest $ "Can't downgrade from " <> (show vehicle.variant)
          when (vehicle.variant == SV.SEDAN && (req.canDowngradeToSedan == Just True)) $
            throwError $ InvalidRequest "Driver with sedan can't downgrade to sedan"
          when (vehicle.variant == SV.SEDAN && (req.canDowngradeToTaxi == Just True)) $
            throwError $ InvalidRequest "Driver with sedan can't downgrade to Taxi"
          when (vehicle.variant == SV.SUV && (req.canDowngradeToTaxi == Just True)) $
            throwError $ InvalidRequest "Driver with SUV can't downgrade to Taxi"
          when (vehicle.variant == SV.TAXI_PLUS && (req.canDowngradeToSedan == Just True || req.canDowngradeToHatchback == Just True)) $
            throwError $ InvalidRequest "Driver with TAXI_PLUS can't downgrade to either sedan or hatchback"
        Nothing ->
          when (req.canDowngradeToSedan == Just True || req.canDowngradeToHatchback == Just True || req.canDowngradeToTaxi == Just True) $
            throwError $ InvalidRequest "Can't downgrade if not vehicle assigned to driver"

updateMetaData ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r
  ) =>
  (Id SP.Person, Id DM.Merchant) ->
  MetaDataReq ->
  m APISuccess
updateMetaData (personId, _) req = do
  _ <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  -- Esq.runTransaction $ do QMeta.updateMetaData personId req.device req.deviceOS req.deviceDateTime req.appPermissions
  QMeta.updateMetaData personId req.device req.deviceOS req.deviceDateTime req.appPermissions
  return Success

sendInviteSms ::
  ( MonadFlow m,
    CoreMetrics m,
    MonadReader r m
  ) =>
  SmsConfig ->
  Text ->
  Text ->
  m SMS.SubmitSmsRes
sendInviteSms smsCfg phoneNumber message = do
  let url = smsCfg.url
  let smsCred = smsCfg.credConfig
  let sender = smsCfg.sender
  SF.submitSms
    url
    SMS.SubmitSms
      { SMS.username = smsCred.username,
        SMS.password = smsCred.password,
        SMS.from = sender,
        SMS.to = phoneNumber,
        SMS.text = message
      }

buildDriver :: (EncFlow m r) => CreatePerson -> Id DM.Merchant -> m SP.Person
buildDriver req merchantId = do
  pid <- generateGUID
  now <- getCurrentTime
  mobileNumber <- Just <$> encrypt req.mobileNumber
  return
    SP.Person
      { -- only these below will be updated in the person table. if you want to add something extra please add in queries also
        SP.id = pid,
        SP.firstName = req.firstName,
        SP.middleName = req.middleName,
        SP.lastName = req.lastName,
        SP.role = SP.DRIVER,
        SP.gender = SP.UNKNOWN,
        SP.hometown = Nothing,
        SP.languagesSpoken = Nothing,
        SP.email = Nothing,
        SP.passwordHash = Nothing,
        SP.identifier = Nothing,
        SP.identifierType = SP.MOBILENUMBER,
        SP.unencryptedMobileNumber = Just req.mobileNumber,
        SP.mobileNumber = mobileNumber,
        SP.mobileCountryCode = Just req.mobileCountryCode,
        SP.isNew = True,
        SP.onboardedFromDashboard = False,
        SP.rating = Nothing,
        SP.deviceToken = Nothing,
        SP.language = Nothing,
        SP.merchantId = merchantId,
        SP.description = Nothing,
        SP.createdAt = now,
        SP.updatedAt = now,
        SP.clientVersion = Nothing,
        SP.whatsappNotificationEnrollStatus = Nothing,
        SP.bundleVersion = Nothing,
        SP.alternateMobileNumber = Nothing,
        SP.unencryptedAlternateMobileNumber = Nothing,
        SP.faceImageId = Nothing
      }

buildVehicle :: MonadFlow m => CreateVehicle -> Id SP.Person -> Id DM.Merchant -> m SV.Vehicle
buildVehicle req personId merchantId = do
  now <- getCurrentTime
  return $
    SV.Vehicle
      { SV.driverId = personId,
        SV.capacity = Just req.capacity,
        SV.category = Just req.category,
        SV.make = Nothing,
        SV.model = req.model,
        SV.size = Nothing,
        SV.merchantId = merchantId,
        SV.variant = req.variant,
        SV.color = req.color,
        SV.vehicleName = Nothing,
        SV.energyType = Nothing,
        SV.registrationNo = req.registrationNo,
        SV.registrationCategory = Nothing,
        SV.vehicleClass = "3WT",
        SV.createdAt = now,
        SV.updatedAt = now
      }

buildMetaData :: MonadFlow m => MetaDataReq -> Id SP.Person -> m MD.MetaData
buildMetaData req personId = do
  now <- getCurrentTime
  return $
    MD.MetaData
      { MD.driverId = personId,
        MD.device = req.device,
        MD.deviceOS = req.deviceOS,
        MD.deviceDateTime = req.deviceDateTime,
        MD.appPermissions = req.appPermissions,
        MD.createdAt = now,
        MD.updatedAt = now
      }

makeDriverInformationRes :: DriverEntityRes -> DM.Merchant -> Maybe (Id DR.DriverReferral) -> DriverStats -> DriverInformationRes
makeDriverInformationRes DriverEntityRes {..} org referralCode driverStats =
  DriverInformationRes
    { organization = DM.makeMerchantAPIEntity org,
      referralCode = referralCode <&> (.getId),
      numberOfRides = driverStats.totalRides,
      ..
    }

getNearbySearchRequests ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    HasCacheConfig r
  ) =>
  (Id SP.Person, Id DM.Merchant) ->
  m GetNearbySearchRequestsRes
getNearbySearchRequests (driverId, merchantId) = do
  nearbyReqs <- runInReplica $ QSRD.findByDriver driverId
  -- nearbyReqs <- QSRD.findByDriver driverId
  transporterConfig <- CQTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  let cancellationScoreRelatedConfig = mkCancellationScoreRelatedConfig transporterConfig
  cancellationRatio <- DP.getLatestCancellationRatio cancellationScoreRelatedConfig merchantId (cast driverId)
  searchRequestForDriverAPIEntity <- mapM (buildSearchRequestForDriverAPIEntity cancellationRatio cancellationScoreRelatedConfig transporterConfig) nearbyReqs
  return $ GetNearbySearchRequestsRes searchRequestForDriverAPIEntity
  where
    buildSearchRequestForDriverAPIEntity cancellationRatio cancellationScoreRelatedConfig transporterConfig nearbyReq = do
      let searchTryId = nearbyReq.searchTryId
      searchTry <- runInReplica $ QST.findById searchTryId >>= fromMaybeM (SearchTryNotFound searchTryId.getId)
      -- searchTry <- QST.findById searchTryId >>= fromMaybeM (SearchTryNotFound searchTryId.getId)
      searchRequest <- runInReplica $ QSR.findById searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
      -- searchRequest <- QSR.findById searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
      bapMetadata <- CQSM.findById (Id searchRequest.bapId)
      popupDelaySeconds <- DP.getPopupDelay searchRequest.providerId (cast driverId) cancellationRatio cancellationScoreRelatedConfig transporterConfig.defaultPopupDelay
      return $ makeSearchRequestForDriverAPIEntity nearbyReq searchRequest searchTry bapMetadata popupDelaySeconds (Seconds 0) searchTry.vehicleVariant -- Seconds 0 as we don't know where he/she lies within the driver pool, anyways this API is not used in prod now.
    mkCancellationScoreRelatedConfig :: TransporterConfig -> CancellationScoreRelatedConfig
    mkCancellationScoreRelatedConfig tc = CancellationScoreRelatedConfig tc.popupDelayToAddAsPenalty tc.thresholdCancellationScore tc.minRidesForCancellationScore

isAllowedExtraFee :: DriverExtraFeeBounds -> Money -> Bool
isAllowedExtraFee extraFee val = extraFee.minFee <= val && val <= extraFee.maxFee

offerQuoteLockKey :: Id Person -> Text
offerQuoteLockKey driverId = "Driver:OfferQuote:DriverId-" <> driverId.getId

-- DEPRECATED
offerQuote ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    HasPrettyLogger m r,
    HasField "driverQuoteExpirationSeconds" r NominalDiffTime,
    HasField "coreVersion" r Text,
    HasField "nwAddress" r BaseUrl,
    HasField "driverUnlockDelay" r Seconds,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m,
    HasPrettyLogger m r,
    EventStreamFlow m r
  ) =>
  (Id SP.Person, Id DM.Merchant) ->
  DriverOfferReq ->
  m APISuccess
offerQuote (driverId, merchantId) DriverOfferReq {..} = do
  let response = Accept
  respondQuote (driverId, merchantId) DriverRespondReq {searchRequestId = Nothing, searchTryId = Just searchRequestId, ..}

respondQuote ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    HasPrettyLogger m r,
    HasField "driverQuoteExpirationSeconds" r NominalDiffTime,
    HasField "coreVersion" r Text,
    HasField "nwAddress" r BaseUrl,
    HasField "driverUnlockDelay" r Seconds,
    -- HasField "searchRequestId" r (Id DST.SearchTry),
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m,
    HasPrettyLogger m r,
    L.MonadFlow m,
    EventStreamFlow m r
  ) =>
  (Id SP.Person, Id DM.Merchant) ->
  DriverRespondReq ->
  m APISuccess
respondQuote (driverId, _) req = do
  Redis.whenWithLockRedis (offerQuoteLockKey driverId) 60 $ do
    searchTryId <- req.searchRequestId <|> req.searchTryId & fromMaybeM (InvalidRequest "searchTryId field is not present.")
    searchTry <- QST.findById searchTryId >>= fromMaybeM (SearchTryNotFound searchTryId.getId)
    now <- getCurrentTime
    when (searchTry.validTill < now) $ throwError SearchRequestExpired
    searchReq <- QSR.findById searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
    let mbOfferedFare = req.offeredFare
    organization <- CQM.findById searchReq.providerId >>= fromMaybeM (MerchantDoesNotExist searchReq.providerId.getId)
    driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    driverInfo <- QDriverInformation.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
    when driverInfo.onRide $ throwError DriverOnRide
    sReqFD <-
      QSRD.findByDriverAndSearchTryId driverId searchTry.id
        >>= fromMaybeM NoSearchRequestForDriver
    driverFCMPulledList <-
      case req.response of
        Pulled -> throwError UnexpectedResponseValue
        Accept -> do
          when (searchReq.autoAssignEnabled == Just True) $ do
            whenM (CS.isSearchTryCancelled searchTryId) $
              throwError (InternalError "SEARCH_TRY_CANCELLED")
            CS.markSearchTryAsAssigned searchTryId
          logDebug $ "offered fare: " <> show req.offeredFare
          whenM thereAreActiveQuotes (throwError FoundActiveQuotes)
          when (sReqFD.response == Just Reject) (throwError QuoteAlreadyRejected)
          quoteLimit <- getQuoteLimit searchReq.providerId searchReq.estimatedDistance
          quoteCount <- runInReplica $ QDrQt.countAllBySTId searchTry.id
          -- quoteCount <- QDrQt.countAllBySTId searchTry.id
          when (quoteCount >= quoteLimit) (throwError QuoteAlreadyRejected)
          farePolicy <- getFarePolicy organization.id sReqFD.vehicleVariant searchReq.area
          let driverExtraFeeBounds = DFarePolicy.findDriverExtraFeeBoundsByDistance searchReq.estimatedDistance <$> farePolicy.driverExtraFeeBounds
          whenJust mbOfferedFare $ \off ->
            whenJust driverExtraFeeBounds $ \driverExtraFeeBounds' ->
              unless (isAllowedExtraFee driverExtraFeeBounds' off) $
                throwError $ NotAllowedExtraFee $ show off
          fareParams <-
            calculateFareParameters
              CalculateFareParametersParams
                { farePolicy = farePolicy,
                  distance = searchReq.estimatedDistance,
                  rideTime = sReqFD.startTime,
                  waitingTime = Nothing,
                  driverSelectedFare = mbOfferedFare,
                  customerExtraFee = searchTry.customerExtraFee,
                  nightShiftCharge = Nothing
                }
          driverQuote <- buildDriverQuote driver searchReq sReqFD searchTry.estimateId fareParams
          triggerQuoteEvent QuoteEventData {quote = driverQuote}
          -- Esq.runTransaction $ do
          _ <- QDrQt.create driverQuote
          _ <- QSRD.updateDriverResponse sReqFD.id req.response
          QDFS.updateStatus sReqFD.driverId DDFS.OFFERED_QUOTE {quoteId = driverQuote.id, validTill = driverQuote.validTill}
          let shouldPullFCMForOthers = (quoteCount + 1) >= quoteLimit || (searchReq.autoAssignEnabled == Just True)
          driverFCMPulledList <- if shouldPullFCMForOthers then QSRD.findAllActiveWithoutRespBySearchTryId searchTryId else pure []
          -- Adding +1 in quoteCount because one more quote added above (QDrQt.create driverQuote)
          sendRemoveRideRequestNotification driverFCMPulledList organization.id driverQuote
          sendDriverOffer organization searchReq searchTry driverQuote
          pure driverFCMPulledList
        Reject -> do
          _ <- QSRD.updateDriverResponse sReqFD.id req.response
          pure []
    DS.driverScoreEventHandler $ buildDriverRespondEventPayload searchTry.id searchReq.providerId driverFCMPulledList
  pure Success
  where
    buildDriverRespondEventPayload searchTryId merchantId restActiveDriverSearchReqs =
      DST.OnDriverAcceptingSearchRequest
        { restDriverIds = map (.driverId) restActiveDriverSearchReqs,
          response = req.response,
          ..
        }

    buildDriverQuote ::
      (MonadFlow m, MonadReader r m, HasField "driverQuoteExpirationSeconds" r NominalDiffTime) =>
      SP.Person ->
      DSR.SearchRequest ->
      SearchRequestForDriver ->
      Id DEstimate.Estimate ->
      Fare.FareParameters ->
      m DDrQuote.DriverQuote
    buildDriverQuote driver searchReq sd estimateId fareParams = do
      guid <- generateGUID
      now <- getCurrentTime
      driverQuoteExpirationSeconds <- asks (.driverQuoteExpirationSeconds)
      let estimatedFare = fareSum fareParams
      pure
        DDrQuote.DriverQuote
          { id = guid,
            requestId = searchReq.id,
            searchTryId = sd.searchTryId,
            searchRequestForDriverId = Just sd.id,
            driverId,
            driverName = driver.firstName,
            driverRating = driver.rating,
            status = DDrQuote.Active,
            vehicleVariant = sd.vehicleVariant,
            distance = searchReq.estimatedDistance,
            distanceToPickup = sd.actualDistanceToPickup,
            durationToPickup = sd.durationToPickup,
            createdAt = now,
            updatedAt = now,
            validTill = addUTCTime driverQuoteExpirationSeconds now,
            providerId = searchReq.providerId,
            estimatedFare,
            fareParams,
            estimateId,
            specialLocationTag = searchReq.specialLocationTag
          }
    thereAreActiveQuotes = do
      driverUnlockDelay <- asks (.driverUnlockDelay)
      activeQuotes <- QDrQt.findActiveQuotesByDriverId driverId driverUnlockDelay
      logPretty DEBUG ("active quotes for driverId = " <> driverId.getId) activeQuotes
      pure $ not $ null activeQuotes
    getQuoteLimit merchantId dist = do
      driverPoolCfg <- DP.getDriverPoolConfig merchantId dist
      pure $ fromIntegral driverPoolCfg.driverQuoteLimit
    sendRemoveRideRequestNotification driverSearchReqs orgId driverQuote = do
      for_ driverSearchReqs $ \driverReq -> do
        -- Esq.runNoTransaction $ do
        _ <- QSRD.updateDriverResponse driverReq.id Pulled
        driver_ <- runInReplica $ QPerson.findById driverReq.driverId >>= fromMaybeM (PersonNotFound driverReq.driverId.getId)
        -- driver_ <- QPerson.findById driverReq.driverId >>= fromMaybeM (PersonNotFound driverReq.driverId.getId)
        Notify.notifyDriverClearedFare orgId driverReq.driverId driverReq.searchTryId driverQuote.estimatedFare driver_.deviceToken

getStats ::
  (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r) =>
  (Id SP.Person, Id DM.Merchant) ->
  Day ->
  m DriverStatsRes
getStats (driverId, merchantId) date = do
  transporterConfig <- CQTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  rides <- runInReplica $ QRide.getRidesForDate driverId date transporterConfig.timeDiffFromUtc
  -- rides <- QRide.getRidesForDate driverId date transporterConfig.timeDiffFromUtc
  let fareParamId = mapMaybe (.fareParametersId) rides
  fareParameters <- (runInReplica . QFP.findAllIn) fareParamId
  -- fareParameters <- QFP.findAllIn fareParamId
  return $
    DriverStatsRes
      { totalRidesOfDay = length rides,
        totalEarningsOfDay = sum (mapMaybe (.fare) rides),
        bonusEarning =
          let (driverSelFares, customerExtFees) = (mapMaybe (.driverSelectedFare) fareParameters, mapMaybe (.customerExtraFee) fareParameters)
              driverSelFares' = getMoney <$> driverSelFares
              customerExtFees' = getMoney <$> customerExtFees
              deadKmFare' =
                ( (getMoney <$>)
                    . mapMaybe
                      ( \x -> case fareParametersDetails x of
                          ProgressiveDetails det -> Just (deadKmFare det)
                          SlabDetails _ -> Nothing
                      )
                )
                  fareParameters
           in Money $ GHCL.sum driverSelFares' + GHCL.sum customerExtFees' + GHCL.sum deadKmFare'
      }

driverPhotoUploadHitsCountKey :: Id SP.Person -> Text
driverPhotoUploadHitsCountKey driverId = "BPP:ProfilePhoto:verify:" <> getId driverId <> ":hitsCount"

driverPhotoUpload :: (Id SP.Person, Id DM.Merchant) -> DriverPhotoUploadReq -> Flow APISuccess
driverPhotoUpload (driverId, merchantId) DriverPhotoUploadReq {..} = do
  checkSlidingWindowLimit (driverPhotoUploadHitsCountKey driverId)
  person <- runInReplica $ QPerson.findById driverId >>= fromMaybeM (PersonNotFound (getId driverId))
  -- person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound (getId driverId))
  encImage <- L.runIO $ base64Encode <$> BS.readFile image
  imageExtension <- validateContentType
  let req = IF.FaceValidationReq {file = encImage}
  _ <- validateFaceImage merchantId req
  filePath <- createFilePath (getId driverId) fileType imageExtension
  transporterConfig <- CQTC.findByMerchantId (person.merchantId) >>= fromMaybeM (TransporterConfigNotFound (getId (person.merchantId)))
  let fileUrl =
        transporterConfig.mediaFileUrlPattern
          & T.replace "<DOMAIN>" "driver-profile-picture"
          & T.replace "<FILE_PATH>" filePath
  result <- try @_ @SomeException $ S3.put (T.unpack filePath) encImage
  case result of
    Left err -> throwError $ InternalError ("S3 Upload Failed: " <> show err)
    Right _ -> do
      case person.faceImageId of
        Just mediaFileId -> do
          -- Esq.runTransaction $ do
          QPerson.updateMediaId driverId Nothing
          MFQuery.deleteById mediaFileId
        Nothing -> return ()
      createMediaEntry driverId Common.AddLinkAsMedia {url = fileUrl, fileType}
  where
    validateContentType = do
      case fileType of
        Common.Image -> case reqContentType of
          "image/png" -> pure "png"
          "image/jpeg" -> pure "jpg"
          _ -> throwError $ FileFormatNotSupported reqContentType
        _ -> throwError $ FileFormatNotSupported reqContentType

createFilePath ::
  (MonadTime m, MonadReader r m, HasField "s3Env" r (S3.S3Env m)) =>
  Text ->
  Common.FileType ->
  Text ->
  m Text
createFilePath driverId fileType imageExtension = do
  pathPrefix <- asks (.s3Env.pathPrefix)
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
  return
    ( pathPrefix <> "/driver-profile-picture/" <> "driver-" <> driverId <> "/"
        <> show fileType
        <> "/"
        <> fileName
        <> imageExtension
    )

createMediaEntry :: Id SP.Person -> Common.AddLinkAsMedia -> Flow APISuccess
createMediaEntry driverId Common.AddLinkAsMedia {..} = do
  fileEntity <- mkFile url
  -- Esq.runTransaction $ do
  MFQuery.create fileEntity
  QPerson.updateMediaId driverId (Just fileEntity.id)
  return Success
  where
    mkFile fileUrl = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        Domain.MediaFile
          { id,
            _type = Domain.Image,
            url = fileUrl,
            createdAt = now
          }

makeAlternatePhoneNumberKey :: Id SP.Person -> Text
makeAlternatePhoneNumberKey id = "DriverAlternatePhoneNumber:PersonId-" <> id.getId

makeAlternateNumberOtpKey :: Id SP.Person -> Text
makeAlternateNumberOtpKey id = "DriverAlternateNumberOtp:PersonId-" <> id.getId

makeAlternateNumberAttemptsKey :: Id SP.Person -> Text
makeAlternateNumberAttemptsKey id = "DriverAlternateNumberAttempts:PersonId-" <> id.getId

makeAlternateNumberVerifiedKey :: Id SP.Person -> Text
makeAlternateNumberVerifiedKey id = "DriverAlternateNumberVerified:PersonId-" <> id.getId

cacheAlternateNumberInfo :: (CacheFlow m r) => Id SP.Person -> Text -> Text -> Int -> Bool -> m ()
cacheAlternateNumberInfo personId phoneNumber otp attemptsLeft verified = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let alternatePhoneNumberKey = makeAlternatePhoneNumberKey personId
      alternateNumberOtpKey = makeAlternateNumberOtpKey personId
      alternateNumberAttemptsKey = makeAlternateNumberAttemptsKey personId
      alternateNumberVerifiedKey = makeAlternateNumberVerifiedKey personId

  Redis.setExp alternatePhoneNumberKey phoneNumber expTime
  Redis.setExp alternateNumberOtpKey otp expTime
  Redis.setExp alternateNumberAttemptsKey attemptsLeft expTime
  Redis.setExp alternateNumberVerifiedKey verified expTime

invalidateAlternateNoCache :: (CacheFlow m r) => Id SP.Person -> m ()
invalidateAlternateNoCache personId = do
  let alternatePhoneNumberKey = makeAlternatePhoneNumberKey personId
      alternateNumberOtpKey = makeAlternateNumberOtpKey personId
      alternateNumberAttemptsKey = makeAlternateNumberAttemptsKey personId

  Redis.del alternatePhoneNumberKey
  Redis.del alternateNumberOtpKey
  Redis.del alternateNumberAttemptsKey

validationCheck :: Validate DriverAlternateNumberReq
validationCheck DriverAlternateNumberReq {..} = do
  sequenceA_
    [ validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode,
      validateField "alternateNumber" alternateNumber P.mobileNumber
    ]

validate ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    HasCacheConfig r,
    CoreMetrics m,
    HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig]
  ) =>
  (Id SP.Person, Id DM.Merchant) ->
  DriverAlternateNumberReq ->
  m DriverAlternateNumberRes
validate (personId, _) phoneNumber = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  -- person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  altNoAttempt <- runInReplica $ QRegister.getAlternateNumberAttempts personId
  -- altNoAttempt <- QRegister.getAlternateNumberAttempts personId
  runRequestValidation validationCheck phoneNumber
  mobileNumberHash <- getDbHash phoneNumber.alternateNumber
  merchant <- CQM.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  mbPerson <- QPerson.findByMobileNumberAndMerchant phoneNumber.mobileCountryCode mobileNumberHash person.merchantId
  deleteOldPersonCheck <- case mbPerson of
    Nothing -> return False
    Just oldPerson -> do
      when (oldPerson.id == person.id) $ throwError $ InvalidRequest "Alternate number already linked"
      DeleteDriverOnCheck.validateDriver merchant oldPerson
  logDebug $ "Delete Driver Check" <> show deleteOldPersonCheck
  when deleteOldPersonCheck $ throwError $ InvalidRequest "Alternate number can't be validated"
  smsCfg <- asks (.smsCfg)
  let useFakeOtpM = useFakeSms smsCfg
  otpCode <- maybe generateOTPCode (return . show) useFakeOtpM
  whenNothing_ useFakeOtpM $ do
    let otpHash = smsCfg.credConfig.otpHash
    let altPhoneNumber = phoneNumber.mobileCountryCode <> phoneNumber.alternateNumber
    let sender = smsCfg.sender
    withLogTag ("personId_" <> getId person.id) $ do
      message <-
        MessageBuilder.buildSendAlternateNumberOTPMessage person.merchantId $
          MessageBuilder.BuildSendOTPMessageReq
            { otp = otpCode,
              hash = otpHash
            }
      Sms.sendSMS person.merchantId (Sms.SendSMSReq message altPhoneNumber sender)
        >>= Sms.checkSmsResult
  let verified = False
  cacheAlternateNumberInfo personId phoneNumber.alternateNumber otpCode altNoAttempt verified
  return $ DriverAlternateNumberRes {attempts = altNoAttempt}

validateAuthVerifyReq :: Validate DriverAlternateNumberOtpReq
validateAuthVerifyReq DriverAlternateNumberOtpReq {..} =
  sequenceA_
    [ validateField "otp" otp $ ExactLength 4 `And` star P.digit
    ]

verifyHitsCountKey :: Id SP.Person -> Text
verifyHitsCountKey id = "Driver:AlternateNumberOtp:verify:" <> getId id <> ":hitsCount"

verifyAuth ::
  (Id SP.Person, Id DM.Merchant) ->
  DriverAlternateNumberOtpReq ->
  Flow APISuccess
verifyAuth (personId, _) req = do
  Redis.whenWithLockRedis (makeAlternatePhoneNumberKey personId) 60 $ do
    runRequestValidation validateAuthVerifyReq req
    person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    -- person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    checkSlidingWindowLimit (verifyHitsCountKey personId)
    verified <- Redis.get (makeAlternateNumberVerifiedKey personId) >>= fromMaybeM (InvalidRequest "Verified not found")
    when verified $ throwError $ AuthBlocked "Already verified."
    altMobNo <- Redis.get (makeAlternatePhoneNumberKey personId) >>= fromMaybeM (InvalidRequest "Alternate Number not found")
    val <- Redis.get (makeAlternateNumberOtpKey personId)
    authValueHash <- case val of
      Nothing -> throwError $ InternalError "Auth not found"
      Just a -> return a
    unless (authValueHash == req.otp) $ throwError InvalidAuthData
    encNewNum <- encrypt altMobNo
    let driver =
          person
            { SP.unencryptedAlternateMobileNumber = Just altMobNo,
              SP.alternateMobileNumber = Just encNewNum
            }
        mobileCountryCode = fromMaybe "+91" person.mobileCountryCode
    mobileNumberHash <- getDbHash altMobNo
    mbPerson <- QPerson.findByMobileNumberAndMerchant mobileCountryCode mobileNumberHash person.merchantId
    -- Esq.runTransaction $ do
    _ <- QPerson.updateAlternateMobileNumberAndCode driver
    expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
    Redis.setExp (makeAlternateNumberVerifiedKey personId) True expTime
    whenJust mbPerson $ \oldPerson -> do
      merchant <- CQM.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
      void $ DeleteDriverOnCheck.deleteDriver merchant.shortId (cast oldPerson.id)
    invalidateAlternateNoCache personId
  return Success

resendOtp ::
  ( HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig],
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CacheFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r
  ) =>
  (Id SP.Person, Id DM.Merchant) ->
  DriverAlternateNumberReq ->
  m ResendAuth
resendOtp (personId, merchantId) req = do
  attemptsLeft :: Int <- do
    res <- Redis.get (makeAlternateNumberAttemptsKey personId)
    return $ fromMaybe 0 res
  logDebug $ "attemptsLeft" <> show attemptsLeft
  unless (attemptsLeft > 0) $ throwError $ AuthBlocked "Attempts limit exceed."
  smsCfg <- asks (.smsCfg)
  let altNumber = req.alternateNumber
      counCode = req.mobileCountryCode
  otpCode <-
    Redis.get (makeAlternateNumberOtpKey personId) >>= \case
      Nothing -> do
        let fakeOtp = show <$> useFakeSms smsCfg
        newOtp <- maybe generateOTPCode return fakeOtp
        expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
        Redis.setExp (makeAlternateNumberOtpKey personId) newOtp expTime
        return newOtp
      Just a -> return a
  let otpHash = smsCfg.credConfig.otpHash
      altphoneNumber = counCode <> altNumber
      sender = smsCfg.sender
  withLogTag ("personId_" <> getId personId) $ do
    message <-
      MessageBuilder.buildSendAlternateNumberOTPMessage merchantId $
        MessageBuilder.BuildSendOTPMessageReq
          { otp = otpCode,
            hash = otpHash
          }
    Sms.sendSMS merchantId (Sms.SendSMSReq message altphoneNumber sender)
      >>= Sms.checkSmsResult
  updAttempts <- Redis.decrby (makeAlternateNumberAttemptsKey personId) 1
  let updAttempt = fromIntegral updAttempts
  return $ ResendAuth {auth = otpCode, attemptsLeft = updAttempt}

remove ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    HasCacheConfig r,
    CoreMetrics m
  ) =>
  (Id SP.Person, Id DM.Merchant) ->
  m APISuccess
remove (personId, _) = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  -- person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let driver =
        person
          { SP.unencryptedAlternateMobileNumber = Nothing,
            SP.alternateMobileNumber = Nothing
          }
  -- Esq.runTransaction $ do
  _ <- QPerson.updateAlternateMobileNumberAndCode driver
  return Success

getDriverPayments :: (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r) => (Id SP.Person, Id DM.Merchant) -> Maybe Day -> Maybe Day -> Maybe DDF.DriverFeeStatus -> Maybe Int -> Maybe Int -> m [DriverPaymentHistoryResp]
getDriverPayments (personId, merchantId_) mbFrom mbTo mbStatus mbLimit mbOffset = do
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit -- TODO move to common code
      offset = fromMaybe 0 mbOffset
      defaultFrom = fromMaybe (fromGregorian 2020 1 1) mbFrom
  transporterConfig <- CQTC.findByMerchantId merchantId_ >>= fromMaybeM (TransporterConfigNotFound merchantId_.getId)
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let today = utctDay now
      from = fromMaybe defaultFrom mbFrom
      to = fromMaybe today mbTo
  let windowStartTime = UTCTime from 0
      windowEndTime = addUTCTime (86399 + transporterConfig.driverPaymentCycleDuration) (UTCTime to 0)
  driverFees <- runInReplica $ QDF.findWindowsWithStatus personId windowStartTime windowEndTime mbStatus limit offset
  -- driverFees <- QDF.findWindowsWithStatus personId windowStartTime windowEndTime mbStatus limit offset
  mapM buildPaymentResp driverFees
  where
    maxLimit = 20
    defaultLimit = 10

    buildPaymentResp DDF.DriverFee {..} = do
      let date = utctDay startTime
          driverFeeId = id
          totalRides = numRides
          charges = round $ fromIntegral govtCharges + fromIntegral platformFee.fee + platformFee.cgst + platformFee.sgst
          chargesBreakup = mkChargesBreakup govtCharges platformFee.fee platformFee.cgst platformFee.sgst
      transactionDetails <- findAllByOrderId (cast id)
      let txnInfo = map mkDriverTxnInfo transactionDetails
      return DriverPaymentHistoryResp {..}

    mkDriverTxnInfo PaymentTransaction {..} = DriverTxnInfo {..}

    mkChargesBreakup govtCharges platformFee cgst sgst =
      [ DriverPaymentBreakup
          { component = "Government Charges",
            amount = fromIntegral govtCharges
          },
        DriverPaymentBreakup
          { component = "Platform Fee",
            amount = fromIntegral platformFee
          },
        DriverPaymentBreakup
          { component = "CGST",
            amount = cgst
          },
        DriverPaymentBreakup
          { component = "SGST",
            amount = sgst
          }
      ]

data DriverDuesResp = DriverDuesResp
  { dues :: [DriverDuesEntity],
    totalDue :: HighPrecMoney,
    overdueThreshold :: HighPrecMoney
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)

data DriverDuesEntity = DriverDuesEntity
  { date :: UTCTime,
    amount :: Money,
    earnings :: Money,
    offers :: [OfferEntity]
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)

data OfferEntity = OfferEntity
  { title :: Maybe Text,
    description :: Maybe Text,
    tnc :: Maybe Text
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)

getDriverDues :: (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r) => (Id SP.Person, Id DM.Merchant) -> m DriverDuesResp
getDriverDues (personId, _merchantId) = do
  driverPlan <- Esq.runInReplica $ QDPlan.findByDriverId personId >>= fromMaybeM (NoCurrentPlanForDriver personId.getId)
  plan <- Esq.runInReplica $ QPD.findByIdAndPaymentMode driverPlan.planId driverPlan.planType >>= fromMaybeM (PlanNotFound driverPlan.planId.getId)
  dueInvoices <- runInReplica $ QDF.findAllPendingAndDueDriverFeeByDriverId personId
  return $
    DriverDuesResp
      { dues = buildDriverDuesEntity <$> dueInvoices,
        totalDue = sum $ map (\dueInvoice -> fromIntegral dueInvoice.govtCharges + fromIntegral dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst) dueInvoices,
        overdueThreshold = plan.maxCreditLimit
      }
  where
    buildDriverDuesEntity DDF.DriverFee {..} =
      DriverDuesEntity
        { date = createdAt,
          amount = round $ fromIntegral govtCharges + fromIntegral platformFee.fee + platformFee.cgst + platformFee.sgst,
          earnings = totalEarnings,
          offers = []
        }
