{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Notifications where

import Data.Aeson (object)
import qualified Data.Text as T
import Data.Time hiding (secondsToNominalDiffTime)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import Domain.Types.Estimate (Estimate)
import qualified Domain.Types.Estimate as DEst
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Domain.Types.Merchant.MerchantServiceUsageConfig (MerchantServiceUsageConfig)
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import qualified Domain.Types.NotificationSoundsConfig as NSC
import Domain.Types.Person as Person
import Domain.Types.Quote (makeQuoteAPIEntity)
import qualified Domain.Types.Quote as DQuote
import Domain.Types.RegistrationToken as RegToken
import qualified Domain.Types.Ride as SRide
import Domain.Types.RiderConfig as DRC
import Domain.Types.SearchRequest as SearchRequest
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification as Notification
import Kernel.External.Types (ServiceFlow)
import qualified Kernel.Prelude as Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto hiding (count, runInReplica)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.CachedQueries.FollowRide as CQFollowRide
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.NotificationSoundsConfig as SQNSC
import qualified Storage.Queries.Person as Person
import Storage.Queries.Person.PersonDefaultEmergencyNumber as QPDEN
import qualified Storage.Queries.Person.PersonDisability as PD
import qualified Storage.Queries.SearchRequest as QSearchReq
import Tools.Error
import qualified Tools.SMS as Sms

data EmptyDynamicParam = EmptyDynamicParam

instance ToJSON EmptyDynamicParam where
  toJSON EmptyDynamicParam = object []

notifyPerson ::
  ( ServiceFlow m r,
    ToJSON a,
    ToJSON b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  Notification.NotificationReq a b ->
  m ()
notifyPerson = runWithServiceConfig Notification.notifyPerson (.notifyPerson)

notifyPersonWithMutableContent ::
  ( ServiceFlow m r,
    ToJSON a,
    ToJSON b
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  Notification.NotificationReq a b ->
  m ()
notifyPersonWithMutableContent = runWithServiceConfig Notification.notifyPersonWithMutableContent (.notifyPerson)

runWithServiceConfig ::
  ServiceFlow m r =>
  (Notification.NotificationServiceConfig -> req -> m resp) ->
  (MerchantServiceUsageConfig -> Notification.NotificationService) ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  req ->
  m resp
runWithServiceConfig func getCfg merchantId merchantOperatingCityId req = do
  merchantConfig <- QMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
  merchantNotificationServiceConfig <-
    QMSC.findByMerchantIdAndService merchantId (DMSC.NotificationService $ getCfg merchantConfig)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "notification" (show $ getCfg merchantConfig))
  case merchantNotificationServiceConfig.serviceConfig of
    DMSC.NotificationServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown ServiceConfig"

notifyOnDriverOfferIncoming ::
  ServiceFlow m r =>
  Id Estimate ->
  [DQuote.Quote] ->
  Person.Person ->
  m ()
notifyOnDriverOfferIncoming estimateId quotes person = do
  let merchantOperatingCityId = person.merchantOperatingCityId
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.DRIVER_QUOTE_INCOMING merchantOperatingCityId
  let notificationSound = maybe Nothing NSC.defaultSound notificationSoundFromConfig
  let notificationData =
        Notification.NotificationReq
          { category = Notification.DRIVER_QUOTE_INCOMING,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product estimateId.getId $ map makeQuoteAPIEntity quotes,
            body = body,
            title = title,
            dynamicParams = EmptyDynamicParam,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
            ttl = Nothing,
            sound = notificationSound
          }
      title = "New driver offers incoming!"
      body =
        unwords
          [ "There are new driver offers!",
            "Check the app for details"
          ]
  notifyPerson person.merchantId merchantOperatingCityId notificationData

newtype RideAssignedParam = RideAssignedParam
  { driverName :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notifyOnRideAssigned ::
  ServiceFlow m r =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyOnRideAssigned booking ride = do
  let personId = booking.riderId
      rideId = ride.id
      driverName = ride.driverName
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.DRIVER_ASSIGNMENT merchantOperatingCityId
  tag <- getDisabilityTag person.hasDisability personId
  notificationSound <- getNotificationSound tag notificationSoundFromConfig
  let notificationData =
        Notification.NotificationReq
          { category = Notification.DRIVER_ASSIGNMENT,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product rideId.getId (),
            body = body,
            title = title,
            dynamicParams = RideAssignedParam driverName,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
            ttl = Nothing,
            sound = notificationSound
          }
      title = T.pack "Driver assigned!"
      body =
        unwords
          [ driverName,
            "will be your driver for this trip."
          ]
  notifyPerson person.merchantId merchantOperatingCityId notificationData

newtype RideStartedParam = RideStartedParam
  { driverName :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notifyOnRideStarted ::
  ServiceFlow m r =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyOnRideStarted booking ride = do
  let personId = booking.riderId
      rideId = ride.id
      driverName = ride.driverName
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.TRIP_STARTED merchantOperatingCityId
  tag <- getDisabilityTag person.hasDisability personId
  notificationSound <- getNotificationSound tag notificationSoundFromConfig
  let notificationData =
        Notification.NotificationReq
          { category = Notification.TRIP_STARTED,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product rideId.getId (),
            body = body,
            title = title,
            dynamicParams = RideStartedParam driverName,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
            ttl = Nothing,
            sound = notificationSound
          }
      title = T.pack "Trip started!"
      body =
        unwords
          [ driverName,
            "has started your trip. Please enjoy the ride!"
          ]
  notifyPerson person.merchantId merchantOperatingCityId notificationData

data RideCompleteParam = RideCompleteParam
  { driverName :: Text,
    fare :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notifyOnRideCompleted ::
  ServiceFlow m r =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyOnRideCompleted booking ride = do
  let personId = booking.riderId
      rideId = ride.id
      driverName = ride.driverName
      totalFare = ride.totalFare
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.TRIP_FINISHED merchantOperatingCityId
  tag <- getDisabilityTag person.hasDisability personId
  notificationSound <- getNotificationSound tag notificationSoundFromConfig
  let notificationData =
        Notification.NotificationReq
          { category = Notification.TRIP_FINISHED,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product rideId.getId (),
            body = body,
            title = title,
            dynamicParams = RideCompleteParam driverName $ show (fromMaybe booking.estimatedFare totalFare),
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
            ttl = Nothing,
            sound = notificationSound
          }
      title = T.pack "Trip finished!"
      body =
        unwords
          [ "Hope you enjoyed your trip with",
            driverName,
            "Total Fare " <> show (fromMaybe booking.estimatedFare totalFare)
          ]
  updateFollowsRideCount personId
  notifyPerson person.merchantId merchantOperatingCityId notificationData
  where
    updateFollowsRideCount personId = do
      void $ QPDEN.updateShareRideForAll personId $ Just False
      emContacts <- QPDEN.findAllByPersonId personId
      let followingContacts = filter (.enableForFollowing) emContacts
      mapM_
        ( \contact -> case contact.contactPersonId of
            Just id -> updateFollowRideCount id
            Nothing -> return ()
        )
        followingContacts
      where
        updateFollowRideCount emPersonId = do
          count <- CQFollowRide.decrementFollowRideCount emPersonId
          when (count <= 0) $ do
            CQFollowRide.clearFollowsRideCounter emPersonId
            Person.updateFollowsRide emPersonId False

notifyOnExpiration ::
  ServiceFlow m r =>
  SearchRequest ->
  m ()
notifyOnExpiration searchReq = do
  let searchRequestId = searchReq.id
  let personId = searchReq.riderId
  person <- Person.findById personId
  case person of
    Just p -> do
      let merchantOperatingCityId = p.merchantOperatingCityId
      notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.EXPIRED_CASE merchantOperatingCityId
      let notificationSound = maybe Nothing NSC.defaultSound notificationSoundFromConfig
      let notificationData =
            Notification.NotificationReq
              { category = Notification.EXPIRED_CASE,
                subCategory = Nothing,
                showNotification = Notification.SHOW,
                messagePriority = Nothing,
                entity = Notification.Entity Notification.SearchRequest searchRequestId.getId (),
                body = body,
                title = title,
                dynamicParams = EmptyDynamicParam,
                auth = Notification.Auth p.id.getId p.deviceToken p.notificationToken,
                ttl = Nothing,
                sound = notificationSound
              }
          title = T.pack "Ride expired!"
          body =
            unwords
              [ "Your ride has expired as you did not confirm any offer.",
                "Please book again to continue."
              ]
      notifyPerson p.merchantId merchantOperatingCityId notificationData
    _ -> pure ()

notifyOnRegistration ::
  ServiceFlow m r =>
  RegistrationToken ->
  Person ->
  Maybe Text ->
  m ()
notifyOnRegistration regToken person mbDeviceToken = do
  let merchantOperatingCityId = person.merchantOperatingCityId
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.REGISTRATION_APPROVED merchantOperatingCityId
  let notificationSound = maybe Nothing NSC.defaultSound notificationSoundFromConfig
  let tokenId = RegToken.id regToken
      notificationData =
        Notification.NotificationReq
          { category = Notification.REGISTRATION_APPROVED,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Merchant tokenId.getId (),
            body = body,
            title = title,
            dynamicParams = EmptyDynamicParam,
            auth = Notification.Auth person.id.getId mbDeviceToken person.notificationToken,
            ttl = Nothing,
            sound = notificationSound
          }
      title = T.pack "Registration Completed!"
      body =
        unwords
          [ "Welcome to Yatri.",
            "Click here to book your first ride with us."
          ]
   in notifyPerson person.merchantId merchantOperatingCityId notificationData

newtype RideCancelParam = RideCancelParam
  { rideTime :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notifyOnBookingCancelled ::
  ServiceFlow m r =>
  SRB.Booking ->
  SBCR.CancellationSource ->
  m ()
notifyOnBookingCancelled booking cancellationSource = do
  person <- Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  tag <- getDisabilityTag person.hasDisability person.id
  notificationSoundFromConfig <- case cancellationSource of
    SBCR.ByDriver -> SQNSC.findByNotificationType Notification.CANCELLED_PRODUCT_DRIVER merchantOperatingCityId
    SBCR.ByUser -> SQNSC.findByNotificationType Notification.CANCELLED_PRODUCT_USER merchantOperatingCityId
    _ -> SQNSC.findByNotificationType Notification.CANCELLED_PRODUCT merchantOperatingCityId
  notificationSound <- getNotificationSound tag notificationSoundFromConfig
  notifyPerson person.merchantId merchantOperatingCityId (notificationData booking.providerName person notificationSound)
  where
    notificationData orgName person notificationSound =
      Notification.NotificationReq
        { category = Notification.CANCELLED_PRODUCT,
          subCategory = Just subCategory,
          showNotification = Notification.SHOW,
          messagePriority = Nothing,
          entity = Notification.Entity Notification.Product booking.id.getId (),
          body = getCancellationText orgName,
          title = title,
          dynamicParams = RideCancelParam $ showTimeIst (booking.startTime),
          auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
          ttl = Nothing,
          sound = notificationSound
        }
    title = T.pack "Ride cancelled!"
    subCategory = case cancellationSource of
      SBCR.ByUser -> Notification.ByUser
      SBCR.ByMerchant -> Notification.ByMerchant
      SBCR.ByDriver -> Notification.ByDriver
      SBCR.ByAllocator -> Notification.ByAllocator
      SBCR.ByApplication -> Notification.ByApplication
    getCancellationText orgName = case cancellationSource of
      SBCR.ByUser ->
        unwords
          [ "You have cancelled your ride for",
            showTimeIst (booking.startTime) <> ".",
            "Check the app for details."
          ]
      SBCR.ByMerchant ->
        unwords
          [ "\"" <> orgName <> "\" agency had to cancel the ride for",
            showTimeIst (booking.startTime) <> ".",
            "Please book again to get another ride."
          ]
      SBCR.ByDriver ->
        unwords
          [ "The driver had to cancel the ride for",
            showTimeIst (booking.startTime) <> ".",
            "Please book again to get another ride."
          ]
      SBCR.ByAllocator ->
        unwords
          [ "The ride for",
            showTimeIst (booking.startTime),
            "was cancelled as we could not find a driver.",
            "Please book again to get another ride."
          ]
      SBCR.ByApplication ->
        unwords
          [ "Sorry your ride for",
            showTimeIst (booking.startTime),
            "was cancelled.",
            "Please try to book again"
          ]

notifyOnBookingReallocated ::
  ServiceFlow m r =>
  SRB.Booking ->
  m ()
notifyOnBookingReallocated booking = do
  person <- Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  tag <- getDisabilityTag person.hasDisability person.id
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.REALLOCATE_PRODUCT merchantOperatingCityId
  notificationSound <- getNotificationSound tag notificationSoundFromConfig
  notifyPerson person.merchantId merchantOperatingCityId (notificationData person notificationSound)
  where
    notificationData person notificationSound =
      Notification.NotificationReq
        { category = Notification.REALLOCATE_PRODUCT,
          subCategory = Nothing,
          showNotification = Notification.SHOW,
          messagePriority = Nothing,
          entity = Notification.Entity Notification.Product booking.id.getId (),
          body = body,
          title = title,
          dynamicParams = EmptyDynamicParam,
          auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
          ttl = Nothing,
          sound = notificationSound
        }
    title = T.pack "Ride cancelled! We are allocating another driver"
    body =
      unwords
        [ "The driver had to cancel the ride for",
          showTimeIst (booking.startTime) <> ".",
          "Please wait until we allocate another driver."
        ]

notifyOnEstimatedReallocated ::
  ServiceFlow m r =>
  SRB.Booking ->
  Id DEst.Estimate ->
  m ()
notifyOnEstimatedReallocated booking estimateId = do
  person <- Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  tag <- getDisabilityTag person.hasDisability person.id
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.REALLOCATE_PRODUCT merchantOperatingCityId
  notificationSound <- getNotificationSound tag notificationSoundFromConfig
  notifyPerson person.merchantId merchantOperatingCityId (notificationData person notificationSound)
  where
    notificationData person notificationSound =
      Notification.NotificationReq
        { category = Notification.REALLOCATE_PRODUCT,
          subCategory = Nothing,
          showNotification = Notification.SHOW,
          messagePriority = Nothing,
          entity = Notification.Entity Notification.Product estimateId.getId (),
          body = body,
          title = title,
          dynamicParams = EmptyDynamicParam,
          auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
          ttl = Nothing,
          sound = notificationSound
        }
    title = T.pack "Searching for a New Driver!"
    body =
      unwords
        [ "The driver had cancelled the ride for",
          showTimeIst (booking.startTime) <> ".",
          "Please wait while we allocate you another driver."
        ]

notifyOnQuoteReceived ::
  ServiceFlow m r =>
  DQuote.Quote ->
  m ()
notifyOnQuoteReceived quote = do
  searchRequest <- QSearchReq.findById quote.requestId >>= fromMaybeM (SearchRequestDoesNotExist quote.requestId.getId)
  person <- Person.findById searchRequest.riderId >>= fromMaybeM (PersonNotFound searchRequest.riderId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.QUOTE_RECEIVED merchantOperatingCityId
  let notificationSound = maybe Nothing NSC.defaultSound notificationSoundFromConfig
  let notificationData = mkNotificationData person notificationSound
  notifyPerson person.merchantId merchantOperatingCityId notificationData
  where
    mkNotificationData person notificationSound = do
      let title = T.pack "Quote received!"
          body =
            unwords
              [ "New quote received with price",
                show quote.estimatedFare <> "."
              ]
      Notification.NotificationReq
        { category = Notification.QUOTE_RECEIVED,
          subCategory = Nothing,
          showNotification = Notification.SHOW,
          messagePriority = Nothing,
          entity = Notification.Entity Notification.Product quote.requestId.getId (),
          body = body,
          title = title,
          dynamicParams = EmptyDynamicParam,
          auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
          ttl = Nothing,
          sound = notificationSound
        }

notifyDriverOnTheWay ::
  ServiceFlow m r =>
  Id Person ->
  m ()
notifyDriverOnTheWay personId = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.DRIVER_ON_THE_WAY merchantOperatingCityId
  let notificationSound = maybe Nothing NSC.defaultSound notificationSoundFromConfig
  let notificationData =
        Notification.NotificationReq
          { category = Notification.DRIVER_ON_THE_WAY,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product personId.getId (),
            body = body,
            title = title,
            dynamicParams = EmptyDynamicParam,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
            ttl = Nothing,
            sound = notificationSound
          }
      title = T.pack "Driver On The Way!"
      body =
        unwords
          [ "Driver is on the way"
          ]
  notifyPerson person.merchantId merchantOperatingCityId notificationData

data DriverReachedParam = DriverReachedParam
  { vehicleNumber :: Text,
    rideOtp :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notifyDriverHasReached ::
  ServiceFlow m r =>
  Id Person ->
  Text ->
  Text ->
  m ()
notifyDriverHasReached personId otp vehicleNumber = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.DRIVER_HAS_REACHED merchantOperatingCityId
  let notificationSound = maybe Nothing NSC.defaultSound notificationSoundFromConfig
  let notificationData =
        Notification.NotificationReq
          { category = Notification.DRIVER_HAS_REACHED,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product personId.getId (),
            body = body,
            title = title,
            dynamicParams = DriverReachedParam vehicleNumber otp,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
            ttl = Nothing,
            sound = notificationSound
          }
      title = T.pack "Driver Has Reached!"
      body =
        unwords
          [ "Use OTP " <> otp <> " to verify the ride with Vehicle No. " <> vehicleNumber
          ]
  notifyPerson person.merchantId merchantOperatingCityId notificationData

notifyOnNewMessage ::
  ( ServiceFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  SRB.Booking ->
  T.Text ->
  m ()
notifyOnNewMessage booking message = do
  person <- runInReplica $ Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  -- person <- Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.CHAT_MESSAGE merchantOperatingCityId
  let notificationSound = maybe Nothing NSC.defaultSound notificationSoundFromConfig
  let notificationData =
        Notification.NotificationReq
          { category = Notification.CHAT_MESSAGE,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product person.id.getId (),
            body = body,
            title = title,
            dynamicParams = EmptyDynamicParam,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
            ttl = Nothing,
            sound = notificationSound
          }
      title = T.pack "Driver"
      body =
        unwords
          [ message
          ]
  notifyPersonWithMutableContent person.merchantId merchantOperatingCityId notificationData

notifySafetyAlert ::
  ServiceFlow m r =>
  SRB.Booking ->
  T.Text ->
  m ()
notifySafetyAlert booking _ = do
  person <- runInReplica $ Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.SAFETY_ALERT_DEVIATION merchantOperatingCityId
  let notificationSound = maybe Nothing NSC.defaultSound notificationSoundFromConfig
  let notificationData =
        Notification.NotificationReq
          { category = Notification.SAFETY_ALERT_DEVIATION,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product person.id.getId (),
            body = body,
            title = title,
            dynamicParams = EmptyDynamicParam,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
            ttl = Nothing,
            sound = notificationSound
          }
      title = "Everything okay?"
      body = "We noticed your ride is on a different route. Are you feeling safe on your trip?"

  notifyPerson person.merchantId person.merchantOperatingCityId notificationData

notifyDriverBirthDay ::
  ServiceFlow m r =>
  Id Person ->
  Text ->
  m ()
notifyDriverBirthDay personId driverName = do
  person <- runInReplica $ Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.DRIVER_BIRTHDAY merchantOperatingCityId
  let notificationSound = maybe Nothing NSC.defaultSound notificationSoundFromConfig
  let notificationData =
        Notification.NotificationReq
          { category = Notification.DRIVER_BIRTHDAY,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product personId.getId (),
            body = body,
            title = title,
            dynamicParams = EmptyDynamicParam,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
            ttl = Nothing,
            sound = notificationSound
          }
      title = T.pack "Driver's Birthday!"
      body =
        unwords
          [ "Today is your driver " <> driverName <> "'s birthday, your warm wishes will make their day even more special!"
          ]
  notifyPerson person.merchantId merchantOperatingCityId notificationData

notifyRideStartToEmergencyContacts ::
  ( EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    ServiceFlow m r
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyRideStartToEmergencyContacts booking ride = do
  rider <- runInReplica $ Person.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  let merchantOperatingCityId = rider.merchantOperatingCityId
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.FOLLOW_RIDE merchantOperatingCityId
  let notificationSound = maybe Nothing NSC.defaultSound notificationSoundFromConfig
  riderConfig <- QRC.findByMerchantOperatingCityId rider.merchantOperatingCityId >>= fromMaybeM (RiderConfigDoesNotExist rider.merchantOperatingCityId.getId)
  now <- getLocalCurrentTime riderConfig.timeDiffFromUtc
  case (rider.shareTripWithEmergencyContacts == Just True && checkTimeConstraintForFollowRide riderConfig now) of
    True -> do
      unless (checkTimeConstraintForFollowRide riderConfig now) $ return ()
      let trackLink = riderConfig.trackingShortUrlPattern <> ride.shortId.getShortId
      emContacts <- QPDEN.findAllByPersonId booking.riderId
      decEmContacts <- decrypt `mapM` emContacts
      let followingContacts = filter (.enableForFollowing) decEmContacts
      for_ followingContacts \contact -> do
        case contact.contactPersonId of
          Just personId -> do
            updateFollowsRideCount personId
            sendFCM personId rider.firstName notificationSound
          Nothing -> sendSMS contact rider.firstName trackLink
    False -> logInfo "Follow ride is not enabled"
  where
    updateFollowsRideCount emPersonId = do
      _ <- CQFollowRide.incrementFollowRideCount emPersonId
      Person.updateFollowsRide emPersonId True

    sendFCM personId name notificationSound = do
      person <- runInReplica $ Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      let notificationData =
            Notification.NotificationReq
              { category = Notification.FOLLOW_RIDE,
                subCategory = Nothing,
                showNotification = Notification.SHOW,
                messagePriority = Nothing,
                entity = Notification.Entity Notification.Product personId.getId (),
                body = body,
                title = title,
                dynamicParams = EmptyDynamicParam,
                auth = Notification.Auth personId.getId person.deviceToken person.notificationToken,
                ttl = Nothing,
                sound = notificationSound
              }
          title = T.pack "Follow Ride"
          body =
            unwords
              [ fromMaybe "" name,
                " wants you to follow their ride"
              ]
      notifyPerson booking.merchantId booking.merchantOperatingCityId notificationData
    sendSMS emergencyContact name trackLink = do
      smsCfg <- asks (.smsCfg)
      let sender = smsCfg.sender
      message <-
        MessageBuilder.buildFollowRideStartedMessage booking.merchantOperatingCityId $
          MessageBuilder.BuildFollowRideMessageReq
            { userName = fromMaybe "" name,
              rideLink = trackLink
            }
      void $
        Sms.sendSMS booking.merchantId booking.merchantOperatingCityId (Sms.SendSMSReq message emergencyContact.mobileNumber sender)
          >>= Sms.checkSmsResult

checkTimeConstraintForFollowRide :: DRC.RiderConfig -> UTCTime -> Bool
checkTimeConstraintForFollowRide config now = do
  let time = timeToTimeOfDay $ utctDayTime now
  isTimeWithinBounds (secondsToTimeOfDay config.safetyCheckStartTime) (secondsToTimeOfDay config.safetyCheckEndTime) time

notifyOnStopReached ::
  ServiceFlow m r =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
notifyOnStopReached booking ride = do
  let personId = booking.riderId
      rideId = ride.id
      driverName = ride.driverName
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  notificationSoundFromConfig <- SQNSC.findByNotificationType Notification.STOP_REACHED merchantOperatingCityId
  let notificationSound = maybe Nothing NSC.defaultSound notificationSoundFromConfig
  let notificationData =
        Notification.NotificationReq
          { category = Notification.STOP_REACHED, --- Notification.STOP_REACHED, FIX THIS
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product rideId.getId (),
            body = body,
            title = title,
            dynamicParams = EmptyDynamicParam,
            auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
            ttl = Nothing,
            sound = notificationSound
          }
      title = T.pack "Stop Reached!"
      body =
        unwords
          [ driverName,
            "has reached the stop. You may add another stop!"
          ]
  notifyPerson person.merchantId merchantOperatingCityId notificationData

getDisabilityTag :: (ServiceFlow m r) => Maybe Bool -> Id Person -> m (Maybe Text)
getDisabilityTag hasDisability personId = case hasDisability of
  Just True -> runInReplica $ fmap (.tag) <$> PD.findByPersonId personId
  _ -> return Nothing

getNotificationSound :: (ServiceFlow m r) => Maybe Text -> Maybe NSC.NotificationSoundsConfig -> m (Maybe Text)
getNotificationSound tag notificationSoundFromConfig =
  return case (tag, notificationSoundFromConfig) of
    (Just "BLIND_LOW_VISION", Just ns) -> ns.blindSound
    (_, Just ns) -> ns.defaultSound
    (_, _) -> Nothing
