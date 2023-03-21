{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Notifications where

import qualified Data.Text as T
import Domain.Types.Booking (Booking)
import qualified Domain.Types.BookingCancellationReason as SBCR
import Domain.Types.Merchant
import Domain.Types.Message.Message as Message
import Domain.Types.Person as Person
import Domain.Types.RegistrationToken as RegToken
import Domain.Types.SearchRequest
import Domain.Types.SearchRequestForDriver
import EulerHS.Prelude
import qualified Kernel.External.FCM.Flow as FCM
import Kernel.External.FCM.Types as FCM
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (HasCacheConfig)
import Storage.CachedQueries.Merchant.TransporterConfig

notifyOnNewSearchRequestAvailable ::
  ( MonadFlow m,
    HedisFlow m r,
    CoreMetrics m,
    HasCacheConfig r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  SearchRequestForDriverAPIEntity ->
  m ()
notifyOnNewSearchRequestAvailable merchantId personId mbDeviceToken entityData = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) notificationData $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    notifType = FCM.NEW_RIDE_AVAILABLE
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.SearchRequest,
          fcmEntityIds = entityData.searchRequestId.getId,
          fcmEntityData = Just entityData,
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType
        }
    title = FCMNotificationTitle "New ride available for offering"
    body =
      FCMNotificationBody $
        unwords
          [ "A new ride for",
            showTimeIst entityData.startTime,
            "is available",
            show entityData.distanceToPickup.getMeters,
            "meters away from you. Estimated base fare is",
            show entityData.baseFare <> " INR, estimated distance is",
            show $ entityData.distance,
            "meters"
          ]

-- | Send FCM "cancel" notification to driver
notifyOnCancel ::
  ( MonadFlow m,
    HedisFlow m r,
    CoreMetrics m,
    HasCacheConfig r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Booking ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  SBCR.CancellationSource ->
  m ()
notifyOnCancel merchantId booking personId mbDeviceToken cancellationSource = do
  cancellationText <- getCancellationText
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPerson transporterConfig.fcmConfig (notificationData cancellationText) $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    notificationData cancellationText =
      FCM.FCMData
        { fcmNotificationType = FCM.CANCELLED_PRODUCT,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = getId booking.id,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title (body cancellationText) FCM.CANCELLED_PRODUCT
        }
    title = FCMNotificationTitle $ T.pack "Ride cancelled!"
    body text =
      FCMNotificationBody text
    getCancellationText = case cancellationSource of
      SBCR.ByUser ->
        return $
          unwords
            [ "Customer had to cancel your ride for",
              showTimeIst (booking.startTime) <> ".",
              "Check the app for more details."
            ]
      SBCR.ByMerchant ->
        return $
          unwords
            [ "Your agency had to cancel the ride for",
              showTimeIst (booking.startTime) <> ".",
              "Check the app for more details."
            ]
      SBCR.ByDriver ->
        return $
          unwords
            [ "You have cancelled the ride for",
              showTimeIst (booking.startTime) <> ".",
              "Check the app for more details."
            ]
      SBCR.ByApplication ->
        return $
          unwords
            [ "The ride for",
              showTimeIst (booking.startTime),
              "was cancelled because quote was not confirmed.",
              "Please book again to get another ride."
            ]
      _ -> throwError (InternalError "Unexpected cancellation reason.")

notifyOnRegistration ::
  ( MonadFlow m,
    HedisFlow m r,
    CoreMetrics m,
    HasCacheConfig r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  RegistrationToken ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyOnRegistration merchantId regToken personId mbToken = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPerson transporterConfig.fcmConfig notificationData $ FCMNotificationRecipient personId.getId mbToken
  where
    tokenId = RegToken.id regToken
    notificationData =
      FCM.FCMData
        { fcmNotificationType = FCM.REGISTRATION_APPROVED,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Merchant,
          fcmEntityIds = getId tokenId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.REGISTRATION_APPROVED
        }
    title = FCMNotificationTitle $ T.pack "Registration Completed!"
    body =
      FCMNotificationBody $
        unwords
          [ "Welcome Yatri Partner!",
            "Click here to set up your account."
          ]

notifyDriver ::
  ( MonadFlow m,
    HedisFlow m r,
    CoreMetrics m,
    HasCacheConfig r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriver merchantId = sendNotificationToDriver merchantId FCM.SHOW Nothing

-- Send notification to device, i.e. notifications that should not be shown to the user,
-- but contains payload used by the app
notifyDevice ::
  ( MonadFlow m,
    HedisFlow m r,
    CoreMetrics m,
    HasCacheConfig r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDevice merchantId = sendNotificationToDriver merchantId FCM.DO_NOT_SHOW (Just FCM.HIGH)

sendNotificationToDriver ::
  ( MonadFlow m,
    HedisFlow m r,
    CoreMetrics m,
    HasCacheConfig r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  FCM.FCMShowNotification ->
  Maybe FCM.FCMAndroidMessagePriority ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
sendNotificationToDriver merchantId displayOption priority notificationType notificationTitle message driverId mbToken = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig priority notificationData $ FCMNotificationRecipient driverId.getId mbToken
  where
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notificationType,
          fcmShowNotification = displayOption,
          fcmEntityType = FCM.Person,
          fcmEntityIds = getId driverId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notificationType
        }
    title = FCM.FCMNotificationTitle notificationTitle
    body =
      FCMNotificationBody message

sendMessageToDriver ::
  ( MonadFlow m,
    HedisFlow m r,
    CoreMetrics m,
    HasCacheConfig r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  FCM.FCMShowNotification ->
  Maybe FCM.FCMAndroidMessagePriority ->
  FCM.FCMNotificationType ->
  Text ->
  Text ->
  Id Person ->
  Id Message.Message ->
  Maybe FCM.FCMRecipientToken ->
  m ()
sendMessageToDriver merchantId displayOption priority notificationType notificationTitle message driverId messageId mbToken = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig priority notificationData $ FCMNotificationRecipient driverId.getId mbToken
  where
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notificationType,
          fcmShowNotification = displayOption,
          fcmEntityType = FCM.Person,
          fcmEntityIds = getId messageId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notificationType
        }
    title = FCM.FCMNotificationTitle notificationTitle
    body =
      FCMNotificationBody message

notifyDriverNewAllocation ::
  ( MonadFlow m,
    HedisFlow m r,
    CoreMetrics m,
    HasCacheConfig r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id bookingId ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriverNewAllocation merchantId bookingId personId mbToken = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) notificationData $ FCMNotificationRecipient personId.getId mbToken
  where
    title = FCM.FCMNotificationTitle "New allocation request."
    body =
      FCM.FCMNotificationBody $
        unwords
          [ "New ride request!",
            "Check the app for more details."
          ]
    notificationData =
      FCM.FCMData
        { fcmNotificationType = FCM.ALLOCATION_REQUEST,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Product,
          fcmEntityIds = getId bookingId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.ALLOCATION_REQUEST
        }

notifyFarePolicyChange ::
  ( MonadFlow m,
    HedisFlow m r,
    CoreMetrics m,
    HasCacheConfig r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id coordinatorId ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyFarePolicyChange merchantId coordinatorId mbToken = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPerson transporterConfig.fcmConfig notificationData $ FCMNotificationRecipient coordinatorId.getId mbToken
  where
    title = FCM.FCMNotificationTitle "Fare policy changed."
    body =
      FCM.FCMNotificationBody $
        unwords
          [ "Fare has been updated."
          ]
    notificationData =
      FCM.FCMData
        { fcmNotificationType = FCM.FARE_POLICY_CHANGED,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = getId coordinatorId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.FARE_POLICY_CHANGED
        }

notifyDiscountChange ::
  ( MonadFlow m,
    HedisFlow m r,
    CoreMetrics m,
    HasCacheConfig r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id coordinatorId ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDiscountChange merchantId coordinatorId mbToken = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPerson transporterConfig.fcmConfig notificationData $ FCMNotificationRecipient coordinatorId.getId mbToken
  where
    title = FCM.FCMNotificationTitle "Discount updated."
    body =
      FCM.FCMNotificationBody $
        unwords
          [ "Discount has been changed."
          ]
    notificationData =
      FCM.FCMData
        { fcmNotificationType = FCM.DISCOUNT_CHANGED,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.Person,
          fcmEntityIds = getId coordinatorId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.DISCOUNT_CHANGED
        }

notifyDriverClearedFare ::
  ( MonadFlow m,
    HedisFlow m r,
    CoreMetrics m,
    HasCacheConfig r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  Id SearchRequest ->
  Money ->
  Maybe FCM.FCMRecipientToken ->
  m ()
notifyDriverClearedFare merchantId driverId sReqId fare mbToken = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) notificationData $ FCMNotificationRecipient driverId.getId mbToken
  where
    title = FCM.FCMNotificationTitle "Clearing Fare!"
    body =
      FCM.FCMNotificationBody $
        unwords
          [ "Clearing fare - ",
            show fare.getMoney <> "."
          ]
    notificationData =
      FCM.FCMData
        { fcmNotificationType = FCM.CLEARED_FARE,
          fcmShowNotification = FCM.DO_NOT_SHOW,
          fcmEntityType = FCM.SearchRequest,
          fcmEntityIds = getId sReqId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body FCM.CLEARED_FARE
        }

notifyOnCancelSearchRequest ::
  ( MonadFlow m,
    HedisFlow m r,
    CoreMetrics m,
    HasCacheConfig r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  Maybe FCM.FCMRecipientToken ->
  Id SearchRequest ->
  m ()
notifyOnCancelSearchRequest merchantId personId mbDeviceToken searchRequestId = do
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  FCM.notifyPersonWithPriority transporterConfig.fcmConfig (Just FCM.HIGH) notificationData $ FCMNotificationRecipient personId.getId mbDeviceToken
  where
    notifType = FCM.CANCELLED_SEARCH_REQUEST
    notificationData =
      FCM.FCMData
        { fcmNotificationType = notifType,
          fcmShowNotification = FCM.SHOW,
          fcmEntityType = FCM.SearchRequest,
          fcmEntityIds = searchRequestId.getId,
          fcmEntityData = (),
          fcmNotificationJSON = FCM.createAndroidNotification title body notifType
        }
    title = FCMNotificationTitle "Search Request cancelled!"
    body =
      FCMNotificationBody $
        unwords
          [ "Search request has been cancelled by customer"
          ]
