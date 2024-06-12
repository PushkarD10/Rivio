{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MerchantPushNotification where

import qualified Database.Beam as B
import Kernel.External.Encryption
import qualified Kernel.External.Notification.Interface.Types
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MerchantPushNotificationT f = MerchantPushNotificationT
  { body :: B.C f Kernel.Prelude.Text,
    fcmNotificationType :: B.C f Kernel.External.Notification.Interface.Types.Category,
    key :: B.C f Kernel.Prelude.Text,
    language :: B.C f Kernel.External.Types.Language,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    title :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantPushNotificationT where
  data PrimaryKey MerchantPushNotificationT f = MerchantPushNotificationId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantPushNotificationId <$> key <*> merchantOperatingCityId

type MerchantPushNotification = MerchantPushNotificationT Identity

$(enableKVPG ''MerchantPushNotificationT ['key, 'merchantOperatingCityId] [])

$(mkTableInstances ''MerchantPushNotificationT "merchant_push_notification")

{-
	DSL Source Link: file://./../../../spec/Storage/Merchant.yaml
-}
