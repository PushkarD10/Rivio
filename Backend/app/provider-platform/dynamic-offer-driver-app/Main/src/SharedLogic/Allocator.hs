{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module SharedLogic.Allocator where

import Data.Singletons.TH
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SearchTry as DST
import Kernel.Prelude
import Kernel.Types.Common (Meters, Seconds)
import Kernel.Types.Id
import Kernel.Utils.Dhall (FromDhall)
import Lib.Scheduler

data AllocatorJobType = SendSearchRequestToDriver | SendPaymentReminderToDriver | UnsubscribeDriverForPaymentOverdue | UnblockDriver
  deriving (Generic, FromDhall, Eq, Ord, Show, Read, FromJSON, ToJSON)

genSingletons [''AllocatorJobType]
showSingInstance ''AllocatorJobType

instance JobProcessor AllocatorJobType where
  restoreAnyJobInfo :: Sing (e :: AllocatorJobType) -> Text -> Maybe (AnyJobInfo AllocatorJobType)
  restoreAnyJobInfo SSendSearchRequestToDriver jobData = AnyJobInfo <$> restoreJobInfo SSendSearchRequestToDriver jobData
  restoreAnyJobInfo SSendPaymentReminderToDriver jobData = AnyJobInfo <$> restoreJobInfo SSendPaymentReminderToDriver jobData
  restoreAnyJobInfo SUnsubscribeDriverForPaymentOverdue jobData = AnyJobInfo <$> restoreJobInfo SUnsubscribeDriverForPaymentOverdue jobData
  restoreAnyJobInfo SUnblockDriver jobData = AnyJobInfo <$> restoreJobInfo SUnblockDriver jobData

data SendSearchRequestToDriverJobData = SendSearchRequestToDriverJobData
  { searchTryId :: Id DST.SearchTry,
    estimatedRideDistance :: Meters,
    driverExtraFeeBounds :: Maybe DFP.DriverExtraFeeBounds
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SendSearchRequestToDriver

type instance JobContent 'SendSearchRequestToDriver = SendSearchRequestToDriverJobData

data SendPaymentReminderToDriverJobData = SendPaymentReminderToDriverJobData
  { startTime :: UTCTime,
    endTime :: UTCTime,
    timeDiff :: Seconds
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SendPaymentReminderToDriver

type instance JobContent 'SendPaymentReminderToDriver = SendPaymentReminderToDriverJobData

data UnsubscribeDriverForPaymentOverdueJobData = UnsubscribeDriverForPaymentOverdueJobData
  { startTime :: UTCTime,
    timeDiff :: Seconds
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'UnsubscribeDriverForPaymentOverdue

type instance JobContent 'UnsubscribeDriverForPaymentOverdue = UnsubscribeDriverForPaymentOverdueJobData

newtype UnblockDriverRequestJobData = UnblockDriverRequestJobData
  { driverId :: Id DP.Driver
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'UnblockDriver

type instance JobContent 'UnblockDriver = UnblockDriverRequestJobData
