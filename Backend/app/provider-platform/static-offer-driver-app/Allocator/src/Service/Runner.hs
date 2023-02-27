{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Service.Runner where

import API (iAmAlive)
import Control.Monad.Catch (Handler (..), catches)
import qualified Data.Map as Map
import qualified Domain.Action.Allocation as Allocation
import qualified Domain.Action.Allocation.Internal as I
import Domain.Types.Merchant
import Environment
import EulerHS.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Logging as Log
import Kernel.Utils.Shutdown
import Tools.Error
import qualified Tools.Metrics as TMetrics

handle :: Allocation.ServiceHandle Flow
handle =
  Allocation.ServiceHandle
    { getConfiguredNotificationTime = I.getConfiguredNotificationTime,
      getConfiguredAllocationTime = I.getConfiguredAllocationTime,
      getConfiguredReallocationsLimit = I.getConfiguredReallocationsLimit,
      getRequests = I.getRequests,
      isBatchNumExceedLimit = I.isBatchNumExceedLimit,
      getNextDriverPoolBatch = I.getNextDriverPoolBatch,
      cleanupDriverPoolBatches = I.cleanupDriverPoolBatches,
      getCurrentNotifications = I.getCurrentNotifications,
      cleanupOldNotifications = I.cleanupOldNotifications,
      sendNewRideNotifications = I.sendNewRideNotifications,
      sendRideNotAssignedNotification = I.sendRideNotAssignedNotification,
      addNotificationStatuses = I.addNotificationStatuses,
      updateNotificationStatuses = I.updateNotificationStatuses,
      resetLastRejectionTimes = I.resetLastRejectionTimes,
      getDriversWithNotification = I.getDriversWithNotification,
      getTopDriversByIdleTime = I.getTopDriversByIdleTime,
      checkAvailability = I.checkAvailability,
      assignDriver = I.assignDriver,
      cancelBooking = I.cancelBooking,
      addAllocationRequest = I.addAllocationRequest,
      getBooking = I.getBooking,
      cleanupNotifications = I.cleanupNotifications,
      removeRequest = I.removeRequest,
      logEvent = I.logEvent,
      logDriverEvents = I.logDriverEvents,
      metrics =
        Allocation.AllocatorMetricsHandle
          { incrementTaskCounter = I.incrementTaskCounter,
            incrementFailedTaskCounter = I.incrementFailedTaskCounter,
            putTaskDuration = I.putTaskDuration,
            incrementErrorCounter = TMetrics.incrementErrorCounter "ALLOCATOR_ERROR"
          }
    }

getMerchantLock :: Flow (ShortId Subscriber)
getMerchantLock = do
  shardMap <- asks (.shards)
  let numShards = Map.size shardMap
  shardCounter <- Redis.incr "beckn:allocation:shardCounter"
  let shardId = fromIntegral $ abs $ shardCounter `rem` fromIntegral numShards
  case Map.lookup shardId shardMap of
    Just subscriberId -> do
      lockAvailable <- Redis.tryLockRedis ("beckn:allocation:lock_" <> getShortId subscriberId) 10
      logInfo ("Counter: " <> show shardCounter <> " | Shard id: " <> show shardId <> " | Lock availability on " <> getShortId subscriberId <> ": " <> show lockAvailable)
      if lockAvailable
        then pure subscriberId
        else getMerchantLock
    Nothing ->
      throwError $
        ShardMappingError $ "Shard " <> show shardId <> " does not have an associated organization."

run :: Flow ()
run = do
  untilShutdown . runnerHandler . withLogTag "Allocation service" $ do
    subscriberId <- getMerchantLock
    log INFO $ "Got lock for " <> subscriberId.getShortId
    iAmAlive
    ((), processTime) <- measureDuration $ do
      requestsNum <- asks (.requestsNumPerIteration)
      eres <- try $ Allocation.process handle subscriberId requestsNum
      whenLeft eres $ Log.logError . show @_ @SomeException
      Redis.unlockRedis $ "beckn:allocation:lock_" <> getShortId subscriberId
    -- If process handling took less than processDelay we delay for remain to processDelay time
    processDelay <- asks (.processDelay)
    liftIO . threadDelay . max 0 . getMicroseconds $ millisecondsToMicroseconds (processDelay - processTime)
  where
    runnerHandler =
      flip
        catches
        [ Handler $ \(RedisError err) -> do
            Log.logTagError "Allocation service" $ show err
            liftIO $ threadDelay 1000000,
          Handler $ \(e :: SomeException) -> Log.logTagError "Allocation service" $ makeLogSomeException e
        ]
