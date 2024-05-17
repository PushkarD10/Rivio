{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal
  ( getRescheduleTime,
    isReceivedMaxDriverQuotes,
    setBatchDurationLock,
    createRescheduleTime,
    isSearchTryValid,
    cancelSearchTry,
    module Reexport,
  )
where

import Domain.Types.DriverPoolConfig
import Domain.Types.SearchTry as DST
import Kernel.Prelude
-- import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis (HedisFlow)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool as Reexport
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.SendSearchRequestToDrivers as Reexport
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.SearchTry as QST
import Tools.Error

isSearchTryValid ::
  ( KvDbFlow m r,
    Log m
  ) =>
  Id SearchTry ->
  m Bool
isSearchTryValid searchTryId = do
  (validTill, status) <- QST.getSearchTryStatusAndValidTill searchTryId >>= fromMaybeM (SearchTryDoesNotExist searchTryId.getId)
  now <- getCurrentTime
  pure $ status == DST.ACTIVE && validTill > now

isReceivedMaxDriverQuotes ::
  ( KvDbFlow m r,
    Log m
  ) =>
  DriverPoolConfig ->
  Id SearchTry ->
  m Bool
isReceivedMaxDriverQuotes driverPoolCfg searchTryId = do
  totalQuotesRecieved <- length <$> QDQ.findAllBySTId searchTryId
  pure (totalQuotesRecieved >= driverPoolCfg.maxDriverQuotesRequired)

getRescheduleTime ::
  ( KvDbFlow m r,
    Log m,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime
  ) =>
  Seconds ->
  m UTCTime
getRescheduleTime singleBatchProcessTime = do
  singleBatchProcessingTempDelay <- asks (.singleBatchProcessingTempDelay)
  now <- getCurrentTime
  return $ singleBatchProcessingTempDelay `addUTCTime` (fromIntegral singleBatchProcessTime `addUTCTime` now) -- Temporarily adding this singleBatchProcessingTempDelayTime for preventing second fcm to be triggered when first is already there, should be removed later once UI fix is done.

setBatchDurationLock ::
  ( MonadFlow m,
    HedisFlow m r
  ) =>
  Id SearchTry ->
  Seconds ->
  m (Maybe UTCTime)
setBatchDurationLock searchRequestId singleBatchProcessTime = do
  now <- getCurrentTime
  res <- Hedis.setNxExpire (getId searchRequestId) (fromIntegral singleBatchProcessTime) now
  if not res
    then do Hedis.get (getId searchRequestId)
    else return Nothing

createRescheduleTime ::
  ( Monad m,
    CacheFlow m r,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime
  ) =>
  Seconds ->
  UTCTime ->
  m UTCTime
createRescheduleTime singleBatchProcessTime lastProcTime = do
  singleBatchProcessingTempDelay <- asks (.singleBatchProcessingTempDelay)
  return $ singleBatchProcessingTempDelay `addUTCTime` (fromIntegral singleBatchProcessTime `addUTCTime` lastProcTime)

cancelSearchTry :: KvDbFlow m r => Id SearchTry -> m ()
-- cancelSearchTry searchTryId = Esq.runTransaction $ QST.updateStatus searchTryId DST.CANCELLED
cancelSearchTry searchTryId = QST.updateStatus DST.CANCELLED searchTryId
