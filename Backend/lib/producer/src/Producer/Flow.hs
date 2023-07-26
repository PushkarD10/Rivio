{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Producer.Flow where

import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BSL
import Data.Text.Encoding as BSE
import qualified Data.Text.Encoding as TE
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics (addGenericLatency))
import Kernel.Types.Flow ()
import Kernel.Utils.Common
import Kernel.Utils.Time ()

getCurrentTimestamp :: (CacheFlow m r) => m Milliseconds
getCurrentTimestamp = liftIO getClockTimeInMs

splitIntoBatches :: Int -> [a] -> [[a]]
splitIntoBatches _ [] = []
splitIntoBatches batchSize xs =
  let (batch, rest) = splitAt batchSize xs
   in batch : splitIntoBatches batchSize rest

getTime :: (CacheFlow m r) => Text -> m Milliseconds
getTime producerTimestampKey = do
  Hedis.safeGet producerTimestampKey >>= \case
    Just currentTime -> return currentTime
    Nothing -> getCurrentTimestamp

runProducer :: Flow ()
runProducer = do
  begTime <- getCurrentTimestamp
  producerTimestampKey <- asks (.producerTimestampKey)
  startTime <- getTime producerTimestampKey
  endTime <- getCurrentTimestamp

  Hedis.set producerTimestampKey endTime

  setName <- asks (.setName)
  currentJobs <- Hedis.zrangebyscore setName (millisToSecondsDouble startTime) (millisToSecondsDouble endTime)
  logDebug $ "Jobs taken out of sortedset" <> show currentJobs

  batchSize <- asks (.batchSize)
  let jobChunks = splitIntoBatches batchSize currentJobs
  logDebug $ "Job chunks producer" <> show jobChunks

  forM_ jobChunks $ \chunk -> do
    streamName <- asks (.streamName)
    entryId <- asks (.entryId)
    eqId <- generateGUID
    let eqIdByteString = TE.encodeUtf8 eqId
    let chunk_ = BSL.toStrict . Ae.encode $ map BSE.decodeUtf8 chunk
    let fieldValue = [(eqIdByteString, chunk_)]
    result <- Hedis.xadd streamName entryId fieldValue
    logDebug $ "Jobs inserted out of stream" <> show result

  endTime <- getCurrentTimestamp
  let diff = endTime - begTime
  waitTimeMilliSec <- asks (.waitTimeMilliSec)
  threadDelayMilliSec $ max 0 (waitTimeMilliSec - diff)

  fork "" $ addGenericLatency "producer" (millisToNominalDiffTime diff)

  runProducer
