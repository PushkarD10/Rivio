{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.TransporterBPPMetrics
  ( module Tools.Metrics.TransporterBPPMetrics,
    module Reexport,
  )
where

import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records.Extra
import Kernel.Types.Common
import Kernel.Utils.Time (getClockTimeInMs)
import Prometheus as P
import Tools.Metrics.TransporterBPPMetrics.Types as Reexport

putFareAndDistanceDeviations :: (MonadIO m, HasBPPMetrics m r) => Text -> Money -> Meters -> m ()
putFareAndDistanceDeviations agencyName fareDiff distanceDiff = do
  countingDeviationMetric <- asks (.bppMetrics.countingDeviation)
  liftIO $ P.withLabel countingDeviationMetric.realFareDeviation agencyName (`P.observe` fromIntegral fareDiff)
  liftIO $ P.withLabel countingDeviationMetric.realDistanceDeviation agencyName (`P.observe` fromIntegral distanceDiff)

type SearchMetricsMVar = MVar Milliseconds

startSearchMetrics :: HasBPPMetrics m r => Text -> m SearchMetricsMVar
startSearchMetrics agencyName = do
  bmContainer <- asks (.bppMetrics)
  startSearchMetrics' agencyName bmContainer

finishSearchMetrics :: HasBPPMetrics m r => Text -> SearchMetricsMVar -> m ()
finishSearchMetrics agencyName searchMetricsMVar = do
  bmContainer <- asks (.bppMetrics)
  finishSearchMetrics' agencyName bmContainer searchMetricsMVar

putSearchDuration :: L.MonadFlow m => Text -> P.Vector P.Label1 P.Histogram -> Double -> m ()
putSearchDuration agencyName searchDurationHistogram duration =
  L.runIO $
    P.withLabel
      searchDurationHistogram
      agencyName
      (`P.observe` duration)

startSearchMetrics' :: MonadFlow m => Text -> BPPMetricsContainer -> m SearchMetricsMVar
startSearchMetrics' agencyName bmContainer = do
  let (_, failureCounter) = bmContainer.searchDuration
      searchDurationTimeout = getSeconds bmContainer.searchDurationTimeout
  startTime <- getClockTimeInMs
  searchMetricsMVar <- liftIO $ newMVar startTime
  fork "BPP Search Metrics" $ do
    liftIO $ threadDelay $ searchDurationTimeout * 1000000
    whenJustM (liftIO $ tryTakeMVar searchMetricsMVar) $ \_ -> do
      liftIO $ P.withLabel failureCounter agencyName P.incCounter
  return searchMetricsMVar

finishSearchMetrics' ::
  MonadFlow m =>
  Text ->
  BPPMetricsContainer ->
  SearchMetricsMVar ->
  m ()
finishSearchMetrics' agencyName bmContainer searchMetricsMVar = do
  let (searchDurationHistogram, _) = bmContainer.searchDuration
  whenJustM (liftIO $ tryTakeMVar searchMetricsMVar) $ \startTime -> do
    endTime <- getClockTimeInMs
    putSearchDuration agencyName searchDurationHistogram $ fromIntegral $ endTime - startTime
