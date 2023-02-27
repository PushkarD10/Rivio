{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Client where

import qualified Domain.Types.MockPlace as DPlace
import EulerHS.Types as Euler
import qualified Kernel.External.Maps.Google.RoadsClient as Roads
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (MonadFlow, callAPI, fromEitherM)

snapToRoad ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  m DPlace.SnapToRoadResponse
snapToRoad roadsUrl apiKey path = do
  let eulerClient = Euler.client (Proxy @Roads.SnapToRoadAPI)
      interpolate = True
  callAPI roadsUrl (eulerClient apiKey interpolate path) "snap-to-road"
    >>= fromEitherM (\err -> InternalError $ "Failed to call snap-to-road API: " <> show err)
