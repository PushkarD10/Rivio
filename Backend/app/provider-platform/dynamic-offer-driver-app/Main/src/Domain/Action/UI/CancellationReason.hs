{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.CancellationReason
  ( list,
    CancellationReasonAPIEntity,
  )
where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.CancellationReason as SCR
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Utils.Common
import qualified Storage.Queries.CancellationReason as QCR

list :: (KvDbFlow m r, EsqDBReplicaFlow m r) => m [CancellationReasonAPIEntity]
list = do
  let cancellationReasons = QCR.findAll Nothing Nothing True
  fmap makeCancellationReasonAPIEntity <$> runInReplica cancellationReasons

data CancellationReasonAPIEntity = CancellationReasonAPIEntity
  { reasonCode :: SCR.CancellationReasonCode,
    description :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeCancellationReasonAPIEntity :: SCR.CancellationReason -> CancellationReasonAPIEntity
makeCancellationReasonAPIEntity SCR.CancellationReason {..} =
  CancellationReasonAPIEntity {..}

-- TODO: Move CancellationAPI Through DSL
