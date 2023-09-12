{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.GoHomeConfig where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common (Meters)
import Lib.Utils ()
import Sequelize
import Tools.Beam.UtilsTH (enableKVPG, mkTableInstancesWithTModifier)

data GoHomeConfigT f = GoHomeConfigT
  { merchantId :: B.C f Text,
    enableGoHome :: B.C f Bool,
    startCnt :: B.C f Int,
    destRadiusMeters :: B.C f Int,
    activeTime :: B.C f Int,
    updateHomeLocationAfterSec :: B.C f Int,
    cancellationCnt :: B.C f Int,
    numHomeLocations :: B.C f Int,
    goHomeFromLocationRadius :: B.C f Meters,
    goHomeWayPointRadius :: B.C f Meters,
    numDriversForDirCheck :: B.C f Int,
    goHomeBatchDelay :: B.C f Int,
    ignoreWaypointsTill :: B.C f Int,
    addStartWaypointAt :: B.C f Int,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table GoHomeConfigT where
  data PrimaryKey GoHomeConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

type GoHomeConfig = GoHomeConfigT Identity

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

goHomeConfigToHSModifiers :: M.Map Text (A.Value -> A.Value)
goHomeConfigToHSModifiers =
  M.empty

goHomeConfigToPSModifiers :: M.Map Text (A.Value -> A.Value)
goHomeConfigToPSModifiers =
  M.empty

$(enableKVPG ''GoHomeConfigT ['merchantId] [])

$(mkTableInstancesWithTModifier ''GoHomeConfigT "go_home_config" [("destRadiusMeters", "dest_radius"), ("cancellationCnt", "cancecllation_cnt")])
