{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.SearchRequestSpecialZone where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import qualified Domain.Types.FareProduct as FareProductD
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize
import qualified Tools.Maps as Maps

instance HasSqlValueSyntax be String => HasSqlValueSyntax be BaseUrl where
  sqlValueSyntax :: HasSqlValueSyntax be String => BaseUrl -> be
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be BaseUrl

data SearchRequestSpecialZoneT f = SearchRequestSpecialZoneT
  { id :: B.C f Text,
    transactionId :: B.C f Text,
    messageId :: B.C f Text,
    startTime :: B.C f Time.UTCTime,
    validTill :: B.C f Time.UTCTime,
    providerId :: B.C f Text,
    fromLocationId :: B.C f Text,
    toLocationId :: B.C f Text,
    area :: B.C f (Maybe FareProductD.Area),
    bapId :: B.C f Text,
    bapUri :: B.C f Text,
    estimatedDistance :: B.C f Meters,
    estimatedDuration :: B.C f Seconds,
    mapsServiceGetDistances :: B.C f (Maybe (Maps.SMapsService 'Maps.GetDistances)),
    mapsServiceGetPlaceName :: B.C f (Maybe (Maps.SMapsService 'Maps.GetPlaceName)),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchRequestSpecialZoneT where
  data PrimaryKey SearchRequestSpecialZoneT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta SearchRequestSpecialZoneT where
  modelFieldModification = searchRequestSpecialZoneTMod
  modelTableName = "search_request_special_zone"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type SearchRequestSpecialZone = SearchRequestSpecialZoneT Identity

instance FromJSON SearchRequestSpecialZone where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON SearchRequestSpecialZone where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show SearchRequestSpecialZone

searchRequestSpecialZoneTMod :: SearchRequestSpecialZoneT (B.FieldModification (B.TableField SearchRequestSpecialZoneT))
searchRequestSpecialZoneTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      transactionId = B.fieldNamed "transaction_id",
      messageId = B.fieldNamed "message_id",
      startTime = B.fieldNamed "start_time",
      validTill = B.fieldNamed "valid_till",
      providerId = B.fieldNamed "provider_id",
      fromLocationId = B.fieldNamed "from_location_id",
      toLocationId = B.fieldNamed "to_location_id",
      area = B.fieldNamed "area",
      bapId = B.fieldNamed "bap_id",
      bapUri = B.fieldNamed "bap_uri",
      estimatedDistance = B.fieldNamed "estimated_distance",
      estimatedDuration = B.fieldNamed "estimated_duration",
      mapsServiceGetDistances = B.fieldNamed "maps_service_get_distances",
      mapsServiceGetPlaceName = B.fieldNamed "maps_service_get_place_name",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

searchRequestSpecialZoneToHSModifiers :: M.Map Text (A.Value -> A.Value)
searchRequestSpecialZoneToHSModifiers =
  M.empty

searchRequestSpecialZoneToPSModifiers :: M.Map Text (A.Value -> A.Value)
searchRequestSpecialZoneToPSModifiers =
  M.empty

instance Serialize SearchRequestSpecialZone where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''SearchRequestSpecialZoneT ['id] [['transactionId], ['messageId]])
