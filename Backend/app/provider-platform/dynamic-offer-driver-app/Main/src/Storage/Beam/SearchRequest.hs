{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.SearchRequest where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import qualified Domain.Types.FareProduct as FareProductD
import qualified Domain.Types.Vehicle.Variant as Variant (Variant)
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize
import qualified Tools.Maps as Maps

instance HasSqlValueSyntax be String => HasSqlValueSyntax be BaseUrl where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be BaseUrl

data SearchRequestT f = SearchRequestT
  { id :: B.C f Text,
    transactionId :: B.C f Text,
    providerId :: B.C f Text,
    fromLocationId :: B.C f Text,
    toLocationId :: B.C f Text,
    area :: B.C f (Maybe FareProductD.Area),
    bapId :: B.C f Text,
    bapUri :: B.C f Text,
    bapCity :: B.C f (Maybe Context.City),
    bapCountry :: B.C f (Maybe Context.Country),
    estimatedDistance :: B.C f Meters,
    estimatedDuration :: B.C f Seconds,
    mapsServiceGetDistances :: B.C f (Maybe (Maps.SMapsService 'Maps.GetDistances)),
    mapsServiceGetEstimatedPickupDistances :: B.C f (Maybe (Maps.SMapsService 'Maps.GetEstimatedPickupDistances)),
    mapsServiceGetPlaceName :: B.C f (Maybe (Maps.SMapsService 'Maps.GetPlaceName)),
    customerLanguage :: B.C f (Maybe Maps.Language),
    device :: B.C f (Maybe Text),
    autoAssignEnabled :: B.C f (Maybe Bool),
    specialLocationTag :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance IsString Variant.Variant where
  fromString = show

instance B.Table SearchRequestT where
  data PrimaryKey SearchRequestT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta SearchRequestT where
  modelFieldModification = searchRequestTMod
  modelTableName = "search_request"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type SearchRequest = SearchRequestT Identity

instance FromJSON SearchRequest where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON SearchRequest where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show SearchRequest

deriving stock instance Ord Context.City

deriving stock instance Ord Context.Country

searchRequestTMod :: SearchRequestT (B.FieldModification (B.TableField SearchRequestT))
searchRequestTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      transactionId = B.fieldNamed "transaction_id",
      providerId = B.fieldNamed "provider_id",
      fromLocationId = B.fieldNamed "from_location_id",
      toLocationId = B.fieldNamed "to_location_id",
      area = B.fieldNamed "area",
      bapId = B.fieldNamed "bap_id",
      bapCity = B.fieldNamed "bap_city",
      bapUri = B.fieldNamed "bap_uri",
      bapCountry = B.fieldNamed "bap_country",
      estimatedDistance = B.fieldNamed "estimated_distance",
      estimatedDuration = B.fieldNamed "estimated_duration",
      mapsServiceGetDistances = B.fieldNamed "maps_service_get_distances",
      mapsServiceGetEstimatedPickupDistances = B.fieldNamed "maps_service_get_estimated_pickup_distances",
      mapsServiceGetPlaceName = B.fieldNamed "maps_service_get_place_name",
      customerLanguage = B.fieldNamed "customer_language",
      device = B.fieldNamed "device",
      autoAssignEnabled = B.fieldNamed "auto_assign_enabled",
      specialLocationTag = B.fieldNamed "special_location_tag",
      createdAt = B.fieldNamed "created_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

searchRequestToHSModifiers :: M.Map Text (A.Value -> A.Value)
searchRequestToHSModifiers =
  M.empty

searchRequestToPSModifiers :: M.Map Text (A.Value -> A.Value)
searchRequestToPSModifiers =
  M.empty

instance Serialize SearchRequest where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''SearchRequestT ['id] [['transactionId]])
