{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Search.Fulfillment
  ( module Beckn.Types.Core.Taxi.Search.Fulfillment,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Search.StartInfo
import Beckn.Types.Core.Taxi.Search.StopInfo
import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions, fromAesonOptions)
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

-- If end = Nothing, then bpp sends quotes only for RENTAL
-- If end is Just, then bpp sends quotes both for RENTAL and ONE_WAY
data FulfillmentInfo = FulfillmentInfo
  { start :: StartInfo,
    end :: Maybe StopInfo,
    tags :: Tags
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data Tags = Tags
  { auto_assign_enabled :: Bool,
    customer_language :: Maybe Language
  }
  deriving (Generic, Show)

instance ToJSON Tags where
  toJSON = genericToJSON tagsJSONOptions

instance FromJSON Tags where
  parseJSON = genericParseJSON tagsJSONOptions

instance ToSchema Tags where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions tagsJSONOptions

tagsJSONOptions :: Options
tagsJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "auto_assign_enabled" -> "./komn/auto_assign_enabled"
        "customer_language" -> "./komn/customer_language"
        a -> a
    }
