{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnSelect.Fulfillment
  ( module Beckn.Types.Core.Taxi.OnSelect.Fulfillment,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.FulfillmentInfo (stripPrefixUnderscoreAndRemoveNullFields)
import Beckn.Types.Core.Taxi.Common.Vehicle as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Agent
import Beckn.Types.Core.Taxi.Search.StartInfo
import Beckn.Types.Core.Taxi.Search.StopInfo
import Data.OpenApi (ToSchema (..))
import Data.OpenApi.Schema (fromAesonOptions)
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data FulfillmentInfo = FulfillmentInfo
  { id :: Text,
    start :: StartInfo,
    end :: Maybe StopInfo,
    vehicle :: Vehicle,
    _type :: Text,
    agent :: Agent
  }
  deriving (Generic, Show)

instance FromJSON FulfillmentInfo where
  parseJSON = genericParseJSON stripPrefixUnderscoreAndRemoveNullFields

instance ToJSON FulfillmentInfo where
  toJSON = genericToJSON stripPrefixUnderscoreAndRemoveNullFields

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions stripPrefixUnderscoreAndRemoveNullFields
