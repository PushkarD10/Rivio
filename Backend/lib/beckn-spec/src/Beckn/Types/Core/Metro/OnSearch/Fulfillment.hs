{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Metro.OnSearch.Fulfillment where

import Beckn.Types.Core.Metro.OnSearch.Location (Location)
import Beckn.Types.Core.Metro.OnSearch.Time (Time)
import Beckn.Types.Core.Metro.OnSearch.Vehicle (Vehicle)
import Kernel.Prelude

data Fulfillment = Fulfillment
  { id :: Text,
    start :: FulfillmentDetails,
    end :: FulfillmentDetails,
    vehicle :: Vehicle
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data FulfillmentDetails = FulfillmentDetails
  { location :: Location,
    time :: Time
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
