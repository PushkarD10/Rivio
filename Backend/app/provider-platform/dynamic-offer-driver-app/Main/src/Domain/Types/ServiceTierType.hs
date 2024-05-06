{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.ServiceTierType where

import Data.Aeson
import qualified EulerHS.Prelude
import Kernel.Prelude
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data ServiceTierType
  = COMFY
  | ECO
  | PREMIUM
  | SUV
  | AUTO_RICKSHAW
  | HATCHBACK
  | SEDAN
  | TAXI
  | TAXI_PLUS
  | BIKE
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, EulerHS.Prelude.Hashable, Enum, Bounded)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ServiceTierType)
$(mkHttpInstancesForEnum ''ServiceTierType)
