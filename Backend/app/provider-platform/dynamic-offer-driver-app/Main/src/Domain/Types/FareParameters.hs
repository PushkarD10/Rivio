{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.FareParameters where

import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty

data FareParameters = FareParameters
  { id :: Id FareParameters,
    baseFare :: Money,
    deadKmFare :: Maybe Money,
    extraKmFare :: Maybe Money,
    driverSelectedFare :: Maybe Money,
    customerExtraFee :: Maybe Money,
    nightShiftRate :: Maybe Centesimal,
    nightCoefIncluded :: Bool,
    waitingChargePerMin :: Maybe Money,
    waitingOrPickupCharges :: Maybe Money,
    serviceCharge :: Maybe Money,
    farePolicyType :: FarePolicyType,
    govtChargesPerc :: Maybe Int,
    earlyEndRidePenalty :: Maybe Money
  }
  deriving (Generic, Show, Eq, PrettyShow)

data FarePolicyType = SLAB | NORMAL
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)
  deriving (PrettyShow) via Showable FarePolicyType
