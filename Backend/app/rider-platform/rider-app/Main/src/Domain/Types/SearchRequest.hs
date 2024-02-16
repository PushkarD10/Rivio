{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.SearchRequest where

import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMeters, Money, Seconds)
import Kernel.Types.Id
import Kernel.Types.Version
import Tools.Beam.UtilsTH

data SearchRequestStatus = NEW | INPROGRESS | CONFIRMED | COMPLETED | CLOSED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data RiderPerferredOption = Rental | OneWay -- this is just to store the rider preference for the ride type to handle backward compatibility
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnum ''RiderPerferredOption)

data SearchRequest = SearchRequest
  { id :: Id SearchRequest,
    startTime :: UTCTime,
    validTill :: UTCTime,
    riderId :: Id DP.Person,
    fromLocation :: DLoc.Location,
    toLocation :: Maybe DLoc.Location,
    distance :: Maybe HighPrecMeters,
    maxDistance :: Maybe HighPrecMeters,
    estimatedRideDuration :: Maybe Seconds,
    device :: Maybe Text,
    merchantId :: Id DMerchant.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version,
    language :: Maybe Maps.Language,
    disabilityTag :: Maybe Text,
    customerExtraFee :: Maybe Money,
    autoAssignEnabled :: Maybe Bool,
    autoAssignEnabledV2 :: Maybe Bool,
    availablePaymentMethods :: [Id DMPM.MerchantPaymentMethod],
    selectedPaymentMethodId :: Maybe (Id DMPM.MerchantPaymentMethod),
    riderPreferredOption :: RiderPerferredOption,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)
