{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.FRFS.Common where

import qualified BecknV2.FRFS.Enums as Spec
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Station as QStation

data DFareBreakUp = DFareBreakUp
  { title :: Text,
    price :: HighPrecMoney,
    pricePerUnit :: HighPrecMoney,
    quantity :: Int
  }

data DOrder = DOrder
  { providerId :: Text,
    totalPrice :: HighPrecMoney,
    fareBreakUp :: [DFareBreakUp],
    bppOrderId :: Text,
    bppItemId :: Text,
    transactionId :: Text,
    orderStatus :: Maybe Spec.OrderStatus,
    messageId :: Text,
    tickets :: [DTicket]
  }

data DTicket = DTicket
  { qrData :: Text,
    bppFulfillmentId :: Text,
    ticketNumber :: Text,
    validTill :: UTCTime,
    status :: Text
  }

getMerchantOperatingCityFromBooking :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DFRFSTicketBooking.FRFSTicketBooking -> m DMOC.MerchantOperatingCity
getMerchantOperatingCityFromBooking tBooking = do
  moCityId <- case tBooking.merchantOperatingCityId of
    Nothing -> do
      station <- QStation.findById tBooking.fromStationId >>= fromMaybeM (InternalError $ "Station with id - " <> show tBooking.fromStationId <> " not found.")
      return station.merchantOperatingCityId
    Just moCityId -> return moCityId
  CQMOC.findById moCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show moCityId)
