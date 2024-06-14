{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.FRFS
  ( frfsStatusUpdate,
    FRFSStatusUpdateReq (..),
  )
where

import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketBooking
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import Environment
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import Tools.Error

newtype FRFSStatusUpdateReq = FRFSStatusUpdateReq
  { bookingIds :: [Id DFRFSTicketBooking.FRFSTicketBooking]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

---------------------------------------------------------------------
frfsStatusUpdate ::
  FRFSStatusUpdateReq ->
  Flow APISuccess
frfsStatusUpdate req = do
  let reqBookingIds = req.bookingIds
  mapM_
    ( \bookingId -> do
        booking <- B.runInReplica $ QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
        let merchantId = booking.merchantId
        FRFSTicketBooking.getFrfsBookingStatus (Just booking.riderId, merchantId) bookingId
    )
    reqBookingIds

  pure Success
