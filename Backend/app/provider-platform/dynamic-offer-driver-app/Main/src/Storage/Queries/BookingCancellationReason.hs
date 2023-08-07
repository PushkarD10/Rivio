{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.BookingCancellationReason where

import Domain.Types.Booking
import Domain.Types.BookingCancellationReason
import Domain.Types.Person
import Domain.Types.Ride
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.BookingCancellationReason

create :: BookingCancellationReason -> SqlDB ()
create = Esq.create

findAllCancelledByDriverId ::
  Transactionable m =>
  Id Person ->
  m Int
findAllCancelledByDriverId driverId = do
  mkCount <$> do
    Esq.findAll $ do
      rideBookingCancellationReason <- from $ table @BookingCancellationReasonT
      where_ $
        rideBookingCancellationReason ^. BookingCancellationReasonDriverId ==. val (Just $ toKey driverId)
          &&. rideBookingCancellationReason ^. BookingCancellationReasonSource ==. val ByDriver
      return (countRows :: SqlExpr (Esq.Value Int))
  where
    mkCount [counter] = counter
    mkCount _ = 0

findByBookingId ::
  Transactionable m =>
  Id Booking ->
  m (Maybe BookingCancellationReason)
findByBookingId bookingId =
  Esq.findOne $ do
    bookingCancellationReason <- from $ table @BookingCancellationReasonT
    where_ $ bookingCancellationReason ^. BookingCancellationReasonBookingId ==. val (toKey bookingId)
    return bookingCancellationReason

findByRideId :: Transactionable m => Id Ride -> m (Maybe BookingCancellationReason)
findByRideId rideId = Esq.findOne $ do
  bookingCancellationReason <- from $ table @BookingCancellationReasonT
  where_ $ bookingCancellationReason ^. BookingCancellationReasonRideId ==. (just . val . toKey $ rideId)
  return bookingCancellationReason

upsert :: BookingCancellationReason -> SqlDB ()
upsert cancellationReason =
  Esq.upsert
    cancellationReason
    [ BookingCancellationReasonBookingId =. val (toKey cancellationReason.bookingId),
      BookingCancellationReasonRideId =. val (toKey <$> cancellationReason.rideId),
      BookingCancellationReasonReasonCode =. val (toKey <$> cancellationReason.reasonCode),
      BookingCancellationReasonAdditionalInfo =. val (cancellationReason.additionalInfo)
    ]

findAllBookingIdsCancelledByDriverId ::
  Transactionable m =>
  Id Person ->
  m [Id Booking]
findAllBookingIdsCancelledByDriverId driverId = do
  Esq.findAll $ do
    rideBookingCancellationReason <- from $ table @BookingCancellationReasonT
    where_ $
      rideBookingCancellationReason ^. BookingCancellationReasonDriverId ==. val (Just $ toKey driverId)
        &&. rideBookingCancellationReason ^. BookingCancellationReasonSource ==. val ByDriver
    return (rideBookingCancellationReason ^. BookingCancellationReasonBookingId)
