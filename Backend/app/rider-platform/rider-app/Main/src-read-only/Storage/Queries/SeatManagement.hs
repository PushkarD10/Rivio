{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SeatManagement where

import qualified Data.Time.Calendar
import qualified Domain.Types.SeatManagement
import qualified Domain.Types.ServiceCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SeatManagement as Beam

create :: KvDbFlow m r => (Domain.Types.SeatManagement.SeatManagement -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.SeatManagement.SeatManagement] -> m ())
createMany = traverse_ create

findByTicketServiceCategoryIdAndDate ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Data.Time.Calendar.Day -> m (Maybe Domain.Types.SeatManagement.SeatManagement))
findByTicketServiceCategoryIdAndDate (Kernel.Types.Id.Id ticketServiceCategoryId) date = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.ticketServiceCategoryId $ Se.Eq ticketServiceCategoryId,
          Se.Is Beam.date $ Se.Eq date
        ]
    ]

updateBlockedSeats :: KvDbFlow m r => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Data.Time.Calendar.Day -> m ())
updateBlockedSeats blocked (Kernel.Types.Id.Id ticketServiceCategoryId) date = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.blocked blocked, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.ticketServiceCategoryId $ Se.Eq ticketServiceCategoryId, Se.Is Beam.date $ Se.Eq date]]

updateBookedSeats :: KvDbFlow m r => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Data.Time.Calendar.Day -> m ())
updateBookedSeats booked (Kernel.Types.Id.Id ticketServiceCategoryId) date = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.booked booked, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.ticketServiceCategoryId $ Se.Eq ticketServiceCategoryId, Se.Is Beam.date $ Se.Eq date]]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.SeatManagement.SeatManagement -> m (Maybe Domain.Types.SeatManagement.SeatManagement))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.SeatManagement.SeatManagement -> m ())
updateByPrimaryKey (Domain.Types.SeatManagement.SeatManagement {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.blocked blocked,
      Se.Set Beam.booked booked,
      Se.Set Beam.date date,
      Se.Set Beam.ticketServiceCategoryId (Kernel.Types.Id.getId ticketServiceCategoryId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SeatManagement Domain.Types.SeatManagement.SeatManagement where
  fromTType' (Beam.SeatManagementT {..}) = do
    pure $
      Just
        Domain.Types.SeatManagement.SeatManagement
          { blocked = blocked,
            booked = booked,
            date = date,
            id = Kernel.Types.Id.Id id,
            ticketServiceCategoryId = Kernel.Types.Id.Id ticketServiceCategoryId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SeatManagement Domain.Types.SeatManagement.SeatManagement where
  toTType' (Domain.Types.SeatManagement.SeatManagement {..}) = do
    Beam.SeatManagementT
      { Beam.blocked = blocked,
        Beam.booked = booked,
        Beam.date = date,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.ticketServiceCategoryId = Kernel.Types.Id.getId ticketServiceCategoryId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
