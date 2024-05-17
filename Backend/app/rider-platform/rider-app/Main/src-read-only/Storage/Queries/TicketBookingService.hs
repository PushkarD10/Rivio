{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketBookingService where

import qualified Domain.Types.TicketBooking
import qualified Domain.Types.TicketBookingService
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketBookingService as Beam

create :: KvDbFlow m r => (Domain.Types.TicketBookingService.TicketBookingService -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.TicketBookingService.TicketBookingService] -> m ())
createMany = traverse_ create

findAllByBookingId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking -> m [Domain.Types.TicketBookingService.TicketBookingService])
findAllByBookingId (Kernel.Types.Id.Id ticketBookingId) = do findAllWithKV [Se.Is Beam.ticketBookingId $ Se.Eq ticketBookingId]

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService -> m (Maybe Domain.Types.TicketBookingService.TicketBookingService))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByShortId :: KvDbFlow m r => (Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> m (Maybe Domain.Types.TicketBookingService.TicketBookingService))
findByShortId (Kernel.Types.Id.ShortId shortId) = do findOneWithKV [Se.Is Beam.shortId $ Se.Eq shortId]

updateAllStatusByBookingId :: KvDbFlow m r => (Domain.Types.TicketBookingService.ServiceStatus -> Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking -> m ())
updateAllStatusByBookingId status (Kernel.Types.Id.Id ticketBookingId) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.ticketBookingId $ Se.Eq ticketBookingId]

updateVerificationById :: KvDbFlow m r => (Domain.Types.TicketBookingService.ServiceStatus -> Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService -> m ())
updateVerificationById status verificationCount (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.verificationCount verificationCount, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService -> m (Maybe Domain.Types.TicketBookingService.TicketBookingService))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.TicketBookingService.TicketBookingService -> m ())
updateByPrimaryKey (Domain.Types.TicketBookingService.TicketBookingService {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount ((.amount) amount),
      Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) amount),
      Se.Set Beam.btype btype,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.expiryDate expiryDate,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.shortId (Kernel.Types.Id.getShortId shortId),
      Se.Set Beam.status status,
      Se.Set Beam.ticketBookingId (Kernel.Types.Id.getId ticketBookingId),
      Se.Set Beam.ticketServiceId (Kernel.Types.Id.getId ticketServiceId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.verificationCount verificationCount,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.TicketBookingService Domain.Types.TicketBookingService.TicketBookingService where
  fromTType' (Beam.TicketBookingServiceT {..}) = do
    pure $
      Just
        Domain.Types.TicketBookingService.TicketBookingService
          { amount = Kernel.Types.Common.mkPrice currency amount,
            btype = btype,
            createdAt = createdAt,
            expiryDate = expiryDate,
            id = Kernel.Types.Id.Id id,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            shortId = Kernel.Types.Id.ShortId shortId,
            status = status,
            ticketBookingId = Kernel.Types.Id.Id ticketBookingId,
            ticketServiceId = Kernel.Types.Id.Id ticketServiceId,
            updatedAt = updatedAt,
            verificationCount = verificationCount,
            merchantId = Kernel.Types.Id.Id <$> merchantId
          }

instance ToTType' Beam.TicketBookingService Domain.Types.TicketBookingService.TicketBookingService where
  toTType' (Domain.Types.TicketBookingService.TicketBookingService {..}) = do
    Beam.TicketBookingServiceT
      { Beam.amount = (.amount) amount,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) amount,
        Beam.btype = btype,
        Beam.createdAt = createdAt,
        Beam.expiryDate = expiryDate,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.status = status,
        Beam.ticketBookingId = Kernel.Types.Id.getId ticketBookingId,
        Beam.ticketServiceId = Kernel.Types.Id.getId ticketServiceId,
        Beam.updatedAt = updatedAt,
        Beam.verificationCount = verificationCount,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId
      }
