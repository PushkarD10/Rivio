{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleRegistrationCertificate (module Storage.Queries.VehicleRegistrationCertificate, module ReExport) where

import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleRegistrationCertificate as Beam
import Storage.Queries.VehicleRegistrationCertificateExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate] -> m ())
createMany = traverse_ create

findAllByFleetOwnerId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate]))
findAllByFleetOwnerId limit offset fleetOwnerId = do findAllWithOptionsKV [Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId] (Se.Desc Beam.updatedAt) limit offset

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m (Maybe Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByRCIdAndFleetOwnerId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate))
findByRCIdAndFleetOwnerId (Kernel.Types.Id.Id id) fleetOwnerId = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id, Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId]]

updateAirConditioned ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m ())
updateAirConditioned airConditioned (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.airConditioned airConditioned, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

updateFleetOwnerId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m ())
updateFleetOwnerId fleetOwnerId (Kernel.Types.Id.Id id) = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.fleetOwnerId fleetOwnerId, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m (Maybe Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m ())
updateByPrimaryKey (Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.airConditioned airConditioned,
      Se.Set Beam.certificateNumberEncrypted (((certificateNumber & unEncrypted . encrypted))),
      Se.Set Beam.certificateNumberHash ((certificateNumber & hash)),
      Se.Set Beam.documentImageId (Kernel.Types.Id.getId documentImageId),
      Se.Set Beam.failedRules failedRules,
      Se.Set Beam.fitnessExpiry fitnessExpiry,
      Se.Set Beam.fleetOwnerId fleetOwnerId,
      Se.Set Beam.insuranceValidity insuranceValidity,
      Se.Set Beam.luggageCapacity luggageCapacity,
      Se.Set Beam.manufacturerModel manufacturerModel,
      Se.Set Beam.permitExpiry permitExpiry,
      Se.Set Beam.pucExpiry pucExpiry,
      Se.Set Beam.reviewRequired reviewRequired,
      Se.Set Beam.reviewedAt reviewedAt,
      Se.Set Beam.userPassedVehicleCategory userPassedVehicleCategory,
      Se.Set Beam.vehicleCapacity vehicleCapacity,
      Se.Set Beam.vehicleClass vehicleClass,
      Se.Set Beam.vehicleColor vehicleColor,
      Se.Set Beam.vehicleEnergyType vehicleEnergyType,
      Se.Set Beam.vehicleManufacturer vehicleManufacturer,
      Se.Set Beam.vehicleModel vehicleModel,
      Se.Set Beam.vehicleRating vehicleRating,
      Se.Set Beam.vehicleVariant vehicleVariant,
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
