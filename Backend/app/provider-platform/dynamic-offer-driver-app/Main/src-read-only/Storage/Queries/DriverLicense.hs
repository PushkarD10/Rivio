{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverLicense (module Storage.Queries.DriverLicense, module ReExport) where

import qualified Domain.Types.DriverLicense
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverLicense as Beam
import Storage.Queries.DriverLicenseExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverLicense.DriverLicense -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverLicense.DriverLicense] -> m ())
createMany = traverse_ create

deleteByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByDriverId (Kernel.Types.Id.Id driverId) = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq driverId]

findByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverLicense.DriverLicense))
findByDriverId (Kernel.Types.Id.Id driverId) = do findOneWithKV [Se.Is Beam.driverId $ Se.Eq driverId]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverLicense.DriverLicense -> m (Maybe Domain.Types.DriverLicense.DriverLicense))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverLicense.DriverLicense -> m (Maybe Domain.Types.DriverLicense.DriverLicense))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverLicense.DriverLicense -> m ())
updateByPrimaryKey (Domain.Types.DriverLicense.DriverLicense {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.classOfVehicles classOfVehicles,
      Se.Set Beam.consent consent,
      Se.Set Beam.consentTimestamp consentTimestamp,
      Se.Set Beam.documentImageId1 (Kernel.Types.Id.getId documentImageId1),
      Se.Set Beam.documentImageId2 (Kernel.Types.Id.getId <$> documentImageId2),
      Se.Set Beam.driverDob driverDob,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.driverName driverName,
      Se.Set Beam.failedRules failedRules,
      Se.Set Beam.licenseExpiry licenseExpiry,
      Se.Set Beam.licenseNumberEncrypted (((licenseNumber & unEncrypted . encrypted))),
      Se.Set Beam.licenseNumberHash ((licenseNumber & hash)),
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
