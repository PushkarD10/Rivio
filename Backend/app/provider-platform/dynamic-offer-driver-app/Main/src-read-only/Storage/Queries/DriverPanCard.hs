{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverPanCard (module Storage.Queries.DriverPanCard, module ReExport) where

import qualified Domain.Types.DriverPanCard
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Documents
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverPanCard as Beam
import Storage.Queries.DriverPanCardExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverPanCard.DriverPanCard -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverPanCard.DriverPanCard] -> m ())
createMany = traverse_ create

deleteByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByDriverId driverId = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverPanCard.DriverPanCard))
findByDriverId driverId = do findOneWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverPanCard.DriverPanCard -> m (Maybe Domain.Types.DriverPanCard.DriverPanCard))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateVerificationStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Documents.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateVerificationStatus verificationStatus driverId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.verificationStatus verificationStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverPanCard.DriverPanCard -> m (Maybe Domain.Types.DriverPanCard.DriverPanCard))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverPanCard.DriverPanCard -> m ())
updateByPrimaryKey (Domain.Types.DriverPanCard.DriverPanCard {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.consent consent,
      Se.Set Beam.consentTimestamp consentTimestamp,
      Se.Set Beam.documentImageId1 (Kernel.Types.Id.getId documentImageId1),
      Se.Set Beam.documentImageId2 (Kernel.Types.Id.getId <$> documentImageId2),
      Se.Set Beam.driverDob driverDob,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.driverName driverName,
      Se.Set Beam.failedRules failedRules,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.panCardNumberEncrypted (panCardNumber & unEncrypted . encrypted),
      Se.Set Beam.panCardNumberHash (panCardNumber & hash),
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.verifiedBy verifiedBy,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

{-
	DSL Source Link: file://./../../../spec/Storage/DriverOnboarding.yaml
-}
