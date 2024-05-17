{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSQuote where

import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSQuote as Beam

create :: KvDbFlow m r => (Domain.Types.FRFSQuote.FRFSQuote -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.FRFSQuote.FRFSQuote] -> m ())
createMany = traverse_ create

findAllBySearchId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m [Domain.Types.FRFSQuote.FRFSQuote])
findAllBySearchId (Kernel.Types.Id.Id searchId) = do findAllWithKV [Se.Is Beam.searchId $ Se.Eq searchId]

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> m (Maybe Domain.Types.FRFSQuote.FRFSQuote))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> m (Maybe Domain.Types.FRFSQuote.FRFSQuote))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.FRFSQuote.FRFSQuote -> m ())
updateByPrimaryKey (Domain.Types.FRFSQuote.FRFSQuote {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam._type _type,
      Se.Set Beam.bppItemId bppItemId,
      Se.Set Beam.bppSubscriberId bppSubscriberId,
      Se.Set Beam.bppSubscriberUrl bppSubscriberUrl,
      Se.Set Beam.fromStationId (Kernel.Types.Id.getId fromStationId),
      Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) price),
      Se.Set Beam.price ((.amount) price),
      Se.Set Beam.providerDescription providerDescription,
      Se.Set Beam.providerId providerId,
      Se.Set Beam.providerName providerName,
      Se.Set Beam.quantity quantity,
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.searchId (Kernel.Types.Id.getId searchId),
      Se.Set Beam.stationsJson stationsJson,
      Se.Set Beam.toStationId (Kernel.Types.Id.getId toStationId),
      Se.Set Beam.validTill validTill,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSQuote Domain.Types.FRFSQuote.FRFSQuote where
  fromTType' (Beam.FRFSQuoteT {..}) = do
    pure $
      Just
        Domain.Types.FRFSQuote.FRFSQuote
          { _type = _type,
            bppItemId = bppItemId,
            bppSubscriberId = bppSubscriberId,
            bppSubscriberUrl = bppSubscriberUrl,
            fromStationId = Kernel.Types.Id.Id fromStationId,
            id = Kernel.Types.Id.Id id,
            price = Kernel.Types.Common.mkPrice currency price,
            providerDescription = providerDescription,
            providerId = providerId,
            providerName = providerName,
            quantity = quantity,
            riderId = Kernel.Types.Id.Id riderId,
            searchId = Kernel.Types.Id.Id searchId,
            stationsJson = stationsJson,
            toStationId = Kernel.Types.Id.Id toStationId,
            validTill = validTill,
            vehicleType = vehicleType,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSQuote Domain.Types.FRFSQuote.FRFSQuote where
  toTType' (Domain.Types.FRFSQuote.FRFSQuote {..}) = do
    Beam.FRFSQuoteT
      { Beam._type = _type,
        Beam.bppItemId = bppItemId,
        Beam.bppSubscriberId = bppSubscriberId,
        Beam.bppSubscriberUrl = bppSubscriberUrl,
        Beam.fromStationId = Kernel.Types.Id.getId fromStationId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) price,
        Beam.price = (.amount) price,
        Beam.providerDescription = providerDescription,
        Beam.providerId = providerId,
        Beam.providerName = providerName,
        Beam.quantity = quantity,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.searchId = Kernel.Types.Id.getId searchId,
        Beam.stationsJson = stationsJson,
        Beam.toStationId = Kernel.Types.Id.getId toStationId,
        Beam.validTill = validTill,
        Beam.vehicleType = vehicleType,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
