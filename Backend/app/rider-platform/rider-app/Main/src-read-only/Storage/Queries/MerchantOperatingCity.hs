{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantOperatingCity where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantOperatingCity as Beam

create :: KvDbFlow m r => (Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.MerchantOperatingCity.MerchantOperatingCity] -> m ())
createMany = traverse_ create

findAllByMerchantIdAndState ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.IndianState -> m [Domain.Types.MerchantOperatingCity.MerchantOperatingCity])
findAllByMerchantIdAndState (Kernel.Types.Id.Id merchantId) state = do findAllWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq merchantId, Se.Is Beam.state $ Se.Eq state]]

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.MerchantOperatingCity.MerchantOperatingCity))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByMerchantIdAndCity :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> m (Maybe Domain.Types.MerchantOperatingCity.MerchantOperatingCity))
findByMerchantIdAndCity (Kernel.Types.Id.Id merchantId) city = do findOneWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq merchantId, Se.Is Beam.city $ Se.Eq city]]

findByMerchantShortIdAndCity ::
  KvDbFlow m r =>
  (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> m (Maybe Domain.Types.MerchantOperatingCity.MerchantOperatingCity))
findByMerchantShortIdAndCity (Kernel.Types.Id.ShortId merchantShortId) city = do findOneWithKV [Se.And [Se.Is Beam.merchantShortId $ Se.Eq merchantShortId, Se.Is Beam.city $ Se.Eq city]]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.MerchantOperatingCity.MerchantOperatingCity))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ())
updateByPrimaryKey (Domain.Types.MerchantOperatingCity.MerchantOperatingCity {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.city city,
      Se.Set Beam.country country,
      Se.Set Beam.lat lat,
      Se.Set Beam.long long,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantShortId (Kernel.Types.Id.getShortId merchantShortId),
      Se.Set Beam.state state,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MerchantOperatingCity Domain.Types.MerchantOperatingCity.MerchantOperatingCity where
  fromTType' (Beam.MerchantOperatingCityT {..}) = do
    pure $
      Just
        Domain.Types.MerchantOperatingCity.MerchantOperatingCity
          { city = city,
            country = country,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            long = long,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantShortId = Kernel.Types.Id.ShortId merchantShortId,
            state = state,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MerchantOperatingCity Domain.Types.MerchantOperatingCity.MerchantOperatingCity where
  toTType' (Domain.Types.MerchantOperatingCity.MerchantOperatingCity {..}) = do
    Beam.MerchantOperatingCityT
      { Beam.city = city,
        Beam.country = country,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.long = long,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantShortId = Kernel.Types.Id.getShortId merchantShortId,
        Beam.state = state,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
