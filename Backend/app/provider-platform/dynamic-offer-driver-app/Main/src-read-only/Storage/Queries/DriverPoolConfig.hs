{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverPoolConfig where

import qualified Domain.Types.DriverPoolConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverPoolConfig as Beam

create :: KvDbFlow m r => (Domain.Types.DriverPoolConfig.DriverPoolConfig -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.DriverPoolConfig.DriverPoolConfig] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  KvDbFlow m r =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.DriverPoolConfig.DriverPoolConfig])
findAllByMerchantOpCityId limit offset (Kernel.Types.Id.Id merchantOperatingCityId) = do findAllWithOptionsKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId] (Se.Desc Beam.tripDistance) limit offset

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.DriverPoolConfig.DriverPoolConfig -> m (Maybe Domain.Types.DriverPoolConfig.DriverPoolConfig))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.DriverPoolConfig.DriverPoolConfig -> m ())
updateByPrimaryKey (Domain.Types.DriverPoolConfig.DriverPoolConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.actualDistanceThreshold actualDistanceThreshold,
      Se.Set Beam.area area,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.distanceBasedBatchSplit distanceBasedBatchSplit,
      Se.Set Beam.driverBatchSize driverBatchSize,
      Se.Set Beam.driverPositionInfoExpiry driverPositionInfoExpiry,
      Se.Set Beam.driverQuoteLimit driverQuoteLimit,
      Se.Set Beam.driverRequestCountLimit driverRequestCountLimit,
      Se.Set Beam.driverToDestinationDistanceThreshold driverToDestinationDistanceThreshold,
      Se.Set Beam.driverToDestinationDuration driverToDestinationDuration,
      Se.Set Beam.maxDriverQuotesRequired maxDriverQuotesRequired,
      Se.Set Beam.maxNumberOfBatches maxNumberOfBatches,
      Se.Set Beam.maxParallelSearchRequests maxParallelSearchRequests,
      Se.Set Beam.maxRadiusOfSearch maxRadiusOfSearch,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.minRadiusOfSearch minRadiusOfSearch,
      Se.Set Beam.poolSortingType poolSortingType,
      Se.Set Beam.radiusShrinkValueForDriversOnRide radiusShrinkValueForDriversOnRide,
      Se.Set Beam.radiusStepSize radiusStepSize,
      Se.Set Beam.scheduleTryTimes scheduleTryTimes,
      Se.Set Beam.singleBatchProcessTime singleBatchProcessTime,
      Se.Set Beam.thresholdToIgnoreActualDistanceThreshold thresholdToIgnoreActualDistanceThreshold,
      Se.Set Beam.tripCategory tripCategory,
      Se.Set Beam.tripDistance tripDistance,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleVariant vehicleVariant
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DriverPoolConfig Domain.Types.DriverPoolConfig.DriverPoolConfig where
  fromTType' (Beam.DriverPoolConfigT {..}) = do
    pure $
      Just
        Domain.Types.DriverPoolConfig.DriverPoolConfig
          { actualDistanceThreshold = actualDistanceThreshold,
            area = area,
            createdAt = createdAt,
            distanceBasedBatchSplit = distanceBasedBatchSplit,
            driverBatchSize = driverBatchSize,
            driverPositionInfoExpiry = driverPositionInfoExpiry,
            driverQuoteLimit = driverQuoteLimit,
            driverRequestCountLimit = driverRequestCountLimit,
            driverToDestinationDistanceThreshold = driverToDestinationDistanceThreshold,
            driverToDestinationDuration = driverToDestinationDuration,
            id = Kernel.Types.Id.Id id,
            maxDriverQuotesRequired = maxDriverQuotesRequired,
            maxNumberOfBatches = maxNumberOfBatches,
            maxParallelSearchRequests = maxParallelSearchRequests,
            maxRadiusOfSearch = maxRadiusOfSearch,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            minRadiusOfSearch = minRadiusOfSearch,
            poolSortingType = poolSortingType,
            radiusShrinkValueForDriversOnRide = radiusShrinkValueForDriversOnRide,
            radiusStepSize = radiusStepSize,
            scheduleTryTimes = scheduleTryTimes,
            singleBatchProcessTime = singleBatchProcessTime,
            thresholdToIgnoreActualDistanceThreshold = thresholdToIgnoreActualDistanceThreshold,
            tripCategory = tripCategory,
            tripDistance = tripDistance,
            updatedAt = updatedAt,
            vehicleVariant = vehicleVariant
          }

instance ToTType' Beam.DriverPoolConfig Domain.Types.DriverPoolConfig.DriverPoolConfig where
  toTType' (Domain.Types.DriverPoolConfig.DriverPoolConfig {..}) = do
    Beam.DriverPoolConfigT
      { Beam.actualDistanceThreshold = actualDistanceThreshold,
        Beam.area = area,
        Beam.createdAt = createdAt,
        Beam.distanceBasedBatchSplit = distanceBasedBatchSplit,
        Beam.driverBatchSize = driverBatchSize,
        Beam.driverPositionInfoExpiry = driverPositionInfoExpiry,
        Beam.driverQuoteLimit = driverQuoteLimit,
        Beam.driverRequestCountLimit = driverRequestCountLimit,
        Beam.driverToDestinationDistanceThreshold = driverToDestinationDistanceThreshold,
        Beam.driverToDestinationDuration = driverToDestinationDuration,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxDriverQuotesRequired = maxDriverQuotesRequired,
        Beam.maxNumberOfBatches = maxNumberOfBatches,
        Beam.maxParallelSearchRequests = maxParallelSearchRequests,
        Beam.maxRadiusOfSearch = maxRadiusOfSearch,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.minRadiusOfSearch = minRadiusOfSearch,
        Beam.poolSortingType = poolSortingType,
        Beam.radiusShrinkValueForDriversOnRide = radiusShrinkValueForDriversOnRide,
        Beam.radiusStepSize = radiusStepSize,
        Beam.scheduleTryTimes = scheduleTryTimes,
        Beam.singleBatchProcessTime = singleBatchProcessTime,
        Beam.thresholdToIgnoreActualDistanceThreshold = thresholdToIgnoreActualDistanceThreshold,
        Beam.tripCategory = tripCategory,
        Beam.tripDistance = tripDistance,
        Beam.updatedAt = updatedAt,
        Beam.vehicleVariant = vehicleVariant
      }
