{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.SearchRequest where

import Data.List (sortBy)
import Data.Ord
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.SearchRequest as Domain
import EulerHS.Prelude (whenNothingM_)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Beam.SearchRequest as BeamSR
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Tools.Error

createDSReq' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => SearchRequest -> m ()
createDSReq' = createWithKV

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => SearchRequest -> m ()
create dsReq = do
  void $ whenNothingM_ (QL.findById dsReq.fromLocation.id) $ do QL.create dsReq.fromLocation
  case dsReq.toLocation of
    Just toLocation -> whenNothingM_ (QL.findById toLocation.id) $ do QL.create toLocation
    _ -> return ()
  createDSReq' dsReq

createDSReq :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => SearchRequest -> m ()
createDSReq searchRequest = do
  fromLocationMap <- SLM.buildPickUpLocationMapping searchRequest.fromLocation.id searchRequest.id.getId DLM.SEARCH_REQUEST (Just searchRequest.providerId) (Just searchRequest.merchantOperatingCityId)
  QLM.create fromLocationMap
  case searchRequest.toLocation of
    Just toLocation -> do
      toLocationMap <- SLM.buildDropLocationMapping toLocation.id searchRequest.id.getId DLM.SEARCH_REQUEST (Just searchRequest.providerId) (Just searchRequest.merchantOperatingCityId)
      QLM.create toLocationMap
    _ -> return ()
  create searchRequest

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id SearchRequest -> m (Maybe SearchRequest)
findById (Id searchRequestId) = findOneWithKV [Se.Is BeamSR.id $ Se.Eq searchRequestId]

findByTransactionId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m (Maybe SearchRequest)
findByTransactionId transactionId = findOneWithKV [Se.And [Se.Is BeamSR.transactionId $ Se.Eq transactionId]]

updateAutoAssign ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id SearchRequest ->
  Bool ->
  m ()
updateAutoAssign searchRequestId autoAssignedEnabled =
  updateOneWithKV
    [Se.Set BeamSR.autoAssignEnabled $ Just autoAssignedEnabled]
    [Se.Is BeamSR.id (Se.Eq $ getId searchRequestId)]

instance FromTType' BeamSR.SearchRequest SearchRequest where
  fromTType' BeamSR.SearchRequestT {..} = do
    pUrl <- parseBaseUrl bapUri
    now <- getCurrentTime
    merchant <- CQM.findById (Id providerId) >>= fromMaybeM (MerchantNotFound providerId)
    merchantOpCityId <- CQMOC.getMerchantOpCityId (Id <$> merchantOperatingCityId) merchant bapCity

    fromLocationMapping <- QLM.getLatestStartByEntityId id >>= fromMaybeM (FromLocationMappingNotFound id)
    fromLocation <- QL.findById fromLocationMapping.locationId >>= fromMaybeM (FromLocationNotFound fromLocationMapping.locationId.getId)

    mappings <- QLM.findByEntityId id
    let mbToLocationMapping = listToMaybe . sortBy (comparing (Down . (.order))) $ filter (\loc -> loc.order /= 0) mappings
    toLocation <- maybe (pure Nothing) (QL.findById . (.locationId)) mbToLocationMapping

    let startTime_ = fromMaybe now startTime
        validTill_ = fromMaybe (addUTCTime 600 startTime_) validTill -- 10 minutes, just to handle backward compatibility
    pure $
      Just
        SearchRequest
          { id = Id id,
            transactionId = transactionId,
            providerId = Id providerId,
            riderId = Id <$> riderId,
            merchantOperatingCityId = merchantOpCityId,
            bapUri = pUrl,
            customerCancellationDues = fromMaybe 0 customerCancellationDues,
            startTime = startTime_,
            isScheduled = fromMaybe False isScheduled,
            validTill = validTill_,
            ..
          }

instance ToTType' BeamSR.SearchRequest SearchRequest where
  toTType' SearchRequest {..} =
    BeamSR.SearchRequestT
      { BeamSR.id = getId id,
        BeamSR.transactionId = transactionId,
        BeamSR.providerId = getId providerId,
        BeamSR.riderId = getId <$> riderId,
        BeamSR.merchantOperatingCityId = Just $ getId merchantOperatingCityId,
        BeamSR.fromLocationId = Just $ getId fromLocation.id,
        BeamSR.toLocationId = (getId . (.id)) <$> toLocation,
        BeamSR.area = area,
        BeamSR.bapId = bapId,
        BeamSR.bapUri = showBaseUrl bapUri,
        BeamSR.bapCity = bapCity,
        BeamSR.bapCountry = bapCountry,
        BeamSR.estimatedDistance = estimatedDistance,
        BeamSR.estimatedDuration = estimatedDuration,
        BeamSR.customerLanguage = customerLanguage,
        BeamSR.disabilityTag = disabilityTag,
        BeamSR.device = device,
        BeamSR.autoAssignEnabled = autoAssignEnabled,
        BeamSR.specialLocationTag = specialLocationTag,
        BeamSR.customerCancellationDues = Just customerCancellationDues,
        BeamSR.isReallocationEnabled = isReallocationEnabled,
        BeamSR.messageId = messageId,
        BeamSR.startTime = Just startTime,
        BeamSR.validTill = Just validTill,
        BeamSR.isScheduled = Just isScheduled,
        BeamSR.pickupZoneGateId = pickupZoneGateId,
        BeamSR.driverDefaultExtraFee = driverDefaultExtraFee,
        BeamSR.createdAt = createdAt
      }
