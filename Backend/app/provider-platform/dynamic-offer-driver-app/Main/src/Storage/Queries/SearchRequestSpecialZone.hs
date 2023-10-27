{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.SearchRequestSpecialZone where

import Data.Ord
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Merchant
import Domain.Types.SearchRequestSpecialZone as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified SharedLogic.Location as SL
import qualified Storage.Beam.SearchRequestSpecialZone as BeamSRSZ
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM

createSearchRequestSpecialZone' :: MonadFlow m => SearchRequestSpecialZone -> m ()
createSearchRequestSpecialZone' = createWithKV

create :: MonadFlow m => SearchRequestSpecialZone -> m ()
create srsz = do
  SL.createLocation srsz.fromLocation >> SL.createLocation srsz.toLocation >> createSearchRequestSpecialZone' srsz

createSearchRequestSpecialZone :: MonadFlow m => SearchRequestSpecialZone -> m ()
createSearchRequestSpecialZone searchRequest = do
  SL.createPickupLocationMapping searchRequest.fromLocation.id searchRequest.id.getId DLM.SEARCH_REQUEST
    >> SL.createDropLocationMapping searchRequest.toLocation.id searchRequest.id.getId DLM.SEARCH_REQUEST
    >> create searchRequest

findById :: MonadFlow m => Id SearchRequestSpecialZone -> m (Maybe SearchRequestSpecialZone)
findById (Id searchRequestSpecialZoneId) = findOneWithKV [Se.Is BeamSRSZ.id $ Se.Eq searchRequestSpecialZoneId]

getRequestIdfromTransactionId :: MonadFlow m => Id SearchRequestSpecialZone -> m (Maybe (Id SearchRequestSpecialZone))
getRequestIdfromTransactionId (Id tId) = findOneWithKV [Se.Is BeamSRSZ.transactionId $ Se.Eq tId] <&> (Domain.id <$>)

findByMsgIdAndBapIdAndBppId :: MonadFlow m => Text -> Text -> Id Merchant -> m (Maybe SearchRequestSpecialZone)
findByMsgIdAndBapIdAndBppId txnId bapId (Id merchantId) = findOneWithKV [Se.And [Se.Is BeamSRSZ.messageId $ Se.Eq txnId, Se.Is BeamSRSZ.providerId $ Se.Eq merchantId, Se.Is BeamSRSZ.bapId $ Se.Eq bapId]]

findByTransactionId ::
  MonadFlow m =>
  Id SearchRequestSpecialZone ->
  m (Maybe (Id SearchRequestSpecialZone))
findByTransactionId (Id tId) = findOneWithKV [Se.Is BeamSRSZ.transactionId $ Se.Eq tId] <&> (Domain.id <$>)

getValidTill :: MonadFlow m => Id SearchRequestSpecialZone -> m (Maybe UTCTime)
getValidTill (Id searchRequestId) = do
  findOneWithKV [Se.Is BeamSRSZ.id $ Se.Eq searchRequestId] <&> (Domain.validTill <$>)

instance FromTType' BeamSRSZ.SearchRequestSpecialZone SearchRequestSpecialZone where
  fromTType' BeamSRSZ.SearchRequestSpecialZoneT {..} = do
    mappings <- QLM.findByEntityId id
    (fl, tl) <-
      if null mappings -- HANDLING OLD DATA : TO BE REMOVED AFTER SOME TIME
        then do
          logInfo "Accessing Search Request Location Table"
          SL.backfillLocationAndLocationMapping fromLocationId toLocationId id DLM.SEARCH_REQUEST
        else do
          let fromLocationMapping = filter (\loc -> loc.order == 0) mappings
              toLocationMappings = filter (\loc -> loc.order /= 0) mappings

          fromLocMap <- listToMaybe fromLocationMapping & fromMaybeM (InternalError "Entity Mappings For FromLocation Not Found")
          fl <- QL.findById fromLocMap.locationId >>= fromMaybeM (InternalError $ "FromLocation not found in booking for fromLocationId: " <> fromLocMap.locationId.getId)

          when (null toLocationMappings) $ throwError (InternalError "Entity Mappings For ToLocation Not Found")
          let toLoc = maximumBy (comparing (.order)) toLocationMappings
          tl <- QL.findById toLoc.locationId >>= fromMaybeM (InternalError $ "ToLocation not found in booking for toLocationId: " <> toLoc.locationId.getId)
          return (fl, tl)
    pUrl <- parseBaseUrl bapUri
    pure $
      Just
        SearchRequestSpecialZone
          { id = Id id,
            transactionId = transactionId,
            messageId = messageId,
            startTime = startTime,
            validTill = validTill,
            providerId = Id providerId,
            fromLocation = fl,
            toLocation = tl,
            area = area,
            bapId = bapId,
            bapUri = pUrl,
            estimatedDistance = estimatedDistance,
            estimatedDuration = estimatedDuration,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamSRSZ.SearchRequestSpecialZone SearchRequestSpecialZone where
  toTType' SearchRequestSpecialZone {..} = do
    BeamSRSZ.SearchRequestSpecialZoneT
      { BeamSRSZ.id = getId id,
        BeamSRSZ.transactionId = transactionId,
        BeamSRSZ.messageId = messageId,
        BeamSRSZ.startTime = startTime,
        BeamSRSZ.validTill = validTill,
        BeamSRSZ.providerId = getId providerId,
        BeamSRSZ.fromLocationId = Just $ getId fromLocation.id,
        BeamSRSZ.toLocationId = Just $ getId toLocation.id,
        BeamSRSZ.area = area,
        BeamSRSZ.bapId = bapId,
        BeamSRSZ.bapUri = showBaseUrl bapUri,
        BeamSRSZ.estimatedDistance = estimatedDistance,
        BeamSRSZ.estimatedDuration = estimatedDuration,
        BeamSRSZ.createdAt = createdAt,
        BeamSRSZ.updatedAt = updatedAt
      }
