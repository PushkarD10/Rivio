{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Domain.Action.UI.Cancel
  ( cancel,
    CancelReq (..),
    CancelRes (..),
    CancelSearch (..),
    mkDomainCancelSearch,
    cancelSearch,
  )
where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.DriverOffer as DDO
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as Ride
import Domain.Types.SearchRequest (SearchRequest)
import Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.DriverOffer as QDOffer
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QR
import Tools.Error
import qualified Tools.Maps as Maps
import qualified Tools.Metrics as Metrics

data CancelReq = CancelReq
  { reasonCode :: SCR.CancellationReasonCode,
    reasonStage :: SCR.CancellationStage,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CancelRes = CancelRes
  { bppBookingId :: Id SRB.BPPBooking,
    bppId :: Text,
    bppUrl :: BaseUrl,
    cancellationSource :: SBCR.CancellationSource,
    transactionId :: Text,
    merchant :: DM.Merchant
  }

data CancelSearch = CancelSearch
  { estimateId :: Id DEstimate.Estimate,
    providerUrl :: BaseUrl,
    providerId :: Text,
    estimateStatus :: DEstimate.EstimateStatus,
    searchReqId :: Id SearchRequest,
    sendToBpp :: Bool,
    merchant :: DM.Merchant
  }

cancel :: (EncFlow m r, Esq.EsqDBReplicaFlow m r, EsqDBFlow m r, HasCacheConfig r, HedisFlow m r, Metrics.CoreMetrics m) => Id SRB.Booking -> (Id Person.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> CancelReq -> m CancelRes
cancel bookingId (_, _, merchantOperatingCityId) req = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  when (booking.status == SRB.CANCELLED) $ throwError (BookingInvalidStatus "This booking is already cancelled")
  canCancelBooking <- isBookingCancellable booking
  unless canCancelBooking $
    throwError $ RideInvalidStatus "Cannot cancel this ride"
  when (booking.status == SRB.NEW) $ throwError (BookingInvalidStatus "NEW")
  bppBookingId <- fromMaybeM (BookingFieldNotPresent "bppBookingId") booking.bppBookingId
  mRide <- Esq.runInReplica $ QR.findActiveByRBId booking.id
  cancellationReason <-
    case mRide of
      Just ride -> do
        res <- try @_ @SomeException (CallBPP.callGetDriverLocation ride.trackingUrl)
        case res of
          Right res' -> do
            disToPickup <- driverDistanceToPickup merchantOperatingCityId (getCoordinates res'.currPoint) (getCoordinates booking.fromLocation)
            buildBookingCancelationReason (Just res'.currPoint) (Just disToPickup) (Just booking.merchantId)
          Left err -> do
            logTagInfo "DriverLocationFetchFailed" $ show err
            buildBookingCancelationReason Nothing Nothing (Just booking.merchantId)
      Nothing -> buildBookingCancelationReason Nothing Nothing (Just booking.merchantId)
  DB.runTransaction $ QBCR.upsert cancellationReason
  return $
    CancelRes
      { bppBookingId = bppBookingId,
        bppId = booking.providerId,
        bppUrl = booking.providerUrl,
        cancellationSource = SBCR.ByUser,
        transactionId = booking.transactionId,
        merchant = merchant
      }
  where
    buildBookingCancelationReason currentDriverLocation disToPickup merchantId = do
      let CancelReq {..} = req
      return $
        SBCR.BookingCancellationReason
          { bookingId = bookingId,
            rideId = Nothing,
            merchantId = merchantId,
            source = SBCR.ByUser,
            reasonCode = Just reasonCode,
            reasonStage = Just reasonStage,
            additionalInfo = additionalInfo,
            driverCancellationLocation = currentDriverLocation,
            driverDistToPickup = disToPickup,
            ..
          }

isBookingCancellable :: EsqDBFlow m r => SRB.Booking -> m Bool
isBookingCancellable booking
  | booking.status `elem` [SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT] = pure True
  | booking.status == SRB.TRIP_ASSIGNED = do
    ride <- QR.findActiveByRBId booking.id >>= fromMaybeM (RideDoesNotExist $ "BookingId: " <> booking.id.getId)
    pure (ride.status == Ride.NEW)
  | otherwise = pure False

mkDomainCancelSearch ::
  (HasFlowEnv m r '["nwAddress" ::: BaseUrl], EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, HasCacheConfig r, HedisFlow m r) =>
  Id Person.Person ->
  Id DEstimate.Estimate ->
  m CancelSearch
mkDomainCancelSearch personId estimateId = do
  estStatus <- QEstimate.getStatus estimateId >>= fromMaybeM (EstimateStatusDoesNotExist estimateId.getId)
  let sendToBpp = estStatus /= DEstimate.NEW
  buildCancelReq estimateId sendToBpp estStatus
  where
    buildCancelReq estId sendToBpp estStatus = do
      estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
      person <- Esq.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      merchant <- CQM.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
      let searchRequestId = estimate.requestId
      pure
        CancelSearch
          { estimateId = estId,
            providerUrl = estimate.providerUrl,
            providerId = estimate.providerId,
            searchReqId = searchRequestId,
            estimateStatus = estStatus,
            sendToBpp,
            merchant = merchant,
            ..
          }

cancelSearch ::
  (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r) =>
  Id Person.Person ->
  CancelSearch ->
  m ()
cancelSearch personId dcr = do
  if dcr.estimateStatus == DEstimate.GOT_DRIVER_QUOTE
    then Esq.runTransaction $ do
      Esq.runTransaction $ QPFS.updateStatus personId DPFS.IDLE
      QEstimate.updateStatus dcr.estimateId DEstimate.DRIVER_QUOTE_CANCELLED
      QDOffer.updateStatus dcr.estimateId DDO.INACTIVE
    else do
      Esq.runTransaction $ do
        Esq.runTransaction $ QPFS.updateStatus personId DPFS.IDLE
        QEstimate.updateStatus dcr.estimateId DEstimate.CANCELLED
        QDOffer.updateStatus dcr.estimateId DDO.INACTIVE
  QPFS.clearCache personId

driverDistanceToPickup ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Metrics.CoreMetrics m,
    Maps.HasCoordinates tripStartPos,
    Maps.HasCoordinates tripEndPos
  ) =>
  Id DMOC.MerchantOperatingCity ->
  tripStartPos ->
  tripEndPos ->
  m Meters
driverDistanceToPickup merchantOperatingCityId tripStartPos tripEndPos = do
  distRes <-
    Maps.getDistanceForCancelRide merchantOperatingCityId $
      Maps.GetDistanceReq
        { origin = tripStartPos,
          destination = tripEndPos,
          travelMode = Just Maps.CAR
        }
  return $ distRes.distance
