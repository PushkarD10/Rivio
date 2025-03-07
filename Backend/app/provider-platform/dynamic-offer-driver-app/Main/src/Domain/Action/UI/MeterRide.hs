{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.MeterRide (postMeterRideAddDestination) where

import qualified API.Types.UI.MeterRide
import Data.OpenApi (ToSchema)
import qualified Domain.Action.UI.FareCalculator as AUF
import qualified Domain.Action.UI.Ride as AUR
import Domain.Types
import Domain.Types.Location (Location (..), LocationAddress)
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified SharedLogic.CallBAPInternal as CallBAPInternal
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Ride as QRide
import Tools.Auth
import Tools.Error

postMeterRideAddDestination ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    API.Types.UI.MeterRide.MeterRideAddDestinationReq ->
    Environment.Flow API.Types.UI.MeterRide.MeterRideAddDestinationResp
  )
postMeterRideAddDestination (_mbPersonId, merchantId, merchantOpCityId) rideId meterRideRequest = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  whenJust ride.toLocation $ \_ -> throwError $ InvalidRequest ("Ride already has a drop location for meter ride " <> ride.id.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  unless (booking.tripCategory == OneWay MeterRide) $
    throwError $ InvalidRequest ("Invalid trip category " <> show booking.tripCategory)

  fareTillNow <- AUF.calculateFareUtil merchantId merchantOpCityId Nothing (LatLong ride.fromLocation.lat ride.fromLocation.lon) (Just $ highPrecMetersToMeters ride.traveledDistance) Nothing Nothing (OneWay MeterRide)
  (_, mbDistance, mbDuration, mbRoute, _) <- AUF.calculateDistanceAndRoutes merchantId merchantOpCityId 100 [meterRideRequest.currentLatLong, meterRideRequest.destinationLatLong]
  fare <- AUF.calculateFareUtil merchantId merchantOpCityId (Just meterRideRequest.destinationLatLong) meterRideRequest.currentLatLong mbDistance mbDuration mbRoute (OneWay MeterRide)
  fareTillNow' <- Kernel.Prelude.listToMaybe fareTillNow.estimatedFares & fromMaybeM (InternalError ("Failed to calculate fareTillNow for given request: " <> ride.id.getId <> " RequestBody: " <> show meterRideRequest.destinationLatLong))
  fare' <- Kernel.Prelude.listToMaybe fare.estimatedFares & fromMaybeM (InternalError ("Failed to calculate fare for given request: " <> ride.id.getId <> " RequestBody: " <> show meterRideRequest.destinationLatLong))
  let estimatedFare = fareTillNow'.minFare + fare'.minFare
      estimatedDistance = highPrecMetersToMeters ride.traveledDistance + fromMaybe 0 mbDistance

  dropLocation <- buildLocation merchantId merchantOpCityId meterRideRequest.destinationLatLong meterRideRequest.destinationLocation
  QL.create dropLocation
  newRideDropLocationMap <- SLM.buildDropLocationMapping dropLocation.id rideId.getId DLM.RIDE (Just merchantId) (Just merchantOpCityId)
  QLM.create newRideDropLocationMap
  newBookingDropLocationMap <- SLM.buildDropLocationMapping dropLocation.id ride.bookingId.getId DLM.BOOKING (Just merchantId) (Just merchantOpCityId)
  QLM.create newBookingDropLocationMap
  fork "update in bap" $ do
    appBackendBapInternal <- asks (.appBackendBapInternal)
    void $ CallBAPInternal.meterRideAddDestination appBackendBapInternal.apiKey appBackendBapInternal.url rideId.getId meterRideRequest
  void $ QBooking.updateEstimatedDistanceAndFare estimatedFare (Just estimatedDistance) booking.id
  pure $ API.Types.UI.MeterRide.MeterRideAddDestinationResp {..}

buildLocation ::
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  LatLong ->
  LocationAddress ->
  Environment.Flow Location
buildLocation merchantId merchantOperatingCityId gps locationAddress = do
  guid <- generateGUID
  now <- getCurrentTime
  return $
    Location
      { id = guid,
        createdAt = now,
        updatedAt = now,
        lat = gps.lat,
        lon = gps.lon,
        address = locationAddress,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOperatingCityId
      }
