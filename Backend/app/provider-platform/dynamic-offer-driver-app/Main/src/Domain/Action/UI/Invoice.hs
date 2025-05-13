{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.Invoice (getInvoice) where

import qualified API.Types.UI.Invoice
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

getInvoice ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) ->
  Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) ->
  Kernel.Prelude.Maybe (Kernel.Prelude.Text) ->
  Environment.Flow [API.Types.UI.Invoice.InvoiceRes]
getInvoice (mbPersonId, merchantId) mbFromDate mbToDate mbRcNo = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  bookings <- CHB.findAllCompletedRiderBookingsByMerchantInRange merchantId personId from to
  invoices <- mapM makeInvoiceResponse bookings
  return $ catMaybes invoices
  where
    makeInvoiceResponse booking = do
      mbRide <- CHR.findRideByBookingId booking.id booking.createdAt
      case mbRide of
        Just ride -> do
          mbDestination <- case booking.toLocationId of
            Just toLocId -> CHL.findLocationById toLocId booking.createdAt
            Nothing -> return Nothing
          return $
            Just $
              DTInvoice.InvoiceRes
                { date = booking.createdAt,
                  driverName = fromMaybe notAvailableText ride.driverName,
                  vehicleNumber = fromMaybe notAvailableText ride.vehicleNumber,
                  chargeableDistance = ride.chargeableDistance,
                  fare = maybe notAvailableText show ride.totalFare
                }
        Nothing -> return Nothing
    getFareBreakup booking (tag, title) = do
      fareBreakup <- CHFB.findFareBreakupByBookingIdAndDescription booking.id tag booking.createdAt
      case fareBreakup of
        Just breakup -> return . Just $ DTInvoice.FareBreakup {price = maybe notAvailableText show breakup.amount, title}
        Nothing -> return Nothing
    notAvailableText = "N/A"
