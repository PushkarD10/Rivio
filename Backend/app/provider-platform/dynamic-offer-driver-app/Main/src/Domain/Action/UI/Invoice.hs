{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.Invoice (getInvoice) where

import qualified API.Types.UI.Invoice
import Control.Monad (msum)
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Kernel.Types.Distance (Meters (..), metersToHighPrecMeters)
import Kernel.Types.Error (PersonError (PersonNotFound))
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import Servant
import qualified Storage.Clickhouse.Ride as CHR
import qualified Storage.Clickhouse.RideDetails as CHRD
import qualified Storage.Queries.Person as QP
import Tools.Auth

getInvoice ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) ->
  Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) ->
  Kernel.Prelude.Maybe (Kernel.Prelude.Text) ->
  Environment.Flow [API.Types.UI.Invoice.InvoiceRes]
getInvoice (mbPersonId, _merchantId, _merchantOpCityId) mbFromDate mbToDate mbRcNo = do
  driver <- join <$> traverse QP.findById mbPersonId >>= fromMaybeM (PersonNotFound $ show mbPersonId)
  rideLs <- CHR.getAllCompletedRidesByDriverId driver.id mbFromDate mbToDate
  mapM (makeInvoiceResponse driver) rideLs
  where
    makeInvoiceResponse driver ride = do
      mbVehicleNumber <- msum <$> CHRD.findByIdAndVehicleNumber (Kernel.Types.Id.cast ride.id) mbRcNo
      pure $
        API.Types.UI.Invoice.InvoiceRes
          { date = ride.createdAt,
            driverName = unwords [driver.firstName, fromMaybe "" driver.lastName],
            vehicleNumber = fromMaybe "N/A" mbVehicleNumber,
            chargeableDistance = metersToHighPrecMeters . Meters $ fromMaybe 0 ride.chargeableDistance,
            fare = fromMaybe 0 ride.fare
          }
