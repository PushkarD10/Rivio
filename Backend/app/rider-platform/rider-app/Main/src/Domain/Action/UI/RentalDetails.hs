module Domain.Action.UI.RentalDetails where

import Domain.Types.RentalDetails
import Kernel.Prelude
import Kernel.Types.Common

mkRentalDetailsAPIEntity :: RentalDetails -> Maybe PriceAPIEntity -> RentalDetailsAPIEntity
mkRentalDetailsAPIEntity RentalDetails {..} tollCharges = do
  RentalDetailsAPIEntity
    { baseFare = baseFare.amountInt,
      perHourCharge = perHourCharge.amountInt,
      perExtraMinRate = perExtraMinRate.amountInt,
      baseFareWithCurrency = mkPriceAPIEntity baseFare,
      perHourChargeWithCurrency = mkPriceAPIEntity perHourCharge,
      perExtraMinRateWithCurrency = mkPriceAPIEntity perExtraMinRate,
      plannedPerKmRate = plannedPerKmRate.amountInt,
      perExtraKmRate = perExtraKmRate.amountInt,
      plannedPerKmRateWithCurrency = mkPriceAPIEntity plannedPerKmRate,
      perExtraKmRateWithCurrency = mkPriceAPIEntity perExtraKmRate,
      nightShiftInfo = mkNightShiftInfoAPIEntity <$> nightShiftInfo,
      includedDistancePerHrWithUnit = convertMetersToDistance distanceUnit $ kilometersToMeters includedKmPerHr,
      deadKmFare = mkPriceAPIEntity deadKmFare,
      ..
    }
