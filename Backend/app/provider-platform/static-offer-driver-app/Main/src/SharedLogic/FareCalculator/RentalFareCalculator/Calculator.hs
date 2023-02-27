{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module SharedLogic.FareCalculator.RentalFareCalculator.Calculator
  ( RentalFareParameters (..),
    calculateRentalFareParameters,
    rentalFareSum,
    rentalFareSumWithDiscount,
  )
where

import Data.Time
  ( LocalTime (localDay),
    TimeZone,
    diffDays,
    minutesToTimeZone,
    utcToLocalTime,
  )
import Domain.Types.FarePolicy.RentalFarePolicy (RentalFarePolicy)
import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common

type TripStartTime = UTCTime

type TripStopTime = UTCTime

data RentalFareParameters = RentalFareParameters
  { baseFare :: Money,
    extraDistanceFare :: Money,
    extraTimeFare :: Money,
    nextDaysFare :: Maybe Money,
    discount :: Maybe Money,
    farePolicy :: RentalFarePolicy
  }
  deriving stock (Show, Eq)

rentalFareSum :: RentalFareParameters -> Money
rentalFareSum RentalFareParameters {..} = do
  let nextDaysFare' = fromMaybe 0 nextDaysFare
  baseFare + extraDistanceFare + extraTimeFare + nextDaysFare'

rentalFareSumWithDiscount :: RentalFareParameters -> Money
rentalFareSumWithDiscount fp@RentalFareParameters {..} = do
  let fareSumm = rentalFareSum fp
  max 0 $ maybe fareSumm (fareSumm -) discount

calculateRentalFareParameters ::
  DRentalFP.RentalFarePolicy ->
  Meters ->
  TripStartTime ->
  TripStopTime ->
  RentalFareParameters
calculateRentalFareParameters farePolicy distance startTime stopTime = do
  let baseFare = farePolicy.baseFare
      extraDistanceFare = calculateExtraDistanceFare farePolicy distance
      extraTimeFare = calculateExtraTimeFare farePolicy startTime stopTime
      nextDaysFare = calculateNextDaysFare farePolicy startTime stopTime
      discount = Nothing -- rental discount is not implemented yet
  RentalFareParameters {..}

calculateExtraDistanceFare ::
  RentalFarePolicy ->
  Meters ->
  Money
calculateExtraDistanceFare farePolicy distance = roundToIntegral $ do
  let distanceInKm = metersToKilometers distance
  let extraDistance = distanceInKm - farePolicy.baseDistance
  if extraDistance > 0
    then realToFrac (toRational extraDistance) * farePolicy.extraKmFare
    else 0

calculateExtraTimeFare ::
  RentalFarePolicy ->
  TripStartTime ->
  TripStopTime ->
  Money
calculateExtraTimeFare farePolicy tripStartTime tripStopTime = roundToIntegral $ do
  let tripTime = diffUTCTime tripStopTime tripStartTime
      tripTimeInMinutes = nominalDiffTimeToSeconds tripTime `div` 60
      extraTime = toInteger tripTimeInMinutes - toInteger farePolicy.baseDuration * 60
  if extraTime > 0
    then realToFrac (toRational extraTime) * farePolicy.extraMinuteFare
    else 0

calculateNextDaysFare ::
  RentalFarePolicy ->
  TripStartTime ->
  TripStopTime ->
  Maybe Money
calculateNextDaysFare farePolicy tripStartTime tripStopTime = do
  let tripDays = calcTripDays tripStartTime tripStopTime
  if tripDays > 0
    then calcNextDaysFare tripDays <$> farePolicy.driverAllowanceForDay
    else Nothing
  where
    calcNextDaysFare :: Integer -> Money -> Money
    calcNextDaysFare tripDays nextDaysFare = fromIntegral tripDays * nextDaysFare

timeZoneIST :: TimeZone
timeZoneIST = minutesToTimeZone 330 -- TODO: Should be configurable. Hardcoded to IST +0530

-- Calculate how many days the trip lasts.
-- Every day is added at midnight local time
calcTripDays :: TripStartTime -> TripStopTime -> Integer
calcTripDays tripStartTime tripStopTime = diffDays (calcDay tripStopTime) (calcDay tripStartTime)
  where
    calcDay = localDay . utcToLocalTime timeZoneIST
