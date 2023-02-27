{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module SharedLogic.FareCalculator.OneWayFareCalculator.Calculator
  ( OneWayFareParameters (..),
    TripEndTime,
    fareSum,
    fareSumWithDiscount,
    calculateFareParameters,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Time
  ( LocalTime (localTimeOfDay),
    TimeOfDay (..),
    TimeZone,
    midnight,
    minutesToTimeZone,
    utcToLocalTime,
  )
import Domain.Types.FarePolicy.OneWayFarePolicy (OneWayFarePolicy)
import Domain.Types.FarePolicy.OneWayFarePolicy.PerExtraKmRate (PerExtraKmRateD (..))
import Kernel.Prelude
import Kernel.Types.Common

type TripEndTime = UTCTime

data OneWayFareParameters = OneWayFareParameters
  { baseFare :: Money,
    distanceFare :: Money,
    nightShiftRate :: Centesimal,
    waitingChargePerMin :: Maybe Money,
    discount :: Maybe Money
  }
  deriving stock (Show, Eq)

fareSum :: OneWayFareParameters -> Money
fareSum OneWayFareParameters {..} =
  roundToIntegral $ nightShiftRate * fromIntegral (baseFare + distanceFare)

fareSumWithDiscount :: OneWayFareParameters -> Money
fareSumWithDiscount fp@OneWayFareParameters {..} = do
  let fareSumm = fareSum fp
  max 0 $ maybe fareSumm (fareSumm -) discount

calculateFareParameters ::
  OneWayFarePolicy ->
  Meters ->
  TripEndTime ->
  OneWayFareParameters
calculateFareParameters farePolicy distance endTime = do
  let baseFare = calculateBaseFare farePolicy
  let distanceFare = calculateDistanceFare farePolicy distance
  let nightShiftRate = calculateNightShiftRate farePolicy endTime
  let waitingChargePerMin = farePolicy.waitingChargePerMin
  let discount = calculateDiscount farePolicy endTime
  OneWayFareParameters baseFare distanceFare nightShiftRate waitingChargePerMin discount

calculateBaseFare ::
  OneWayFarePolicy ->
  Money
calculateBaseFare farePolicy = fromMaybe 0 $ farePolicy.baseFare

calculateDistanceFare ::
  OneWayFarePolicy ->
  Meters ->
  Money
calculateDistanceFare farePolicy distance = do
  let sortedPerExtraKmRateList = NonEmpty.sortBy (compare `on` (.distanceRangeStart)) farePolicy.perExtraKmRateList -- sort it again just in case
  let baseDistance = (.distanceRangeStart) $ NonEmpty.head sortedPerExtraKmRateList
      extraDistance = distance - baseDistance
  if extraDistance <= 0
    then 0
    else do
      roundToIntegral $ calculateExtraDistFare 0 extraDistance sortedPerExtraKmRateList
  where
    calculateExtraDistFare summ extraDist (PerExtraKmRate lowerBorder perKmRate :| sndPerExtraKmRate@(PerExtraKmRate upperBorder _) : perExtraKmRateList) = do
      let boundSize = upperBorder - lowerBorder
      let distWithinBounds = min extraDist boundSize
          fareWithinBounds = fromIntegral distWithinBounds / 1000 * perKmRate
      calculateExtraDistFare (summ + fareWithinBounds) (extraDist - distWithinBounds) (sndPerExtraKmRate :| perExtraKmRateList)
    calculateExtraDistFare summ 0 _ = summ
    calculateExtraDistFare summ extraDist (PerExtraKmRate _ perKmRate :| []) = summ + (fromIntegral extraDist / 1000 * perKmRate)

calculateNightShiftRate ::
  OneWayFarePolicy ->
  TripEndTime ->
  Centesimal
calculateNightShiftRate farePolicy tripStartTime = do
  let timeOfDay = localTimeOfDay $ utcToLocalTime timeZoneIST tripStartTime
  let nightShiftRate = fromMaybe 1 farePolicy.nightShiftRate
  let nightShiftStart = fromMaybe midnight farePolicy.nightShiftStart
  let nightShiftEnd = fromMaybe midnight farePolicy.nightShiftEnd
  if isTimeWithinBounds nightShiftStart nightShiftEnd timeOfDay
    then nightShiftRate
    else 1

calculateDiscount :: OneWayFarePolicy -> TripEndTime -> Maybe Money
calculateDiscount farePolicy tripStartTime = do
  let discount = calculateDiscount' 0 farePolicy.discountList
  if discount <= 0 then Nothing else Just discount
  where
    calculateDiscount' summ (discount : discountList) = do
      if discount.enabled && (discount.fromDate <= tripStartTime && tripStartTime <= discount.toDate)
        then calculateDiscount' (summ + discount.discount) discountList
        else calculateDiscount' summ discountList
    calculateDiscount' summ [] = summ

timeZoneIST :: TimeZone
timeZoneIST = minutesToTimeZone 330 -- TODO: Should be configurable. Hardcoded to IST +0530

isTimeWithinBounds :: TimeOfDay -> TimeOfDay -> TimeOfDay -> Bool
isTimeWithinBounds startTime endTime time =
  if startTime >= endTime
    then do
      let midnightBeforeTimeleap = TimeOfDay 23 59 60
      (startTime < time && time < midnightBeforeTimeleap) || (midnight <= time && time < endTime)
    else startTime < time && time < endTime
