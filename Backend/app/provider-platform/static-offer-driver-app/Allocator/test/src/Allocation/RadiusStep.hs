{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Allocation.RadiusStep where

import Allocation.Internal
import qualified Data.Map as Map
import Domain.Action.Allocation
import qualified Domain.Types.Booking as SRB
import Domain.Types.Person
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Test.Tasty
import Test.Tasty.HUnit

booking01Id :: Id SRB.Booking
booking01Id = Id "booking01"

driverPool1 :: [Id Driver]
driverPool1 = [Id "driver01", Id "driver02"]

driverPool2 :: [Id Driver]
driverPool2 = [Id "driver01", Id "driver02", Id "driver03"]

driverPool3 :: [Id Driver]
driverPool3 = [Id "driver01", Id "driver02", Id "driver03", Id "driver04"]

radiusStep :: TestTree
radiusStep = testCaseSteps "Allocation - Reject - Allocation - Reject - Allocation - Accept" $ \step -> do
  r@Repository {..} <- initRepository
  let driverPoolPerRide =
        Map.fromList
          [ ((booking01Id, 0), driverPool1),
            ((booking01Id, 1), driverPool2),
            ((booking01Id, 2), driverPool3)
          ]
  addBooking r booking01Id 0
  addDriverPool r driverPoolPerRide
  step "Requests for allocation both drivers from first radius step notified"
  addRequest Allocation r booking01Id
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r booking01Id (Id "driver01") Notified
  checkNotificationStatus r booking01Id (Id "driver02") Notified
  checkFreeNotificationStatus r booking01Id (Id "driver03")
  checkFreeNotificationStatus r booking01Id (Id "driver04")
  step "The both drivers reject, send requests to drivers from the second radius step"
  addResponse r booking01Id (Id "driver01") Reject
  addResponse r booking01Id (Id "driver02") Reject
  void $ process (handle r) org1 numRequestsToProcess
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r booking01Id (Id "driver01") Notified
  checkNotificationStatus r booking01Id (Id "driver02") Notified
  checkNotificationStatus r booking01Id (Id "driver03") Notified
  checkFreeNotificationStatus r booking01Id (Id "driver04")
  step "All drivers reject, send requests to drivers from the third radius step"
  addResponse r booking01Id (Id "driver01") Reject
  addResponse r booking01Id (Id "driver02") Reject
  addResponse r booking01Id (Id "driver03") Reject
  void $ process (handle r) org1 numRequestsToProcess
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r booking01Id (Id "driver01") Notified
  checkNotificationStatus r booking01Id (Id "driver02") Notified
  checkNotificationStatus r booking01Id (Id "driver03") Notified
  checkNotificationStatus r booking01Id (Id "driver04") Notified
  step "The 4rd driver agrees - the booking assigned to him"
  addResponse r booking01Id (Id "driver04") Accept
  void $ process (handle r) org1 numRequestsToProcess
  onRide2 <- readIORef onRideVar
  onRide2 @?= [Id "driver04"]
  checkRideStatus r booking01Id SRB.TRIP_ASSIGNED
