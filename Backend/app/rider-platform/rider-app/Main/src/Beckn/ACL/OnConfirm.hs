{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnConfirm (buildOnConfirmReqV2) where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import Data.Fixed (Fixed (MkFixed))
import qualified Domain.Action.Beckn.OnConfirm as DOnConfirm
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Error

buildOnConfirmReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text],
    MonadTime m
  ) =>
  Spec.OnConfirmReq ->
  Bool ->
  m (Maybe DOnConfirm.OnConfirmReq)
buildOnConfirmReqV2 req isValueAddNP' = do
  ContextV2.validateContext Context.ON_CONFIRM req.onConfirmReqContext
  currTime <- getCurrentTime
  handleErrorV2 req $ \message -> do
    case parseData message isValueAddNP' currTime of
      Right dReq -> return $ Just dReq
      Left err -> throwError . InvalidBecknSchema $ "on_confirm error:-" <> show err
  where
    parseData :: Spec.ConfirmReqMessage -> Bool -> UTCTime -> Either Text DOnConfirm.OnConfirmReq
    parseData message isValueAddNP now = do
      let order = message.confirmReqMessageOrder
      bppBookingIdText <- order.orderId & maybe (Left "Missing OrderId") Right
      let bppBookingId = Id bppBookingIdText
          fulf = order.orderFulfillments >>= listToMaybe
          mbRideOtp =
            fulf >>= (.fulfillmentStops) >>= Utils.getStartLocation >>= (.stopAuthorization)
              >>= \auth -> if auth.authorizationType == Just (show Enums.OTP) then auth.authorizationToken else Nothing

      let fulfState = fulf >>= (.fulfillmentState) >>= (.fulfillmentStateDescriptor) >>= (.descriptorCode)
      let isRideAssigned = fulfState == Just (show Enums.RIDE_ASSIGNED)

      if isRideAssigned
        then do
          let driverImage = fulf >>= (.fulfillmentAgent) >>= (.agentPerson) >>= (.personImage) >>= (.imageUrl)
              driverMobileCountryCode = Just "+91" -- TODO: check how to get countrycode via ONDC
              driverRating = Just $ toCentesimal 5 -- Default value for driver_rating for not-value-add-np.
              oneYearAgo = - (365 * 24 * 60 * 60)
              driverRegisteredAt = Just $ addUTCTime oneYearAgo now
              isDriverBirthDay = False
              isFreeRide = False

          rideOtp <- maybe (Left "Missing rideOtp in on_confirm") Right mbRideOtp
          bppRideId <- fulf >>= (.fulfillmentId) & maybe (Left "Missing fulfillmentId") (Right . Id)
          driverName <- fulf >>= (.fulfillmentAgent) >>= (.agentPerson) >>= (.personName) & maybe (Left "Missing fulfillment.agent.person.name in on_confirm") Right
          driverMobileNumber <- fulf >>= (.fulfillmentAgent) >>= (.agentContact) >>= (.contactPhone) & maybe (Left "Missing fulfillment.agent.contact.phone in on_confirm") Right

          vehicleNumber <- fulf >>= (.fulfillmentVehicle) >>= (.vehicleRegistration) & maybe (Left "Missing fulfillment.vehicle.registration in on_confirm") Right
          vehicleColor <- fulf >>= (.fulfillmentVehicle) >>= (.vehicleColor) & maybe (Left "Missing fulfillment.vehicle.color in on_confirm") Right
          vehicleModel <- fulf >>= (.fulfillmentVehicle) >>= (.vehicleModel) & maybe (Left "Missing fulfillment.vehicle.model in on_confirm") Right

          Right $ DOnConfirm.RideAssigned DOnConfirm.RideAssignedInfo {..}
        else do
          -- when its not a value-add-np ride flow, we need on_confirm to have RIDE_ASSIGNED state.
          if not isValueAddNP
            then Left $ "Invalid fulfillment state descriptor code in on_confirm:-" <> show fulfState <> ",expected:-" <> show Enums.RIDE_ASSIGNED
            else Right $ DOnConfirm.BookingConfirmed DOnConfirm.BookingConfirmedInfo {bppBookingId, specialZoneOtp = mbRideOtp}

handleErrorV2 ::
  (MonadFlow m) =>
  Spec.OnConfirmReq ->
  (Spec.ConfirmReqMessage -> m (Maybe DOnConfirm.OnConfirmReq)) ->
  m (Maybe DOnConfirm.OnConfirmReq)
handleErrorV2 req action =
  case req.onConfirmReqError of
    Nothing -> req.onConfirmReqMessage & maybe (pure Nothing) action
    Just err -> do
      logTagError "on_confirm req" $ "on_confirm error:-" <> show err
      pure Nothing

toCentesimal :: Int -> Centesimal
toCentesimal = Centesimal . MkFixed . toInteger
