{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Payment
  ( DPayment.PaymentStatusResp (..),
    createOrder,
    getStatus,
    getOrder,
    juspayWebhookHandler,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Juspay as Juspay
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (Value)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import Servant (BasicAuthData)
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import Tools.Metrics
import qualified Tools.Payment as Payment

-- create order -----------------------------------------------------

createOrder ::
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Id DRide.Ride ->
  Flow Payment.CreateOrderResp
createOrder (personId, merchantId, merchantOperatingCityId) rideId = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.status == DRide.COMPLETED) $ throwError (RideInvalidStatus $ show ride.status)
  totalFare <- ride.totalFare & fromMaybeM (RideFieldNotPresent "totalFare")
  person <- runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound $ getId personId)
  riderId <- runInReplica $ QRide.findRiderIdByRideId ride.id >>= fromMaybeM (InternalError "riderId not found")
  unless (person.id == riderId) $ throwError NotAnExecutor
  customerEmail <- person.email & fromMaybeM (PersonFieldNotPresent "email") >>= decrypt
  customerPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  let createOrderReq =
        Payment.CreateOrderReq
          { orderShortId = ride.shortId.getShortId, -- should be Alphanumeric with character length less than 18.
            amount = totalFare,
            customerId = person.id.getId,
            customerEmail,
            customerPhone,
            paymentPageClientId = "yatrisathi",
            customerFirstName = person.firstName,
            customerLastName = person.lastName
          }

  let commonMerchantId = cast @DM.Merchant @DPayment.Merchant merchantId
      commonPersonId = cast @DP.Person @DPayment.Person personId
      orderId = cast @DRide.Ride @DOrder.PaymentOrder rideId
      createOrderCall = Payment.createOrder merchantOperatingCityId -- api call
  DPayment.createOrderService commonMerchantId commonPersonId orderId createOrderReq createOrderCall

-- order status -----------------------------------------------------

getStatus ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    CoreMetrics m
  ) =>
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Id DOrder.PaymentOrder ->
  m DPayment.PaymentStatusResp
getStatus (personId, _, merchantOperatingCityId) orderId = do
  let commonPersonId = cast @DP.Person @DPayment.Person personId
      orderStatusCall = Payment.orderStatus merchantOperatingCityId -- api call
  DPayment.orderStatusService commonPersonId orderId orderStatusCall

getOrder ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    CoreMetrics m
  ) =>
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Id DOrder.PaymentOrder ->
  m DOrder.PaymentOrderAPIEntity
getOrder (personId, _, _) orderId = do
  order <- runInReplica $ QOrder.findById orderId >>= fromMaybeM (PaymentOrderNotFound orderId.getId)
  unless (order.personId == cast personId) $ throwError NotAnExecutor
  mkOrderAPIEntity order

mkOrderAPIEntity :: EncFlow m r => DOrder.PaymentOrder -> m DOrder.PaymentOrderAPIEntity
mkOrderAPIEntity DOrder.PaymentOrder {..} = do
  clientAuthToken_ <- decrypt clientAuthToken
  return $ DOrder.PaymentOrderAPIEntity {clientAuthToken = clientAuthToken_, ..}

-- webhook ----------------------------------------------------------

juspayWebhookHandler ::
  Id DMOC.MerchantOperatingCity ->
  BasicAuthData ->
  Value ->
  Flow AckResponse
juspayWebhookHandler merchantOperatingCityId authData value = do
  merchantServiceConfig <-
    CQMSC.findByMerchantIdAndService merchantOperatingCityId (DMSC.PaymentService Payment.Juspay)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOperatingCityId.getId "Payment" (show Payment.Juspay))
  case merchantServiceConfig.serviceConfig of
    DMSC.PaymentServiceConfig psc -> do
      _ <- Juspay.orderStatusWebhook psc DPayment.juspayWebhookService authData value
      pure Ack
    _ -> throwError $ InternalError "Unknown Service Config"
