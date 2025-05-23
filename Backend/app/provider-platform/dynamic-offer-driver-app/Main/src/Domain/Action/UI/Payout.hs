{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Domain.Action.UI.Payout
  ( juspayPayoutWebhookHandler,
    castPayoutOrderStatus,
    payoutProcessingLockKey,
    processPreviousPayoutAmount,
  )
where

import Data.Time (utctDay)
import qualified Domain.Types.DailyStats as DS
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Vehicle as DV
import Environment
import Kernel.Beam.Functions as B (runInReplica)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payout.Interface as Juspay
import qualified Kernel.External.Payout.Interface.Juspay as Juspay
import qualified Kernel.External.Payout.Interface.Types as IPayout
import qualified Kernel.External.Payout.Juspay.Types.Payout as Payout
import qualified Kernel.External.Payout.Types as TPayout
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import Servant (BasicAuthData)
import SharedLogic.Merchant
import Storage.Beam.Payment ()
import Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Vehicle as QV
import Tools.Error
import qualified Tools.Payout as Payout
import Utils.Common.Cac.KeyNameConstants

-- webhook ----------------------------------------------------------

juspayPayoutWebhookHandler ::
  ShortId DM.Merchant ->
  Maybe Context.City ->
  BasicAuthData ->
  Value ->
  Flow AckResponse
juspayPayoutWebhookHandler merchantShortId mbOpCity authData value = do
  merchant <- findMerchantByShortId merchantShortId
  merchanOperatingCityId <- CQMOC.getMerchantOpCityId Nothing merchant mbOpCity
  let merchantId = merchant.id
      serviceName' = DEMSC.PayoutService TPayout.Juspay
  merchantServiceConfig <-
    CQMSC.findByMerchantIdAndServiceWithCity merchantId serviceName' merchanOperatingCityId
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payout" (show TPayout.Juspay))
  psc <- case merchantServiceConfig.serviceConfig of
    DMSC.PayoutServiceConfig psc' -> pure psc'
    _ -> throwError $ InternalError "Unknown Service Config"
  orderStatusResp <- Juspay.payoutOrderStatusWebhook psc authData value
  osr <- case orderStatusResp of
    Nothing -> throwError $ InternalError "Order Contents not found."
    Just osr' -> pure osr'
  logDebug $ "Webhook Payout Resp: " <> show osr
  case osr of
    IPayout.OrderStatusPayoutResp {..} -> do
      payoutOrder <- QPayoutOrder.findByOrderId payoutOrderId >>= fromMaybeM (PayoutOrderNotFound payoutOrderId)
      unless (payoutOrder.status `elem` [Payout.SUCCESS, Payout.FULFILLMENTS_SUCCESSFUL]) do
        case payoutOrder.entityName of
          Just DPayment.DRIVER_DAILY_STATS -> do
            forM_ (listToMaybe =<< payoutOrder.entityIds) $ \dailyStatsId -> do
              dailyStats <- QDailyStats.findByPrimaryKey dailyStatsId >>= fromMaybeM (InternalError "DriverStats Not Found")
              Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey dailyStats.driverId.getId) 3 3 $ do
                let dPayoutStatus = castPayoutOrderStatus payoutStatus
                when (dailyStats.payoutStatus /= DS.Success) $ QDailyStats.updatePayoutStatusById dPayoutStatus dailyStatsId
              fork "Update Payout Status and Transactions for DailyStats" $ do
                callPayoutService dailyStats.driverId payoutOrderId
          Just DPayment.MANUAL -> do
            forM_ (listToMaybe =<< payoutOrder.entityIds) $ \driverId -> do
              fork "Update Payout Status and Transactions for Manual Payout" $ do
                callPayoutService (Id driverId) payoutOrderId
          Just DPayment.BACKLOG -> do
            whenJust payoutOrder.entityIds $ \entityIds -> do
              fork "Update Payout Status for Backlog" $ do
                mapM_ (updateStatsWithLock merchantId merchanOperatingCityId payoutStatus payoutOrderId) entityIds
          _ -> pure ()
      pure ()
    IPayout.BadStatusResp -> pure ()
  pure Ack
  where
    callPayoutService driverId payoutOrderId = do
      driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      let createPayoutOrderStatusReq = IPayout.PayoutOrderStatusReq {orderId = payoutOrderId}
          serviceName = DEMSC.PayoutService TPayout.Juspay
          createPayoutOrderStatusCall = Payout.payoutOrderStatus driver.merchantId driver.merchantOperatingCityId serviceName
      void $ DPayment.payoutStatusService (cast driver.merchantId) (cast driver.id) createPayoutOrderStatusReq createPayoutOrderStatusCall

    updateStatsWithLock merchantId merchanOperatingCityId payoutStatus payoutOrderId dStatsId = do
      let dPayoutStatus = castPayoutOrderStatus payoutStatus
      dailyStats <- QDailyStats.findByPrimaryKey dStatsId >>= fromMaybeM (InternalError "DriverStats Not Found")
      Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey dailyStats.driverId.getId) 3 3 $ do
        when (dailyStats.payoutStatus /= DS.Success) $ QDailyStats.updatePayoutStatusById dPayoutStatus dStatsId
      let createPayoutOrderStatusReq = IPayout.PayoutOrderStatusReq {orderId = payoutOrderId}
          serviceName = DEMSC.PayoutService TPayout.Juspay
          createPayoutOrderStatusCall = Payout.payoutOrderStatus merchantId merchanOperatingCityId serviceName
      void $ DPayment.payoutStatusService (cast merchantId) (cast dailyStats.driverId) createPayoutOrderStatusReq createPayoutOrderStatusCall

castPayoutOrderStatus :: Payout.PayoutOrderStatus -> DS.PayoutStatus
castPayoutOrderStatus payoutOrderStatus =
  case payoutOrderStatus of
    Payout.SUCCESS -> DS.Success
    Payout.FULFILLMENTS_SUCCESSFUL -> DS.Success
    Payout.ERROR -> DS.Failed
    Payout.FAILURE -> DS.Failed
    Payout.FULFILLMENTS_FAILURE -> DS.Failed
    Payout.CANCELLED -> DS.ManualReview
    Payout.FULFILLMENTS_CANCELLED -> DS.ManualReview
    Payout.FULFILLMENTS_MANUAL_REVIEW -> DS.ManualReview
    _ -> DS.Processing

payoutProcessingLockKey :: Text -> Text
payoutProcessingLockKey driverId = "Payout:Processing:DriverId" <> driverId

processPreviousPayoutAmount :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Id Person.Person -> Maybe Text -> Id DMOC.MerchantOperatingCity -> m ()
processPreviousPayoutAmount personId mbVpa merchOpCity = do
  mbVehicle <- QV.findById personId
  let vehicleCategory = fromMaybe DV.AUTO_CATEGORY ((.category) =<< mbVehicle)
  payoutConfig <- CPC.findByPrimaryKey merchOpCity vehicleCategory >>= fromMaybeM (InternalError "Payout config not present")
  when payoutConfig.isPayoutEnabled $ do
    redisLockDriverId <- Redis.tryLockRedis lockKey 10800
    dailyStats_ <- if not redisLockDriverId then pure [] else QDailyStats.findAllByPayoutStatusAndReferralEarningsAndDriver DS.Verifying personId
    transporterConfig <- SCTC.findByMerchantOpCityId merchOpCity (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchOpCity.getId)
    localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
    let dailyStats = filter (\ds -> (ds.activatedValidRides <= transporterConfig.maxPayoutReferralForADay) && ds.merchantLocalDate /= (utctDay localTime)) dailyStats_ -- filter out the flagged payouts and current day payout earning
    when (length dailyStats > 0) $ do
      person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      let statsIds = map (.id) dailyStats
          pendingAmount = sum $ map (.referralEarnings) dailyStats
      case (mbVpa, pendingAmount <= payoutConfig.thresholdPayoutAmountPerPerson) of
        (Just vpa, True) -> do
          uid <- generateGUID
          Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey personId.getId) 3 3 $ do
            mapM_ (QDailyStats.updatePayoutStatusById DS.Processing) statsIds
            mapM_ (QDailyStats.updatePayoutOrderId (Just uid)) statsIds
          phoneNo <- mapM decrypt person.mobileNumber
          let createPayoutOrderReq =
                Juspay.CreatePayoutOrderReq
                  { orderId = uid,
                    amount = pendingAmount,
                    customerPhone = fromMaybe "6666666666" phoneNo, -- dummy no.
                    customerEmail = fromMaybe "dummymail@gmail.com" person.email, -- dummy mail
                    customerId = personId.getId,
                    orderType = payoutConfig.orderType,
                    remark = payoutConfig.remark,
                    customerName = person.firstName,
                    customerVpa = vpa
                  }
          let serviceName = DEMSC.PayoutService TPayout.Juspay
          let entityName = DPayment.BACKLOG
              createPayoutOrderCall = Payout.createPayoutOrder person.merchantId merchOpCity serviceName
          merchantOperatingCity <- CQMOC.findById (cast merchOpCity) >>= fromMaybeM (MerchantOperatingCityNotFound merchOpCity.getId)
          logDebug $ "calling create payoutOrder with driverId: " <> personId.getId <> " | amount: " <> show pendingAmount <> " | orderId: " <> show uid
          void $ DPayment.createPayoutService (cast person.merchantId) (cast personId) (Just statsIds) (Just entityName) (show merchantOperatingCity.city) createPayoutOrderReq createPayoutOrderCall
        (_, False) -> do
          Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey personId.getId) 3 3 $ do
            mapM_ (QDailyStats.updatePayoutStatusById DS.ManualReview) statsIds -- don't pay if amount is greater than threshold amount
        _ -> pure ()
  where
    lockKey = "ProcessBacklogPayout:DriverId-" <> personId.getId
