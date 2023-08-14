{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Plan where

import qualified Data.List as List
import Data.OpenApi (ToSchema (..))
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.DriverPlan
import Domain.Types.Mandate (MandateStatus)
import qualified Domain.Types.Mandate as DM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import Domain.Types.Plan as P
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.Payment as SPayment
import qualified Storage.CachedQueries.DriverInformation as CDI
import qualified Storage.CachedQueries.Merchant.TransporterConfig as QTC
import qualified Storage.CachedQueries.Plan as QPD
import Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.Invoice as QINV
import qualified Storage.Queries.Mandate as QM
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Notifications
import Tools.Payment as Payment
import Tools.SMS as Sms hiding (Success)

---------------------------------------------------------------------------------------------------------
--------------------------------------- Request & Response Types ----------------------------------------
---------------------------------------------------------------------------------------------------------

data PlanListAPIRes = PlanListAPIRes
  { list :: [PlanEntity],
    subscriptionStartTime :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PlanEntity = PlanEntity
  { id :: Text,
    name :: Text,
    description :: Text,
    planFareBreakup :: [PlanFareBreakup],
    freeRideCount :: Int,
    frequency :: Text,
    offers :: [OfferEntity],
    paymentMode :: PaymentMode,
    totalPlanCreditLimit :: Money,
    currentDues :: Money
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PlanFareBreakup = PlanFareBreakup
  { component :: Text,
    amount :: HighPrecMoney
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data OfferEntity = OfferEntity
  { title :: Maybe Text,
    description :: Maybe Text,
    tnc :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CurrentPlanRes = CurrentPlanRes
  { currentPlanDetails :: PlanEntity,
    mandateDetails :: Maybe MandateDetailsEntity,
    autoPayStatus :: Maybe DI.DriverAutoPayStatus,
    subscribed :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PlanSubscribeRes = PlanSubscribeRes
  { orderId :: Id DOrder.PaymentOrder,
    orderResp :: Payment.CreateOrderResp
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MandateDetailsEntity = MandateDetails
  { status :: MandateStatus,
    startDate :: UTCTime,
    endDate :: UTCTime,
    mandateId :: Text,
    payerVpa :: Maybe Text,
    frequency :: Text,
    maxAmount :: Money
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

---------------------------------------------------------------------------------------------------------
--------------------------------------------- Controllers -----------------------------------------------
---------------------------------------------------------------------------------------------------------

-- This API is for listing all the AUTO PAY plans
planList :: (Id SP.Person, Id DM.Merchant) -> Maybe Int -> Maybe Int -> Flow PlanListAPIRes
planList (driverId, merchantId) _mbLimit _mbOffset = do
  plans <- QPD.findByMerchantIdAndPaymentMode merchantId AUTOPAY
  transporterConfig <- QTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  plansList <- mapM (convertPlanToPlanEntity driverId) plans
  return $
    PlanListAPIRes
      { list = plansList,
        subscriptionStartTime = transporterConfig.subscriptionStartTime
      }

-- This API is for listing current driver plan
currentPlan :: (Id SP.Person, Id DM.Merchant) -> Flow CurrentPlanRes
currentPlan (driverId, _merchantId) = do
  driverInfo <- CDI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  driverPlan <- B.runInReplica $ QDPlan.findByDriverId driverId >>= fromMaybeM (NoCurrentPlanForDriver driverId.getId)
  plan <- QPD.findByIdAndPaymentMode driverPlan.planId (getDriverPaymentMode driverInfo.autoPayStatus) >>= fromMaybeM (PlanNotFound driverPlan.planId.getId)
  mandateDetailsEntity <- mkMandateDetailEntity driverPlan.mandateId
  currentPlanEntity <- convertPlanToPlanEntity driverId plan
  return CurrentPlanRes {currentPlanDetails = currentPlanEntity, mandateDetails = mandateDetailsEntity, autoPayStatus = driverInfo.autoPayStatus, subscribed = driverInfo.subscribed}
  where
    getDriverPaymentMode = \case
      Just DI.ACTIVE -> AUTOPAY
      Just DI.SUSPENDED -> MANUAL
      Just DI.PAUSED_PSP -> MANUAL
      Just DI.CANCELLED_PSP -> MANUAL
      _ -> MANUAL

-- This API is to create a mandate order if the driver has not subscribed to Mandate even once or has Cancelled Mandate from PSP App.
planSubscribe :: Id Plan -> Bool -> (Id SP.Person, Id DM.Merchant) -> Flow PlanSubscribeRes
planSubscribe planId isDashboard (driverId, merchantId) = do
  driverInfo <- CDI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  unless (driverInfo.autoPayStatus `elem` [Nothing, Just DI.CANCELLED_PSP, Just DI.PAUSED_PSP, Just DI.PENDING]) $ throwError InvalidAutoPayStatus
  plan <- QPD.findByIdAndPaymentMode planId MANUAL >>= fromMaybeM (PlanNotFound planId.getId)
  driverPlan <- B.runInReplica $ QDPlan.findByDriverId driverId

  when (driverInfo.autoPayStatus == Just DI.PAUSED_PSP) $ do
    let mbMandateId = (.mandateId) =<< driverPlan
    whenJust mbMandateId $ \mandateId -> do
      fork "Cancelling paused Mandate" $ do
        void $ Payment.mandateRevoke merchantId (Payment.MandateRevokeReq {mandateId = mandateId.getId})

  unless (driverInfo.autoPayStatus == Just DI.PENDING) $ CDI.updateAutoPayStatus (Just DI.PENDING) (cast driverId)
  when (isNothing driverPlan) $ do
    newDriverPlan <- mkDriverPlan plan
    QDPlan.create newDriverPlan
  when (isJust driverPlan) $ do
    unless (driverInfo.autoPayStatus == Just DI.PENDING && maybe False (\dp -> dp.planId == planId) driverPlan) $ QDF.updateRegisterationFeeStatusByDriverId DF.INACTIVE driverId
    QDPlan.updatePlanIdByDriverId driverId planId
  (createOrderResp, orderId) <- createMandateInvoiceAndOrder driverId merchantId plan
  when isDashboard $ do
    let mbPaymentLink = createOrderResp.payment_links
    whenJust mbPaymentLink $ \paymentLinks -> do
      let webPaymentLink = show paymentLinks.web
      smsCfg <- asks (.smsCfg)
      driver <- QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      mobileNumber <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
      countryCode <- driver.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
      let phoneNumber = countryCode <> mobileNumber
      message <-
        MessageBuilder.buildSendPaymentLink merchantId $
          MessageBuilder.BuildSendPaymentLinkReq
            { paymentLink = webPaymentLink,
              amount = show createOrderResp.sdk_payload.payload.amount
            }
      Sms.sendSMS merchantId (Sms.SendSMSReq message phoneNumber smsCfg.sender)
        >>= Sms.checkSmsResult
  return $
    PlanSubscribeRes
      { orderId = orderId,
        orderResp = createOrderResp
      }
  where
    mkDriverPlan plan = do
      now <- getCurrentTime
      return $
        DriverPlan
          { driverId = cast driverId,
            planId = plan.id,
            planType = plan.paymentMode,
            mandateId = Nothing,
            createdAt = now,
            updatedAt = now,
            ..
          }

-- This API is to switch between plans of current Payment Method Preference.
planSelect :: Id Plan -> (Id SP.Person, Id DM.Merchant) -> Flow APISuccess
planSelect planId (driverId, _) = do
  void $ B.runInReplica $ QDPlan.findByDriverId driverId >>= fromMaybeM (NoCurrentPlanForDriver driverId.getId)
  driverInfo <- CDI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  void $ QPD.findByIdAndPaymentMode planId (getDriverPaymentMode driverInfo.autoPayStatus) >>= fromMaybeM (PlanNotFound planId.getId)
  QDPlan.updatePlanIdByDriverId driverId planId
  return Success
  where
    getDriverPaymentMode = \case
      Just DI.ACTIVE -> AUTOPAY
      Just DI.SUSPENDED -> MANUAL
      Just DI.PAUSED_PSP -> MANUAL
      Just DI.CANCELLED_PSP -> MANUAL
      _ -> MANUAL

-- This API is to make Mandate Inactive and switch to Manual plan type from Autopay.
planSuspend :: Bool -> (Id SP.Person, Id DM.Merchant) -> Flow APISuccess
planSuspend isDashboard (driverId, _merchantId) = do
  driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  driverInfo <- CDI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  unless (driverInfo.autoPayStatus == Just DI.ACTIVE) $ throwError InvalidAutoPayStatus
  driverPlan <- B.runInReplica $ QDPlan.findByDriverId driverId >>= fromMaybeM (NoCurrentPlanForDriver driverId.getId)
  mandate <- validateActiveMandateExists driverId driverPlan
  Redis.whenWithLockRedis (DF.paymentProcessingLockKey driverPlan.driverId.getId) 60 $ do
    QM.updateStatus mandate.id DM.INACTIVE
    QDPlan.updatePaymentModeByDriverId (cast driverPlan.driverId) MANUAL
    CDI.updateAutoPayStatus (Just DI.SUSPENDED) (cast driverId)
  when isDashboard $ notifyPaymentModeManualOnSuspend _merchantId driverId driver.deviceToken
  return Success

-- This API is to make Mandate Active and switch to Autopay plan type. If an only if an Auto Pay plan was paused/cancelled by driver from App.
planResume :: (Id SP.Person, Id DM.Merchant) -> Flow APISuccess
planResume (driverId, _merchantId) = do
  driverInfo <- CDI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  unless (driverInfo.autoPayStatus == Just DI.SUSPENDED) $ throwError InvalidAutoPayStatus
  driverPlan <- B.runInReplica $ QDPlan.findByDriverId driverId >>= fromMaybeM (NoCurrentPlanForDriver driverId.getId)
  mandate <- validateInActiveMandateExists driverId driverPlan
  Redis.whenWithLockRedis (DF.paymentProcessingLockKey driverPlan.driverId.getId) 60 $ do
    QM.updateStatus mandate.id DM.ACTIVE
    QDPlan.updatePaymentModeByDriverId (cast driverPlan.driverId) AUTOPAY
    CDI.updateAutoPayStatus (Just DI.ACTIVE) (cast driverId)
  return Success

---------------------------------------------------------------------------------------------------------
------------------------------------------ Helper Functions ---------------------------------------------
---------------------------------------------------------------------------------------------------------

validateActiveMandateExists :: Id SP.Person -> DriverPlan -> Flow DM.Mandate
validateActiveMandateExists driverId driverPlan = do
  case driverPlan.mandateId of
    Nothing -> throwError $ ActiveMandateDoNotExist driverId.getId
    Just mandateId -> do
      mandate <- B.runInReplica $ QM.findById mandateId >>= fromMaybeM (MandateNotFound mandateId.getId)
      unless (mandate.status == DM.ACTIVE) $ throwError (ActiveMandateDoNotExist driverId.getId)
      return mandate

validateInActiveMandateExists :: Id SP.Person -> DriverPlan -> Flow DM.Mandate
validateInActiveMandateExists driverId driverPlan = do
  case driverPlan.mandateId of
    Nothing -> throwError $ InActiveMandateDoNotExist driverId.getId
    Just mandateId -> do
      mandate <- QM.findById mandateId >>= fromMaybeM (MandateNotFound mandateId.getId)
      unless (mandate.status == DM.INACTIVE) $ throwError (InActiveMandateDoNotExist driverId.getId)
      return mandate

createMandateInvoiceAndOrder :: Id SP.Person -> Id DM.Merchant -> Plan -> Flow (Payment.CreateOrderResp, Id DOrder.PaymentOrder)
createMandateInvoiceAndOrder driverId merchantId plan = do
  driverFees <- QDF.findAllPendingAndDueDriverFeeByDriverId driverId
  driverRegisterationFee <- QDF.findLatestRegisterationFeeByDriverId (cast driverId)
  allPlans <- QPD.fetchAllPlan
  let allPlansMaxAmount = List.maximum $ allPlans <&> (.maxAmount)
  let currentDues = sum $ map (\dueInvoice -> fromIntegral dueInvoice.govtCharges + fromIntegral dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst) driverFees
  case driverRegisterationFee of
    Just registerFee -> do
      invoice <- QINV.findByDriverFeeIdAndActiveStatus registerFee.id
      case invoice of
        Just inv -> SPayment.createOrder (driverId, merchantId) (registerFee : driverFees) (Just $ mandateOrder currentDues allPlansMaxAmount) (Just (inv.id, inv.invoiceShortId))
        Nothing -> throwError $ InternalError "driverFee without invoice"
    Nothing -> do
      driverFee <- mkDriverFee
      QDF.create driverFee
      if not (null driverFees)
        then SPayment.createOrder (driverId, merchantId) (driverFee : driverFees) (Just $ mandateOrder currentDues allPlansMaxAmount) Nothing
        else do
          SPayment.createOrder (driverId, merchantId) [driverFee] (Just $ mandateOrder currentDues allPlansMaxAmount) Nothing
  where
    mandateOrder currentDues allPlansMaxAmount =
      SPayment.MandateOrder
        { maxAmount = max allPlansMaxAmount currentDues,
          _type = Payment.REQUIRED,
          frequency = Payment.ASPRESENTED
        }
    mkDriverFee = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        DF.DriverFee
          { id = id,
            merchantId = merchantId,
            payBy = now,
            status = DF.PAYMENT_PENDING,
            numRides = 0,
            createdAt = now,
            updatedAt = now,
            platformFee = DF.PlatformFee (round plan.registrationAmount) 0.0 0.0,
            totalEarnings = 0,
            feeType = DF.MANDATE_REGISTRATION,
            govtCharges = 0,
            startTime = now,
            endTime = now,
            collectedBy = Nothing,
            driverId = cast driverId
          }

convertPlanToPlanEntity :: Id SP.Person -> Plan -> Flow PlanEntity
convertPlanToPlanEntity driverId plan@Plan {..} = do
  dueInvoices <- B.runInReplica $ QDF.findAllPendingAndDueDriverFeeByDriverId driverId
  offers <- Payment.offerList merchantId =<< makeOfferReq
  let planFareBreakup = mkPlanFareBreakup offers.offerResp
  planBaseFrequcency <- case planBaseAmount of
    PERRIDE_BASE _ -> return "PER_RIDE"
    DAILY_BASE _ -> return "DAILY"
    WEEKLY_BASE _ -> return "WEEKLY"
    MONTHLY_BASE _ -> return "MONTHLY"
  return
    PlanEntity
      { id = plan.id.getId,
        offers = makeOfferEntity <$> offers.offerResp,
        frequency = planBaseFrequcency,
        currentDues = round . sum $ map (\dueInvoice -> fromIntegral dueInvoice.govtCharges + fromIntegral dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst) dueInvoices,
        totalPlanCreditLimit = round maxCreditLimit,
        ..
      }
  where
    makeOfferEntity offer =
      OfferEntity
        { title = offer.offerDescription.title,
          description = offer.offerDescription.description,
          tnc = offer.offerDescription.tnc
        }
    makeOfferReq = do
      driver <- QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      let offerOrder = Payment.OfferOrder {orderId = Nothing, amount = plan.maxAmount, currency = Payment.INR}
          customerReq = Payment.OfferCustomer {customerId = driverId.getId, email = driver.email, mobile = Nothing}
      transporterConfig <- QTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
      now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
      return
        Payment.OfferListReq
          { order = offerOrder,
            customer = Just customerReq,
            planId = plan.id.getId,
            registrationDate = now
          }
    mkPlanFareBreakup offers = do
      let baseAmount = case plan.planBaseAmount of
            PERRIDE_BASE amount -> amount
            DAILY_BASE amount -> amount
            WEEKLY_BASE amount -> amount
            MONTHLY_BASE amount -> amount
          (discountAmount, finalOrderAmount) =
            if null offers
              then (0.0, baseAmount)
              else do
                let bestOffer = minimumBy (comparing (.finalOrderAmount)) offers
                (bestOffer.discountAmount, bestOffer.finalOrderAmount)
      [ PlanFareBreakup {component = "INITIAL_BASE_FEE", amount = baseAmount},
        PlanFareBreakup {component = "REGISTRATION_FEE", amount = plan.registrationAmount},
        PlanFareBreakup {component = "MAX_FEE_LIMIT", amount = plan.maxAmount},
        PlanFareBreakup {component = "DISCOUNTED_FEE", amount = discountAmount},
        PlanFareBreakup {component = "FINAL_FEE", amount = finalOrderAmount}
        ]

mkMandateDetailEntity :: Maybe (Id DM.Mandate) -> Flow (Maybe MandateDetailsEntity)
mkMandateDetailEntity mandateId = do
  case mandateId of
    Just id -> do
      mandate <- B.runInReplica $ QM.findById id >>= fromMaybeM (MandateNotFound id.getId)
      return $
        Just
          MandateDetails
            { status = mandate.status,
              startDate = mandate.startDate,
              endDate = mandate.endDate,
              mandateId = mandate.id.getId,
              payerVpa = mandate.payerVpa,
              frequency = "Aspresented",
              maxAmount = round mandate.maxAmount
            }
    Nothing -> return Nothing
