{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverPlan where

import qualified Domain.Types.DriverPlan
import qualified Domain.Types.Plan
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverPlan as Beam
import qualified Storage.Queries.Transformers.DriverPlan

instance FromTType' Beam.DriverPlan Domain.Types.DriverPlan.DriverPlan where
  fromTType' (Beam.DriverPlanT {..}) = do
    merchantId' <- Storage.Queries.Transformers.DriverPlan.getMerchantId merchantId driverId serviceName
    merchantOpCityId' <- Storage.Queries.Transformers.DriverPlan.getMerchantOpCityId merchantOpCityId driverId serviceName
    subscriptionServiceRelatedData' <- Storage.Queries.Transformers.DriverPlan.getSubscriptionServiceRelatedData rentedVehicleNumber
    pure $
      Just
        Domain.Types.DriverPlan.DriverPlan
          { autoPayStatus = autoPayStatus,
            coinCovertedToCashLeft = coinCovertedToCashLeft,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            enableServiceUsageCharge = Kernel.Prelude.fromMaybe False enableServiceUsageCharge,
            lastPaymentLinkSentAtIstDate = lastPaymentLinkSentAtIstDate,
            mandateId = Kernel.Types.Id.Id <$> mandateId,
            mandateSetupDate = mandateSetupDate,
            merchantId = merchantId',
            merchantOpCityId = merchantOpCityId',
            payerVpa = payerVpa,
            planId = Kernel.Types.Id.Id planId,
            planType = planType,
            serviceName = Kernel.Prelude.fromMaybe Domain.Types.Plan.YATRI_SUBSCRIPTION serviceName,
            subscriptionServiceRelatedData = subscriptionServiceRelatedData',
            totalCoinsConvertedCash = totalCoinsConvertedCash,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverPlan Domain.Types.DriverPlan.DriverPlan where
  toTType' (Domain.Types.DriverPlan.DriverPlan {..}) = do
    Beam.DriverPlanT
      { Beam.autoPayStatus = autoPayStatus,
        Beam.coinCovertedToCashLeft = coinCovertedToCashLeft,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.enableServiceUsageCharge = Kernel.Prelude.Just enableServiceUsageCharge,
        Beam.lastPaymentLinkSentAtIstDate = lastPaymentLinkSentAtIstDate,
        Beam.mandateId = Kernel.Types.Id.getId <$> mandateId,
        Beam.mandateSetupDate = mandateSetupDate,
        Beam.merchantId = Kernel.Prelude.Just (Kernel.Types.Id.getId merchantId),
        Beam.merchantOpCityId = Kernel.Prelude.Just (Kernel.Types.Id.getId merchantOpCityId),
        Beam.payerVpa = payerVpa,
        Beam.planId = Kernel.Types.Id.getId planId,
        Beam.planType = planType,
        Beam.serviceName = Kernel.Prelude.Just serviceName,
        Beam.rentedVehicleNumber = Storage.Queries.Transformers.DriverPlan.getCommodityData subscriptionServiceRelatedData,
        Beam.totalCoinsConvertedCash = totalCoinsConvertedCash,
        Beam.updatedAt = updatedAt
      }

{-
	DSL Source Link: file://./../../../../spec/Storage/DriverPlan.yaml
-}
