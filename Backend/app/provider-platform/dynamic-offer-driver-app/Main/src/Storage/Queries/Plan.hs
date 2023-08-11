{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Plan where

import Domain.Types.Merchant
import Domain.Types.Plan
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id as KTI
import Kernel.Types.Logging
import qualified Sequelize as Se
import qualified Storage.Beam.Plan as BeamP

-- create :: Plan -> SqlDB ()
-- create = Esq.create

create :: (L.MonadFlow m, Log m) => Plan -> m ()
create = createWithKV

-- findByIdAndPaymentMode :: Transactionable m => Id Plan -> PaymentMode -> m (Maybe Plan)
-- findByIdAndPaymentMode planId paymentMode = Esq.findOne $ do
--   plan <- from $ table @PlanT
--   where_ $
--     plan ^. PlanTId ==. val (toKey planId)
--       &&. plan ^. PlanPaymentMode ==. val paymentMode
--   return plan

findByIdAndPaymentMode :: (L.MonadFlow m, Log m) => Id Plan -> PaymentMode -> m (Maybe Plan)
findByIdAndPaymentMode (Id planId) paymentMode = findOneWithKV [Se.And [Se.Is BeamP.id $ Se.Eq planId, Se.Is BeamP.paymentMode $ Se.Eq paymentMode]]

-- findByMerchantId :: Transactionable m => Id Merchant -> m (Maybe Plan)
-- findByMerchantId merchantId = do
--   findOne $ do
--     plan <- from $ table @PlanT
--     where_ $
--       plan ^. PlanMerchantId ==. val (toKey merchantId)
--     return plan

findByMerchantId :: (L.MonadFlow m, Log m) => Id Merchant -> m [Plan]
findByMerchantId (Id merchantId) = findAllWithKV [Se.Is BeamP.merchantId $ Se.Eq merchantId]

-- findByMerchantIdAndPaymentMode :: Transactionable m => Id Merchant -> PaymentMode -> m [Plan]
-- findByMerchantIdAndPaymentMode merchantId paymentMode = Esq.findAll $ do
--   plan <- from $ table @PlanT
--   where_ $
--     plan ^. PlanMerchantId ==. val (toKey merchantId)
--       &&. plan ^. PlanPaymentMode ==. val paymentMode
--   return plan

findByMerchantIdAndPaymentMode :: (L.MonadFlow m, Log m) => Id Merchant -> PaymentMode -> m [Plan]
findByMerchantIdAndPaymentMode (Id merchantId) paymentMode = findAllWithKV [Se.And [Se.Is BeamP.merchantId $ Se.Eq merchantId, Se.Is BeamP.paymentMode $ Se.Eq paymentMode]]

instance FromTType' BeamP.Plan Plan where
  fromTType' BeamP.PlanT {..} = do
    pure $
      Just
        Plan
          { id = Id id,
            paymentMode = paymentMode,
            merchantId = Id merchantId,
            name = name,
            description = description,
            maxAmount = maxAmount,
            registrationAmount = registrationAmount,
            isOfferApplicable = isOfferApplicable,
            maxCreditLimit = maxCreditLimit,
            planBaseAmount = planBaseAmount,
            -- rideCountBasedFeePolicy :: RideCountBasedFeePolicyConfig, -- todo
            -- distanceBasedFeePolicy :: , -- todo
            freeRideCount = freeRideCount,
            frequency = frequency,
            planType = planType
          }

instance ToTType' BeamP.Plan Plan where
  toTType' Plan {..} = do
    BeamP.PlanT
      { BeamP.id = getId id,
        BeamP.paymentMode = paymentMode,
        BeamP.merchantId = getId merchantId,
        BeamP.name = name,
        BeamP.description = description,
        BeamP.maxAmount = maxAmount,
        BeamP.registrationAmount = registrationAmount,
        BeamP.isOfferApplicable = isOfferApplicable,
        BeamP.maxCreditLimit = maxCreditLimit,
        BeamP.planBaseAmount = planBaseAmount,
        BeamP.freeRideCount = freeRideCount,
        BeamP.frequency = frequency,
        BeamP.planType = planType
      }
