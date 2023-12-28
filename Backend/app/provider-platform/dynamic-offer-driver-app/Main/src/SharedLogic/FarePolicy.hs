{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FarePolicy where

import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.FareProduct as FareProductD
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Vehicle.Variant (Variant (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FareProduct as FareProduct
import qualified Storage.CachedQueries.FarePolicy as QFP
import qualified Storage.CachedQueries.FareProduct as QFareProduct
import Tools.Error
import Tools.Maps

data FarePoliciesProduct = FarePoliciesProduct
  { farePolicies :: [FarePolicyD.FullFarePolicy],
    flow :: FareProductD.FlowType,
    area :: FareProductD.Area,
    specialLocationTag :: Maybe Text
  }

getFarePolicy :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DMOC.MerchantOperatingCity -> Variant -> Maybe FareProductD.Area -> m FarePolicyD.FullFarePolicy
getFarePolicy merchantOpCityId vehVariant Nothing = do
  fareProduct <- QFareProduct.findByMerchantVariantArea merchantOpCityId vehVariant FareProductD.Default >>= fromMaybeM NoFareProduct
  farePolicy <- QFP.findById fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
  return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleVariant farePolicy
getFarePolicy merchantOpCityId vehVariant (Just area) = do
  mbFareProduct <- QFareProduct.findByMerchantVariantArea merchantOpCityId vehVariant area
  case mbFareProduct of
    Just fareProduct -> do
      farePolicy <- QFP.findById fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
      return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleVariant farePolicy
    Nothing -> do
      fareProduct <- QFareProduct.findByMerchantVariantArea merchantOpCityId vehVariant FareProductD.Default >>= fromMaybeM NoFareProduct
      farePolicy <- QFP.findById fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
      return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleVariant farePolicy

getAllFarePoliciesProduct :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Merchant -> Id DMOC.MerchantOperatingCity -> LatLong -> LatLong -> m FarePoliciesProduct
getAllFarePoliciesProduct merchantId merchantOpCityId fromlocaton toLocation = do
  allFareProducts <- FareProduct.getAllFareProducts merchantId merchantOpCityId fromlocaton toLocation
  farePolicies <-
    mapM
      ( \fareProduct -> do
          farePolicy <- QFP.findById fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
          return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleVariant farePolicy
      )
      allFareProducts.fareProducts
  return $
    FarePoliciesProduct
      { farePolicies,
        flow = maybe FareProductD.NORMAL (.flow) (listToMaybe allFareProducts.fareProducts),
        area = allFareProducts.area,
        specialLocationTag = allFareProducts.specialLocationTag
      }
