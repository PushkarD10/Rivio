{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.AadhaarVerification
  ( module Reexport,
    generateAadhaarOtp,
    verifyAadhaarOtp,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DM
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Kernel.External.AadhaarVerification as Reexport hiding
  ( generateAadhaarOtp,
    verifyAadhaarOtp,
  )
import qualified Kernel.External.AadhaarVerification as Verification
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Cac.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Tools.Error

generateAadhaarOtp ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DM.MerchantOperatingCity ->
  AadhaarOtpReq ->
  m AadhaarVerificationResp
generateAadhaarOtp = runWithServiceConfig Verification.generateAadhaarOtp

verifyAadhaarOtp ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DM.MerchantOperatingCity ->
  AadhaarOtpVerifyReq ->
  m AadhaarOtpVerifyRes
verifyAadhaarOtp = runWithServiceConfig Verification.verifyAadhaarOtp

runWithServiceConfig ::
  ServiceFlow m r =>
  (AadhaarVerificationServiceConfig -> req -> m resp) ->
  Id DM.Merchant ->
  Id DM.MerchantOperatingCity ->
  req ->
  m resp
runWithServiceConfig func _merchantId merchantOpCityId req = do
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity (DMSC.AadhaarVerificationService merchantServiceUsageConfig.aadhaarVerificationService) merchantOpCityId
      >>= fromMaybeM (InternalError $ "No Aadhaar Verification service provider configured for the merchant, merchantOpCityId:" <> merchantOpCityId.getId)
  case merchantServiceConfig.serviceConfig of
    DMSC.AadhaarVerificationServiceConfig vsc -> func vsc req
    _ -> throwError $ InternalError "Unknown Service Config"
