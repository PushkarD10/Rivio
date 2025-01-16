{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Tools.Call
  ( module Reexport,
    initiateCall,
  )
where

import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Domain.Types.MerchantServiceUsageConfig (MerchantServiceUsageConfig)
import Kernel.External.Call as Reexport hiding
  ( initiateCall,
  )
import qualified Kernel.External.Call as Call
import Kernel.External.Types
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Cac.MerchantServiceUsageConfig as QMSUC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import Tools.Error

initiateCall ::
  ( ServiceFlow m r,
    ToJSON a
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  InitiateCallReq a ->
  m InitiateCallResp
initiateCall = runWithServiceConfig Call.initiateCall (.initiateCall)

runWithServiceConfig ::
  ServiceFlow m r =>
  (CallServiceConfig -> req -> m resp) ->
  (MerchantServiceUsageConfig -> CallService) ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  req ->
  m resp
runWithServiceConfig func getCfg _merchantId merchantOpCityId req = do
  merchantConfig <- QMSUC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  merchantCallServiceConfig <-
    QMSC.findByServiceAndCity (DMSC.CallService $ getCfg merchantConfig) merchantOpCityId
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "call" (show $ getCfg merchantConfig))
  case merchantCallServiceConfig.serviceConfig of
    DMSC.CallServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown ServiceConfig"
