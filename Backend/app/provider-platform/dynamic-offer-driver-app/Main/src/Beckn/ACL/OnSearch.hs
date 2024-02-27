{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnSearch where

import qualified Beckn.OnDemand.Transformer.OnSearch as TOnSearch
import qualified BecknV2.OnDemand.Types as Spec
import BecknV2.Utils
import qualified Domain.Action.Beckn.Search as DSearch
import Domain.Types.BecknConfig
import Kernel.Prelude
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Storage.CachedQueries.BecknConfig as QBC

mkOnSearchRequest ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  DSearch.DSearchRes ->
  Context.Action ->
  Context.Domain ->
  Text ->
  Maybe Text ->
  Text ->
  BaseUrl ->
  Maybe Text ->
  Maybe BaseUrl ->
  Context.City ->
  Context.Country ->
  m Spec.OnSearchReq
mkOnSearchRequest res@DSearch.DSearchRes {..} action domain messageId transactionId bapId bapUri bppId bppUri city country = do
  bppConfig <- QBC.findByMerchantIdDomainAndVehicle provider.id "MOBILITY" AUTO_RICKSHAW >>= fromMaybeM (InternalError $ "Beckn Config not found for merchantId:-" <> show provider.id.getId <> ",domain:-MOBILITY,vehicleVariant:-" <> show AUTO_RICKSHAW)
  ttlInInt <- bppConfig.onSearchTTLSec & fromMaybeM (InternalError "Invalid ttl")
  let ttlToNominalDiffTime = intToNominalDiffTime ttlInInt
      ttlToISO8601Duration = formatTimeDifference ttlToNominalDiffTime
  TOnSearch.buildOnSearchRideReq ttlToISO8601Duration bppConfig res action domain messageId transactionId bapId bapUri bppId bppUri city country
