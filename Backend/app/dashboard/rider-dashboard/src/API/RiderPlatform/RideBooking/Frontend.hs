{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.RideBooking.Frontend where

import qualified "rider-app" API.Dashboard.RideBooking.Frontend as BAP
import qualified Dashboard.RiderPlatform.Customer as Common
import qualified "rider-app" Domain.Action.UI.Frontend as DFrontend
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "rider-app" Domain.Types.Person as DP
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp.RideBooking as Client
import Servant
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

type API =
  "frontend"
    :> ( ApiAuth 'APP_BACKEND 'CUSTOMERS 'FLOW_STATUS
           :> BAP.PersonFlowStatusAPI
           :<|> ApiAuth 'APP_BACKEND 'CUSTOMERS 'NOTIFYEVENT
             :> BAP.NotifyEventAPI
       )

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  callGetPersonFlowStatus merchantId city
    :<|> callNotifyEvent merchantId city

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  BAP.RideNotifyEventEndPoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.FlowStatusAPI endpoint) (Just APP_BACKEND) (Just apiTokenInfo) Nothing Nothing

callGetPersonFlowStatus :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id DP.Person -> Maybe Bool -> FlowHandler DFrontend.GetPersonFlowStatusRes
callGetPersonFlowStatus merchantShortId opCity apiTokenInfo personId isPolling = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderApp checkedMerchantId opCity (.rideBooking.flowStatus.personFlowStatus) personId isPolling

callNotifyEvent :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id DP.Person -> DFrontend.NotifyEventReq -> FlowHandler DFrontend.NotifyEventResp
callNotifyEvent merchantShortId opCity apiTokenInfo personId req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction BAP.NotifyEventEndPoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId opCity (.rideBooking.flowStatus.notifyEvent) personId req
