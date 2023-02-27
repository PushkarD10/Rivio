{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Person where

import qualified Domain.Action.Dashboard.Person as DPerson
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (Unauthorized, throwError)
import Tools.Auth

type API =
  "admin"
    :> "person"
    :> ( "list"
           :> DashboardAuth 'DASHBOARD_ADMIN
           :> QueryParam "searchString" Text
           :> QueryParam "limit" Integer
           :> QueryParam "offset" Integer
           :> Get '[JSON] DPerson.ListPersonRes
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "personId" (Id DP.Person)
             :> "assignRole"
             :> Capture "roleId" (Id DRole.Role)
             :> Post '[JSON] APISuccess
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "personId" (Id DP.Person)
             :> "assignMerchantAccess"
             :> ReqBody '[JSON] DPerson.MerchantAccessReq
             :> Post '[JSON] APISuccess
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "personId" (Id DP.Person)
             :> "resetMerchantAccess"
             :> ReqBody '[JSON] DPerson.MerchantAccessReq
             :> Post '[JSON] APISuccess
           :<|> "create"
             :> DashboardAuth 'DASHBOARD_ADMIN
             :> ReqBody '[JSON] DPerson.CreatePersonReq
             :> Post '[JSON] DPerson.CreatePersonRes
       )
    :<|> "user"
      :> ( "profile"
             :> DashboardAuth 'DASHBOARD_USER
             :> Get '[JSON] DP.PersonAPIEntity
             :<|> "getCurrentMerchant"
               :> DashboardAuth 'DASHBOARD_USER
               :> Get '[JSON] DPerson.MerchantAccessRes
             :<|> DashboardAuth 'DASHBOARD_USER
               :> "changePassword"
               :> ReqBody '[JSON] DPerson.ChangePasswordReq
               :> Post '[JSON] APISuccess
         )

handler :: FlowServer API
handler =
  ( listPerson
      :<|> assignRole
      :<|> assignMerchantAccess
      :<|> resetMerchantAccess
      :<|> createPerson
  )
    :<|> ( profile
             :<|> getCurrentMerchant
             :<|> changePassword
         )

listPerson :: TokenInfo -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler DPerson.ListPersonRes
listPerson tokenInfo mbSearchString mbLimit =
  withFlowHandlerAPI . DPerson.listPerson tokenInfo mbSearchString mbLimit

createPerson :: TokenInfo -> DPerson.CreatePersonReq -> FlowHandler DPerson.CreatePersonRes
createPerson tokenInfo = withFlowHandlerAPI . DPerson.createPerson tokenInfo

assignRole :: TokenInfo -> Id DP.Person -> Id DRole.Role -> FlowHandler APISuccess
assignRole tokenInfo personId =
  withFlowHandlerAPI . DPerson.assignRole tokenInfo personId

assignMerchantAccess :: TokenInfo -> Id DP.Person -> DPerson.MerchantAccessReq -> FlowHandler APISuccess
assignMerchantAccess tokenInfo personId =
  withFlowHandlerAPI . DPerson.assignMerchantAccess tokenInfo personId

resetMerchantAccess :: TokenInfo -> Id DP.Person -> DPerson.MerchantAccessReq -> FlowHandler APISuccess
resetMerchantAccess tokenInfo personId =
  withFlowHandlerAPI . DPerson.resetMerchantAccess tokenInfo personId

profile :: TokenInfo -> FlowHandler DP.PersonAPIEntity
profile =
  withFlowHandlerAPI . DPerson.profile

getCurrentMerchant :: TokenInfo -> FlowHandler DPerson.MerchantAccessRes
getCurrentMerchant =
  withFlowHandlerAPI . DPerson.getCurrentMerchant

changePassword :: TokenInfo -> DPerson.ChangePasswordReq -> FlowHandler APISuccess
changePassword req =
  withFlowHandlerAPI . DPerson.changePassword req
