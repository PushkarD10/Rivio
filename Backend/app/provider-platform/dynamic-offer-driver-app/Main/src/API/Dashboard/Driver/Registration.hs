{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Driver.Registration where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Registration as Common
import qualified Domain.Action.Dashboard.Driver.Registration as DReg
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type API =
  Common.DocumentsListAPI
    :<|> Common.GetDocumentAPI
    :<|> Common.UploadDocumentAPI
    :<|> Common.RegisterDLAPI
    :<|> Common.RegisterRCAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  documentsList merchantId
    :<|> getDocument merchantId
    :<|> uploadDocument merchantId
    :<|> registerDL merchantId
    :<|> registerRC merchantId

documentsList :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler Common.DocumentsListResponse
documentsList merchantShortId = withFlowHandlerAPI . DReg.documentsList merchantShortId

getDocument :: ShortId DM.Merchant -> Id Common.Image -> FlowHandler Common.GetDocumentResponse
getDocument merchantShortId = withFlowHandlerAPI . DReg.getDocument merchantShortId

uploadDocument :: ShortId DM.Merchant -> Id Common.Driver -> Common.UploadDocumentReq -> FlowHandler Common.UploadDocumentResp
uploadDocument merchantShortId driverId_ = withFlowHandlerAPI . DReg.uploadDocument merchantShortId driverId_

registerDL :: ShortId DM.Merchant -> Id Common.Driver -> Common.RegisterDLReq -> FlowHandler APISuccess
registerDL merchantShortId driverId_ = withFlowHandlerAPI . DReg.registerDL merchantShortId driverId_

registerRC :: ShortId DM.Merchant -> Id Common.Driver -> Common.RegisterRCReq -> FlowHandler APISuccess
registerRC merchantShortId driverId_ = withFlowHandlerAPI . DReg.registerRC merchantShortId driverId_
