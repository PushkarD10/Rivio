{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.IssueList where

import qualified Domain.Action.Dashboard.IssueList as DDI
import qualified Domain.Types.Issue as DI
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type API =
  "issue"
    :> ListCustomerIssue

type ListCustomerIssue =
  "list"
    :> Capture "mobileCountryCode" Text
    :> Capture "mobileNumber" Text
    :> Get '[JSON] [DI.Issue]

handler :: ShortId DM.Merchant -> FlowServer API
handler = listIssue

listIssue :: ShortId DM.Merchant -> Text -> Text -> FlowHandler [DI.Issue]
listIssue merchantShortId mobileCountryCode mobileNumber = withFlowHandlerAPI $ DDI.getIssueList merchantShortId mobileCountryCode mobileNumber
