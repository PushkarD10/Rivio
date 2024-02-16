{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.API.OnSelect where

import Beckn.Types.Core.Taxi.OnSelect (OnSelectMessage)
import qualified BecknV2.OnDemand.Types as Spec
import EulerHS.Prelude (ByteString)
import Kernel.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknCallbackReq)
import Kernel.Utils.Servant.JSONBS
import Servant (JSON, Post, ReqBody, (:>))

type OnSelectReq = BecknCallbackReq OnSelectMessage

type OnSelectReqV2 = Spec.OnSelectReq

type OnSelectRes = AckResponse

type OnSelectAPI =
  "on_select"
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] OnSelectRes

onSelectAPI :: Proxy OnSelectAPI
onSelectAPI = Proxy

type OnSelectAPIV1 =
  "on_select"
    :> ReqBody '[JSON] OnSelectReq
    :> Post '[JSON] OnSelectRes

onSelectAPIV1 :: Proxy OnSelectAPIV1
onSelectAPIV1 = Proxy

type OnSelectAPIV2 =
  "on_select"
    :> ReqBody '[JSON] OnSelectReqV2
    :> Post '[JSON] OnSelectRes

onSelectAPIV2 :: Proxy OnSelectAPIV2
onSelectAPIV2 = Proxy
