{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Plan where

import qualified Domain.Action.UI.Driver as Driver
import qualified Domain.Action.UI.Plan as DPlan
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverPlan as DPlan
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Plan as DPlan
import qualified Domain.Types.Vehicle as Vehicle
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.DriverInformation as DI
import Tools.Auth

type API =
  "plan"
    :> ( "list"
           :> TokenAuth
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> QueryParam "vehicleVariant" Vehicle.Variant
           :> Get '[JSON] DPlan.PlanListAPIRes
           :<|> "suspend"
             :> TokenAuth
             :> Put '[JSON] APISuccess
           :<|> "resume"
             :> TokenAuth
             :> Put '[JSON] APISuccess
           :<|> "currentPlan"
             :> TokenAuth
             :> Get '[JSON] DPlan.CurrentPlanRes
           :<|> Capture "planId" (Id DPlan.Plan)
             :> "subscribe"
             :> TokenAuth
             :> Post '[JSON] DPlan.PlanSubscribeRes
           :<|> Capture "planId" (Id DPlan.Plan)
             :> "select"
             :> TokenAuth
             :> Put '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  planList
    :<|> planSuspend
    :<|> planResume
    :<|> currentPlan
    :<|> planSubscribe
    :<|> planSelect

planList :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Int -> Maybe Int -> Maybe Vehicle.Variant -> FlowHandler DPlan.PlanListAPIRes
planList (driverId, merchantId, merchantOpCityId) mbLimit mbOffset = withFlowHandlerAPI . DPlan.planList (driverId, merchantId, merchantOpCityId) DPlan.YATRI_SUBSCRIPTION mbLimit mbOffset

planSuspend :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler APISuccess
planSuspend = withFlowHandlerAPI . DPlan.planSuspend DPlan.YATRI_SUBSCRIPTION False

planResume :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler APISuccess
planResume = withFlowHandlerAPI . DPlan.planResume DPlan.YATRI_SUBSCRIPTION

currentPlan :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler DPlan.CurrentPlanRes
currentPlan = withFlowHandlerAPI . DPlan.currentPlan DPlan.YATRI_SUBSCRIPTION

planSubscribe :: Id DPlan.Plan -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler DPlan.PlanSubscribeRes
planSubscribe planId (personId, merchantId, merchantOpCityId) = withFlowHandlerAPI $ do
  driverInfo <- DI.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  autoPayStatus <- fst <$> DPlan.getSubcriptionStatusWithPlan DPlan.YATRI_SUBSCRIPTION personId
  if autoPayStatus == Just DI.SUSPENDED
    then do
      void $ DPlan.planResume DPlan.YATRI_SUBSCRIPTION (personId, merchantId, merchantOpCityId)
      Driver.ClearDuesRes {..} <- Driver.clearDriverDues (personId, merchantId, merchantOpCityId) DPlan.YATRI_SUBSCRIPTION Nothing
      return $ DPlan.PlanSubscribeRes {..}
    else do DPlan.planSubscribe DPlan.YATRI_SUBSCRIPTION planId (False, Nothing) (personId, merchantId, merchantOpCityId) driverInfo DPlan.NoData

planSelect :: Id DPlan.Plan -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler APISuccess
planSelect planId = withFlowHandlerAPI . DPlan.planSwitch DPlan.YATRI_SUBSCRIPTION planId
