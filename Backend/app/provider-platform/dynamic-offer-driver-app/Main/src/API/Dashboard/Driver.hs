{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Driver where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver as Common
import qualified Domain.Action.Dashboard.Driver as DDriver
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (throwError)

type API =
  "driver"
    :> ( Common.DriverDocumentsInfoAPI
           :<|> Common.DriverAadhaarInfoAPI
           :<|> Common.DriverAadhaarInfoByPhoneAPI
           :<|> Common.DriverListAPI
           :<|> Common.DriverOutstandingBalanceAPI
           :<|> Common.DriverActivityAPI
           :<|> Common.EnableDriverAPI
           :<|> Common.DisableDriverAPI
           :<|> Common.BlockDriverWithReasonAPI
           :<|> Common.BlockDriverAPI
           :<|> Common.DriverBlockReasonListAPI
           :<|> Common.DriverCashCollectionAPI
           :<|> Common.DriverCashExemptionAPI
           :<|> Common.UnblockDriverAPI
           :<|> Common.DriverLocationAPI
           :<|> Common.DriverInfoAPI
           :<|> Common.DeleteDriverAPI
           :<|> Common.UnlinkVehicleAPI
           :<|> Common.UnlinkDLAPI
           :<|> Common.UnlinkAadhaarAPI
           :<|> Common.EndRCAssociationAPI
           :<|> Common.UpdatePhoneNumberAPI
           :<|> Common.UpdateDriverAadhaarAPI
           :<|> Common.AddVehicleAPI
           :<|> Common.UpdateDriverNameAPI
           :<|> Common.SetRCStatusAPI
           :<|> Common.DeleteRCAPI
           :<|> Common.ClearOnRideStuckDrivers
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  driverDocumentsInfo merchantId
    :<|> driverAadhaarInfo merchantId
    :<|> driverAadhaarInfoByPhone merchantId
    :<|> listDrivers merchantId
    :<|> getDriverDue merchantId
    :<|> driverActivity merchantId
    :<|> enableDriver merchantId
    :<|> disableDriver merchantId
    :<|> blockDriverWithReason merchantId
    :<|> blockDriver merchantId
    :<|> blockReasonList merchantId
    :<|> collectCash merchantId
    :<|> exemptCash merchantId
    :<|> unblockDriver merchantId
    :<|> driverLocation merchantId
    :<|> driverInfo merchantId
    :<|> deleteDriver merchantId
    :<|> unlinkVehicle merchantId
    :<|> unlinkDL merchantId
    :<|> unlinkAadhaar merchantId
    :<|> endRCAssociation merchantId
    :<|> updatePhoneNumber merchantId
    :<|> updateByPhoneNumber merchantId
    :<|> addVehicle merchantId
    :<|> updateDriverName merchantId
    :<|> setRCStatus merchantId
    :<|> deleteRC merchantId
    :<|> clearOnRideStuckDrivers merchantId

driverDocumentsInfo :: ShortId DM.Merchant -> FlowHandler Common.DriverDocumentsInfoRes
driverDocumentsInfo = withFlowHandlerAPI . DDriver.driverDocumentsInfo

driverAadhaarInfo :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler Common.DriverAadhaarInfoRes
driverAadhaarInfo merchantShortId = withFlowHandlerAPI . DDriver.driverAadhaarInfo merchantShortId

driverAadhaarInfoByPhone :: ShortId DM.Merchant -> Text -> FlowHandler Common.DriverAadhaarInfoByPhoneReq
driverAadhaarInfoByPhone merchantShortId = withFlowHandlerAPI . DDriver.driverAadhaarInfoByPhone merchantShortId

listDrivers :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Text -> FlowHandler Common.DriverListRes
listDrivers merchantShortId mbLimit mbOffset verified enabled blocked mbSubscribed vechicleNumberSearchString =
  withFlowHandlerAPI . DDriver.listDrivers merchantShortId mbLimit mbOffset verified enabled blocked mbSubscribed vechicleNumberSearchString

getDriverDue :: ShortId DM.Merchant -> Maybe Text -> Text -> FlowHandler [Common.DriverOutstandingBalanceResp]
getDriverDue merchantShortId mobileCountryCode phone =
  withFlowHandlerAPI $ DDriver.getDriverDue merchantShortId mobileCountryCode phone

driverActivity :: ShortId DM.Merchant -> FlowHandler Common.DriverActivityRes
driverActivity = withFlowHandlerAPI . DDriver.driverActivity

enableDriver :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
enableDriver merchantShortId = withFlowHandlerAPI . DDriver.enableDriver merchantShortId

disableDriver :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
disableDriver merchantShortId = withFlowHandlerAPI . DDriver.disableDriver merchantShortId

blockDriverWithReason :: ShortId DM.Merchant -> Id Common.Driver -> Common.BlockDriverWithReasonReq -> FlowHandler APISuccess
blockDriverWithReason merchantShortId driverId = withFlowHandlerAPI . DDriver.blockDriverWithReason merchantShortId driverId

blockDriver :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
blockDriver merchantShortId = withFlowHandlerAPI . DDriver.blockDriver merchantShortId

blockReasonList :: ShortId DM.Merchant -> FlowHandler [Common.BlockReason]
blockReasonList _ = withFlowHandlerAPI DDriver.blockReasonList

collectCash :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
collectCash merchantShortId = withFlowHandlerAPI . DDriver.collectCash merchantShortId

exemptCash :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
exemptCash merchantShortId = withFlowHandlerAPI . DDriver.exemptCash merchantShortId

unblockDriver :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
unblockDriver merchantShortId = withFlowHandlerAPI . DDriver.unblockDriver merchantShortId

driverLocation :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation merchantShortId mbLimit mbOffset = withFlowHandlerAPI . DDriver.driverLocation merchantShortId mbLimit mbOffset

driverInfo :: ShortId DM.Merchant -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> FlowHandler Common.DriverInfoRes
driverInfo merchantShortId mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber = withFlowHandlerAPI . DDriver.driverInfo merchantShortId mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber

deleteDriver :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
deleteDriver merchantShortId = withFlowHandlerAPI . DDriver.deleteDriver merchantShortId

unlinkVehicle :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
unlinkVehicle merchantShortId = withFlowHandlerAPI . DDriver.unlinkVehicle merchantShortId

unlinkDL :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
unlinkDL merchantShortId = withFlowHandlerAPI . DDriver.unlinkDL merchantShortId

unlinkAadhaar :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
unlinkAadhaar merchantShortId = withFlowHandlerAPI . DDriver.unlinkAadhaar merchantShortId

endRCAssociation :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
endRCAssociation merchantShortId = withFlowHandlerAPI . DDriver.endRCAssociation merchantShortId

updatePhoneNumber :: ShortId DM.Merchant -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> FlowHandler APISuccess
updatePhoneNumber merchantShortId driverId = withFlowHandlerAPI . DDriver.updatePhoneNumber merchantShortId driverId

updateByPhoneNumber :: ShortId DM.Merchant -> Text -> Common.UpdateDriverDataReq -> FlowHandler APISuccess
updateByPhoneNumber merchantShortId mobileNo = withFlowHandlerAPI . DDriver.updateByPhoneNumber merchantShortId mobileNo

addVehicle :: ShortId DM.Merchant -> Id Common.Driver -> Common.AddVehicleReq -> FlowHandler APISuccess
addVehicle merchantShortId driverId = withFlowHandlerAPI . DDriver.addVehicle merchantShortId driverId

updateDriverName :: ShortId DM.Merchant -> Id Common.Driver -> Common.UpdateDriverNameReq -> FlowHandler APISuccess
updateDriverName merchantShortId driverId = withFlowHandlerAPI . DDriver.updateDriverName merchantShortId driverId

clearOnRideStuckDrivers :: ShortId DM.Merchant -> FlowHandler Common.ClearOnRideStuckDriversRes
clearOnRideStuckDrivers = withFlowHandlerAPI . DDriver.clearOnRideStuckDrivers

setRCStatus :: ShortId DM.Merchant -> Id Common.Driver -> Common.RCStatusReq -> FlowHandler APISuccess
setRCStatus merchantShortId driverId = withFlowHandlerAPI . DDriver.setRCStatus merchantShortId driverId

deleteRC :: ShortId DM.Merchant -> Id Common.Driver -> Common.DeleteRCReq -> FlowHandler APISuccess
deleteRC merchantShortId driverId = withFlowHandlerAPI . DDriver.deleteRC merchantShortId driverId
