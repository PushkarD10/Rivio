{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Driver
  ( API.Types.ProviderPlatform.Management.Driver.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Driver
import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Domain.Action.Dashboard.Management.Driver as Domain.Action.Dashboard.Management.Driver
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.Driver.API)
handler merchantId city = getDriverDocumentsInfo merchantId city :<|> postDriverPersonNumbers merchantId city :<|> getDriverAadhaarInfo merchantId city :<|> getDriverAadhaarInfobyMobileNumber merchantId city :<|> getDriverList merchantId city :<|> getDriverActivity merchantId city :<|> postDriverDisable merchantId city :<|> postDriverAcRestrictionUpdate merchantId city :<|> postDriverBlockWithReason merchantId city :<|> postDriverBlock merchantId city :<|> getDriverBlockReasonList merchantId city :<|> postDriverUnblock merchantId city :<|> getDriverLocation merchantId city :<|> deleteDriverPermanentlyDelete merchantId city :<|> postDriverUnlinkDL merchantId city :<|> postDriverUnlinkAadhaar merchantId city :<|> postDriverUpdatePhoneNumber merchantId city :<|> postDriverUpdateByPhoneNumber merchantId city :<|> postDriverUpdateName merchantId city :<|> postDriverDeleteRC merchantId city :<|> getDriverClearStuckOnRide merchantId city :<|> postDriverSendDummyNotification merchantId city :<|> postDriverChangeOperatingCity merchantId city :<|> getDriverGetOperatingCity merchantId city :<|> postDriverPauseOrResumeServiceCharges merchantId city :<|> postDriverUpdateRCInvalidStatus merchantId city :<|> postDriverUpdateVehicleVariant merchantId city :<|> postDriverBulkReviewRCVariant merchantId city :<|> postDriverUpdateDriverTag merchantId city :<|> postDriverClearFee merchantId city

getDriverDocumentsInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler Dashboard.Common.Driver.DriverDocumentsInfoRes)
getDriverDocumentsInfo a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverDocumentsInfo a2 a1

postDriverPersonNumbers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.PersonIdsReq -> Environment.FlowHandler [Dashboard.Common.PersonRes])
postDriverPersonNumbers a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverPersonNumbers a3 a2 a1

getDriverAadhaarInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverAadhaarInfoRes)
getDriverAadhaarInfo a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverAadhaarInfo a3 a2 a1

getDriverAadhaarInfobyMobileNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverAadhaarInfoByPhoneReq)
getDriverAadhaarInfobyMobileNumber a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverAadhaarInfobyMobileNumber a3 a2 a1

getDriverList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverListRes)
getDriverList a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverList a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getDriverActivity :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler Dashboard.Common.Driver.DriverActivityRes)
getDriverActivity a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverActivity a2 a1

postDriverDisable :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverDisable a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverDisable a3 a2 a1

postDriverAcRestrictionUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateACUsageRestrictionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverAcRestrictionUpdate a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverAcRestrictionUpdate a4 a3 a2 a1

postDriverBlockWithReason :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Driver.BlockDriverWithReasonReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverBlockWithReason a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverBlockWithReason a5 a4 a3 a2 a1

postDriverBlock :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverBlock a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverBlock a3 a2 a1

getDriverBlockReasonList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Driver.BlockReason])
getDriverBlockReasonList a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverBlockReasonList a2 a1

postDriverUnblock :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUnblock a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUnblock a4 a3 a2 a1

getDriverLocation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Dashboard.Common.Driver.DriverIds -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.DriverLocationRes)
getDriverLocation a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverLocation a5 a4 a3 a2 a1

deleteDriverPermanentlyDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteDriverPermanentlyDelete a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.deleteDriverPermanentlyDelete a3 a2 a1

postDriverUnlinkDL :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUnlinkDL a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUnlinkDL a3 a2 a1

postDriverUnlinkAadhaar :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUnlinkAadhaar a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUnlinkAadhaar a3 a2 a1

postDriverUpdatePhoneNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdatePhoneNumberReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdatePhoneNumber a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdatePhoneNumber a4 a3 a2 a1

postDriverUpdateByPhoneNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverDataReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateByPhoneNumber a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateByPhoneNumber a4 a3 a2 a1

postDriverUpdateName :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverNameReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateName a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateName a4 a3 a2 a1

postDriverDeleteRC :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.DeleteRCReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverDeleteRC a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverDeleteRC a4 a3 a2 a1

getDriverClearStuckOnRide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.ClearOnRideStuckDriversRes)
getDriverClearStuckOnRide a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverClearStuckOnRide a3 a2 a1

postDriverSendDummyNotification :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverSendDummyNotification a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverSendDummyNotification a3 a2 a1

postDriverChangeOperatingCity :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.ChangeOperatingCityReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverChangeOperatingCity a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverChangeOperatingCity a4 a3 a2 a1

getDriverGetOperatingCity :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Ride) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.GetOperatingCityResp)
getDriverGetOperatingCity a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.getDriverGetOperatingCity a5 a4 a3 a2 a1

postDriverPauseOrResumeServiceCharges :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.PauseOrResumeServiceChargesReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverPauseOrResumeServiceCharges a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverPauseOrResumeServiceCharges a4 a3 a2 a1

postDriverUpdateRCInvalidStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateRCInvalidStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateRCInvalidStatus a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateRCInvalidStatus a4 a3 a2 a1

postDriverUpdateVehicleVariant :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateVehicleVariantReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateVehicleVariant a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateVehicleVariant a4 a3 a2 a1

postDriverBulkReviewRCVariant :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> [API.Types.ProviderPlatform.Management.Driver.ReviewRCVariantReq] -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Driver.ReviewRCVariantRes])
postDriverBulkReviewRCVariant a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverBulkReviewRCVariant a3 a2 a1

postDriverUpdateDriverTag :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverTagReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateDriverTag a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverUpdateDriverTag a4 a3 a2 a1

postDriverClearFee :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.ClearDriverFeeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverClearFee a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.Driver.postDriverClearFee a4 a3 a2 a1
