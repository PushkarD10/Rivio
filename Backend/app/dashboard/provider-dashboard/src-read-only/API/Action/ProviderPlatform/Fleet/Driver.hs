{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Fleet.Driver
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified Dashboard.Common
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import qualified Domain.Action.ProviderPlatform.Fleet.Driver
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("driver" :> (PostDriverFleetAddVehicle :<|> PostDriverFleetAddRCWithoutDriver :<|> GetDriverFleetGetAllVehicle :<|> GetDriverFleetGetAllDriver :<|> PostDriverFleetUnlink :<|> PostDriverFleetRemoveVehicle :<|> PostDriverFleetRemoveDriver :<|> GetDriverFleetTotalEarning :<|> GetDriverFleetVehicleEarning :<|> GetDriverFleetDriverEarning :<|> GetDriverFleetDriverVehicleAssociation :<|> GetDriverFleetDriverAssociation :<|> GetDriverFleetVehicleAssociation :<|> PostDriverFleetVehicleDriverRCstatus :<|> PostDriverUpdateFleetOwnerInfo :<|> GetDriverFleetOwnerInfo :<|> PostDriverFleetSendJoiningOtp :<|> PostDriverFleetVerifyJoiningOtp :<|> PostDriverFleetLinkRCWithDriver))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDriverFleetAddVehicle merchantId city :<|> postDriverFleetAddRCWithoutDriver merchantId city :<|> getDriverFleetGetAllVehicle merchantId city :<|> getDriverFleetGetAllDriver merchantId city :<|> postDriverFleetUnlink merchantId city :<|> postDriverFleetRemoveVehicle merchantId city :<|> postDriverFleetRemoveDriver merchantId city :<|> getDriverFleetTotalEarning merchantId city :<|> getDriverFleetVehicleEarning merchantId city :<|> getDriverFleetDriverEarning merchantId city :<|> getDriverFleetDriverVehicleAssociation merchantId city :<|> getDriverFleetDriverAssociation merchantId city :<|> getDriverFleetVehicleAssociation merchantId city :<|> postDriverFleetVehicleDriverRCstatus merchantId city :<|> postDriverUpdateFleetOwnerInfo merchantId city :<|> getDriverFleetOwnerInfo merchantId city :<|> postDriverFleetSendJoiningOtp merchantId city :<|> postDriverFleetVerifyJoiningOtp merchantId city :<|> postDriverFleetLinkRCWithDriver merchantId city

type PostDriverFleetAddVehicle = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'ADD_VEHICLE_FLEET :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetAddVehicle)

type PostDriverFleetAddRCWithoutDriver = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'ADD_RC_FLEET_WITHOUT_DRIVER :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetAddRCWithoutDriver)

type GetDriverFleetGetAllVehicle = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'GET_ALL_VEHICLE_FOR_FLEET :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetGetAllVehicle)

type GetDriverFleetGetAllDriver = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'GET_ALL_DRIVERS_FOR_FLEET :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetGetAllDriver)

type PostDriverFleetUnlink = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'FLEET_UNLINK_VEHICLE :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetUnlink)

type PostDriverFleetRemoveVehicle = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'FLEET_REMOVE_VEHICLE :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetRemoveVehicle)

type PostDriverFleetRemoveDriver = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'FLEET_REMOVE_DRIVER :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetRemoveDriver)

type GetDriverFleetTotalEarning = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'FLEET_TOTAL_EARNING :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetTotalEarning)

type GetDriverFleetVehicleEarning = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'FLEET_VEHICLE_EARNING :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetVehicleEarning)

type GetDriverFleetDriverEarning = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'FLEET_DRIVER_EARNING :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetDriverEarning)

type GetDriverFleetDriverVehicleAssociation =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'FLEET
      'GET_DRIVER_VEHICLE_ASSOCIATION
      :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetDriverVehicleAssociation
  )

type GetDriverFleetDriverAssociation = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'GET_DRIVER_ASSOCIATION :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetDriverAssociation)

type GetDriverFleetVehicleAssociation = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'GET_VEHICLE_ASSOCIATION :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetVehicleAssociation)

type PostDriverFleetVehicleDriverRCstatus =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'FLEET
      'SET_VEHICLE_DRIVER_RC_STATUS_FOR_FLEET
      :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetVehicleDriverRCstatus
  )

type PostDriverUpdateFleetOwnerInfo = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'UPDATE_FLEET_OWNER_INFO :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverUpdateFleetOwnerInfo)

type GetDriverFleetOwnerInfo = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'GET_FLEET_OWNER_INFO :> API.Types.ProviderPlatform.Fleet.Driver.GetDriverFleetOwnerInfo)

type PostDriverFleetSendJoiningOtp = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'SEND_FLEET_JOINING_OTP :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetSendJoiningOtp)

type PostDriverFleetVerifyJoiningOtp = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'VERIFY_FLEET_JOINING_OTP :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetVerifyJoiningOtp)

type PostDriverFleetLinkRCWithDriver = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'LINK_RC_WITH_DRIVER :> API.Types.ProviderPlatform.Fleet.Driver.PostDriverFleetLinkRCWithDriver)

postDriverFleetAddVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.Driver.AddVehicleReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetAddVehicle merchantShortId opCity apiTokenInfo mobileNo countryCode req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetAddVehicle merchantShortId opCity apiTokenInfo mobileNo countryCode req

postDriverFleetAddRCWithoutDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.ProviderPlatform.Management.DriverRegistration.RegisterRCReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetAddRCWithoutDriver merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetAddRCWithoutDriver merchantShortId opCity apiTokenInfo req

getDriverFleetGetAllVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.ListVehicleRes)
getDriverFleetGetAllVehicle merchantShortId opCity apiTokenInfo mblimit mboffset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetGetAllVehicle merchantShortId opCity apiTokenInfo mblimit mboffset

getDriverFleetGetAllDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetListDriverRes)
getDriverFleetGetAllDriver merchantShortId opCity apiTokenInfo mblimit mboffset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetGetAllDriver merchantShortId opCity apiTokenInfo mblimit mboffset

postDriverFleetUnlink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetUnlink merchantShortId opCity apiTokenInfo driverId vehicleNo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetUnlink merchantShortId opCity apiTokenInfo driverId vehicleNo

postDriverFleetRemoveVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetRemoveVehicle merchantShortId opCity apiTokenInfo vehicleNo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetRemoveVehicle merchantShortId opCity apiTokenInfo vehicleNo

postDriverFleetRemoveDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetRemoveDriver merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetRemoveDriver merchantShortId opCity apiTokenInfo driverId

getDriverFleetTotalEarning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetTotalEarningResponse)
getDriverFleetTotalEarning merchantShortId opCity apiTokenInfo from to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetTotalEarning merchantShortId opCity apiTokenInfo from to

getDriverFleetVehicleEarning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetEarningListRes)
getDriverFleetVehicleEarning merchantShortId opCity apiTokenInfo vehicleNo limit offset from to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetVehicleEarning merchantShortId opCity apiTokenInfo vehicleNo limit offset from to

getDriverFleetDriverEarning :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Driver.SortOn -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetEarningListRes)
getDriverFleetDriverEarning merchantShortId opCity apiTokenInfo mobileCountryCode mobileNo limit offset from to sortDesc sortOn = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetDriverEarning merchantShortId opCity apiTokenInfo mobileCountryCode mobileNo limit offset from to sortDesc sortOn

getDriverFleetDriverVehicleAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes)
getDriverFleetDriverVehicleAssociation merchantShortId opCity apiTokenInfo limit offset countryCode phoneNo vehicleNo includeStats from to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetDriverVehicleAssociation merchantShortId opCity apiTokenInfo limit offset countryCode phoneNo vehicleNo includeStats from to

getDriverFleetDriverAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Driver.DriverMode -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes)
getDriverFleetDriverAssociation merchantShortId opCity apiTokenInfo isActive limit offset countryCode phoneNo includeStats from to status = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetDriverAssociation merchantShortId opCity apiTokenInfo isActive limit offset countryCode phoneNo includeStats from to status

getDriverFleetVehicleAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Driver.FleetVehicleStatus -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.DrivertoVehicleAssociationRes)
getDriverFleetVehicleAssociation merchantShortId opCity apiTokenInfo limit offset vehicleNo includeStats from to status = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetVehicleAssociation merchantShortId opCity apiTokenInfo limit offset vehicleNo includeStats from to status

postDriverFleetVehicleDriverRCstatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Fleet.Driver.RCStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetVehicleDriverRCstatus merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetVehicleDriverRCstatus merchantShortId opCity apiTokenInfo driverId req

postDriverUpdateFleetOwnerInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Fleet.Driver.UpdateFleetOwnerInfoReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUpdateFleetOwnerInfo merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverUpdateFleetOwnerInfo merchantShortId opCity apiTokenInfo driverId req

getDriverFleetOwnerInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Driver.FleetOwnerInfoRes)
getDriverFleetOwnerInfo merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.getDriverFleetOwnerInfo merchantShortId opCity apiTokenInfo driverId

postDriverFleetSendJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> Environment.FlowHandler Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes)
postDriverFleetSendJoiningOtp merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetSendJoiningOtp merchantShortId opCity apiTokenInfo req

postDriverFleetVerifyJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.Driver.VerifyFleetJoiningOtpReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetVerifyJoiningOtp merchantShortId opCity apiTokenInfo authId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetVerifyJoiningOtp merchantShortId opCity apiTokenInfo authId req

postDriverFleetLinkRCWithDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Fleet.Driver.LinkRCWithDriverForFleetReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverFleetLinkRCWithDriver merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Driver.postDriverFleetLinkRCWithDriver merchantShortId opCity apiTokenInfo req
