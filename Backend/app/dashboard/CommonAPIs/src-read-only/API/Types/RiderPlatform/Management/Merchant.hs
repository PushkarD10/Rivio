{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Merchant where

import qualified Dashboard.Common.Merchant
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import qualified Lib.Types.SpecialLocation
import Servant
import Servant.Client

data MerchantUpdateReq = MerchantUpdateReq
  { name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    exoPhones :: Kernel.Prelude.Maybe (Kernel.Prelude.NonEmpty Dashboard.Common.Merchant.ExophoneReq),
    fcmConfig :: Kernel.Prelude.Maybe Dashboard.Common.Merchant.FCMConfigUpdateReq,
    gatewayUrl :: Kernel.Prelude.Maybe Kernel.Prelude.BaseUrl,
    registryUrl :: Kernel.Prelude.Maybe Kernel.Prelude.BaseUrl
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("merchant" :> (PostMerchantUpdate :<|> GetMerchantServiceUsageConfig :<|> PostMerchantServiceConfigMapsUpdate :<|> PostMerchantServiceUsageConfigMapsUpdate :<|> PostMerchantServiceConfigSmsUpdate :<|> PostMerchantServiceUsageConfigSmsUpdate :<|> PostMerchantConfigOperatingCityCreateHelper :<|> PostMerchantSpecialLocationUpsertHelper :<|> DeleteMerchantSpecialLocationDelete :<|> PostMerchantSpecialLocationGatesUpsertHelper :<|> DeleteMerchantSpecialLocationGatesDelete))

type PostMerchantUpdate = ("update" :> ReqBody '[JSON] API.Types.RiderPlatform.Management.Merchant.MerchantUpdateReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetMerchantServiceUsageConfig = ("serviceUsageConfig" :> Get '[JSON] Dashboard.Common.Merchant.ServiceUsageConfigRes)

type PostMerchantServiceConfigMapsUpdate =
  ( "serviceConfig" :> "maps" :> "update" :> ReqBody '[JSON] Dashboard.Common.Merchant.MapsServiceConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantServiceUsageConfigMapsUpdate =
  ( "serviceUsageConfig" :> "maps" :> "update" :> ReqBody '[JSON] Dashboard.Common.Merchant.MapsServiceUsageConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantServiceConfigSmsUpdate =
  ( "serviceConfig" :> "sms" :> "update" :> ReqBody '[JSON] Dashboard.Common.Merchant.SmsServiceConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantServiceUsageConfigSmsUpdate =
  ( "serviceUsageConfig" :> "sms" :> "update" :> ReqBody '[JSON] Dashboard.Common.Merchant.SmsServiceUsageConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigOperatingCityCreate =
  ( "config" :> "operatingCity" :> "create"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           Dashboard.Common.Merchant.CreateMerchantOperatingCityReq
      :> Post '[JSON] Dashboard.Common.Merchant.CreateMerchantOperatingCityRes
  )

type PostMerchantConfigOperatingCityCreateHelper =
  ( "config" :> "operatingCity" :> "create" :> ReqBody '[JSON] Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT
      :> Post
           '[JSON]
           Dashboard.Common.Merchant.CreateMerchantOperatingCityRes
  )

type PostMerchantSpecialLocationUpsert =
  ( "specialLocation" :> "upsert"
      :> QueryParam
           "specialLocationId"
           (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation)
      :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp Dashboard.Common.Merchant.UpsertSpecialLocationReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantSpecialLocationUpsertHelper =
  ( "specialLocation" :> "upsert" :> QueryParam "specialLocationId" (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation)
      :> ReqBody
           '[JSON]
           Dashboard.Common.Merchant.UpsertSpecialLocationReqT
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type DeleteMerchantSpecialLocationDelete =
  ( "specialLocation" :> Capture "specialLocationId" (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation) :> "delete"
      :> Delete
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantSpecialLocationGatesUpsert =
  ( "specialLocation"
      :> Capture
           "specialLocationId"
           (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation)
      :> "gates"
      :> "upsert"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           Dashboard.Common.Merchant.UpsertSpecialLocationGateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantSpecialLocationGatesUpsertHelper =
  ( "specialLocation"
      :> Capture
           "specialLocationId"
           (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation)
      :> "gates"
      :> "upsert"
      :> ReqBody '[JSON] Dashboard.Common.Merchant.UpsertSpecialLocationGateReqT
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type DeleteMerchantSpecialLocationGatesDelete =
  ( "specialLocation"
      :> Capture
           "specialLocationId"
           (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation)
      :> "gates"
      :> "delete"
      :> Capture "gateName" Kernel.Prelude.Text
      :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess
  )

data MerchantAPIs = MerchantAPIs
  { postMerchantUpdate :: API.Types.RiderPlatform.Management.Merchant.MerchantUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getMerchantServiceUsageConfig :: EulerHS.Types.EulerClient Dashboard.Common.Merchant.ServiceUsageConfigRes,
    postMerchantServiceConfigMapsUpdate :: Dashboard.Common.Merchant.MapsServiceConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantServiceUsageConfigMapsUpdate :: Dashboard.Common.Merchant.MapsServiceUsageConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantServiceConfigSmsUpdate :: Dashboard.Common.Merchant.SmsServiceConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantServiceUsageConfigSmsUpdate :: Dashboard.Common.Merchant.SmsServiceUsageConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigOperatingCityCreate :: Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT -> EulerHS.Types.EulerClient Dashboard.Common.Merchant.CreateMerchantOperatingCityRes,
    postMerchantSpecialLocationUpsert :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation) -> Dashboard.Common.Merchant.UpsertSpecialLocationReqT -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteMerchantSpecialLocationDelete :: Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantSpecialLocationGatesUpsert :: Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Dashboard.Common.Merchant.UpsertSpecialLocationGateReqT -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteMerchantSpecialLocationGatesDelete :: Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkMerchantAPIs :: (Client EulerHS.Types.EulerClient API -> MerchantAPIs)
mkMerchantAPIs merchantClient = (MerchantAPIs {..})
  where
    postMerchantUpdate :<|> getMerchantServiceUsageConfig :<|> postMerchantServiceConfigMapsUpdate :<|> postMerchantServiceUsageConfigMapsUpdate :<|> postMerchantServiceConfigSmsUpdate :<|> postMerchantServiceUsageConfigSmsUpdate :<|> postMerchantConfigOperatingCityCreate :<|> postMerchantSpecialLocationUpsert :<|> deleteMerchantSpecialLocationDelete :<|> postMerchantSpecialLocationGatesUpsert :<|> deleteMerchantSpecialLocationGatesDelete = merchantClient
