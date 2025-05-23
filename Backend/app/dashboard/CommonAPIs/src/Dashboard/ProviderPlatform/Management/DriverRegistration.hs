{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Management.DriverRegistration
  ( module Dashboard.ProviderPlatform.Management.DriverRegistration,
    module Reexport,
  )
where

import API.Types.ProviderPlatform.Management.DriverRegistration as Reexport
import Dashboard.Common as Reexport
import Kernel.External.Notification.FCM.Types (FCMRecipientToken)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess (APISuccess)
import Servant hiding (Summary)

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data DriverRegistrationEndpoint
  = UploadDocumentEndpoint
  | RegisterDLEndpoint
  | RegisterRCEndpoint
  | GenerateAadhaarOtpEndpoint
  | VerifyAadhaarOtpEndpoint
  | UpdateDocumentEndpoint
  | AuthEndpoint
  | PostDriverRegistrationDocumentUploadEndpoint
  | PostDriverRegistrationRegisterDlEndpoint
  | PostDriverRegistrationRegisterRcEndpoint
  | PostDriverRegistrationRegisterGenerateAadhaarOtpEndpoint
  | PostDriverRegistrationRegisterVerifyAadhaarOtpEndpoint
  | PostDriverRegistrationDocumentsUpdateEndpoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord, ToSchema)

derivePersistField "DriverRegistrationEndpoint"

instance HideSecrets UploadDocumentReq where
  type ReqWithoutSecrets UploadDocumentReq = UploadDocumentTReq
  hideSecrets UploadDocumentReq {..} = UploadDocumentTReq {..}

-- auth  API ------------------------
-- ----------------------------------------

type AuthAPI =
  "auth"
    :> ReqBody '[JSON] AuthReq
    :> Post '[JSON] AuthRes

data AuthReq = AuthReq
  { mobileNumber :: Text,
    mobileCountryCode :: Text
  }
  deriving (Generic, FromJSON, ToSchema, ToJSON)

data AuthRes = AuthRes
  { authId :: Text,
    attempts :: Int
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)

-- verify  API ------------------------
-- ----------------------------------------

type VerifyAPI =
  Capture "authId" Text
    :> "verify"
    :> ReqBody '[JSON] AuthVerifyReq
    :> Post '[JSON] APISuccess

---------- Verify Login --------
data AuthVerifyReq = AuthVerifyReq
  { otp :: Text,
    deviceToken :: FCMRecipientToken
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
