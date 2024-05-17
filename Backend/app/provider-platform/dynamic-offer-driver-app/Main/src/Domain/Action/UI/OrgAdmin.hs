{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.OrgAdmin
  ( OrgAdminProfileRes (..),
    UpdateOrgAdminProfileReq (..),
    UpdateOrgAdminProfileRes,
    getProfile,
    updateProfile,
  )
where

import Control.Applicative
import qualified Domain.Action.UI.Person as SP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import Kernel.External.Encryption (decrypt)
import Kernel.External.Notification.FCM.Types
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.PersonExtra as QPerson
import Tools.Error

data OrgAdminProfileRes = OrgAdminProfileRes
  { id :: Id SP.Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    maskedMobileNumber :: Maybe Text,
    maskedDeviceToken :: Maybe FCMRecipientToken,
    organization :: DM.MerchantAPIEntity
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data UpdateOrgAdminProfileReq = UpdateOrgAdminProfileReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type UpdateOrgAdminProfileRes = OrgAdminProfileRes

getProfile :: (KvDbFlow m r, EncFlow m r) => SP.Person -> m OrgAdminProfileRes
getProfile admin = do
  let merchantId = admin.merchantId
  org <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  decAdmin <- decrypt admin
  let personAPIEntity = SP.makePersonAPIEntity decAdmin
  return $ makeOrgAdminProfileRes personAPIEntity (DM.makeMerchantAPIEntity org)

updateProfile :: (KvDbFlow m r, EncFlow m r, MonadFlow m) => SP.Person -> UpdateOrgAdminProfileReq -> m UpdateOrgAdminProfileRes
updateProfile admin req = do
  let merchantId = admin.merchantId
      updAdmin =
        admin{firstName = fromMaybe admin.firstName req.firstName,
              middleName = req.middleName <|> admin.middleName,
              lastName = req.lastName <|> admin.lastName,
              deviceToken = req.deviceToken <|> admin.deviceToken
             }
  _ <- QPerson.updatePersonRec updAdmin.id updAdmin
  org <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  decUpdAdmin <- decrypt updAdmin
  let personAPIEntity = SP.makePersonAPIEntity decUpdAdmin
  return $ makeOrgAdminProfileRes personAPIEntity (DM.makeMerchantAPIEntity org)

makeOrgAdminProfileRes :: SP.PersonAPIEntity -> DM.MerchantAPIEntity -> OrgAdminProfileRes
makeOrgAdminProfileRes SP.PersonAPIEntity {..} org =
  OrgAdminProfileRes
    { organization = org,
      ..
    }
