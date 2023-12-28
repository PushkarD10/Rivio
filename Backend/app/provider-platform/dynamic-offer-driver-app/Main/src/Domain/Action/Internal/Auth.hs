{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.Auth where

import Data.OpenApi (ToSchema)
import Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Auth (verifyPerson)

newtype InternalResp = InternalResp
  { driverId :: Id DP.Person
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

internalAuth :: (HasField "locationTrackingServiceKey" AppEnv Text) => Maybe RegToken -> Maybe Text -> Maybe (Id Merchant) -> Flow InternalResp
internalAuth token apiKey merchantId = do
  locationTrackingServiceKey <- asks (.locationTrackingServiceKey)
  unless (apiKey == Just locationTrackingServiceKey) $ do
    throwError $ InvalidRequest "Invalid API key"
  (driverId, currentMerchantId, _merchantOpCityId) <- verifyPerson (fromMaybe "" token)
  unless (currentMerchantId == fromMaybe "" merchantId) $ do
    throwError $ InvalidRequest "Invalid merchant id"
  pure $
    InternalResp
      { driverId
      }
