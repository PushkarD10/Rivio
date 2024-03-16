{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module BecknV2.OnDemand.Utils.Common where

import BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Aeson as A
import Data.Data (Data, gmapQ)
import Data.Generics.Aliases (ext1Q)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.UUID as UUID
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Error
import Kernel.Utils.Common
import Text.Printf (printf)

allNothing :: (Data d) => d -> Bool
allNothing = not . or . gmapQ (const True `ext1Q` isJust)

getStartLocation :: [Spec.Stop] -> Maybe Spec.Stop
getStartLocation = find (\stop -> stop.stopType == Just (show Enums.START))

getDropLocation :: [Spec.Stop] -> Maybe Spec.Stop
getDropLocation = find (\stop -> stop.stopType == Just (show Enums.END))

getTransactionId :: (MonadFlow m) => Spec.Context -> m Text
getTransactionId context = context.contextTransactionId <&> UUID.toText & fromMaybeM (InvalidRequest "Transaction Id not found")

getMessageId :: (MonadFlow m) => Spec.Context -> m Text
getMessageId context = context.contextMessageId <&> UUID.toText & fromMaybeM (InvalidRequest "Transaction Id not found")

_decodeReq :: (MonadFlow m, A.FromJSON v1, A.FromJSON v2) => ByteString -> m (Either v1 v2)
_decodeReq reqBS =
  case A.eitherDecodeStrict reqBS of
    Right reqV1 -> pure $ Left reqV1
    Left _ ->
      case A.eitherDecodeStrict reqBS of
        Right reqV2 -> pure $ Right reqV2
        Left err -> throwError . InvalidRequest $ "Unable to parse request: " <> T.pack err <> T.decodeUtf8 reqBS

gpsToText :: Gps.Gps -> Maybe Text
gpsToText Gps.Gps {..} = Just $ T.pack (printf "%.6f" lat) <> ", " <> T.pack (printf "%.6f" lon)
