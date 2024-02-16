{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Confirm (API, handler) where

import qualified Beckn.ACL.Confirm as ACL
import qualified Beckn.ACL.OnConfirm as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude (ByteString)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import qualified SharedLogic.Booking as SBooking
import qualified SharedLogic.CallBAP as BP
import Storage.Beam.SystemConfigs ()

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> Confirm.ConfirmAPI

handler :: FlowServer API
handler = confirm

confirm ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  -- Confirm.ConfirmReq ->
  ByteString ->
  FlowHandler AckResponse
confirm transporterId (SignatureAuthResult _ subscriber) reqBS = withFlowHandlerBecknAPI do
  req <- decodeReq reqBS
  (dConfirmReq, callbackUrl, bapId, msgId, city, country, txnId, bppId, bppUri) <- case req of
    Right reqV2 -> do
      transactionId <- Utils.getTransactionId reqV2.confirmReqContext
      Utils.withTransactionIdLogTag transactionId $ do
        logTagInfo "Confirm APIV2 Flow" "Reached"
        dConfirmReq <- ACL.buildConfirmReqV2 reqV2
        let context = reqV2.confirmReqContext
        callbackUrl <- Utils.getContextBapUri context
        bppUri <- Utils.getContextBppUri context
        messageId <- Utils.getMessageId context
        bapId <- Utils.getContextBapId context
        city <- Utils.getContextCity context
        country <- Utils.getContextCountry context
        pure (dConfirmReq, callbackUrl, bapId, messageId, city, country, Just transactionId, context.contextBppId, bppUri)
    Left reqV1 ->
      withTransactionIdLogTag reqV1 $ do
        logTagInfo "Confirm APIV1 Flow" "Reached"
        dConfirmReq <- ACL.buildConfirmReq reqV1
        pure (dConfirmReq, reqV1.context.bap_uri, reqV1.context.bap_id, reqV1.context.message_id, reqV1.context.city, reqV1.context.country, reqV1.context.transaction_id, reqV1.context.bpp_id, reqV1.context.bpp_uri)

  Redis.whenWithLockRedis (confirmLockKey dConfirmReq.bookingId.getId) 60 $ do
    now <- getCurrentTime
    (transporter, eitherQuote) <- DConfirm.validateRequest subscriber transporterId dConfirmReq now
    fork "confirm" $ do
      isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
      Redis.whenWithLockRedis (confirmProcessingLockKey dConfirmReq.bookingId.getId) 60 $ do
        dConfirmRes <- DConfirm.handler transporter dConfirmReq eitherQuote
        case dConfirmRes.rideInfo of
          Just rideInfo' -> do
            fork "on_confirm/on_update" $ do
              handle (errHandler dConfirmRes transporter (Just rideInfo'.driver)) $ do
                callOnConfirm dConfirmRes isBecknSpecVersion2 msgId txnId bapId callbackUrl bppId bppUri city country
                void $ BP.sendRideAssignedUpdateToBAP dConfirmRes.booking rideInfo'.ride rideInfo'.driver rideInfo'.vehicle
          Nothing -> do
            fork "on_confirm/on_update" $ do
              handle (errHandler dConfirmRes transporter Nothing) $ do
                callOnConfirm dConfirmRes isBecknSpecVersion2 msgId txnId bapId callbackUrl bppId bppUri city country
  pure Ack
  where
    errHandler dConfirmRes transporter mbDriver exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = SBooking.cancelBooking dConfirmRes.booking mbDriver transporter
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = SBooking.cancelBooking dConfirmRes.booking mbDriver transporter
      | otherwise = throwM exc

    callOnConfirm dConfirmRes isBecknSpecVersion2 msgId txnId bapId callbackUrl bppId bppUri city country = do
      if isBecknSpecVersion2
        then do
          context <- ContextV2.buildContextV2 Context.CONFIRM Context.MOBILITY msgId txnId bapId callbackUrl bppId bppUri city country
          onConfirmMessage <- ACL.buildOnConfirmMessageV2 dConfirmRes
          void $ BP.callOnConfirmV2 dConfirmRes.transporter context onConfirmMessage
        else do
          context <- buildTaxiContext Context.CONFIRM msgId txnId bapId callbackUrl bppId bppUri city country False
          onConfirmMessage <- ACL.buildOnConfirmMessage dConfirmRes
          void $ BP.callOnConfirm dConfirmRes.transporter context onConfirmMessage

confirmLockKey :: Text -> Text
confirmLockKey id = "Driver:Confirm:BookingId-" <> id

confirmProcessingLockKey :: Text -> Text
confirmProcessingLockKey id = "Driver:Confirm:Processing:BookingId-" <> id

decodeReq :: MonadFlow m => ByteString -> m (Either Confirm.ConfirmReq Confirm.ConfirmReqV2)
decodeReq reqBS =
  case A.eitherDecodeStrict reqBS of
    Right reqV1 -> pure $ Left reqV1
    Left _ ->
      case A.eitherDecodeStrict reqBS of
        Right reqV2 -> pure $ Right reqV2
        Left err -> throwError . InvalidRequest $ "Unable to parse request: " <> T.pack err <> T.decodeUtf8 reqBS
