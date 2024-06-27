{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.MessageBuilder
  ( BuildSendOTPMessageReq (..),
    BuildSOSAlertMessageReq (..),
    buildSendOTPMessage,
    BuildSendBookingOTPMessageReq (..),
    buildSendBookingOTPMessage,
    BuildGenericMessageReq (..),
    buildGenericMessage,
    buildSOSAlertMessage,
    BuildMarkRideAsSafeMessageReq (..),
    buildMarkRideAsSafeMessage,
    BuildFollowRideMessageReq (..),
    buildFollowRideStartedMessage,
    BuildAddedAsEmergencyContactMessageReq (..),
    buildAddedAsEmergencyContactMessage,
    BuildTicketBookingCancelledMessageReq (..),
    buildTicketBookingCancelled,
    BuildFRFSTicketBookedMessageReq (..),
    buildFRFSTicketBookedMessage,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrgConfig as DPOC
import qualified Domain.Types.PartnerOrganization as DPO
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import qualified Storage.CachedQueries.PartnerOrgConfig as CQPOC
import Tools.Error
import qualified UrlShortner.Common as UrlShortner

templateText :: Text -> Text
templateText txt = "{#" <> txt <> "#}"

data BuildSendOTPMessageReq = BuildSendOTPMessageReq
  { otp :: Text,
    hash :: Text
  }
  deriving (Generic)

buildSendOTPMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildSendOTPMessageReq -> m Text
buildSendOTPMessage merchantOperatingCityId req = do
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId DMM.SEND_OTP
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.SEND_OTP))
  return $
    merchantMessage.message
      & T.replace (templateText "otp") req.otp
      & T.replace (templateText "hash") req.hash

data BuildSendBookingOTPMessageReq = BuildSendBookingOTPMessageReq
  { otp :: Text,
    amount :: Text
  }
  deriving (Generic)

buildSendBookingOTPMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildSendBookingOTPMessageReq -> m Text
buildSendBookingOTPMessage merchantOperatingCityId req = do
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId DMM.SEND_BOOKING_OTP
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.SEND_BOOKING_OTP))
  return $
    merchantMessage.message
      & T.replace (templateText "otp") req.otp
      & T.replace (templateText "amount") req.amount

data BuildGenericMessageReq = BuildGenericMessageReq {}
  deriving (Generic)

buildGenericMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> DMM.MessageKey -> BuildGenericMessageReq -> m Text
buildGenericMessage merchantOpCityId messageKey _ = do
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOpCityId messageKey
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show messageKey))
  let jsonData = merchantMessage.jsonData
  return $
    merchantMessage.message
      & T.replace (templateText "var1") (fromMaybe "" jsonData.var1)
      & T.replace (templateText "var2") (fromMaybe "" jsonData.var2)
      & T.replace (templateText "var3") (fromMaybe "" jsonData.var3)

data BuildSOSAlertMessageReq = BuildSOSAlertMessageReq
  { userName :: Text,
    rideLink :: Text
  }
  deriving (Generic)

buildSOSAlertMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildSOSAlertMessageReq -> m Text
buildSOSAlertMessage merchantOperatingCityId req = do
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId DMM.SEND_SOS_ALERT
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.SEND_SOS_ALERT))
  return $
    merchantMessage.message
      & T.replace (templateText "userName") req.userName
      & T.replace (templateText "rideLink") req.rideLink

newtype BuildMarkRideAsSafeMessageReq = BuildMarkRideAsSafeMessageReq
  { userName :: Text
  }
  deriving (Generic)

buildMarkRideAsSafeMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildMarkRideAsSafeMessageReq -> m Text
buildMarkRideAsSafeMessage merchantOperatingCityId req = do
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId DMM.MARK_RIDE_AS_SAFE
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.MARK_RIDE_AS_SAFE))
  return $
    merchantMessage.message
      & T.replace (templateText "userName") req.userName

data BuildFollowRideMessageReq = BuildFollowRideMessageReq
  { userName :: Text,
    rideLink :: Text
  }
  deriving (Generic)

buildFollowRideStartedMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildFollowRideMessageReq -> m Text
buildFollowRideStartedMessage merchantOperatingCityId req = do
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId DMM.FOLLOW_RIDE
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.FOLLOW_RIDE))
  return $
    merchantMessage.message
      & T.replace (templateText "userName") req.userName
      & T.replace (templateText "rideLink") req.rideLink

data BuildAddedAsEmergencyContactMessageReq = BuildAddedAsEmergencyContactMessageReq
  { userName :: Text,
    appUrl :: Text
  }
  deriving (Generic)

buildAddedAsEmergencyContactMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildAddedAsEmergencyContactMessageReq -> m Text
buildAddedAsEmergencyContactMessage merchantOperatingCityId req = do
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId DMM.ADDED_AS_EMERGENCY_CONTACT
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.ADDED_AS_EMERGENCY_CONTACT))
  return $
    merchantMessage.message
      & T.replace (templateText "userName") req.userName
      & T.replace (templateText "appUrl") req.appUrl

data BuildTicketBookingCancelledMessageReq = BuildTicketBookingCancelledMessageReq
  { personName :: Text,
    categoryName :: Text
  }
  deriving (Generic)

buildTicketBookingCancelled :: (EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> BuildTicketBookingCancelledMessageReq -> m Text
buildTicketBookingCancelled merchantOperatingCityId req = do
  merchantMessage <-
    QMM.findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId DMM.TICKET_BOOKING_CANCELLED
      >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId (show DMM.TICKET_BOOKING_CANCELLED))
  return $
    merchantMessage.message
      & T.replace (templateText "personName") req.personName
      & T.replace (templateText "categoryName") req.categoryName

data BuildFRFSTicketBookedMessageReq = BuildFRFSTicketBookedMessageReq
  { countOfTickets :: Int,
    bookingId :: Id DFTB.FRFSTicketBooking
  }
  deriving (Generic, Show)

buildFRFSTicketBookedMessage :: (EsqDBFlow m r, CacheFlow m r, HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig]) => Id DPO.PartnerOrganization -> BuildFRFSTicketBookedMessageReq -> m (Maybe Text)
buildFRFSTicketBookedMessage pOrgId req = do
  smsPOCfg <- do
    pOrgCfg <- CQPOC.findByIdAndCfgType pOrgId DPOC.TICKET_SMS >>= fromMaybeM (PartnerOrgConfigNotFound pOrgId.getId $ show DPOC.TICKET_SMS)
    DPOC.getTicketSMSConfig pOrgCfg.config

  forM smsPOCfg.template $
    \msg -> do
      let ticketPlural = bool "tickets are" "ticket is" $ req.countOfTickets == 1
          baseUrl = smsPOCfg.publicUrl & T.replace (templateText "FRFS_BOOKING_ID") req.bookingId.getId
          shortUrlReq =
            UrlShortner.GenerateShortUrlReq
              { baseUrl,
                customShortCode = Nothing,
                shortCodeLength = Nothing,
                expiryInHours = Nothing
              }
      res <- UrlShortner.generateShortUrl shortUrlReq
      let url = res.shortUrl
      logDebug $ "Generated short url: " <> url
      pure $
        msg
          & T.replace (templateText "TICKET_PLURAL") ticketPlural
          & T.replace (templateText "URL") url
