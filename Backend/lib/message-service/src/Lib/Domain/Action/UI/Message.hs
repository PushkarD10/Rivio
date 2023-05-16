{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Domain.Action.UI.Message where

import qualified AWS.S3 as S3
import Data.OpenApi hiding (description, info, title, url)
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import Kernel.External.Types
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.JSON (stripPrefixUnderscoreIfAny)
import qualified Lib.Domain.Types.Message.MediaFile as MF
import qualified Lib.Domain.Types.Message.Message as Domain
import Lib.Environment
import qualified Lib.Storage.Queries.Message.MediaFile as MFQ
import qualified Lib.Storage.Queries.Message.MessageReport as MRQ

data MediaFileApiResponse = MediaFileApiResponse
  { url :: Text,
    fileType :: MF.MediaType
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data MessageAPIEntityResponse = MessageAPIEntityResponse
  { title :: Text,
    description :: Text,
    _type :: Domain.MessageType,
    created_at :: UTCTime,
    label :: Maybe Text,
    reply :: Maybe Text,
    readStatus :: Bool,
    messageId :: Id Domain.Message,
    mediaFiles :: [MediaFileApiResponse]
  }
  deriving (Generic, ToSchema, FromJSON)

instance ToJSON MessageAPIEntityResponse where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

newtype MessageReplyReq = MessageReplyReq {reply :: Text}
  deriving (Generic, ToSchema, ToJSON, FromJSON)

messageList :: (HasField "language" a (Maybe Language)) => Id Domain.Person -> Maybe Int -> Maybe Int -> Flow (Maybe a) -> Flow [MessageAPIEntityResponse]
messageList driverId mbLimit mbOffset findByPersonId = do
  person <- findByPersonId >>= fromMaybeM (PersonNotFound driverId.getId)
  messageDetails <- Esq.runInReplica $ MRQ.findByDriverIdAndLanguage (cast driverId) (fromMaybe ENGLISH person.language) mbLimit mbOffset
  mapM makeMessageAPIEntity messageDetails
  where
    makeMessageAPIEntity (messageReport, rawMessage, messageTranslation) = do
      mediaFilesApiType <- map (\mediaFile -> MediaFileApiResponse mediaFile.url mediaFile._type) <$> MFQ.findAllIn rawMessage.mediaFiles
      pure $
        MessageAPIEntityResponse
          { title = maybe rawMessage.title (.title) messageTranslation,
            description = maybe rawMessage.description (.description) messageTranslation,
            _type = rawMessage._type,
            label = messageTranslation >>= (.label),
            reply = messageReport.reply,
            created_at = rawMessage.createdAt,
            readStatus = messageReport.readStatus,
            messageId = rawMessage.id,
            mediaFiles = mediaFilesApiType
          }

-- messageLiked :: Id SP.Person -> Id Domain.Message -> Flow APISuccess
-- messageLiked driverId messageId = do
--   _ <- Esq.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
--   messageDetails <- Esq.runInReplica $ MRQ.findByMessageIdAndDriverId messageId (cast driverId) >>= fromMaybeM (InvalidRequest "Message not found")
--   unless (messageDetails.readStatus) $
--     throwError $ InvalidRequest "Message is not seen"
--   let val = if messageDetails.likeStatus then (-1) else 1
--   Esq.runTransaction $ do
--     when messageDetails.readStatus $ MQ.updateMessageLikeCount messageId val
--     MRQ.updateMessageLikeByMessageIdAndDriverIdAndReadStatus messageId (cast driverId)
--   return Success

getMessage :: (HasField "language" a (Maybe Language)) => Id Domain.Person -> Id Domain.Message -> Flow (Maybe a) -> Flow MessageAPIEntityResponse
getMessage driverId messageId findByPersonId = do
  person <- findByPersonId >>= fromMaybeM (PersonNotFound driverId.getId)
  messageDetails <-
    Esq.runInReplica $
      MRQ.findByDriverIdMessageIdAndLanguage (cast driverId) messageId (fromMaybe ENGLISH person.language)
        >>= fromMaybeM (InvalidRequest "Message not found")
  makeMessageAPIEntity messageDetails
  where
    makeMessageAPIEntity (messageReport, rawMessage, messageTranslation) = do
      mediaFilesApiType <- map (\mediaFile -> MediaFileApiResponse mediaFile.url mediaFile._type) <$> MFQ.findAllIn rawMessage.mediaFiles
      pure $
        MessageAPIEntityResponse
          { title = maybe rawMessage.title (.title) messageTranslation,
            description = maybe rawMessage.description (.description) messageTranslation,
            _type = rawMessage._type,
            label = messageTranslation >>= (.label),
            reply = messageReport.reply,
            created_at = rawMessage.createdAt,
            readStatus = messageReport.readStatus,
            messageId = rawMessage.id,
            mediaFiles = mediaFilesApiType
          }

fetchMedia :: Id Domain.Person -> Text -> Flow (Maybe a) -> Flow Text
fetchMedia driverId filePath findByPersonId = do
  _ <- findByPersonId >>= fromMaybeM (PersonNotFound driverId.getId)
  S3.get $ T.unpack filePath

messageSeen :: Id Domain.Person -> Id Domain.Message -> Flow (Maybe a) -> Flow APISuccess
messageSeen driverId messageId findByPersonId = do
  _ <- findByPersonId >>= fromMaybeM (PersonNotFound driverId.getId)
  Esq.runTransaction $ MRQ.updateSeenAndReplyByMessageIdAndDriverId messageId (cast driverId) True Nothing
  return Success

messageResponse :: Id Domain.Person -> Id Domain.Message -> MessageReplyReq -> Flow (Maybe a) -> Flow APISuccess
messageResponse driverId messageId MessageReplyReq {..} findByPersonId = do
  _ <- findByPersonId >>= fromMaybeM (PersonNotFound driverId.getId)
  Esq.runTransaction $ MRQ.updateSeenAndReplyByMessageIdAndDriverId messageId (cast driverId) True (Just reply)
  return Success
