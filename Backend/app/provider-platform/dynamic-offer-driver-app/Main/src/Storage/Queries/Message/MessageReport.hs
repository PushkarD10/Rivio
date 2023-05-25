{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Message.MessageReport where

import qualified Domain.Types.Message.Message as Msg
import Domain.Types.Message.MessageReport
import qualified Domain.Types.Message.MessageTranslation as MTD
import qualified Domain.Types.Person as P
import Kernel.External.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Esqueleto.DeletedEntity as EsqDE
import Kernel.Types.Common (MonadTime (getCurrentTime))
import Kernel.Types.Id
import Storage.Tabular.Message.Instances ()
import qualified Storage.Tabular.Message.Message as M
import Storage.Tabular.Message.MessageReport
import qualified Storage.Tabular.Message.MessageTranslation as MT
import qualified Storage.Tabular.Person as PT

createMany :: [MessageReport] -> SqlDB ()
createMany = Esq.createMany

create :: MessageReport -> SqlDB ()
create = Esq.create

fullMessage ::
  Language ->
  From
    ( Table MessageReportT
        :& Table M.MessageT
        :& MbTable MT.MessageTranslationT
    )
fullMessage lang =
  table
    @MessageReportT
    `innerJoin` table @M.MessageT
      `Esq.on` ( \(messageReport :& message) ->
                   messageReport ^. MessageReportMessageId ==. message ^. M.MessageTId
               )
    `leftJoin` table @MT.MessageTranslationT
      `Esq.on` ( \(_ :& message :& messageTranslation) ->
                   just (message ^. M.MessageTId) ==. messageTranslation ?. MT.MessageTranslationMessageId
                     &&. messageTranslation ?. MT.MessageTranslationLanguage ==. val (Just lang)
               )

findByDriverIdAndLanguage :: Transactionable m => Id P.Driver -> Language -> Maybe Int -> Maybe Int -> m [(MessageReport, Msg.RawMessage, Maybe MTD.MessageTranslation)]
findByDriverIdAndLanguage driverId language mbLimit mbOffset = do
  let limitVal = min (fromMaybe 10 mbLimit) 10
      offsetVal = fromMaybe 0 mbOffset
  Esq.findAll $ do
    (messageReport :& message :& mbMessageTranslation) <- from (fullMessage language)
    where_ $
      messageReport ^. MessageReportDriverId ==. val (toKey $ cast driverId)
    orderBy [desc $ messageReport ^. MessageReportCreatedAt]
    limit $ fromIntegral limitVal
    offset $ fromIntegral offsetVal
    return (messageReport, message, mbMessageTranslation)

findByDriverIdMessageIdAndLanguage :: Transactionable m => Id P.Driver -> Id Msg.Message -> Language -> m (Maybe (MessageReport, Msg.RawMessage, Maybe MTD.MessageTranslation))
findByDriverIdMessageIdAndLanguage driverId messageId language = do
  Esq.findOne $ do
    (messageReport :& message :& mbMessageTranslation) <- from (fullMessage language)
    where_ $
      messageReport ^. MessageReportTId ==. val (toKey (messageId, driverId))
    return (messageReport, message, mbMessageTranslation)

findByMessageIdAndDriverId :: Transactionable m => Id Msg.Message -> Id P.Driver -> m (Maybe MessageReport)
findByMessageIdAndDriverId messageId driverId =
  Esq.findOne $ do
    messageReport <- from $ table @MessageReportT
    where_ $
      messageReport ^. MessageReportTId ==. val (toKey (messageId, driverId))
    return messageReport

findByMessageIdAndStatusWithLimitAndOffset ::
  Transactionable m =>
  Maybe Int ->
  Maybe Int ->
  Id Msg.Message ->
  Maybe DeliveryStatus ->
  m [(MessageReport, P.Person)]
findByMessageIdAndStatusWithLimitAndOffset mbLimit mbOffset messageId mbDeliveryStatus = do
  findAll $ do
    (messageReport :& person) <-
      from $
        table @MessageReportT
          `innerJoin` table @PT.PersonT
            `Esq.on` ( \(messageReport :& person) ->
                         messageReport ^. MessageReportDriverId ==. person ^. PT.PersonTId
                     )
    where_ $
      messageReport ^. MessageReportMessageId ==. val (toKey messageId)
        &&. if isJust mbDeliveryStatus then messageReport ^. MessageReportDeliveryStatus ==. val (fromMaybe Success mbDeliveryStatus) else val True
    orderBy [desc $ messageReport ^. MessageReportCreatedAt]
    limit limitVal
    offset offsetVal
    return (messageReport, person)
  where
    limitVal = min (maybe 10 fromIntegral mbLimit) 20
    offsetVal = maybe 0 fromIntegral mbOffset

getMessageCountByStatus :: Transactionable m => Id Msg.Message -> DeliveryStatus -> m Int
getMessageCountByStatus messageId status =
  mkCount <$> do
    Esq.findAll $ do
      messageReport <- from $ table @MessageReportT
      where_ $
        messageReport ^. MessageReportMessageId ==. val (toKey messageId)
          &&. messageReport ^. MessageReportDeliveryStatus ==. val status
      return (countRows :: SqlExpr (Esq.Value Int))
  where
    mkCount [counter] = counter
    mkCount _ = 0

getMessageCountByReadStatus :: Transactionable m => Id Msg.Message -> m Int
getMessageCountByReadStatus messageId =
  mkCount <$> do
    Esq.findAll $ do
      messageReport <- from $ table @MessageReportT
      where_ $
        messageReport ^. MessageReportMessageId ==. val (toKey messageId)
          &&. messageReport ^. MessageReportReadStatus
      return (countRows :: SqlExpr (Esq.Value Int))
  where
    mkCount [counter] = counter
    mkCount _ = 0

updateSeenAndReplyByMessageIdAndDriverId :: Id Msg.Message -> Id P.Driver -> Bool -> Maybe Text -> SqlDB ()
updateSeenAndReplyByMessageIdAndDriverId messageId driverId readStatus reply = do
  now <- getCurrentTime
  Esq.update $ \mr -> do
    set
      mr
      [ MessageReportReadStatus =. val readStatus,
        MessageReportReply =. val reply,
        MessageReportUpdatedAt =. val now
      ]
    where_ $
      mr ^. MessageReportMessageId ==. val (toKey messageId)
        &&. mr ^. MessageReportDriverId ==. val (toKey $ cast driverId)

updateMessageLikeByMessageIdAndDriverIdAndReadStatus :: Id Msg.Message -> Id P.Driver -> SqlDB ()
updateMessageLikeByMessageIdAndDriverIdAndReadStatus messageId driverId = do
  now <- getCurrentTime
  Esq.update $ \mr -> do
    set
      mr
      [ MessageReportLikeStatus =. not_ (mr ^. MessageReportLikeStatus),
        MessageReportUpdatedAt =. val now
      ]
    where_ $
      mr ^. MessageReportMessageId ==. val (toKey messageId)
        &&. mr ^. MessageReportDriverId ==. val (toKey $ cast driverId)
        &&. mr ^. MessageReportReadStatus ==. val True

updateDeliveryStatusByMessageIdAndDriverId :: Id Msg.Message -> Id P.Driver -> DeliveryStatus -> SqlDB ()
updateDeliveryStatusByMessageIdAndDriverId messageId driverId deliveryStatus = do
  now <- getCurrentTime
  Esq.update $ \mr -> do
    set
      mr
      [ MessageReportDeliveryStatus =. val deliveryStatus,
        MessageReportUpdatedAt =. val now
      ]
    where_ $
      mr ^. MessageReportMessageId ==. val (toKey messageId)
        &&. mr ^. MessageReportDriverId ==. val (toKey $ cast driverId)

deleteByPersonId :: EsqDE.DeletedBy -> Id P.Person -> SqlDB ()
deleteByPersonId deletedBy personId =
  EsqDE.deleteP deletedBy $ do
    messageReports <- from $ table @MessageReportT
    where_ $ messageReports ^. MessageReportDriverId ==. val (toKey personId)
    pure messageReports
