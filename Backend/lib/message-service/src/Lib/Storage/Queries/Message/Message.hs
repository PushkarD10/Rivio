{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Lib.Storage.Queries.Message.Message where

import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Lib.Domain.Types.Message.Message
import Lib.Storage.Tabular.Message.Instances ()
import Lib.Storage.Tabular.Message.Message

create :: Message -> SqlDB ()
create msg = Esq.runTransaction $
  withFullEntity msg $ \(message, messageTranslations) -> do
    Esq.create' message
    traverse_ Esq.create' messageTranslations

findById :: Transactionable m => Id Message -> m (Maybe RawMessage)
findById = Esq.findById

findAllWithLimitOffset ::
  Transactionable m =>
  Maybe Int ->
  Maybe Int ->
  Id Merchant ->
  m [RawMessage]
findAllWithLimitOffset mbLimit mbOffset merchantId = do
  findAll $ do
    message <-
      from $
        table @MessageT
    where_ $
      message ^. MessageMerchantId ==. val merchantId.getId
    orderBy [desc $ message ^. MessageCreatedAt]
    limit limitVal
    offset offsetVal
    return message
  where
    limitVal = min (maybe 10 fromIntegral mbLimit) 10
    offsetVal = maybe 0 fromIntegral mbOffset

updateMessageLikeCount :: Id Message -> Int -> SqlDB ()
updateMessageLikeCount messageId value = do
  Esq.update $ \msg -> do
    set msg [MessageLikeCount =. (msg ^. MessageLikeCount) +. val value]
    where_ $ msg ^. MessageId ==. val (getId messageId)

updateMessageViewCount :: Id Message -> Int -> SqlDB ()
updateMessageViewCount messageId value = do
  Esq.update $ \msg -> do
    set msg [MessageViewCount =. (msg ^. MessageViewCount) +. val value]
    where_ $ msg ^. MessageId ==. val (getId messageId)
