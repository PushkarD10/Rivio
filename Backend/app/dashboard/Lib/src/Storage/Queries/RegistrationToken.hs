{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.RegistrationToken where

import Domain.Types.Merchant
import Domain.Types.Person
import Domain.Types.RegistrationToken
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.RegistrationToken

create :: RegistrationToken -> SqlDB ()
create = Esq.create

findByToken :: Transactionable m => RegToken -> m (Maybe RegistrationToken)
findByToken token =
  findOne $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $ regToken ^. RegistrationTokenToken ==. val token
    return regToken

findAllByPersonId :: Transactionable m => Id Person -> m [RegistrationToken]
findAllByPersonId personId =
  findAll $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $ regToken ^. RegistrationTokenPersonId ==. val (toKey personId)
    return regToken

findAllByPersonIdAndMerchantId :: Transactionable m => Id Person -> Id Merchant -> m [RegistrationToken]
findAllByPersonIdAndMerchantId personId merchantId =
  findAll $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $
      regToken ^. RegistrationTokenPersonId ==. val (toKey personId)
        &&. regToken ^. RegistrationTokenMerchantId ==. val (toKey merchantId)
    return regToken

deleteAllByPersonId :: Id Person -> SqlDB ()
deleteAllByPersonId personId =
  Esq.delete $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $ regToken ^. RegistrationTokenPersonId ==. val (toKey personId)

deleteAllByPersonIdAndMerchantId :: Id Person -> Id Merchant -> SqlDB ()
deleteAllByPersonIdAndMerchantId personId merchantId =
  Esq.delete $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $
      regToken ^. RegistrationTokenPersonId ==. val (toKey personId)
        &&. regToken ^. RegistrationTokenMerchantId ==. val (toKey merchantId)

deleteById :: Id RegistrationToken -> SqlDB ()
deleteById = Esq.deleteByKey @RegistrationTokenT
