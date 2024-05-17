{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.EstimateBreakup where

import Domain.Types.Estimate
import qualified Domain.Types.Estimate as DEB
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.EstimateBreakup as BeamEB

create :: KvDbFlow m r => EstimateBreakup -> m ()
create = createWithKV

findAllByEstimateIdT :: KvDbFlow m r => Id Estimate -> m [EstimateBreakup]
findAllByEstimateIdT (Id estimateId) = findAllWithKVAndConditionalDB [Se.Is BeamEB.estimateId $ Se.Eq estimateId] Nothing

instance FromTType' BeamEB.EstimateBreakup EstimateBreakup where
  fromTType' BeamEB.EstimateBreakupT {..} = do
    let price =
          DEB.EstimateBreakupPrice
            { value = mkPrice (Just priceCurrency) priceValue
            }
    pure $
      Just
        EstimateBreakup
          { id = Id id,
            estimateId = Id estimateId,
            title = title,
            price = price
          }

instance ToTType' BeamEB.EstimateBreakup EstimateBreakup where
  toTType' EstimateBreakup {..} = do
    BeamEB.EstimateBreakupT
      { BeamEB.id = getId id,
        BeamEB.estimateId = getId estimateId,
        BeamEB.title = title,
        BeamEB.priceCurrency = price.value.currency,
        BeamEB.priceValue = price.value.amount
      }
