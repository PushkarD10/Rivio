{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.FareParameters where

import Domain.Types.FareParameters
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Queries.FullEntityBuilders (buildFullFareParameters)
import Storage.Tabular.FareParameters (EntityField (FareParametersId), FareParametersT)
import Storage.Tabular.FareParameters.Instances

create :: FareParameters -> SqlDB ()
create fareParams =
  withFullEntity fareParams $ \(fareParams', fareParamsDetais) -> do
    Esq.create' fareParams'
    case fareParamsDetais of
      ProgressiveDetailsT fppdt -> Esq.create' fppdt
      SlabDetailsT fpsdt -> Esq.create' fpsdt

findById :: Transactionable m => Id FareParameters -> m (Maybe FareParameters)
findById fareParametersId = buildDType $ do
  res <- Esq.findById' @FareParametersT (toKey fareParametersId)
  join <$> mapM buildFullFareParameters res

findAllIn :: Transactionable m => [Id FareParameters] -> m [FareParameters]
findAllIn fareParamIds =
  buildDType $ do
    res <- Esq.findAll' $ do
      fareParamFile <- from $ table @FareParametersT
      where_ $
        fareParamFile ^. FareParametersId `in_` valList (map getId fareParamIds)
      pure fareParamFile
    catMaybes <$> mapM buildFullFareParameters res
