{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Estimate where

import Data.Tuple.Extra
import Domain.Types.Estimate
import Domain.Types.SearchRequest
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id (Id (getId))
import Storage.Queries.FullEntityBuilders (buildFullEstimate)
import Storage.Tabular.Estimate
import Storage.Tabular.TripTerms

-- order of creating entites make sense!
create :: Estimate -> SqlDB ()
create estimate =
  Esq.withFullEntity estimate $ \(estimateT, estimateBreakupT, mbTripTermsT) -> do
    traverse_ Esq.create' mbTripTermsT
    Esq.create' estimateT
    traverse_ Esq.create' estimateBreakupT

createMany :: [Estimate] -> SqlDB ()
createMany estimates =
  Esq.withFullEntities estimates $ \list -> do
    let estimateTs = map fst3 list
        estimateBreakupT = map snd3 list
        tripTermsTs = mapMaybe thd3 list
    Esq.createMany' tripTermsTs
    Esq.createMany' estimateTs
    traverse_ Esq.createMany' estimateBreakupT

fullEstimateTable ::
  From
    ( Table EstimateT
        :& Esq.MbTable TripTermsT
    )
fullEstimateTable =
  table @EstimateT
    `leftJoin` table @TripTermsT
      `Esq.on` ( \(estimate :& mbTripTerms) ->
                   estimate ^. EstimateTripTermsId ==. mbTripTerms ?. TripTermsTId
               )

findById :: Transactionable m => Id Estimate -> m (Maybe Estimate)
findById estimateId = Esq.buildDType $ do
  mbFullEstimateT <- Esq.findOne' $ do
    (estimate :& mbTripTerms) <- from fullEstimateTable
    where_ $ estimate ^. EstimateTId ==. val (toKey estimateId)
    pure (estimate, mbTripTerms)
  mapM buildFullEstimate mbFullEstimateT

findAllBySRId :: Transactionable m => Id SearchRequest -> m [Estimate]
findAllBySRId searchRequestId = Esq.buildDType $ do
  fullEstimateTs <- Esq.findAll' $ do
    (estimate :& mbTripTerms) <- from fullEstimateTable
    where_ $ estimate ^. EstimateRequestId ==. val (toKey searchRequestId)
    pure (estimate, mbTripTerms)
  mapM buildFullEstimate fullEstimateTs

findByBPPEstimateId :: Transactionable m => Id BPPEstimate -> m (Maybe Estimate)
findByBPPEstimateId bppEstimateId_ = Esq.buildDType $ do
  mbFullEstimateT <- Esq.findOne' $ do
    (estimate :& mbTripTerms) <- from fullEstimateTable
    where_ $ estimate ^. EstimateBppEstimateId ==. val (getId bppEstimateId_)
    pure (estimate, mbTripTerms)
  mapM buildFullEstimate mbFullEstimateT

updateStatus ::
  Id Estimate ->
  EstimateStatus ->
  SqlDB ()
updateStatus estimateId status_ = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ EstimateUpdatedAt =. val now,
        EstimateStatus =. val status_
      ]
    where_ $ tbl ^. EstimateId ==. val (getId estimateId)

getStatus ::
  (Transactionable m) =>
  Id Estimate ->
  m (Maybe EstimateStatus)
getStatus estimateId = do
  findOne $ do
    estimateT <- from $ table @EstimateT
    where_ $
      estimateT ^. EstimateId ==. val (getId estimateId)
    return $ estimateT ^. EstimateStatus

updateStatusbyRequestId ::
  Id SearchRequest ->
  EstimateStatus ->
  SqlDB ()
updateStatusbyRequestId searchId status_ = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ EstimateUpdatedAt =. val now,
        EstimateStatus =. val status_
      ]
    where_ $ tbl ^. EstimateRequestId ==. val (toKey searchId)
