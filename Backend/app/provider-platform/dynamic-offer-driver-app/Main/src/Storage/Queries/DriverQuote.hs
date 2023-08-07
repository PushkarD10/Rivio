{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverQuote where

import Data.Int (Int32)
import qualified Domain.Types.DriverQuote as Domain
import qualified Domain.Types.Estimate as DEstimate
import Domain.Types.Person
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.Vehicle.Variant as VehVar
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (addUTCTime, secondsToNominalDiffTime)
import Storage.Queries.FullEntityBuilders (buildFullDriverQuote)
import Storage.Tabular.DriverQuote
import qualified Storage.Tabular.FareParameters as Fare
import qualified Storage.Tabular.FareParameters.Instances as FareParamsT

-- import Domain.Action.UI.Cancel (CancelSearch(estimateId))
-- import Domain.Action.UI.Cancel (CancelSearch(estimateId))

create :: Domain.DriverQuote -> SqlDB ()
create dQuote = Esq.runTransaction $
  withFullEntity dQuote $ \(dQuoteT, (fareParams', fareParamsDetais)) -> do
    Esq.create' fareParams'
    case fareParamsDetais of
      FareParamsT.ProgressiveDetailsT fppdt -> Esq.create' fppdt
      FareParamsT.SlabDetailsT fpsdt -> Esq.create' fpsdt
    Esq.create' dQuoteT

baseDriverQuoteQuery ::
  From
    ( SqlExpr (Entity DriverQuoteT)
        :& SqlExpr (Entity Fare.FareParametersT)
    )
baseDriverQuoteQuery =
  table @DriverQuoteT
    `innerJoin` table @Fare.FareParametersT
      `Esq.on` ( \(rb :& farePars) ->
                   rb ^. DriverQuoteFareParametersId ==. farePars ^. Fare.FareParametersTId
               )

findById :: (Transactionable m) => Id Domain.DriverQuote -> m (Maybe Domain.DriverQuote)
findById dQuoteId = buildDType $ do
  res <- Esq.findOne' $ do
    (dQuote :& farePars) <-
      from baseDriverQuoteQuery
    where_ $ dQuote ^. DriverQuoteTId ==. val (toKey dQuoteId)
    pure (dQuote, farePars)
  join <$> mapM buildFullDriverQuote res

findActiveQuoteByDriverIdAndVehVarAndEstimateId :: (Transactionable m) => Id DEstimate.Estimate -> Id Person -> VehVar.Variant -> UTCTime -> m (Maybe Domain.DriverQuote)
findActiveQuoteByDriverIdAndVehVarAndEstimateId estimateId driverId vehicleVariant now = do
  buildDType $ do
    res <- Esq.findOne' $ do
      (dQuote :& farePars) <-
        from baseDriverQuoteQuery
      where_ $
        dQuote ^. DriverQuoteEstimateId ==. val (toKey estimateId)
          &&. dQuote ^. DriverQuoteDriverId ==. val (toKey driverId)
          &&. dQuote ^. DriverQuoteStatus ==. val Domain.Active
          &&. dQuote ^. DriverQuoteVehicleVariant ==. val vehicleVariant
          &&. dQuote ^. DriverQuoteValidTill >=. val now
      pure (dQuote, farePars)
    join <$> mapM buildFullDriverQuote res

setInactiveBySTId :: Id DST.SearchTry -> SqlDB ()
setInactiveBySTId searchTryId = Esq.update $ \p -> do
  set p [DriverQuoteStatus =. val Domain.Inactive]
  where_ $ p ^. DriverQuoteSearchTryId ==. val (toKey searchTryId)

setInactiveBySRId :: Id DSR.SearchRequest -> SqlDB ()
setInactiveBySRId searchReqId = Esq.update $ \p -> do
  set p [DriverQuoteStatus =. val Domain.Inactive]
  where_ $ p ^. DriverQuoteRequestId ==. val (toKey searchReqId)

findActiveQuotesByDriverId :: (Transactionable m, MonadTime m) => Id Person -> Seconds -> m [Domain.DriverQuote]
findActiveQuotesByDriverId driverId driverUnlockDelay = do
  now <- getCurrentTime
  buildDType $ do
    let delayToAvoidRaces = secondsToNominalDiffTime . negate $ driverUnlockDelay
    res <- Esq.findAll' $ do
      (dQuote :& farePars) <-
        from baseDriverQuoteQuery
      where_ $
        dQuote ^. DriverQuoteDriverId ==. val (toKey driverId)
          &&. dQuote ^. DriverQuoteStatus ==. val Domain.Active
          &&. dQuote ^. DriverQuoteValidTill >. val (addUTCTime delayToAvoidRaces now)
      pure (dQuote, farePars)
    catMaybes <$> mapM buildFullDriverQuote res

findAllBySTId :: (Transactionable m) => Id DST.SearchTry -> m [Domain.DriverQuote]
findAllBySTId searchTryId = do
  buildDType $ do
    res <- Esq.findAll' $ do
      (dQuote :& farePars) <-
        from baseDriverQuoteQuery
      where_ $
        dQuote ^. DriverQuoteStatus ==. val Domain.Active
          &&. dQuote ^. DriverQuoteSearchTryId ==. val (toKey searchTryId)
      pure (dQuote, farePars)
    catMaybes <$> mapM buildFullDriverQuote res

countAllBySTId :: (Transactionable m) => Id DST.SearchTry -> m Int32
countAllBySTId searchTryId = do
  fmap (fromMaybe 0) $
    Esq.findOne $ do
      dQuote <- from $ table @DriverQuoteT
      where_ $
        dQuote ^. DriverQuoteStatus ==. val Domain.Active
          &&. dQuote ^. DriverQuoteSearchTryId ==. val (toKey searchTryId)
      pure (countRows @Int32)

deleteByDriverId :: Id Person -> SqlDB ()
deleteByDriverId personId =
  Esq.delete $ do
    driverQuotes <- from $ table @DriverQuoteT
    where_ $ driverQuotes ^. DriverQuoteDriverId ==. val (toKey personId)

findDriverQuoteBySTId :: (Transactionable m) => Id DST.SearchTry -> DTypeBuilder m (Maybe DriverQuoteT)
findDriverQuoteBySTId searchTryId = Esq.findOne' $ do
  driverQuote <- from $ table @DriverQuoteT
  where_ $ driverQuote ^. DriverQuoteSearchTryId ==. val (toKey searchTryId)
  pure driverQuote

setInactiveAllDQByEstId :: Id DEstimate.Estimate -> UTCTime -> SqlDB ()
setInactiveAllDQByEstId estimateId now = do
  Esq.update $ \p -> do
    set
      p
      [ DriverQuoteStatus =. val Domain.Inactive,
        DriverQuoteUpdatedAt =. val now
      ]
    where_ $
      p ^. DriverQuoteEstimateId ==. val (toKey estimateId)
        &&. p ^. DriverQuoteStatus ==. val Domain.Active
        &&. p ^. DriverQuoteValidTill >=. val now
