{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.DriverLocation where

import Domain.Types.DriverLocation
import Domain.Types.Merchant
import Domain.Types.Person
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common (MonadTime (getCurrentTime))
import Kernel.Types.Id
import Storage.Tabular.DriverLocation

create :: Id Person -> LatLong -> UTCTime -> Id Merchant -> SqlDB ()
create drLocationId latLong coordinatesUpdatedAt merchantId = do
  now <- getCurrentTime
  -- Tricky query to be able to insert meaningful Point
  Esq.insertSelect $
    return $
      DriverLocationT
        <# val (toKey drLocationId)
        <#> val latLong.lat
        <#> val latLong.lon
        <#> Esq.getPoint (val latLong.lat, val latLong.lon)
        <#> val coordinatesUpdatedAt
        <#> val now
        <#> val now
        <#> val (toKey merchantId)

findById ::
  Transactionable m =>
  Id Person ->
  m (Maybe DriverLocation)
findById = Esq.findById

upsertGpsCoord :: Id Person -> LatLong -> UTCTime -> Id Merchant -> SqlDB DriverLocation
upsertGpsCoord drLocationId latLong calculationTime merchantId = do
  mbDrLoc <- Esq.findById @DriverLocation @DriverLocationT drLocationId
  now <- getCurrentTime
  case mbDrLoc of
    Nothing -> do
      Storage.Queries.DriverLocation.create drLocationId latLong calculationTime merchantId
      return $ DriverLocation drLocationId latLong.lat latLong.lon calculationTime now now merchantId
    Just oldLocation -> do
      Esq.update $ \tbl -> do
        set
          tbl
          [ DriverLocationLat =. val latLong.lat,
            DriverLocationLon =. val latLong.lon,
            DriverLocationCoordinatesCalculatedAt =. val calculationTime,
            DriverLocationPoint =. Esq.getPoint (val latLong.lat, val latLong.lon),
            DriverLocationUpdatedAt =. val now
          ]
        where_ $ tbl ^. DriverLocationTId ==. val (toKey $ cast drLocationId)
      return $ oldLocation{lat = latLong.lat, lon = latLong.lon, coordinatesCalculatedAt = calculationTime, updatedAt = now}

deleteById :: Id Person -> SqlDB ()
deleteById = deleteByKey @DriverLocationT
