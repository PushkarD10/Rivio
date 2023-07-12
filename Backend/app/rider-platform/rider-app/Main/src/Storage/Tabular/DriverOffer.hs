{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverOffer where

import qualified Domain.Types.DriverOffer as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import qualified Storage.Tabular.Estimate as TEstimate
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Merchant.MerchantOperatingCity (MerchantOperatingCityTId)

derivePersistField "Domain.DriverOfferStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverOfferT sql=driver_offer
      id Text
      estimateId TEstimate.EstimateTId
      merchantId MerchantTId Maybe
      merchantOperatingCityId MerchantOperatingCityTId Maybe
      driverName Text
      durationToPickup Int
      distanceToPickup HighPrecMeters
      validTill UTCTime
      bppQuoteId Text
      rating Centesimal Maybe
      status Domain.DriverOfferStatus
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey DriverOfferT where
  type DomainKey DriverOfferT = Id Domain.DriverOffer
  fromKey (DriverOfferTKey _id) = Id _id
  toKey (Id id) = DriverOfferTKey id

instance FromTType DriverOfferT Domain.DriverOffer where
  fromTType DriverOfferT {..} = do
    return $
      Domain.DriverOffer
        { id = Id id,
          bppQuoteId = Id bppQuoteId,
          merchantId = fromKey <$> merchantId,
          merchantOperatingCityId = fromKey <$> merchantOperatingCityId,
          estimateId = fromKey estimateId,
          ..
        }

instance ToTType DriverOfferT Domain.DriverOffer where
  toTType Domain.DriverOffer {..} = do
    DriverOfferT
      { id = getId id,
        bppQuoteId = bppQuoteId.getId,
        merchantId = toKey <$> merchantId,
        merchantOperatingCityId = toKey <$> merchantOperatingCityId,
        estimateId = toKey estimateId,
        ..
      }
