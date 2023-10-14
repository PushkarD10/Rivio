{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Vehicle where

import Data.Either (fromRight)
import qualified Database.Beam as B
import Domain.Types.Merchant
import Domain.Types.Person
import Domain.Types.Vehicle
import qualified Domain.Types.Vehicle.Variant as Variant
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Vehicle as BeamV

create :: MonadFlow m => Vehicle -> m ()
create = createWithKV

upsert :: MonadFlow m => Vehicle -> m ()
upsert a@Vehicle {..} = do
  res <- findOneWithKV [Se.Is BeamV.registrationNo $ Se.Eq a.registrationNo]
  if isJust res
    then
      updateOneWithKV
        [ Se.Set BeamV.capacity capacity,
          Se.Set BeamV.category category,
          Se.Set BeamV.make make,
          Se.Set BeamV.model model,
          Se.Set BeamV.size size,
          Se.Set BeamV.variant variant,
          Se.Set BeamV.color color,
          Se.Set BeamV.energyType energyType,
          Se.Set BeamV.registrationCategory registrationCategory,
          Se.Set BeamV.fleetOwnerId fleetOwnerId,
          Se.Set BeamV.updatedAt updatedAt
        ]
        [Se.Is BeamV.registrationNo (Se.Eq a.registrationNo)]
    else createWithKV a

findById :: (MonadFlow m) => Id Person -> m (Maybe Vehicle)
findById (Id driverId) = findOneWithKV [Se.Is BeamV.driverId $ Se.Eq driverId]

findByFleetOwnerId :: (MonadFlow m) => Text -> m [Vehicle]
findByFleetOwnerId fleetOwnerId = findAllWithKV [Se.Is BeamV.fleetOwnerId $ Se.Eq (Just fleetOwnerId)]

findByVehicleNoAndFleetOwnerId :: (MonadFlow m) => Text -> Text -> m (Maybe Vehicle)
findByVehicleNoAndFleetOwnerId registrationNo fleetOwnerId = findOneWithKV [Se.And [Se.Is BeamV.registrationNo $ Se.Eq registrationNo, Se.Is BeamV.fleetOwnerId $ Se.Eq (Just fleetOwnerId)]]

updateVehicleRec :: MonadFlow m => Vehicle -> m ()
updateVehicleRec vehicle = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamV.capacity vehicle.capacity,
      Se.Set BeamV.category vehicle.category,
      Se.Set BeamV.make vehicle.make,
      Se.Set BeamV.vehicleName vehicle.vehicleName,
      Se.Set BeamV.model vehicle.model,
      Se.Set BeamV.size vehicle.size,
      Se.Set BeamV.variant vehicle.variant,
      Se.Set BeamV.color vehicle.color,
      Se.Set BeamV.energyType vehicle.energyType,
      Se.Set BeamV.registrationNo vehicle.registrationNo,
      Se.Set BeamV.registrationCategory vehicle.registrationCategory,
      Se.Set BeamV.updatedAt now
    ]
    [Se.Is BeamV.driverId (Se.Eq $ getId vehicle.driverId)]

updateVehicleName :: (MonadFlow m) => Maybe Text -> Id Person -> m ()
updateVehicleName vehicleName (Id driverId) = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set BeamV.updatedAt now, Se.Set BeamV.vehicleName vehicleName]
    [Se.Is BeamV.driverId (Se.Eq driverId)]

updateVehicleColorAndVariant :: (MonadFlow m) => Maybe Text -> Maybe Variant -> Text -> UTCTime -> m ()
updateVehicleColorAndVariant mbColor mbVariant rcNo now = do
  updateWithKV
    ( [Se.Set BeamV.updatedAt now]
        <> [Se.Set BeamV.color (fromJust mbColor) | isJust mbColor]
        <> [Se.Set BeamV.variant (fromJust mbVariant) | isJust mbVariant]
    )
    [Se.Is BeamV.registrationNo (Se.Eq rcNo)]

deleteById :: MonadFlow m => Id Person -> m ()
deleteById (Id driverId) = deleteWithKV [Se.Is BeamV.driverId (Se.Eq driverId)]

findByAnyOf :: MonadFlow m => Maybe Text -> Maybe (Id Person) -> m (Maybe Vehicle)
findByAnyOf registrationNoM vehicleIdM =
  findOneWithKV
    [ Se.And
        ( []
            <> if isJust vehicleIdM
              then [Se.Is BeamV.driverId $ Se.Eq (getId (fromJust vehicleIdM))]
              else
                []
                  <> ([Se.Is BeamV.registrationNo $ Se.Eq (fromJust registrationNoM) | isJust registrationNoM])
        )
    ]

findAllByVariantRegNumMerchantId :: MonadFlow m => Maybe Variant.Variant -> Maybe Text -> Integer -> Integer -> Id Merchant -> m [Vehicle]
findAllByVariantRegNumMerchantId variantM mbRegNum limitVal offsetVal (Id merchantId') = do
  dbConf <- getMasterBeamConfig
  vehicles <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limitVal $
            B.offset_ offsetVal $
              B.orderBy_ (\vehicle -> B.desc_ vehicle.createdAt) $
                B.filter_'
                  ( \BeamV.VehicleT {..} ->
                      merchantId B.==?. B.val_ merchantId'
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\variant' -> B.sqlBool_ (variant B.==. B.val_ variant')) variantM
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\regNoStr -> B.sqlBool_ (registrationNo `B.like_` B.val_ ("%" <> regNoStr <> "%"))) mbRegNum
                  )
                  $ B.all_ (BeamCommon.vehicle BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] vehicles)

findByRegistrationNo :: MonadFlow m => Text -> m (Maybe Vehicle)
findByRegistrationNo registrationNo = findOneWithKV [Se.Is BeamV.registrationNo $ Se.Eq registrationNo]

findAllByFleetOwnerId :: MonadFlow m => Text -> m [Vehicle]
findAllByFleetOwnerId fleetOwnerId = findAllWithKV [Se.Is BeamV.fleetOwnerId $ Se.Eq $ Just fleetOwnerId]

instance FromTType' BeamV.Vehicle Vehicle where
  fromTType' BeamV.VehicleT {..} = do
    pure $
      Just
        Vehicle
          { driverId = Id driverId,
            merchantId = Id merchantId,
            variant = variant,
            model = model,
            color = color,
            vehicleName = vehicleName,
            registrationNo = registrationNo,
            capacity = capacity,
            category = category,
            make = make,
            size = size,
            energyType = energyType,
            registrationCategory = registrationCategory,
            vehicleClass = vehicleClass,
            fleetOwnerId = fleetOwnerId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamV.Vehicle Vehicle where
  toTType' Vehicle {..} = do
    BeamV.VehicleT
      { BeamV.driverId = getId driverId,
        BeamV.merchantId = getId merchantId,
        BeamV.variant = variant,
        BeamV.model = model,
        BeamV.color = color,
        BeamV.vehicleName = vehicleName,
        BeamV.registrationNo = registrationNo,
        BeamV.capacity = capacity,
        BeamV.category = category,
        BeamV.make = make,
        BeamV.size = size,
        BeamV.energyType = energyType,
        BeamV.registrationCategory = registrationCategory,
        BeamV.vehicleClass = vehicleClass,
        BeamV.fleetOwnerId = fleetOwnerId,
        BeamV.createdAt = createdAt,
        BeamV.updatedAt = updatedAt
      }
