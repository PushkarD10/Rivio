{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverFee where

import Domain.Types.DriverFee
import qualified Domain.Types.DriverFee as Domain
import Domain.Types.Person
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney, Money)
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import Kernel.Types.Time
import qualified Sequelize as Se
import qualified Storage.Beam.DriverFee as BeamDF

create :: (L.MonadFlow m, Log m) => DriverFee -> m ()
create = createWithKV

findById :: (L.MonadFlow m, Log m) => Id DriverFee -> m (Maybe DriverFee)
findById (Id driverFeeId) = findOneWithKV [Se.Is BeamDF.id $ Se.Eq driverFeeId]

findPendingFeesByDriverFeeId :: (L.MonadFlow m, Log m) => Id DriverFee -> m (Maybe DriverFee)
findPendingFeesByDriverFeeId (Id driverFeeId) =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDF.id $ Se.Eq driverFeeId,
          Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE]
        ]
    ]

findPendingFeesByDriverId :: (L.MonadFlow m, Log m) => Id Driver -> m (Maybe DriverFee)
findPendingFeesByDriverId (Id driverId) =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE],
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
    ]

findLatestFeeByDriverId :: (L.MonadFlow m, Log m) => Id Driver -> m (Maybe DriverFee)
findLatestFeeByDriverId (Id driverId) =
  findAllWithOptionsKV
    [Se.Is BeamDF.driverId $ Se.Eq driverId]
    (Se.Desc BeamDF.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

findLatestRegisterationFeeByDriverId :: (L.MonadFlow m, Log m) => Id Driver -> m (Maybe DriverFee)
findLatestRegisterationFeeByDriverId (Id driverId) =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.driverId (Se.Eq driverId),
          Se.Is BeamDF.feeType (Se.Eq MANDATE_REGISTRATION),
          Se.Is BeamDF.status (Se.Eq PAYMENT_PENDING)
        ]
    ]
    (Se.Desc BeamDF.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

findOldestFeeByStatus :: (L.MonadFlow m, Log m) => Id Driver -> DriverFeeStatus -> m (Maybe DriverFee)
findOldestFeeByStatus (Id driverId) status =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.status $ Se.Eq status
        ]
    ]
    (Se.Asc BeamDF.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

findFeesInRangeWithStatus :: (L.MonadFlow m, Log m) => UTCTime -> UTCTime -> DriverFeeStatus -> m [DriverFee]
findFeesInRangeWithStatus startTime endTime status =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.status $ Se.Eq status,
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
    ]

findWindowsWithStatus :: (L.MonadFlow m, Log m) => Id Person -> UTCTime -> UTCTime -> Maybe DriverFeeStatus -> Int -> Int -> m [DriverFee]
findWindowsWithStatus (Id driverId) startTime endTime mbStatus limitVal offsetVal =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
          <> [Se.Is BeamDF.status $ Se.Eq $ fromJust mbStatus | isJust mbStatus]
    ]
    (Se.Desc BeamDF.createdAt)
    (Just limitVal)
    (Just offsetVal)

findOngoingAfterEndTime :: (L.MonadFlow m, Log m) => Id Person -> UTCTime -> m (Maybe DriverFee)
findOngoingAfterEndTime (Id driverId) now =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.status $ Se.Eq ONGOING,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq now,
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
    ]

findUnpaidAfterPayBy :: (L.MonadFlow m, Log m) => Id Person -> UTCTime -> m (Maybe DriverFee)
findUnpaidAfterPayBy (Id driverId) now =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE],
          Se.Is BeamDF.payBy $ Se.LessThanOrEq now,
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
    ]

updateFee :: (L.MonadFlow m, Log m) => Id DriverFee -> Maybe Money -> Money -> Money -> HighPrecMoney -> HighPrecMoney -> UTCTime -> m ()
updateFee driverFeeId mbFare govtCharges platformFee cgst sgst now = do
  driverFeeObject <- findById driverFeeId
  case driverFeeObject of
    Just df -> do
      let govtCharges' = df.govtCharges
      let platformFee' = df.platformFee.fee
      let cgst' = df.platformFee.cgst
      let sgst' = df.platformFee.sgst
      let totalEarnings = df.totalEarnings
      let numRides = df.numRides
      let fare = fromMaybe 0 mbFare
      updateOneWithKV
        [ Se.Set BeamDF.govtCharges $ govtCharges' + govtCharges,
          Se.Set BeamDF.platformFee $ platformFee' + platformFee,
          Se.Set BeamDF.cgst $ cgst' + cgst,
          Se.Set BeamDF.sgst $ sgst' + sgst,
          Se.Set BeamDF.status ONGOING,
          Se.Set BeamDF.totalEarnings $ totalEarnings + fare,
          Se.Set BeamDF.numRides $ numRides + 1, -- in the api, num_rides needed without cost contribution?
          Se.Set BeamDF.updatedAt now
        ]
        [Se.Is BeamDF.id (Se.Eq (getId driverFeeId))]
    Nothing -> pure ()

updateStatusByIds :: (L.MonadFlow m, Log m) => DriverFeeStatus -> [Id DriverFee] -> UTCTime -> m ()
updateStatusByIds status driverFeeIds now =
  updateWithKV
    [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now]
    [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]

findAllPendingAndDueDriverFeeByDriverId :: (L.MonadFlow m, Log m) => Id Person -> m [DriverFee]
findAllPendingAndDueDriverFeeByDriverId (Id driverId) = findAllWithKV [Se.And [Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE, Se.Is BeamDF.status $ Se.Eq PAYMENT_OVERDUE, Se.Is BeamDF.driverId $ Se.Eq driverId]]

findLatestByFeeTypeAndStatus :: (L.MonadFlow m, Log m) => Domain.FeeType -> [Domain.DriverFeeStatus] -> Id Person -> m (Maybe DriverFee)
findLatestByFeeTypeAndStatus feeType status driverId = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.feeType $ Se.Eq feeType,
          Se.Is BeamDF.status $ Se.In status,
          Se.Is BeamDF.driverId $ Se.Eq driverId.getId
        ]
    ]
    (Se.Desc BeamDF.updatedAt)
    (Just 1)
    Nothing
    <&> listToMaybe

updateStatus :: (L.MonadFlow m, Log m) => DriverFeeStatus -> Id DriverFee -> UTCTime -> m ()
updateStatus status (Id driverFeeId) now = do
  updateOneWithKV
    [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now]
    [Se.Is BeamDF.id (Se.Eq driverFeeId)]

updateRegisterationFeeStatusByDriverId :: (L.MonadFlow m, Log m, MonadTime m) => DriverFeeStatus -> Id Person -> m ()
updateRegisterationFeeStatusByDriverId status (Id driverId) = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now]
    [Se.And [Se.Is BeamDF.driverId (Se.Eq driverId), Se.Is BeamDF.feeType (Se.Eq MANDATE_REGISTRATION), Se.Is BeamDF.status (Se.Eq PAYMENT_PENDING)]]

updateCollectedPaymentStatus :: (L.MonadFlow m, Log m) => DriverFeeStatus -> Maybe Text -> Id DriverFee -> UTCTime -> m ()
updateCollectedPaymentStatus status collectorId (Id driverFeeId) now = do
  updateOneWithKV
    [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now, Se.Set BeamDF.collectedBy collectorId]
    [Se.Is BeamDF.id (Se.Eq driverFeeId)]

instance FromTType' BeamDF.DriverFee DriverFee where
  fromTType' BeamDF.DriverFeeT {..} = do
    pure $
      Just
        DriverFee
          { id = Id id,
            merchantId = Id merchantId,
            driverId = Id driverId,
            govtCharges = govtCharges,
            platformFee = Domain.PlatformFee platformFee cgst sgst,
            numRides = numRides,
            payBy = payBy,
            totalEarnings = totalEarnings,
            startTime = startTime,
            endTime = endTime,
            status = status,
            feeType = feeType,
            collectedBy = collectedBy,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamDF.DriverFee DriverFee where
  toTType' DriverFee {..} = do
    BeamDF.DriverFeeT
      { BeamDF.id = getId id,
        BeamDF.merchantId = getId merchantId,
        BeamDF.driverId = getId driverId,
        BeamDF.govtCharges = govtCharges,
        BeamDF.platformFee = platformFee.fee,
        BeamDF.cgst = platformFee.cgst,
        BeamDF.sgst = platformFee.sgst,
        BeamDF.numRides = numRides,
        BeamDF.payBy = payBy,
        BeamDF.totalEarnings = totalEarnings,
        BeamDF.startTime = startTime,
        BeamDF.endTime = endTime,
        BeamDF.status = status,
        BeamDF.feeType = feeType,
        BeamDF.collectedBy = collectedBy,
        BeamDF.createdAt = createdAt,
        BeamDF.updatedAt = updatedAt
      }
