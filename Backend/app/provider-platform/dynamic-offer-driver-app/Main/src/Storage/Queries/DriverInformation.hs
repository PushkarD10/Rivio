{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverInformation where

import Control.Applicative (liftA2)
import Domain.Types.DriverInformation
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person as Person
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Esqueleto.DeletedEntity as EsqDE
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.DriverInformation
import Storage.Tabular.DriverLocation
import Storage.Tabular.Person

create :: DriverInformation -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id Person.Driver -> m (Maybe DriverInformation)
findById = Esq.findById . cast

fetchAllByIds :: Transactionable m => Id Merchant -> [Id Driver] -> m [DriverInformation]
fetchAllByIds merchantId driversIds = Esq.findAll $ do
  (driverInformation :& person) <-
    from $
      table @DriverInformationT
        `innerJoin` table @PersonT
          `Esq.on` ( \(driverInformation :& person) ->
                       driverInformation ^. DriverInformationDriverId ==. person ^. PersonTId
                   )
  where_ $
    driverInformation ^. DriverInformationDriverId `in_` valList personsKeys
      &&. (person ^. PersonMerchantId ==. (val . toKey $ merchantId))
  return driverInformation
  where
    personsKeys = toKey . cast <$> driversIds

fetchAllAvailableByIds :: Transactionable m => [Id Person.Driver] -> m [DriverInformation]
fetchAllAvailableByIds driversIds = Esq.findAll $ do
  driverInformation <- from $ table @DriverInformationT
  where_ $
    driverInformation ^. DriverInformationDriverId `in_` valList personsKeys
      &&. driverInformation ^. DriverInformationActive
      &&. not_ (driverInformation ^. DriverInformationOnRide)
  return driverInformation
  where
    personsKeys = toKey . cast <$> driversIds

updateActivity :: Id Person.Driver -> Bool -> Maybe DriverMode -> SqlDB ()
updateActivity driverId isActive mode = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverInformationActive =. val isActive,
        DriverInformationMode =. val mode,
        DriverInformationUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey $ cast driverId)

updateEnabledState :: Id Driver -> Bool -> SqlDB ()
updateEnabledState driverId isEnabled = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      $ [ DriverInformationEnabled =. val isEnabled,
          DriverInformationUpdatedAt =. val now
        ]
        <> [DriverInformationLastEnabledOn =. val (Just now) | isEnabled]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey $ cast driverId)

updateEnabledVerifiedState :: Id Person.Driver -> Bool -> Bool -> SqlDB ()
updateEnabledVerifiedState driverId isEnabled isVerified = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      $ [ DriverInformationEnabled =. val isEnabled,
          DriverInformationVerified =. val isVerified,
          DriverInformationUpdatedAt =. val now
        ]
        <> [DriverInformationLastEnabledOn =. val (Just now) | isEnabled]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey $ cast driverId)

updateBlockedState :: Id Person.Driver -> Bool -> SqlDB ()
updateBlockedState driverId isBlocked = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverInformationBlocked =. val isBlocked,
        DriverInformationUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey $ cast driverId)

verifyAndEnableDriver :: Id Person -> SqlDB ()
verifyAndEnableDriver driverId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverInformationEnabled =. val True,
        DriverInformationVerified =. val True,
        DriverInformationUpdatedAt =. val now,
        DriverInformationLastEnabledOn =. val (Just now)
      ]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey driverId)

updateEnabledStateReturningIds :: EsqDBFlow m r => Id Merchant -> [Id Driver] -> Bool -> m [Id Driver]
updateEnabledStateReturningIds merchantId driverIds isEnabled =
  Esq.runTransaction $ do
    present <- fmap (cast . (.driverId)) <$> fetchAllByIds merchantId driverIds
    updateEnabledStateForIds present
    pure present
  where
    updateEnabledStateForIds :: [Id Driver] -> SqlDB ()
    updateEnabledStateForIds present = do
      now <- getCurrentTime
      Esq.update $ \tbl -> do
        set
          tbl
          $ [ DriverInformationEnabled =. val isEnabled,
              DriverInformationUpdatedAt =. val now
            ]
            <> [DriverInformationLastEnabledOn =. val (Just now) | isEnabled]
        where_ $ tbl ^. DriverInformationDriverId `in_` valList (map (toKey . cast) present)

updateOnRide ::
  Id Person.Driver ->
  Bool ->
  SqlDB ()
updateOnRide driverId onRide = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverInformationOnRide =. val onRide,
        DriverInformationUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey $ cast driverId)

updateNotOnRideMultiple :: [Id Person.Driver] -> SqlDB ()
updateNotOnRideMultiple driverIds = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverInformationOnRide =. val False,
        DriverInformationUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverInformationDriverId `in_` valList (toKey . cast <$> driverIds)

deleteById :: EsqDE.DeletedBy -> Id Driver -> SqlDB ()
deleteById deletedBy = EsqDE.deleteByIdP @DriverInformationT deletedBy . cast @Driver @Person

findAllWithLimitOffsetByMerchantId ::
  Transactionable m =>
  Maybe Text ->
  Maybe DbHash ->
  Maybe Integer ->
  Maybe Integer ->
  Id Merchant ->
  m [(Person, DriverInformation)]
findAllWithLimitOffsetByMerchantId mbSearchString mbSearchStrDBHash mbLimit mbOffset merchantId = do
  findAll $ do
    (person :& driverInformation) <-
      from $
        table @PersonT
          `innerJoin` table @DriverInformationT
            `Esq.on` ( \(person :& driverInformation) ->
                         driverInformation ^. DriverInformationDriverId ==. person ^. PersonTId
                     )
    where_ $
      person ^. PersonRole ==. val Person.DRIVER
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
        &&. Esq.whenJust_ (liftA2 (,) mbSearchString mbSearchStrDBHash) (filterBySearchString person)
    orderBy [desc $ driverInformation ^. DriverInformationCreatedAt]
    limit limitVal
    offset offsetVal
    return (person, driverInformation)
  where
    limitVal = maybe 100 fromIntegral mbLimit
    offsetVal = maybe 0 fromIntegral mbOffset

    filterBySearchString person (searchStr, searchStrDBHash) = do
      let likeSearchStr = (%) ++. val searchStr ++. (%)
      ( concat_ @Text [person ^. PersonFirstName, val " ", unMaybe $ person ^. PersonMiddleName, val " ", unMaybe $ person ^. PersonLastName]
          `ilike` likeSearchStr
        )
        ||. person ^. PersonMobileNumberHash ==. val (Just searchStrDBHash)
    unMaybe = maybe_ (val "") identity

getDriversWithOutdatedLocationsToMakeInactive :: Transactionable m => UTCTime -> m [Person]
getDriversWithOutdatedLocationsToMakeInactive before = do
  findAll $ do
    (driverInformation :& _ :& person) <-
      from $
        table @DriverInformationT
          `innerJoin` table @DriverLocationT
            `Esq.on` ( \(driverInformation :& drLoc) ->
                         driverInformation ^. DriverInformationDriverId ==. drLoc ^. DriverLocationDriverId
                           &&. drLoc ^. DriverLocationUpdatedAt <. val before
                     )
          `innerJoin` table @PersonT
            `Esq.on` ( \(driverInformation :& _ :& person) ->
                         driverInformation ^. DriverInformationDriverId ==. person ^. PersonTId
                     )
    where_ $ driverInformation ^. DriverInformationActive
    orderBy [asc $ driverInformation ^. DriverInformationUpdatedAt]
    pure person

addReferralCode :: Id Person -> EncryptedHashedField 'AsEncrypted Text -> SqlDB ()
addReferralCode personId code = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverInformationReferralCode =. val (Just (code & unEncrypted . (.encrypted)))
      ]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey personId)

countDrivers :: Transactionable m => Id Merchant -> m (Int, Int)
countDrivers merchantId =
  getResults <$> do
    findAll $ do
      (driverInformation :& person) <-
        from $
          table @DriverInformationT
            `innerJoin` table @PersonT
              `Esq.on` ( \(driverInformation :& person) ->
                           driverInformation ^. DriverInformationDriverId ==. person ^. PersonTId
                       )
      where_ $
        person ^. PersonMerchantId ==. (val . toKey $ merchantId)
      groupBy (driverInformation ^. DriverInformationActive)
      pure (driverInformation ^. DriverInformationActive, count @Int $ person ^. PersonId)
  where
    getResults :: [(Bool, Int)] -> (Int, Int)
    getResults = foldl func (0, 0)

    func (active, inactive) (activity, counter) =
      if activity then (active + counter, inactive) else (active, inactive + counter)

updateDowngradingOptions :: Id Person -> Bool -> Bool -> Bool -> SqlDB ()
updateDowngradingOptions personId canDowngradeToSedan canDowngradeToHatchback canDowngradeToTaxi = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverInformationCanDowngradeToSedan =. val canDowngradeToSedan,
        DriverInformationCanDowngradeToHatchback =. val canDowngradeToHatchback,
        DriverInformationCanDowngradeToTaxi =. val canDowngradeToTaxi,
        DriverInformationUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey personId)
