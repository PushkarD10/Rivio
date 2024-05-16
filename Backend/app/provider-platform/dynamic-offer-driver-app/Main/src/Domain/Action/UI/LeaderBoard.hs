{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.LeaderBoard where

import Control.Monad
import Data.Aeson hiding (Success)
import qualified Data.HashMap.Strict as HM
import Data.Time hiding (getCurrentTime)
import Data.Time.Calendar ()
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import Domain.Action.UI.Ride.EndRide.Internal as RideEndInt
import qualified Domain.Types.LeaderBoardConfigs as LConfig
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Person
import Domain.Types.Person as SP
import GHC.Float (double2Int)
import GHC.Num.Integer (integerFromInt, integerToInt)
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import Kernel.Utils.Error
import Storage.CachedQueries.Merchant.LeaderBoardConfig as QLeaderConfig
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as QMerchantOpCity
import qualified Storage.Queries.Person as QPerson

data DriversInfo = DriversInfo
  { rank :: Integer,
    name :: Text,
    totalRides :: Int,
    totalDistance :: Meters,
    isCurrentDriver :: Bool,
    gender :: Maybe SP.Gender
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data LeaderBoardRes = LeaderBoardRes
  { driverList :: [DriversInfo],
    lastUpdatedAt :: Maybe UTCTime,
    totalEligibleDrivers :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getDailyDriverLeaderBoard ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, EncFlow m r, CacheFlow m r) =>
  (Id Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Day ->
  Maybe Bool ->
  m LeaderBoardRes
getDailyDriverLeaderBoard (personId, merchantId, merchantOpCityId) day fillData = do
  now <- getCurrentTime
  let currentDate = RideEndInt.getCurrentDate now
      dateDiff = diffDays currentDate day
  dailyLeaderBoardConfig <- QLeaderConfig.findLeaderBoardConfigbyType LConfig.DAILY merchantOpCityId >>= fromMaybeM (InternalError "Leaderboard configs not present")
  unless dailyLeaderBoardConfig.isEnabled . throwError $ InvalidRequest "Leaderboard Not Available"

  fork "Backfill leaderboard data" $ do
    let backFillInRedis = fromMaybe False fillData
    when backFillInRedis $ do
      leaderboardBackfillLock :: Maybe Bool <- Redis.safeGet makeLeaderboardBackfillKeyLock
      case leaderboardBackfillLock of
        Just True -> pure ()
        _ -> do
          Redis.setExp makeLeaderboardBackfillKeyLock True 86400
          startDailyBackFill merchantId merchantOpCityId day
          let (_, currDayIndex) = sundayStartWeek day
              weekStartDate = addDays (fromIntegral (- currDayIndex)) day
              weekEndDate = addDays (fromIntegral (6 - currDayIndex)) day
          startWeeklyBackFill merchantId merchantOpCityId weekStartDate weekEndDate

  let numberOfSets = fromIntegral dailyLeaderBoardConfig.numberOfSets
  when (dateDiff > numberOfSets - 1 || dateDiff < 0) $
    throwError $ InvalidRequest "Date outside Range"
  getDriverListFromLeaderBoard (personId, merchantId, merchantOpCityId) day day (integerToInt dateDiff) dailyLeaderBoardConfig

getYearFromDay :: Day -> Integer
getYearFromDay day = let (year, _, _) = toGregorian day in year

getLastDayOfYear :: Integer -> Day
getLastDayOfYear year = fromGregorian year 12 31

getWeeklyDriverLeaderBoard ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, EncFlow m r, Redis.HedisFlow m r, CacheFlow m r) =>
  (Id Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Day ->
  Day ->
  m LeaderBoardRes
getWeeklyDriverLeaderBoard (personId, merchantId, merchantOpCityId) fromDate toDate = do
  now <- getCurrentTime
  let currentDate = RideEndInt.getCurrentDate now
      (currWeekNumber, _) = sundayStartWeek currentDate
      (reqWeekNumber, reqDayIndex) = sundayStartWeek fromDate
      (lastWeekOfYear, _) = sundayStartWeek $ getLastDayOfYear $ getYearFromDay fromDate
  let weekDiff = (currWeekNumber - reqWeekNumber + lastWeekOfYear) `mod` lastWeekOfYear
  weeklyLeaderBoardConfig <- QLeaderConfig.findLeaderBoardConfigbyType LConfig.WEEKLY merchantOpCityId >>= fromMaybeM (InternalError "Leaderboard configs not present")
  unless weeklyLeaderBoardConfig.isEnabled . throwError $ InvalidRequest "Leaderboard Not Available"
  let numberOfSets = weeklyLeaderBoardConfig.numberOfSets
  when (weekDiff > numberOfSets - 1 || weekDiff < 0) $ throwError $ InvalidRequest "Week outside Range"
  when (diffDays toDate fromDate /= 6 || reqDayIndex /= 0) $ throwError $ InvalidRequest "Invalid Input"
  getDriverListFromLeaderBoard (personId, merchantId, merchantOpCityId) fromDate toDate weekDiff weeklyLeaderBoardConfig

getMonthlyDriverLeaderBoard :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, EncFlow m r, Redis.HedisFlow m r, CacheFlow m r) => (Id Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Int -> m LeaderBoardRes
getMonthlyDriverLeaderBoard (personId, merchantId, merchantOpCityId) month = do
  now <- getCurrentTime
  let currentDay = RideEndInt.getCurrentDate now
      fromDate = fromGregorian (getYearFromDay currentDay) month 1
      monthDiff = RideEndInt.getMonth currentDay - month
  monthlyLeaderBoardConfig <- QLeaderConfig.findLeaderBoardConfigbyType LConfig.MONTHLY merchantOpCityId >>= fromMaybeM (InternalError "Leaderboard configs not present")
  unless monthlyLeaderBoardConfig.isEnabled . throwError $ InvalidRequest "Leaderboard Not Available"
  when ((monthDiff < 0 && 12 + monthDiff > monthlyLeaderBoardConfig.numberOfSets - 1) || monthDiff > monthlyLeaderBoardConfig.numberOfSets - 1) $ throwError $ InvalidRequest "Month outside Range"
  getDriverListFromLeaderBoard (personId, merchantId, merchantOpCityId) fromDate fromDate monthDiff monthlyLeaderBoardConfig

getDriverListFromLeaderBoard :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, EncFlow m r, Redis.HedisFlow m r, CacheFlow m r) => (Id Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Day -> Day -> Int -> LConfig.LeaderBoardConfigs -> m LeaderBoardRes
getDriverListFromLeaderBoard (personId, merchantId, merchantOpCityId) fromDate toDate dateDiff leaderBoardConfig = do
  now <- getCurrentTime
  let leaderBoardType = leaderBoardConfig.leaderBoardType
      useCityBasedLeaderboard = leaderBoardConfig.useOperatingCityBasedLeaderBoard
      driverLeaderBoardKey = RideEndInt.makeDriverLeaderBoardKey leaderBoardType False useCityBasedLeaderboard merchantId merchantOpCityId fromDate toDate
      cachedDriverLeaderBoardKey = RideEndInt.makeDriverLeaderBoardKey leaderBoardType True useCityBasedLeaderboard merchantId merchantOpCityId fromDate toDate
  driversWithScoresMap :: [(Text, Double)] <- concat <$> Redis.withNonCriticalRedis (Redis.get cachedDriverLeaderBoardKey)
  let driverIds = map (Id . fst) driversWithScoresMap
  driverDetailsMap <- HM.fromList . map (\driver -> (driver.id.getId, (fromMaybe "Driver" $ getPersonFullName driver, driver.gender))) <$> B.runInReplica (QPerson.getDriversByIdIn driverIds)
  (drivers', isCurrentDriverInTop) <-
    foldlM
      ( \(acc, isCurrentDriverInTop) ((driverId, score), index) -> do
          (fullName, gender) <- HM.lookup driverId driverDetailsMap & fromMaybeM (PersonFieldNotPresent "DriverDetails")
          let (totalRides, totalDistance) = RideEndInt.getRidesAndDistancefromZscore score leaderBoardConfig.zScoreBase
              isCurrentDriver = personId.getId == driverId
          pure
            ( acc
                <> [ DriversInfo
                       { rank = index,
                         name = fullName,
                         gender = Just gender,
                         ..
                       }
                   ],
              isCurrentDriverInTop || isCurrentDriver
            )
      )
      ([], False)
      (zip driversWithScoresMap [1, 2 ..])
  totalEligibleDrivers <- Redis.withNonCriticalRedis $ Redis.zCard driverLeaderBoardKey
  if not isCurrentDriverInTop && dateDiff == 0
    then do
      person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      mbCurrPersonRank <- Redis.withNonCriticalRedis $ Redis.zRevRank driverLeaderBoardKey personId.getId
      mbCurrDriverZscore <- Redis.withNonCriticalRedis $ Redis.zScore driverLeaderBoardKey personId.getId
      let currDriverZscore = fromMaybe 0 mbCurrDriverZscore
      currPersonRank <-
        case mbCurrPersonRank of
          Nothing -> Redis.withNonCriticalRedis $ Redis.zCard driverLeaderBoardKey
          Just rank -> pure rank
      let (currPersonTotalRides, currPersonTotalDistance) = RideEndInt.getRidesAndDistancefromZscore currDriverZscore leaderBoardConfig.zScoreBase
      currPersonFullName <- getPersonFullName person & fromMaybeM (PersonFieldNotPresent "firstName")
      let currDriverInfo = DriversInfo (currPersonRank + 1) currPersonFullName currPersonTotalRides currPersonTotalDistance True (Just person.gender)
      return $ LeaderBoardRes (currDriverInfo : drivers') (Just now) (fromIntegral totalEligibleDrivers)
    else return $ LeaderBoardRes drivers' (Just now) (fromIntegral totalEligibleDrivers)

splitIntoBatches :: Int -> [a] -> [[a]]
splitIntoBatches _ [] = []
splitIntoBatches batchSize xs =
  let (batch, rest) = splitAt batchSize xs
   in batch : splitIntoBatches batchSize rest

startDailyBackFill :: (Esq.EsqDBFlow m r, EncFlow m r, Redis.HedisFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Day -> m ()
startDailyBackFill merchantId merchantOpCityId day = do
  oldDriverScoresMap :: [(Text, Double)] <- Redis.zrevrangeWithscores (RideEndInt.makeDriverLeaderBoardKey LConfig.DAILY False (Just False) merchantId merchantOpCityId day day) 0 3000000
  let oldDriverScoreMapBatchList = splitIntoBatches 10 oldDriverScoresMap
  backfillData oldDriverScoreMapBatchList
  merchantsOpCities <- QMerchantOpCity.findAllByMerchantId merchantId
  updateDailyCachedLeaderBoard merchantsOpCities
  where
    backfillData [] = pure ()
    backfillData (driverBatch : remaining) = do
      let driverIds = map (Id . fst) driverBatch
      let driverScores = map snd driverBatch
      driverIdToCityMapping <- HM.fromList . map (\driver -> (driver.id, driver.merchantOperatingCityId)) <$> B.runInReplica (QPerson.getDriversByIdIn driverIds)
      fillData driverIds driverScores driverIdToCityMapping
      backfillData remaining
    fillData [] _ _ = pure ()
    fillData _ [] _ = pure ()
    fillData (driverId : remain) (score : restScr) driverIdToCityMapping = do
      let opcityid = HM.lookup driverId driverIdToCityMapping
      case opcityid of
        Just cityId -> do
          Redis.zAddExp (RideEndInt.makeDriverLeaderBoardKey LConfig.DAILY False (Just True) merchantId cityId day day) driverId.getId (integerFromInt $ double2Int score) 86400
          fillData remain restScr driverIdToCityMapping
        Nothing -> fillData remain restScr driverIdToCityMapping
    updateDailyCachedLeaderBoard [] = pure ()
    updateDailyCachedLeaderBoard (currCity : cities) = do
      let cityId = currCity.id
      currConfig <- QLeaderConfig.findLeaderBoardConfigbyType LConfig.DAILY cityId >>= fromMaybeM (InternalError "Leaderboard configs not present")
      let limit = integerFromInt currConfig.leaderBoardLengthLimit
      driversListWithScores <- Redis.zrevrangeWithscores (RideEndInt.makeDriverLeaderBoardKey LConfig.DAILY False (Just True) merchantId cityId day day) 0 (limit -1)
      Redis.setExp (RideEndInt.makeDriverLeaderBoardKey LConfig.DAILY True (Just True) merchantId cityId day day) driversListWithScores (currConfig.leaderBoardExpiry.getSeconds * currConfig.numberOfSets)
      updateDailyCachedLeaderBoard cities

startWeeklyBackFill :: (Esq.EsqDBFlow m r, EncFlow m r, Redis.HedisFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Day -> Day -> m ()
startWeeklyBackFill merchantId merchantOpCityId weekStartDate weekEndDate = do
  oldDriverScoresMap :: [(Text, Double)] <- Redis.zrevrangeWithscores (RideEndInt.makeDriverLeaderBoardKey LConfig.WEEKLY False (Just False) merchantId merchantOpCityId weekStartDate weekEndDate) 0 3000000
  let oldDriverScoreMapBatchList = splitIntoBatches 10 oldDriverScoresMap
  backfillData oldDriverScoreMapBatchList
  merchantsOpCities <- QMerchantOpCity.findAllByMerchantId merchantId
  updateDailyCachedLeaderBoard merchantsOpCities
  where
    backfillData [] = pure ()
    backfillData (driverBatch : remaining) = do
      let driverIds = map (Id . fst) driverBatch
      let driverScores = map snd driverBatch
      driverIdToCityMapping <- HM.fromList . map (\driver -> (driver.id, driver.merchantOperatingCityId)) <$> B.runInReplica (QPerson.getDriversByIdIn driverIds)
      fillData driverIds driverScores driverIdToCityMapping
      backfillData remaining
    fillData [] _ _ = pure ()
    fillData _ [] _ = pure ()
    fillData (driverId : remain) (score : restScr) driverIdToCityMapping = do
      let opcityid = HM.lookup driverId driverIdToCityMapping
      case opcityid of
        Just cityId -> do
          Redis.zAddExp (RideEndInt.makeDriverLeaderBoardKey LConfig.WEEKLY False (Just True) merchantId cityId weekStartDate weekEndDate) driverId.getId (integerFromInt $ double2Int score) 86400
          fillData remain restScr driverIdToCityMapping
        Nothing -> fillData remain restScr driverIdToCityMapping
    updateDailyCachedLeaderBoard [] = pure ()
    updateDailyCachedLeaderBoard (currCity : cities) = do
      let cityId = currCity.id
      currConfig <- QLeaderConfig.findLeaderBoardConfigbyType LConfig.WEEKLY cityId >>= fromMaybeM (InternalError "Leaderboard configs not present")
      let limit = integerFromInt currConfig.leaderBoardLengthLimit
      driversListWithScores <- Redis.zrevrangeWithscores (RideEndInt.makeDriverLeaderBoardKey LConfig.WEEKLY False (Just True) merchantId cityId weekStartDate weekEndDate) 0 (limit -1)
      Redis.setExp (RideEndInt.makeDriverLeaderBoardKey LConfig.WEEKLY True (Just True) merchantId cityId weekStartDate weekEndDate) driversListWithScores (currConfig.leaderBoardExpiry.getSeconds * currConfig.numberOfSets)
      updateDailyCachedLeaderBoard cities

makeLeaderboardBackfillKeyLock :: Text
makeLeaderboardBackfillKeyLock = "Driver_leaderboard_backfill_key_lock"
