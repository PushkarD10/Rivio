{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Merchant
  ( postMerchantServiceConfigMapsUpdate,
    postMerchantServiceUsageConfigMapsUpdate,
    postMerchantUpdate,
    getMerchantServiceUsageConfig,
    postMerchantServiceConfigSmsUpdate,
    postMerchantServiceUsageConfigSmsUpdate,
    postMerchantConfigOperatingCityCreate,
    postMerchantSpecialLocationUpsert,
    deleteMerchantSpecialLocationDelete,
    postMerchantSpecialLocationGatesUpsert,
    deleteMerchantSpecialLocationGatesDelete,
    buildMerchantServiceConfig,
  )
where

import Control.Applicative
import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Merchant as Common
import qualified Data.Text as T
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.Geometry as DGEO
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.MerchantServiceUsageConfig as DMSUC
import qualified Domain.Types.RiderConfig as DRC
import Environment
import qualified "shared-services" IssueManagement.Common as ICommon
import qualified "shared-services" IssueManagement.Domain.Types.Issue.IssueConfig as DIConfig
import qualified "shared-services" IssueManagement.Storage.CachedQueries.Issue.IssueConfig as CQIssueConfig
import Kernel.External.Call (CallService (Exotel))
import qualified Kernel.External.Maps as Maps
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.External.SMS as SMS
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runTransaction)
import qualified Kernel.Storage.Esqueleto.Transactionable as Esq
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Queries.GateInfoGeom as QGIG
import qualified Lib.Queries.SpecialLocation as QSL
import qualified Lib.Queries.SpecialLocationGeom as QSLG
import qualified Lib.Types.GateInfo as D
import qualified Lib.Types.SpecialLocation as SL
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantMessage as CQMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRC
import qualified Storage.Queries.BecknConfig as SQBC
import qualified Storage.Queries.Geometry as QGEO
import qualified Storage.Queries.MerchantServiceConfig as SQMSC
import Tools.Error

---------------------------------------------------------------------
postMerchantUpdate :: ShortId DM.Merchant -> Context.City -> Common.MerchantUpdateReq -> Flow APISuccess
postMerchantUpdate merchantShortId city req = do
  runRequestValidation Common.validateMerchantUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city)

  let updMerchant =
        merchant{DM.name = fromMaybe merchant.name req.name,
                 DM.gatewayUrl = fromMaybe merchant.gatewayUrl req.gatewayUrl,
                 DM.registryUrl = fromMaybe merchant.registryUrl req.registryUrl
                }
  now <- getCurrentTime

  mbAllExophones <- forM req.exoPhones $ \exophones -> do
    allExophones <- CQExophone.findAllExophones
    let alreadyUsedPhones = getAllPhones $ filter (\exophone -> exophone.merchantOperatingCityId /= merchantOperatingCity.id) allExophones
    let reqPhones = getAllPhones $ toList exophones
    let busyPhones = filter (`elem` alreadyUsedPhones) reqPhones
    unless (null busyPhones) $ do
      throwError $ InvalidRequest $ "Next phones are already in use: " <> show busyPhones
    pure allExophones

  void $ CQM.update updMerchant
  whenJust req.exoPhones \exophones -> do
    CQExophone.deleteByMerchantOperatingCityId merchantOperatingCity.id
    forM_ exophones $ \exophoneReq -> do
      exophone <- buildExophone merchant.id merchantOperatingCity.id now exophoneReq
      CQExophone.create exophone

  CQM.clearCache updMerchant
  whenJust mbAllExophones $ \allExophones -> do
    let oldExophones = filter (\exophone -> exophone.merchantOperatingCityId == merchantOperatingCity.id) allExophones
    CQExophone.clearCache merchantOperatingCity.id oldExophones
  logTagInfo "dashboard -> merchantUpdate : " (show merchant.id)
  pure Success
  where
    getAllPhones es = (es <&> (.primaryPhone)) <> (es <&> (.backupPhone))

buildExophone :: MonadGuid m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> UTCTime -> Common.ExophoneReq -> m DExophone.Exophone
buildExophone merchantId merchantOperatingCityId now req = do
  uid <- generateGUID
  pure
    DExophone.Exophone
      { id = uid,
        merchantId,
        merchantOperatingCityId,
        primaryPhone = req.primaryPhone,
        backupPhone = req.backupPhone,
        isPrimaryDown = False,
        callService = req.callService,
        updatedAt = now,
        createdAt = now,
        enableAlternateNumber = Just False
      }

---------------------------------------------------------------------
getMerchantServiceUsageConfig ::
  ShortId DM.Merchant ->
  Context.City ->
  Flow Common.ServiceUsageConfigRes
getMerchantServiceUsageConfig merchantShortId city = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city)
  config <- CQMSUC.findByMerchantOperatingCityId merchantOperatingCity.id >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCity.id.getId)
  pure $ mkServiceUsageConfigRes config

mkServiceUsageConfigRes :: DMSUC.MerchantServiceUsageConfig -> Common.ServiceUsageConfigRes
mkServiceUsageConfigRes DMSUC.MerchantServiceUsageConfig {..} =
  Common.ServiceUsageConfigRes
    { getEstimatedPickupDistances = Nothing,
      getPickupRoutes = Just getPickupRoutes,
      getTripRoutes = Just getTripRoutes,
      snapToRoadProvidersList = [],
      ..
    }

---------------------------------------------------------------------
buildMerchantServiceConfig ::
  MonadTime m =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DMSC.ServiceConfig ->
  m DMSC.MerchantServiceConfig
buildMerchantServiceConfig merchantId merchantOperatingCityId serviceConfig = do
  now <- getCurrentTime
  pure
    DMSC.MerchantServiceConfig
      { merchantId,
        serviceConfig,
        merchantOperatingCityId,
        updatedAt = now,
        createdAt = now
      }

postMerchantServiceConfigMapsUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MapsServiceConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceConfigMapsUpdate merchantShortId city req = do
  merchant <- findMerchantByShortId merchantShortId
  let serviceName = DMSC.MapsService $ Common.getMapsServiceFromReq req
  serviceConfig <- DMSC.MapsServiceConfig <$> Common.buildMapsServiceConfig req
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound ("merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city))
  merchantServiceConfig <- buildMerchantServiceConfig merchant.id merchantOperatingCity.id serviceConfig
  _ <- CQMSC.upsertMerchantServiceConfig merchantServiceConfig
  CQMSC.clearCache merchant.id merchantOperatingCity.id serviceName
  logTagInfo "dashboard -> postMerchantServiceConfigMapsUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
postMerchantServiceConfigSmsUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.SmsServiceConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceConfigSmsUpdate merchantShortId city req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound ("merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city))
  let serviceName = DMSC.SmsService $ Common.getSmsServiceFromReq req
  serviceConfig <- DMSC.SmsServiceConfig <$> Common.buildSmsServiceConfig req
  merchantServiceConfig <- buildMerchantServiceConfig merchant.id merchantOperatingCity.id serviceConfig
  _ <- CQMSC.upsertMerchantServiceConfig merchantServiceConfig
  CQMSC.clearCache merchant.id merchantOperatingCity.id serviceName
  logTagInfo "dashboard -> postMerchantServiceConfigSmsUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
postMerchantServiceUsageConfigMapsUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MapsServiceUsageConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceUsageConfigMapsUpdate merchantShortId city req = do
  runRequestValidation Common.validateMapsServiceUsageConfigUpdateReq req
  whenJust req.getEstimatedPickupDistances $ \_ ->
    throwError (InvalidRequest "getEstimatedPickupDistances is not allowed for bap")
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound ("merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city))
  forM_ Maps.availableMapsServices $ \service -> do
    when (Common.mapsServiceUsedInReq req service) $ do
      void $
        CQMSC.findByMerchantOpCityIdAndService merchant.id merchantOperatingCity.id (DMSC.MapsService service)
          >>= fromMaybeM (InvalidRequest $ "Merchant config for maps service " <> show service <> " is not provided")

  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOperatingCityId merchantOperatingCity.id
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCity.id.getId)
  let updMerchantServiceUsageConfig =
        merchantServiceUsageConfig{getDistances = fromMaybe merchantServiceUsageConfig.getDistances req.getDistances,
                                   getRoutes = fromMaybe merchantServiceUsageConfig.getRoutes req.getRoutes,
                                   snapToRoad = fromMaybe merchantServiceUsageConfig.snapToRoad req.snapToRoad,
                                   getPlaceName = fromMaybe merchantServiceUsageConfig.getPlaceName req.getPlaceName,
                                   getPlaceDetails = fromMaybe merchantServiceUsageConfig.getPlaceDetails req.getPlaceDetails,
                                   autoComplete = fromMaybe merchantServiceUsageConfig.autoComplete req.autoComplete
                                  }
  _ <- CQMSUC.updateMerchantServiceUsageConfig updMerchantServiceUsageConfig
  CQMSUC.clearCache merchantOperatingCity.id
  logTagInfo "dashboard -> postMerchantServiceUsageConfigMapsUpdate : " (show merchantOperatingCity.id)
  pure Success

---------------------------------------------------------------------
postMerchantServiceUsageConfigSmsUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.SmsServiceUsageConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceUsageConfigSmsUpdate merchantShortId city req = do
  runRequestValidation Common.validateSmsServiceUsageConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound ("merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city))
  forM_ SMS.availableSmsServices $ \service -> do
    when (Common.smsServiceUsedInReq req service) $ do
      void $
        CQMSC.findByMerchantOpCityIdAndService merchant.id merchantOperatingCity.id (DMSC.SmsService service)
          >>= fromMaybeM (InvalidRequest $ "Merchant config for sms service " <> show service <> " is not provided")

  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOperatingCityId merchantOperatingCity.id
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCity.id.getId)
  let updMerchantServiceUsageConfig =
        merchantServiceUsageConfig{smsProvidersPriorityList = req.smsProvidersPriorityList
                                  }
  _ <- CQMSUC.updateMerchantServiceUsageConfig updMerchantServiceUsageConfig
  CQMSUC.clearCache merchantOperatingCity.id
  logTagInfo "dashboard -> postMerchantServiceUsageConfigSmsUpdate : " (show merchantOperatingCity.id)
  pure Success

postMerchantSpecialLocationUpsert :: ShortId DM.Merchant -> Context.City -> Maybe (Id SL.SpecialLocation) -> Common.UpsertSpecialLocationReqT -> Flow APISuccess
postMerchantSpecialLocationUpsert merchantShortId _city mbSpecialLocationId request = do
  existingSLWithGeom <- maybe (return Nothing) (Esq.runInReplica . QSL.findByIdWithGeom) mbSpecialLocationId
  let mbExistingSL = fst <$> existingSLWithGeom
      mbGeom = snd =<< existingSLWithGeom
  updatedSL <- mkSpecialLocation mbExistingSL mbGeom
  void $
    runTransaction $
      if isJust mbExistingSL then QSLG.updateSpecialLocation updatedSL else QSLG.create updatedSL
  return Success
  where
    mkSpecialLocation :: Maybe SL.SpecialLocation -> Maybe Text -> Flow SL.SpecialLocation
    mkSpecialLocation mbExistingSpLoc mbGeometry = do
      let geom = request.geom <|> mbGeometry
      id <- maybe generateGUID (return . (.id)) mbExistingSpLoc
      now <- getCurrentTime
      merchantOperatingCity <- maybe (return Nothing) (CQMOC.findByMerchantShortIdAndCity merchantShortId) request.city
      locationName <-
        fromMaybeM (InvalidRequest "Location Name cannot be empty for a new special location") $
          request.locationName <|> (mbExistingSpLoc <&> (.locationName))
      category <- fromMaybeM (InvalidRequest "Category is a required field for a new special location") $ request.category <|> (mbExistingSpLoc <&> (.category))
      return $
        SL.SpecialLocation
          { gates = [],
            createdAt = maybe now (.createdAt) mbExistingSpLoc,
            updatedAt = now,
            merchantOperatingCityId = (.id.getId) <$> merchantOperatingCity,
            ..
          }

deleteMerchantSpecialLocationDelete :: ShortId DM.Merchant -> Context.City -> Id SL.SpecialLocation -> Flow APISuccess
deleteMerchantSpecialLocationDelete _merchantShortid _city specialLocationId = do
  void $ QSL.findById specialLocationId >>= fromMaybeM (InvalidRequest "Special Location with given id not found")
  void $ runTransaction $ QSL.deleteById specialLocationId
  void $ runTransaction $ QGI.deleteAll specialLocationId
  pure Success

postMerchantSpecialLocationGatesUpsert :: ShortId DM.Merchant -> Context.City -> Id SL.SpecialLocation -> Common.UpsertSpecialLocationGateReqT -> Flow APISuccess
postMerchantSpecialLocationGatesUpsert _merchantShortId _city specialLocationId request = do
  void $ QSL.findById specialLocationId >>= fromMaybeM (InvalidRequest "Cound not find a special location with the provided id")
  existingGates <- QGI.findAllGatesBySpecialLocationId specialLocationId
  createOrUpdateGate existingGates request
  return Success
  where
    createOrUpdateGate :: [(D.GateInfo, Maybe Text)] -> Common.UpsertSpecialLocationGateReqT -> Flow ()
    createOrUpdateGate existingGates req = do
      let existingGatewithGeom = find (\(gate, _mbGeom) -> normalizeName gate.name == normalizeName req.name) existingGates
          existingGate = fst <$> existingGatewithGeom
          mbGeom = snd =<< existingGatewithGeom
      updatedGate <- mkGate req existingGate mbGeom
      void $
        runTransaction $
          if isNothing existingGate then QGIG.create updatedGate else QGIG.updateGate updatedGate

    mkGate :: Common.UpsertSpecialLocationGateReqT -> Maybe D.GateInfo -> Maybe Text -> Flow D.GateInfo
    mkGate reqT mbGate mbGeom = do
      id <- cast <$> maybe generateGUID (return . (.id)) mbGate
      now <- getCurrentTime
      latitude <- fromMaybeM (InvalidRequest "Latitude field cannot be empty for a new gate") $ reqT.latitude <|> (mbGate <&> (.point.lat))
      longitude <- fromMaybeM (InvalidRequest "Longitude field cannot be empty for a new gate") $ reqT.longitude <|> (mbGate <&> (.point.lon))
      address <- fromMaybeM (InvalidRequest "Address cannot be empty for a new gate") $ reqT.address <|> (mbGate >>= (.address))
      let canQueueUpOnGate = fromMaybe False $ reqT.canQueueUpOnGate <|> (mbGate <&> (.canQueueUpOnGate))
          defaultDriverExtra = reqT.defaultDriverExtra <|> (mbGate >>= (.defaultDriverExtra))
          geom = reqT.geom <|> mbGeom
      return $
        D.GateInfo
          { name = reqT.name,
            address = Just address,
            geom,
            createdAt = maybe now (.createdAt) mbGate,
            updatedAt = now,
            point = LatLong {lat = latitude, lon = longitude},
            gateType = D.Pickup,
            ..
          }

deleteMerchantSpecialLocationGatesDelete :: ShortId DM.Merchant -> Context.City -> Id SL.SpecialLocation -> Text -> Flow APISuccess
deleteMerchantSpecialLocationGatesDelete _merchantShortId _city specialLocationId gateName = do
  existingGates <- QGI.findAllGatesBySpecialLocationId specialLocationId
  let existingGate = fst <$> find (\(gate, _mbGeom) -> normalizeName gate.name == normalizeName gateName) existingGates
  case existingGate of
    Nothing -> throwError $ InvalidRequest "Could not find any gates with the specified name for the given specialLocationId"
    Just gate -> runTransaction $ QGI.deleteById gate.id
  return Success

normalizeName :: Text -> Text
normalizeName = T.strip . T.toLower

postMerchantConfigOperatingCityCreate :: ShortId DM.Merchant -> Context.City -> Common.CreateMerchantOperatingCityReqT -> Flow Common.CreateMerchantOperatingCityRes
postMerchantConfigOperatingCityCreate merchantShortId city req = do
  merchant <- findMerchantByShortId merchantShortId
  baseOperatingCity <- CQMOC.findByMerchantIdAndCity merchant.id city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show city)
  now <- getCurrentTime
  let baseOperatingCityId = baseOperatingCity.id

  cityAlreadyCreated <- CQMOC.findByMerchantIdAndCity merchant.id req.city
  newMerchantOperatingCityId <-
    case cityAlreadyCreated of
      Just newCity -> return newCity.id
      Nothing -> generateGUID

  -- city
  let mbNewOperatingCity =
        case cityAlreadyCreated of
          Nothing -> do
            let newOperatingCity = buildMerchantOperatingCity merchant.id newMerchantOperatingCityId now
            Just newOperatingCity
          _ -> Nothing

  -- merchant message
  mbMerchantMessages <-
    CQMM.findAllByMerchantOpCityId newMerchantOperatingCityId >>= \case
      [] -> do
        merchantMessages <- CQMM.findAllByMerchantOpCityId baseOperatingCityId
        let newMerchantMessages = map (buildMerchantMessage newMerchantOperatingCityId now) merchantMessages
        return $ Just newMerchantMessages
      _ -> return Nothing -- ignore

  -- merchant payment method
  mbMerchantPaymentMethods <-
    CQMPM.findAllByMerchantOperatingCityId newMerchantOperatingCityId >>= \case
      [] -> do
        merchantPaymentMethods <- CQMPM.findAllByMerchantOperatingCityId baseOperatingCityId
        newMerchantPaymentMethods <- mapM (buildMerchantPaymentMethod newMerchantOperatingCityId now) merchantPaymentMethods
        return $ Just newMerchantPaymentMethods
      _ -> return Nothing

  -- merchant service usage config
  mbMerchantServiceUsageConfig <-
    CQMSUC.findByMerchantOperatingCityId newMerchantOperatingCityId >>= \case
      Nothing -> do
        merchantServiceUsageConfig <- CQMSUC.findByMerchantOperatingCityId baseOperatingCityId >>= fromMaybeM (InvalidRequest "Merchant Service Usage Config not found")
        let newMerchantServiceUsageConfig = buildMerchantServiceUsageConfig newMerchantOperatingCityId now merchantServiceUsageConfig
        return $ Just newMerchantServiceUsageConfig
      _ -> return Nothing

  -- merchant service config
  mbMerchantServiceConfig <-
    SQMSC.findAllByMerchantOperatingCityId newMerchantOperatingCityId >>= \case
      [] -> do
        merchantServiceConfigs <- SQMSC.findAllByMerchantOperatingCityId baseOperatingCityId
        let newMerchantServiceConfigs = map (buildMerchantServiceConfigs newMerchantOperatingCityId now) merchantServiceConfigs
        return $ Just newMerchantServiceConfigs
      _ -> return Nothing

  -- rider_config
  mbRiderConfig <-
    CQRC.findByMerchantOperatingCityId newMerchantOperatingCityId >>= \case
      Nothing -> do
        riderConfig <- CQRC.findByMerchantOperatingCityId baseOperatingCityId >>= fromMaybeM (InvalidRequest "Transporter Config not found")
        let newRiderConfig = buildRiderConfig newMerchantOperatingCityId now riderConfig
        return $ Just newRiderConfig
      _ -> return Nothing

  -- geometry
  mbGeometry <-
    QGEO.findGeometryByStateAndCity req.city req.state >>= \case
      Nothing -> do
        Just <$> buildGeometry
      _ -> return Nothing

  -- exophone
  mbExophone <-
    CQExophone.findAllByMerchantOperatingCityId newMerchantOperatingCityId >>= \case
      [] -> do
        exophones <- CQExophone.findAllByMerchantOperatingCityId baseOperatingCityId
        return $ Just exophones
      _ -> return Nothing

  -- issue config
  mbIssueConfig <-
    CQIssueConfig.findByMerchantOpCityId (cast newMerchantOperatingCityId) ICommon.DRIVER >>= \case
      Nothing -> do
        issueConfig <- CQIssueConfig.findByMerchantOpCityId (cast baseOperatingCityId) ICommon.DRIVER >>= fromMaybeM (InvalidRequest "Issue Config not found")
        newIssueConfig <- buildIssueConfig newMerchantOperatingCityId now issueConfig
        return $ Just newIssueConfig
      _ -> return Nothing

  -- beckn config
  mbBecknConfig <-
    SQBC.findAllByMerchantOperatingCityId (Just newMerchantOperatingCityId) >>= \case
      [] -> do
        becknConfig <- SQBC.findAllByMerchantOperatingCityId (Just baseOperatingCityId)
        newBecknConfig <- mapM (buildBecknConfig newMerchantOperatingCityId now) becknConfig
        return $ Just newBecknConfig
      _ -> return Nothing

  whenJust mbNewOperatingCity $ \newOperatingCity -> CQMOC.create newOperatingCity
  whenJust mbGeometry $ \geometry -> QGEO.create geometry
  whenJust mbMerchantMessages $ \merchantMessages -> mapM_ CQMM.create merchantMessages
  whenJust mbMerchantPaymentMethods $ \mPM -> mapM_ CQMPM.create mPM
  whenJust mbMerchantServiceUsageConfig $ \mSUC -> CQMSUC.create mSUC
  whenJust mbMerchantServiceConfig $ \merchantServiceConfigs -> mapM_ SQMSC.create merchantServiceConfigs
  whenJust mbBecknConfig $ \becknConfig -> mapM_ SQBC.create becknConfig
  whenJust mbRiderConfig $ \riderConfig -> CQRC.create riderConfig

  whenJust mbExophone $ \exophones -> do
    whenJust (find (\ex -> ex.callService == Exotel) exophones) $ \exophone -> do
      exophone' <- buildNewExophone newMerchantOperatingCityId now exophone
      CQExophone.create exophone'
  whenJust mbIssueConfig $ \issueConfig -> CQIssueConfig.create issueConfig

  when req.enableForMerchant $ do
    let newOrigin = updateGeoRestriction merchant.geofencingConfig.origin
        newDestination = updateGeoRestriction merchant.geofencingConfig.destination

    when (checkGeofencingConfig merchant.geofencingConfig.origin) $ do
      CQM.updateGeofencingConfig merchant.id newOrigin newDestination
      CQM.clearCache merchant

  pure $ Common.CreateMerchantOperatingCityRes newMerchantOperatingCityId.getId
  where
    updateGeoRestriction = \case
      Unrestricted -> Unrestricted
      Regions regions -> Regions $ regions <> [(show req.city)]
    checkGeofencingConfig = \case
      Regions regions -> notElem (show req.city) regions
      Unrestricted -> True

    buildGeometry = do
      id <- generateGUID
      pure
        DGEO.Geometry
          { id,
            region = show req.city,
            state = req.state,
            city = req.city,
            geom = Just req.geom
          }

    buildMerchantOperatingCity merchantId cityId currentTime = do
      DMOC.MerchantOperatingCity
        { id = cityId,
          merchantId,
          merchantShortId,
          lat = req.lat,
          long = req.long,
          city = req.city,
          state = req.state,
          country = req.country,
          distanceUnit = fromMaybe Meter req.distanceUnit,
          createdAt = currentTime,
          updatedAt = currentTime
        }

    buildNewExophone newCityId currentTime DExophone.Exophone {..} = do
      newId <- generateGUID
      return
        DExophone.Exophone
          { id = newId,
            merchantOperatingCityId = newCityId,
            primaryPhone = req.exophone,
            backupPhone = req.exophone,
            isPrimaryDown = False,
            createdAt = currentTime,
            updatedAt = currentTime,
            ..
          }

    buildMerchantMessage newCityId currentTime DMM.MerchantMessage {..} =
      DMM.MerchantMessage
        { merchantOperatingCityId = newCityId,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

    buildMerchantPaymentMethod newCityId currentTime DMPM.MerchantPaymentMethod {..} = do
      newId <- generateGUID
      return
        DMPM.MerchantPaymentMethod
          { id = newId,
            merchantOperatingCityId = newCityId,
            createdAt = currentTime,
            updatedAt = currentTime,
            ..
          }

    buildMerchantServiceUsageConfig newCityId currentTime DMSUC.MerchantServiceUsageConfig {..} = do
      DMSUC.MerchantServiceUsageConfig
        { merchantOperatingCityId = newCityId,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

    {- Do it once service config on basis on city id is implemented -}
    buildMerchantServiceConfigs newCityId currentTime DMSC.MerchantServiceConfig {..} =
      DMSC.MerchantServiceConfig
        { merchantOperatingCityId = newCityId,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

    buildRiderConfig newCityId currentTime DRC.RiderConfig {..} =
      DRC.RiderConfig
        { merchantOperatingCityId = newCityId,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

    buildIssueConfig newCityId currentTime DIConfig.IssueConfig {..} = do
      newId <- generateGUID
      return
        DIConfig.IssueConfig
          { id = newId,
            merchantOperatingCityId = cast newCityId,
            createdAt = currentTime,
            updatedAt = currentTime,
            ..
          }
    buildBecknConfig newCityId currentTime DBC.BecknConfig {..} = do
      newId <- generateGUID
      return
        DBC.BecknConfig
          { id = newId,
            merchantOperatingCityId = Just newCityId,
            createdAt = currentTime,
            updatedAt = currentTime,
            ..
          }
