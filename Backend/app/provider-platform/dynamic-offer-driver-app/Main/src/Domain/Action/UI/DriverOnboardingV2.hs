{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.DriverOnboardingV2 where

import qualified API.Types.UI.DriverOnboardingV2
import Data.OpenApi (ToSchema)
import Domain.Types.Common
import qualified Domain.Types.DocumentVerificationConfig
import Domain.Types.DriverInformation
import Domain.Types.FarePolicy
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import Domain.Types.ServiceTierType
import qualified Domain.Types.Vehicle as DTV
import Domain.Types.VehicleServiceTier
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Prelude
import Kernel.Beam.Functions
import Kernel.External.Types (Language (..))
import qualified Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Beckn.DecimalValue as DecimalValue
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import SharedLogic.DriverOnboarding
import SharedLogic.FarePolicy
import SharedLogic.VehicleServiceTier
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as PersonQuery
import qualified Storage.Queries.Translations as MTQuery
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as QRC
import Tools.Auth
import Tools.Error

stringToPrice :: Currency -> Text -> Maybe Price
stringToPrice currency value = do
  (DecimalValue.DecimalValue v) <- DecimalValue.valueFromString value
  return $
    Price
      { amountInt = Money $ Kernel.Prelude.roundToIntegral v,
        amount = HighPrecMoney v,
        currency
      }

mkDocumentVerificationConfigAPIEntity :: Language -> Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig -> Environment.Flow API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity
mkDocumentVerificationConfigAPIEntity language Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig {..} = do
  mbTitle <- MTQuery.findByErrorAndLanguage ((show documentType) <> "_Title") language
  mbDescription <- MTQuery.findByErrorAndLanguage ((show documentType) <> "_Description") language
  return $
    API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity
      { title = maybe title (.message) mbTitle,
        description = maybe description (Just . (.message)) mbDescription,
        ..
      }

getOnboardingConfigs ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Environment.Flow API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigList
  )
getOnboardingConfigs (mbPersonId, _, merchanOperatingCityId) mbOnlyVehicle = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let personLangauge = fromMaybe ENGLISH person.language
  cabConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchanOperatingCityId DTV.CAR
  autoConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchanOperatingCityId DTV.AUTO_CATEGORY
  bikeConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchanOperatingCityId DTV.MOTORCYCLE

  cabConfigs <- mapM (mkDocumentVerificationConfigAPIEntity personLangauge) (filterVehicleDocuments cabConfigsRaw)
  autoConfigs <- mapM (mkDocumentVerificationConfigAPIEntity personLangauge) (filterVehicleDocuments autoConfigsRaw)
  bikeConfigs <- mapM (mkDocumentVerificationConfigAPIEntity personLangauge) (filterVehicleDocuments bikeConfigsRaw)

  return $
    API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigList
      { cabs = toMaybe cabConfigs,
        autos = toMaybe autoConfigs,
        bikes = toMaybe bikeConfigs
      }
  where
    toMaybe :: [a] -> Kernel.Prelude.Maybe [a]
    toMaybe [] = Kernel.Prelude.Nothing
    toMaybe xs = Kernel.Prelude.Just xs

    filterVehicleDocuments :: [Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig] -> [Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig]
    filterVehicleDocuments docs =
      if mbOnlyVehicle == Just True
        then filter (\Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig {..} -> documentType `elem` vehicleDocumentTypes) docs
        else docs

getDriverRateCard ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Domain.Types.ServiceTierType.ServiceTierType ->
    Environment.Flow [API.Types.UI.DriverOnboardingV2.RateCardItem]
  )
getDriverRateCard (mbPersonId, _, merchantOperatingCityId) serviceTierType = do
  _personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  fullFarePolicy <- getFarePolicy merchantOperatingCityId (OneWay OneWayOnDemandDynamicOffer) serviceTierType Nothing Nothing Nothing
  let rateCard = catMaybes $ mkFarePolicyBreakups EulerHS.Prelude.id mkBreakupItem Nothing Nothing (fullFarePolicyToFarePolicy fullFarePolicy)
  return rateCard
  where
    mkBreakupItem :: Text -> Text -> Maybe API.Types.UI.DriverOnboardingV2.RateCardItem
    mkBreakupItem title valueInText = do
      priceObject <- stringToPrice INR valueInText -- change INR to proper currency after Roman changes
      return $
        API.Types.UI.DriverOnboardingV2.RateCardItem
          { title,
            price = priceObject.amountInt,
            priceWithCurrency = mkPriceAPIEntity priceObject
          }

postDriverUpdateAirCondition ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.UpdateAirConditionUpdateRequest ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postDriverUpdateAirCondition (mbPersonId, _, merchanOperatingCityId) API.Types.UI.DriverOnboardingV2.UpdateAirConditionUpdateRequest {..} = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchanOperatingCityId
  checkAndUpdateAirConditioned False isAirConditioned personId cityVehicleServiceTiers
  return Success

getDriverVehicleServiceTiers ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers
  )
getDriverVehicleServiceTiers (mbPersonId, _, merchanOperatingCityId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverInfo <- runInReplica $ QDI.findById personId >>= fromMaybeM DriverInfoNotFound
  vehicle <- runInReplica $ QVehicle.findById personId >>= fromMaybeM (VehicleNotFound personId.getId)
  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchanOperatingCityId
  let personLangauge = fromMaybe ENGLISH person.language

  let driverVehicleServiceTierTypes = selectVehicleTierForDriverWithUsageRestriction person driverInfo vehicle cityVehicleServiceTiers
  let tierOptions =
        driverVehicleServiceTierTypes <&> \(VehicleServiceTier {..}, usageRestricted) -> do
          API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTier
            { isSelected = serviceTierType `elem` vehicle.selectedServiceTiers,
              isDefault = vehicle.variant `elem` defaultForVehicleVariant,
              isUsageRestricted = Just usageRestricted,
              ..
            }

  mbAirConditioned <- do
    let serviceTierACThresholds = map (\(VehicleServiceTier {..}, _) -> airConditioned) driverVehicleServiceTierTypes
    if any isJust serviceTierACThresholds
      then do
        let isACAllowedForDriver = checkIfACAllowedForDriver driverInfo (catMaybes serviceTierACThresholds)
        let isACWorking = vehicle.airConditioned /= Just False
        restrictionMessageItem <-
          if not isACAllowedForDriver
            then MTQuery.findByErrorAndLanguage "AC_RESTRICTION_MESSAGE" personLangauge
            else
              if isACWorking
                then MTQuery.findByErrorAndLanguage "AC_WORKING_MESSAGE" personLangauge
                else return Nothing
        return $
          Just $
            API.Types.UI.DriverOnboardingV2.AirConditionedTier
              { isWorking = isACAllowedForDriver && isACWorking,
                restrictionMessage = restrictionMessageItem <&> (.message),
                usageRestrictionType = driverInfo.acUsageRestrictionType
              }
      else return Nothing

  return $
    API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers
      { tiers = tierOptions,
        canSwitchToRental = Just driverInfo.canSwitchToRental,
        canSwitchToInterCity = Just driverInfo.canSwitchToInterCity,
        airConditioned = mbAirConditioned
      }

postDriverUpdateServiceTiers ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers ->
    Environment.Flow APISuccess
  )
postDriverUpdateServiceTiers (mbPersonId, _, merchanOperatingCityId) API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers {..} = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchanOperatingCityId

  whenJust airConditioned $ \ac -> checkAndUpdateAirConditioned False ac.isWorking personId cityVehicleServiceTiers
  driverInfo <- QDI.findById personId >>= fromMaybeM DriverInfoNotFound
  vehicle <- QVehicle.findById personId >>= fromMaybeM (VehicleNotFound personId.getId)

  let driverVehicleServiceTierTypes = selectVehicleTierForDriverWithUsageRestriction person driverInfo vehicle cityVehicleServiceTiers
  mbSelectedServiceTiers <-
    driverVehicleServiceTierTypes `forM` \(driverServiceTier, _) -> do
      let isAlreadySelected = driverServiceTier.serviceTierType `elem` vehicle.selectedServiceTiers
          isDefault = vehicle.variant `elem` driverServiceTier.defaultForVehicleVariant
          mbServiceTierDriverRequest = find (\tier -> tier.serviceTierType == driverServiceTier.serviceTierType) tiers
          isSelected = maybe isAlreadySelected (.isSelected) mbServiceTierDriverRequest

      if isSelected || isDefault
        then return $ Just driverServiceTier.serviceTierType
        else return Nothing

  QVehicle.updateSelectedServiceTiers (catMaybes mbSelectedServiceTiers) personId
  when (isJust canSwitchToInterCity || isJust canSwitchToRental) $ do
    let canSwitchToInterCity' = fromMaybe driverInfo.canSwitchToInterCity canSwitchToInterCity
    let canSwitchToRental' = fromMaybe driverInfo.canSwitchToRental canSwitchToRental
    QDI.updateRentalAndInterCitySwitch canSwitchToRental' canSwitchToInterCity' personId

  return Success
