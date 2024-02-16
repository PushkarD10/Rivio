{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnSearch
  ( DOnSearchReq (..),
    ProviderInfo (..),
    EstimateInfo (..),
    DEstimate.FareRange (..),
    QuoteInfo (..),
    QuoteDetails (..),
    OneWayQuoteDetails (..),
    OneWaySpecialZoneQuoteDetails (..),
    InterCityQuoteDetails (..),
    RentalQuoteDetails (..),
    EstimateBreakupInfo (..),
    BreakupPriceInfo (..),
    NightShiftInfo (..),
    WaitingChargesInfo (..),
    onSearch,
    validateRequest,
  )
where

import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RentalDetails as DRentalDetails
import Domain.Types.SearchRequest
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SpecialZoneQuote as DSpecialZoneQuote
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant
import Environment
import Kernel.Beam.Functions
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSearchReq
import Tools.Error
import Tools.Event
import qualified Tools.Metrics as Metrics

data DOnSearchReq = DOnSearchReq
  { requestId :: Id DSearchReq.SearchRequest,
    providerInfo :: ProviderInfo,
    estimatesInfo :: [EstimateInfo],
    quotesInfo :: [QuoteInfo],
    paymentMethodsInfo :: [DMPM.PaymentMethodInfo]
  }

data ValidatedOnSearchReq = ValidatedOnSearchReq
  { requestId :: Id DSearchReq.SearchRequest,
    providerInfo :: ProviderInfo,
    estimatesInfo :: [EstimateInfo],
    quotesInfo :: [QuoteInfo],
    _searchRequest :: SearchRequest,
    merchant :: DMerchant.Merchant,
    paymentMethodsInfo :: [DMPM.PaymentMethodInfo]
  }

data ProviderInfo = ProviderInfo
  { providerId :: Text,
    name :: Text,
    url :: BaseUrl,
    mobileNumber :: Text,
    ridesCompleted :: Int
  }

data EstimateInfo = EstimateInfo
  { bppEstimateId :: Id DEstimate.BPPEstimate,
    vehicleVariant :: VehicleVariant,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    itemId :: Text,
    totalFareRange :: DEstimate.FareRange,
    descriptions :: [Text],
    estimateBreakupList :: [EstimateBreakupInfo],
    nightShiftInfo :: Maybe NightShiftInfo,
    waitingCharges :: Maybe WaitingChargesInfo,
    driversLocation :: [LatLong],
    specialLocationTag :: Maybe Text
  }

data NightShiftInfo = NightShiftInfo
  { nightShiftCharge :: Money,
    oldNightShiftCharge :: Maybe Centesimal,
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }

newtype WaitingChargesInfo = WaitingChargesInfo
  { waitingChargePerMin :: Maybe Money
  }

data EstimateBreakupInfo = EstimateBreakupInfo
  { title :: Text,
    price :: BreakupPriceInfo
  }

data BreakupPriceInfo = BreakupPriceInfo
  { currency :: Text,
    value :: Money
  }

data QuoteInfo = QuoteInfo
  { vehicleVariant :: VehicleVariant,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    quoteDetails :: QuoteDetails,
    itemId :: Text,
    descriptions :: [Text],
    specialLocationTag :: Maybe Text
  }

data QuoteDetails
  = OneWayDetails OneWayQuoteDetails
  | InterCityDetails InterCityQuoteDetails
  | RentalDetails RentalQuoteDetails
  | OneWaySpecialZoneDetails OneWaySpecialZoneQuoteDetails

newtype OneWayQuoteDetails = OneWayQuoteDetails
  { distanceToNearestDriver :: HighPrecMeters
  }

newtype OneWaySpecialZoneQuoteDetails = OneWaySpecialZoneQuoteDetails
  { quoteId :: Text
  }

newtype InterCityQuoteDetails = InterCityQuoteDetails
  { quoteId :: Text
  }

data RentalQuoteDetails = RentalQuoteDetails
  { id :: Text,
    baseFare :: Money,
    perHourCharge :: Money,
    perExtraMinRate :: Money,
    includedKmPerHr :: Kilometers,
    plannedPerKmRate :: Money,
    perExtraKmRate :: Money,
    nightShiftInfo :: Maybe NightShiftInfo
  }

validateRequest :: DOnSearchReq -> Flow ValidatedOnSearchReq
validateRequest DOnSearchReq {..} = do
  _searchRequest <- runInReplica $ QSearchReq.findById requestId >>= fromMaybeM (SearchRequestDoesNotExist requestId.getId)
  merchant <- QMerch.findById _searchRequest.merchantId >>= fromMaybeM (MerchantNotFound _searchRequest.merchantId.getId)
  return $ ValidatedOnSearchReq {..}

onSearch ::
  Text ->
  ValidatedOnSearchReq ->
  Flow ()
onSearch transactionId ValidatedOnSearchReq {..} = do
  Metrics.finishSearchMetrics merchant.name transactionId
  now <- getCurrentTime

  let merchantOperatingCityId = _searchRequest.merchantOperatingCityId
  estimates <- traverse (buildEstimate providerInfo now _searchRequest) (filterEstimtesByPrefference estimatesInfo)
  quotes <- traverse (buildQuote requestId providerInfo now _searchRequest) (filterQuotesByPrefference quotesInfo)
  merchantPaymentMethods <- CQMPM.findAllByMerchantOperatingCityId merchantOperatingCityId
  let paymentMethods = intersectPaymentMethods paymentMethodsInfo merchantPaymentMethods
  forM_ estimates $ \est -> do
    triggerEstimateEvent EstimateEventData {estimate = est, personId = _searchRequest.riderId, merchantId = _searchRequest.merchantId}
  _ <- QEstimate.createMany estimates
  _ <- QQuote.createMany quotes
  _ <- QPFS.updateStatus _searchRequest.riderId DPFS.GOT_ESTIMATE {requestId = _searchRequest.id, validTill = _searchRequest.validTill}
  _ <- QSearchReq.updatePaymentMethods _searchRequest.id (paymentMethods <&> (.id))
  QPFS.clearCache _searchRequest.riderId
  where
    {- Author: Hemant Mangla
      Rider quotes and estimates are filtered based on their preferences.
      Currently, riders preferring rentals receive only rental options.
      Ideally, rental options should also be available for one-way preferences, but frontend limitations prevent this.
      Once the frontend is updated for compatibility, we can extend this feature.
    -}
    filterQuotesByPrefference :: [QuoteInfo] -> [QuoteInfo]
    filterQuotesByPrefference _quotesInfo =
      case _searchRequest.riderPreferredOption of
        Rental -> filter (\qInfo -> case qInfo.quoteDetails of RentalDetails _ -> True; _ -> False) _quotesInfo
        _ -> filter (\qInfo -> case qInfo.quoteDetails of RentalDetails _ -> False; _ -> True) _quotesInfo

    filterEstimtesByPrefference :: [EstimateInfo] -> [EstimateInfo]
    filterEstimtesByPrefference _estimateInfo =
      case _searchRequest.riderPreferredOption of
        Rental -> []
        OneWay ->
          case quotesInfo of
            (qInfo : _) ->
              case qInfo.quoteDetails of
                OneWaySpecialZoneDetails _ -> []
                _ -> _estimateInfo
            _ -> _estimateInfo

buildEstimate ::
  MonadFlow m =>
  ProviderInfo ->
  UTCTime ->
  SearchRequest ->
  EstimateInfo ->
  m DEstimate.Estimate
buildEstimate providerInfo now _searchRequest EstimateInfo {..} = do
  uid <- generateGUID
  tripTerms <- buildTripTerms descriptions
  estimateBreakupList' <- buildEstimateBreakUp estimateBreakupList uid
  pure
    DEstimate.Estimate
      { id = uid,
        requestId = _searchRequest.id,
        merchantId = Just _searchRequest.merchantId,
        merchantOperatingCityId = Just _searchRequest.merchantOperatingCityId,
        providerMobileNumber = providerInfo.mobileNumber,
        providerName = providerInfo.name,
        providerCompletedRidesCount = providerInfo.ridesCompleted,
        providerId = providerInfo.providerId,
        providerUrl = providerInfo.url,
        estimatedDistance = _searchRequest.distance,
        estimatedDuration = _searchRequest.estimatedRideDuration,
        device = _searchRequest.device,
        createdAt = now,
        updatedAt = now,
        status = DEstimate.NEW,
        estimateBreakupList = estimateBreakupList',
        driversLocation = driversLocation,
        nightShiftInfo =
          nightShiftInfo <&> \nightShiftInfo' ->
            DEstimate.NightShiftInfo
              { nightShiftCharge = nightShiftInfo'.nightShiftCharge,
                oldNightShiftCharge = nightShiftInfo'.oldNightShiftCharge, -- TODO: Doesn't make sense, to be removed
                nightShiftStart = nightShiftInfo'.nightShiftStart,
                nightShiftEnd = nightShiftInfo'.nightShiftEnd
              },
        waitingCharges =
          DEstimate.WaitingCharges
            { waitingChargePerMin = waitingCharges >>= (.waitingChargePerMin)
            },
        ..
      }

buildQuote ::
  MonadFlow m =>
  Id DSearchReq.SearchRequest ->
  ProviderInfo ->
  UTCTime ->
  SearchRequest ->
  QuoteInfo ->
  m DQuote.Quote
buildQuote requestId providerInfo now _searchRequest QuoteInfo {..} = do
  uid <- generateGUID
  tripTerms <- buildTripTerms descriptions
  quoteDetails' <- case quoteDetails of
    OneWayDetails oneWayDetails ->
      pure.DQuote.OneWayDetails $ mkOneWayQuoteDetails oneWayDetails
    RentalDetails rentalDetails -> do
      DQuote.RentalDetails <$> buildRentalDetails rentalDetails
    OneWaySpecialZoneDetails details -> do
      DQuote.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneQuoteDetails details
    InterCityDetails details -> do
      DQuote.InterCityDetails <$> buildInterCityQuoteDetails details
  pure
    DQuote.Quote
      { id = uid,
        providerMobileNumber = providerInfo.mobileNumber,
        providerName = providerInfo.name,
        providerCompletedRidesCount = providerInfo.ridesCompleted,
        providerId = providerInfo.providerId,
        providerUrl = providerInfo.url,
        createdAt = now,
        quoteDetails = quoteDetails',
        merchantId = _searchRequest.merchantId,
        merchantOperatingCityId = _searchRequest.merchantOperatingCityId,
        ..
      }

mkOneWayQuoteDetails :: OneWayQuoteDetails -> DQuote.OneWayQuoteDetails
mkOneWayQuoteDetails OneWayQuoteDetails {..} = DQuote.OneWayQuoteDetails {..}

buildOneWaySpecialZoneQuoteDetails :: MonadFlow m => OneWaySpecialZoneQuoteDetails -> m DSpecialZoneQuote.SpecialZoneQuote
buildOneWaySpecialZoneQuoteDetails OneWaySpecialZoneQuoteDetails {..} = do
  id <- generateGUID
  pure DSpecialZoneQuote.SpecialZoneQuote {..}

buildInterCityQuoteDetails :: MonadFlow m => InterCityQuoteDetails -> m DSpecialZoneQuote.SpecialZoneQuote
buildInterCityQuoteDetails InterCityQuoteDetails {..} = do
  id <- generateGUID
  pure DSpecialZoneQuote.SpecialZoneQuote {..}

buildRentalDetails :: MonadFlow m => RentalQuoteDetails -> m DRentalDetails.RentalDetails
buildRentalDetails RentalQuoteDetails {..} = do
  let quoteId = Id id
      nightShiftinfo' =
        ( \nightShiftInfo'' ->
            DRentalDetails.NightShiftInfo nightShiftInfo''.nightShiftCharge Nothing nightShiftInfo''.nightShiftStart nightShiftInfo''.nightShiftEnd
        )
          <$> nightShiftInfo
  pure DRentalDetails.RentalDetails {id = quoteId, nightShiftInfo = nightShiftinfo', ..}

buildTripTerms ::
  MonadFlow m =>
  [Text] ->
  m (Maybe DTripTerms.TripTerms)
buildTripTerms [] = pure Nothing
buildTripTerms descriptions = do
  id <- generateGUID
  pure . Just $ DTripTerms.TripTerms {..}

buildEstimateBreakUp ::
  MonadFlow m =>
  [EstimateBreakupInfo] ->
  Id DEstimate.Estimate ->
  m [DEstimate.EstimateBreakup]
buildEstimateBreakUp estimatesItems estId =
  estimatesItems
    `for` \estimateItem -> do
      id <- generateGUID
      price' <- mkEstimatePrice estimateItem.price
      pure
        DEstimate.EstimateBreakup
          { title = estimateItem.title,
            price = price',
            estimateId = estId,
            ..
          }

mkEstimatePrice ::
  MonadFlow m =>
  BreakupPriceInfo ->
  m DEstimate.EstimateBreakupPrice
mkEstimatePrice BreakupPriceInfo {..} = pure DEstimate.EstimateBreakupPrice {..}

intersectPaymentMethods :: [DMPM.PaymentMethodInfo] -> [DMPM.MerchantPaymentMethod] -> [DMPM.MerchantPaymentMethod]
intersectPaymentMethods providerPaymentMethods = filter (\mpm -> any (compareMerchantPaymentMethod mpm) providerPaymentMethods)

compareMerchantPaymentMethod :: DMPM.MerchantPaymentMethod -> DMPM.PaymentMethodInfo -> Bool
compareMerchantPaymentMethod DMPM.MerchantPaymentMethod {..} providerPaymentMethod =
  paymentType == providerPaymentMethod.paymentType
    && paymentInstrument == providerPaymentMethod.paymentInstrument
    && collectedBy == providerPaymentMethod.collectedBy
