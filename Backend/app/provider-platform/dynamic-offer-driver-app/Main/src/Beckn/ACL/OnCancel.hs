{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnCancel
  ( buildOnCancelMessageV2,
    module Reexport,
  )
where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.OnDemand.Utils.Common as BUtils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as CU
import BecknV2.OnDemand.Utils.Payment
import qualified Data.List as L
import Domain.Types
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.DriverQuote as DQ
import qualified Domain.Types.Merchant as DM
import Domain.Types.OnCancel as Reexport
import qualified Domain.Types.OnCancel as OC
import Domain.Types.Ride
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FareCalculator
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRiderDetails
import Tools.Error

buildOnCancelMessageV2 ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  DM.Merchant ->
  Maybe Context.City ->
  Maybe Context.Country ->
  Text ->
  OnCancelBuildReq ->
  m Spec.OnCancelReq
buildOnCancelMessageV2 merchant mbBapCity mbBapCountry cancelStatus (OC.BookingCancelledBuildReqV2 OC.DBookingCancelledReqV2 {..}) = do
  msgId <- generateGUID
  let bppId = getShortId $ merchant.subscriberId
      city = fromMaybe merchant.city mbBapCity
      country = fromMaybe merchant.country mbBapCountry
  bppUri <- BUtils.mkBppUri merchant.id.getId
  riderId <- booking.riderId & fromMaybeM (BookingFieldNotPresent "rider_id")
  ride <- QRide.findOneByBookingId booking.id >>= fromMaybeM (RideDoesNotExist booking.id.getId)
  riderDetails <- runInReplica $ QRiderDetails.findById riderId >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
  driverQuote <- QDQ.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
  customerPhoneNo <- decrypt riderDetails.mobileNumber
  let vehicleCategory = BUtils.mapVariantToVehicle booking.vehicleVariant
  becknConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id (show Context.MOBILITY) vehicleCategory >>= fromMaybeM (InternalError "Beckn Config not found")
  buildOnCancelReq Context.ON_CANCEL Context.MOBILITY msgId bppId bppUri city country cancelStatus merchant driverQuote customerPhoneNo (OC.BookingCancelledBuildReqV2 OC.DBookingCancelledReqV2 {..}) ride.status becknConfig

buildOnCancelReq ::
  (MonadFlow m, EncFlow m r) =>
  Context.Action ->
  Context.Domain ->
  Text ->
  Text ->
  BaseUrl ->
  Context.City ->
  Context.Country ->
  Text ->
  DM.Merchant ->
  DQ.DriverQuote ->
  Text ->
  OC.OnCancelBuildReq ->
  RideStatus ->
  DBC.BecknConfig ->
  m Spec.OnCancelReq
buildOnCancelReq action domain messageId bppSubscriberId bppUri city country cancelStatus merchant driverQuote customerPhoneNo (OC.BookingCancelledBuildReqV2 OC.DBookingCancelledReqV2 {..}) rideStatus becknConfig = do
  context <- CU.buildContextV2 action domain messageId (Just booking.transactionId) booking.bapId booking.bapUri (Just bppSubscriberId) (Just bppUri) city country (Just "PT2M")
  pure $
    Spec.OnCancelReq
      { onCancelReqError = Nothing,
        onCancelReqContext = context,
        onCancelReqMessage = buildOnCancelMessageReqV2 booking cancelStatus cancellationSource merchant driverQuote customerPhoneNo becknConfig rideStatus
      }

buildOnCancelMessageReqV2 :: DRB.Booking -> Text -> SBCR.CancellationSource -> DM.Merchant -> DQ.DriverQuote -> Text -> DBC.BecknConfig -> RideStatus -> Maybe Spec.ConfirmReqMessage
buildOnCancelMessageReqV2 booking cancelStatus cancellationSource merchant driverQuote customerPhoneNo becknConfig rideStatus = do
  Just $
    Spec.ConfirmReqMessage
      { confirmReqMessageOrder = tfOrder booking cancelStatus cancellationSource merchant driverQuote customerPhoneNo becknConfig rideStatus
      }

tfOrder :: DRB.Booking -> Text -> SBCR.CancellationSource -> DM.Merchant -> DQ.DriverQuote -> Text -> DBC.BecknConfig -> RideStatus -> Spec.Order
tfOrder booking cancelStatus cancellationSource merchant driverQuote customerPhoneNo becknConfig rideStatus = do
  Spec.Order
    { orderId = Just booking.id.getId,
      orderStatus = Just cancelStatus,
      orderFulfillments = tfFulfillments booking driverQuote customerPhoneNo rideStatus,
      orderCancellation = tfCancellation cancellationSource,
      orderBilling = Nothing,
      orderCancellationTerms = Just $ tfCancellationTerms becknConfig,
      orderItems = tfItems booking merchant,
      orderPayments = tfPayments driverQuote merchant becknConfig,
      orderProvider = Nothing,
      orderQuote = tfQuotation booking,
      orderCreatedAt = Just booking.createdAt,
      orderUpdatedAt = Just booking.updatedAt
    }

tfFulfillments :: DRB.Booking -> DQ.DriverQuote -> Text -> RideStatus -> Maybe [Spec.Fulfillment]
tfFulfillments booking driverQuote customerPhoneNo rideStatus = do
  let stops = BUtils.mkStops' booking.fromLocation booking.toLocation booking.specialZoneOtpCode
  Just
    [ Spec.Fulfillment
        { fulfillmentId = Just booking.quoteId,
          fulfillmentState = mkFulfillmentState rideStatus,
          fulfillmentStops = stops,
          fulfillmentType = Just $ BUtils.mkFulfillmentType booking.tripCategory,
          fulfillmentAgent = tfAgent booking driverQuote,
          fulfillmentCustomer = tfCustomer booking customerPhoneNo,
          fulfillmentTags = Nothing,
          fulfillmentVehicle = tfVehicle booking
        }
    ]
  where
    mkFulfillmentState rideStatus' =
      Just $
        Spec.FulfillmentState
          { fulfillmentStateDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show (BUtils.mapRideStatus rideStatus'),
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  }
          }

tfQuotation :: DRB.Booking -> Maybe Spec.Quotation
tfQuotation booking =
  Just
    Spec.Quotation
      { quotationBreakup = mkQuotationBreakup booking,
        quotationPrice = tfQuotationPrice booking,
        quotationTtl = Nothing
      }

tfQuotationPrice :: DRB.Booking -> Maybe Spec.Price
tfQuotationPrice booking =
  Just
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Just $ encodeToText booking.estimatedFare,
        priceValue = Just $ encodeToText booking.estimatedFare
      }

mkQuotationBreakup :: DRB.Booking -> Maybe [Spec.QuotationBreakupInner]
mkQuotationBreakup booking =
  -- TODO::Beckn, `quotationBreakupInnerTitle` may not be according to spec.
  Just $
    mkFareParamsBreakups mkPrice mkQuotationBreakupInner booking.fareParams
  where
    mkPrice money =
      Just
        Spec.Price
          { priceComputedValue = Nothing,
            priceCurrency = Just "INR",
            priceMaximumValue = Nothing,
            priceMinimumValue = Nothing,
            priceOfferedValue = Nothing,
            priceValue = Just $ encodeToText money
          }

    mkQuotationBreakupInner title price =
      Spec.QuotationBreakupInner
        { quotationBreakupInnerPrice = price,
          quotationBreakupInnerTitle = Just title
        }

tfPayments :: DQ.DriverQuote -> DM.Merchant -> DBC.BecknConfig -> Maybe [Spec.Payment]
tfPayments res merchant bppConfig = do
  let amount = Just $ show res.estimatedFare.getMoney
  let mkParams :: (Maybe BknPaymentParams) = decodeFromText =<< bppConfig.paymentParamsJson
  Just $ L.singleton $ mkPayment (show merchant.city) (show bppConfig.collectedBy) Enums.NOT_PAID amount Nothing mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee

tfItems :: DRB.Booking -> DM.Merchant -> Maybe [Spec.Item]
tfItems booking merchant = do
  Just
    [ Spec.Item
        { itemDescriptor = Nothing,
          itemFulfillmentIds = Just [booking.quoteId],
          itemId = Just $ Common.mkItemId merchant.shortId.getShortId booking.vehicleVariant,
          itemLocationIds = Nothing,
          itemPaymentIds = Nothing,
          itemPrice = tfItemPrice booking,
          itemTags = Nothing
        }
    ]

tfItemPrice :: DRB.Booking -> Maybe Spec.Price
tfItemPrice booking =
  Just
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Nothing,
        priceValue = Just $ encodeToText booking.estimatedFare
      }

tfVehicle :: DRB.Booking -> Maybe Spec.Vehicle
tfVehicle booking = do
  let (category, variant) = BUtils.castVariant booking.vehicleVariant
  Just $
    Spec.Vehicle
      { vehicleCategory = Just category,
        vehicleVariant = Just variant,
        vehicleColor = Nothing,
        vehicleMake = Nothing,
        vehicleModel = Nothing,
        vehicleRegistration = Nothing
      }

tfAgent :: DRB.Booking -> DQ.DriverQuote -> Maybe Spec.Agent
tfAgent _booking driverQuote = do
  Just $
    Spec.Agent
      { agentContact = Nothing,
        agentPerson =
          Just
            Spec.Person
              { personId = Nothing,
                personImage = Nothing,
                personName = Just driverQuote.driverName,
                personTags = Nothing
              }
      }

tfCustomer :: DRB.Booking -> Text -> Maybe Spec.Customer
tfCustomer booking customerPhoneNo = do
  Just $
    Spec.Customer
      { customerContact =
          Just
            Spec.Contact
              { contactPhone = Just customerPhoneNo
              },
        customerPerson =
          Just
            Spec.Person
              { personId = Nothing,
                personImage = Nothing,
                personName = booking.riderName,
                personTags = Nothing
              }
      }

tfCancellation :: SBCR.CancellationSource -> Maybe Spec.Cancellation
tfCancellation cancellationSource =
  Just $
    Spec.Cancellation
      { cancellationCancelledBy = castCancellatonSource cancellationSource
      }
  where
    castCancellatonSource = \case
      SBCR.ByUser -> Just (show Enums.CONSUMER)
      SBCR.ByDriver -> Just (show Enums.PROVIDER)
      _ -> Just (show Enums.PROVIDER) -- if it is cancelled by any other source like by ByMerchant, ByAllocator or ByApplication then we are considering as ByProvider

tfCancellationTerms :: DBC.BecknConfig -> [Spec.CancellationTerm]
tfCancellationTerms becknConfig =
  L.singleton
    Spec.CancellationTerm
      { cancellationTermCancellationFee = BUtils.tfCancellationFee becknConfig.cancellationFeeAmount,
        cancellationTermFulfillmentState = BUtils.tfFulfillmentState Enums.RIDE_CANCELLED,
        cancellationTermReasonRequired = Just False -- TODO : Make true if reason parsing is added
      }
