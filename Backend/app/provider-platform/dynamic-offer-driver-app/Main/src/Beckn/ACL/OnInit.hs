{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnInit where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.OnDemand.Utils.Common as Utils hiding (mkStops)
import Beckn.Types.Core.Taxi.OnInit as OnInit
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import Domain.Action.Beckn.Init as DInit
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.Location as DL
import Kernel.Prelude
import Kernel.Utils.Common (encodeToText)
import SharedLogic.FareCalculator

mkOnInitMessage :: DInit.InitRes -> OnInit.OnInitMessage
mkOnInitMessage res = do
  let rb = res.booking
      vehicleVariant = Common.castVariant res.booking.vehicleVariant
      itemId = Common.mkItemId res.transporter.shortId.getShortId res.booking.vehicleVariant
      fareDecimalValue = fromIntegral rb.estimatedFare
      currency = "INR"
      breakup_ =
        mkFareParamsBreakups (OnInit.BreakupItemPrice currency . fromIntegral) OnInit.BreakupItem rb.fareParams
          & filter (Common.filterRequiredBreakups $ DFParams.getFareParametersType rb.fareParams) -- TODO: Remove after roll out
  OnInit.OnInitMessage
    { order =
        OnInit.Order
          { id = res.booking.id.getId,
            items =
              [ OnInit.OrderItem
                  { id = itemId,
                    fulfillment_id = res.booking.quoteId,
                    price =
                      OnInit.Price
                        { currency,
                          value = fareDecimalValue
                        },
                    descriptor =
                      OnInit.Descriptor
                        { short_desc = Just itemId,
                          code = Nothing
                        }
                  }
              ],
            fulfillment =
              OnInit.FulfillmentInfo
                { id = res.booking.quoteId,
                  _type = Common.mkFulfillmentType res.booking.tripCategory,
                  start =
                    OnInit.StartInfo
                      { location =
                          OnInit.Location
                            { gps =
                                OnInit.Gps
                                  { lat = res.booking.fromLocation.lat,
                                    lon = res.booking.fromLocation.lon
                                  },
                              address = castAddress res.booking.fromLocation.address
                            },
                        authorization = Nothing
                      },
                  end =
                    ( \toLocation ->
                        OnInit.StopInfo
                          { location =
                              OnInit.Location
                                { gps =
                                    OnInit.Gps
                                      { lat = toLocation.lat,
                                        lon = toLocation.lon
                                      },
                                  address = castAddress toLocation.address
                                }
                          }
                    )
                      <$> res.booking.toLocation,
                  vehicle =
                    OnInit.Vehicle
                      { category = vehicleVariant
                      },
                  agent =
                    res.driverName >>= \driverName ->
                      Just
                        OnInit.Agent
                          { name = driverName,
                            rateable = True,
                            tags = Nothing,
                            phone = Nothing,
                            image = Nothing
                          }
                },
            state = OnInit.NEW,
            quote =
              OnInit.Quote
                { price =
                    OnInit.QuotePrice
                      { currency,
                        value = fareDecimalValue,
                        offered_value = fareDecimalValue
                      },
                  breakup = Just breakup_
                },
            provider =
              res.driverId >>= \dId ->
                Just
                  OnInit.Provider
                    { id = dId
                    },
            payment =
              OnInit.Payment
                { params =
                    OnInit.PaymentParams
                      { collected_by = OnInit.BPP, --maybe OnInit.BPP (Common.castDPaymentCollector . (.collectedBy)) res.paymentMethodInfo,
                        instrument = Common.castDPaymentInstrument . (.paymentInstrument) <$> res.paymentMethodInfo,
                        currency = currency,
                        amount = Just fareDecimalValue
                      },
                  _type = maybe OnInit.ON_FULFILLMENT (Common.castDPaymentType . (.paymentType)) res.paymentMethodInfo,
                  uri = res.booking.paymentUrl
                }
          }
    }
  where
    castAddress DL.LocationAddress {..} = OnInit.Address {area_code = areaCode, locality = area, ward = Nothing, ..}

mkOnInitMessageV2 :: DInit.InitRes -> Spec.ConfirmReqMessage
mkOnInitMessageV2 res =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder res
    }

tfOrder :: DInit.InitRes -> Spec.Order
tfOrder res =
  Spec.Order
    { orderBilling = Nothing,
      orderCancellation = Nothing,
      orderCancellationTerms = Nothing,
      orderFulfillments = tfFulfillments res,
      orderId = Just res.booking.id.getId,
      orderItems = tfItems res,
      orderPayments = tfPayments res,
      orderProvider = tfProvider res,
      orderQuote = tfQuotation res,
      orderStatus = Nothing
    }

tfFulfillments :: DInit.InitRes -> Maybe [Spec.Fulfillment]
tfFulfillments res =
  Just
    [ Spec.Fulfillment
        { fulfillmentAgent = Nothing,
          fulfillmentCustomer = Nothing,
          fulfillmentId = Just res.booking.quoteId,
          fulfillmentState = Nothing,
          fulfillmentStops = Utils.mkStops' res.booking.fromLocation res.booking.toLocation Nothing,
          fulfillmentTags = Nothing,
          fulfillmentType = Just $ Common.mkFulfillmentType res.booking.tripCategory,
          fulfillmentVehicle = tfVehicle res
        }
    ]

tfItems :: DInit.InitRes -> Maybe [Spec.Item]
tfItems res =
  Just
    [ Spec.Item
        { itemDescriptor = tfItemDescriptor res,
          itemFulfillmentIds = Just [res.booking.quoteId],
          itemId = Just $ Common.mkItemId res.transporter.shortId.getShortId res.booking.vehicleVariant,
          itemLocationIds = Nothing,
          itemPaymentIds = Nothing,
          itemPrice = tfItemPrice res,
          itemTags = Nothing
        }
    ]

tfItemDescriptor :: DInit.InitRes -> Maybe Spec.Descriptor
tfItemDescriptor res =
  Just
    Spec.Descriptor
      { descriptorCode = Just "RIDE", -- TODO : maybe make Enum for it?
        descriptorShortDesc = Just $ Common.mkItemId res.transporter.shortId.getShortId res.booking.vehicleVariant,
        descriptorName = Nothing
      }

tfItemPrice :: DInit.InitRes -> Maybe Spec.Price
tfItemPrice res =
  Just
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Just $ encodeToText res.booking.estimatedFare,
        priceValue = Just $ encodeToText res.booking.estimatedFare
      }

-- TODO: Discuss payment info transmission with ONDC
tfPayments :: DInit.InitRes -> Maybe [Spec.Payment]
tfPayments res = do
  let paymentMethodInfo = res.paymentMethodInfo
  Just
    [ Spec.Payment
        { paymentCollectedBy = Just $ show Enums.SELLER,
          paymentId = Nothing,
          paymentParams = mkParams paymentMethodInfo,
          paymentStatus = Just $ show Enums.NOT_PAID,
          paymentTags = Nothing,
          paymentType = Just $ maybe (show Enums.ON_FULFILLMENT) (Utils.castDPaymentType . (.paymentType)) paymentMethodInfo
        }
    ]
  where
    mkParams _paymentMethodInfo =
      Just
        Spec.PaymentParams
          { paymentParamsAmount = Just $ encodeToText res.booking.estimatedFare,
            paymentParamsBankAccountNumber = Nothing,
            paymentParamsBankCode = Nothing,
            paymentParamsCurrency = Just "INR",
            paymentParamsVirtualPaymentAddress = Nothing
          }

tfProvider :: DInit.InitRes -> Maybe Spec.Provider
tfProvider res = do
  let providerId = res.bppSubscriberId
  return $
    Spec.Provider
      { providerDescriptor = Nothing,
        providerFulfillments = Nothing,
        providerId = providerId,
        providerItems = Nothing,
        providerLocations = Nothing,
        providerPayments = Nothing
      }

tfQuotation :: DInit.InitRes -> Maybe Spec.Quotation
tfQuotation res =
  Just
    Spec.Quotation
      { quotationBreakup = mkQuotationBreakup res,
        quotationPrice = tfQuotationPrice res,
        quotationTtl = Nothing
      }

tfQuotationPrice :: DInit.InitRes -> Maybe Spec.Price
tfQuotationPrice res =
  Just
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Just $ encodeToText res.booking.estimatedFare,
        priceValue = Just $ encodeToText res.booking.estimatedFare
      }

mkQuotationBreakup :: DInit.InitRes -> Maybe [Spec.QuotationBreakupInner]
mkQuotationBreakup res =
  let rb = res.booking
      fareParams = mkFareParamsBreakups mkPrice mkQuotationBreakupInner rb.fareParams
   in Just $ filter (filterRequiredBreakups $ DFParams.getFareParametersType rb.fareParams) fareParams -- TODO: Remove after roll out
  where
    mkPrice money =
      Just
        Spec.Price
          { priceComputedValue = Nothing,
            priceCurrency = Just "INR",
            priceMaximumValue = Nothing,
            priceMinimumValue = Nothing,
            priceOfferedValue = Just $ encodeToText money,
            priceValue = Just $ encodeToText money
          }

    mkQuotationBreakupInner title price =
      Spec.QuotationBreakupInner
        { quotationBreakupInnerPrice = price,
          quotationBreakupInnerTitle = Just title
        }

    filterRequiredBreakups :: DFParams.FareParametersType -> Spec.QuotationBreakupInner -> Bool
    filterRequiredBreakups fParamsType breakup = do
      case fParamsType of
        DFParams.Progressive ->
          breakup.quotationBreakupInnerTitle == Just (show Enums.BASE_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.SERVICE_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.DEAD_KILOMETER_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.EXTRA_DISTANCE_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.DRIVER_SELECTED_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.CUSTOMER_SELECTED_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.TOTAL_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.WAITING_OR_PICKUP_CHARGES)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.EXTRA_TIME_FARE)
        DFParams.Slab ->
          breakup.quotationBreakupInnerTitle == Just (show Enums.BASE_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.SERVICE_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.WAITING_OR_PICKUP_CHARGES)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.PLATFORM_FEE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.SGST)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.CGST)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.FIXED_GOVERNMENT_RATE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.CUSTOMER_SELECTED_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.TOTAL_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.NIGHT_SHIFT_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.EXTRA_TIME_FARE)
        DFParams.Rental ->
          breakup.quotationBreakupInnerTitle == Just (show Enums.BASE_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.SERVICE_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.DEAD_KILOMETER_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.EXTRA_DISTANCE_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.TIME_BASED_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.DRIVER_SELECTED_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.CUSTOMER_SELECTED_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.TOTAL_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.NIGHT_SHIFT_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.WAITING_OR_PICKUP_CHARGES)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.EXTRA_TIME_FARE)

tfVehicle :: DInit.InitRes -> Maybe Spec.Vehicle
tfVehicle res = do
  let (category, variant) = Utils.castVariant res.booking.vehicleVariant
  Just
    Spec.Vehicle
      { vehicleCategory = Just category,
        vehicleVariant = Just variant,
        vehicleColor = Nothing,
        vehicleMake = Nothing,
        vehicleModel = Nothing,
        vehicleRegistration = Nothing
      }
