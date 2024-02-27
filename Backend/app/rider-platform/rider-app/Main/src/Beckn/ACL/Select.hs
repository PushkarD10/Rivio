{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Select (buildSelectReqV2) where

import qualified Beckn.OnDemand.Utils.Common as UCommon
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import BecknV2.Utils
import Control.Lens ((%~))
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Action.UI.Select as DSelect
import qualified Domain.Types.Location as Location
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.ValueAddNP as CQVNP
import Tools.Error

buildSelectReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl], CacheFlow m r, EsqDBFlow m r) =>
  DSelect.DSelectRes ->
  m Spec.SelectReq
buildSelectReqV2 dSelectRes = do
  endLoc <- dSelectRes.searchRequest.toLocation & fromMaybeM (InternalError "To location address not found")
  isValueAddNP <- CQVNP.isValueAddNP dSelectRes.providerId
  let message = buildSelectReqMessage dSelectRes endLoc isValueAddNP
      messageId = dSelectRes.estimate.bppEstimateId.getId
      transactionId = dSelectRes.searchRequest.id.getId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack dSelectRes.merchant.id.getId)
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle dSelectRes.merchant.id "MOBILITY" (UCommon.mapVariantToVehicle dSelectRes.variant) >>= fromMaybeM (InternalError "Beckn Config not found")
  ttlInInt <- bapConfig.selectTTLSec & fromMaybeM (InternalError "Invalid ttl")
  let ttlToNominalDiffTime = intToNominalDiffTime ttlInInt
      ttlToISO8601Duration = formatTimeDifference ttlToNominalDiffTime
  context <- ContextV2.buildContextV2 Context.SELECT Context.MOBILITY messageId (Just transactionId) dSelectRes.merchant.bapId bapUrl (Just dSelectRes.providerId) (Just dSelectRes.providerUrl) dSelectRes.city dSelectRes.merchant.country (Just ttlToISO8601Duration)
  pure $ Spec.SelectReq {selectReqContext = context, selectReqMessage = message}

buildSelectReqMessage :: DSelect.DSelectRes -> Location.Location -> Bool -> Spec.ConfirmReqMessage
buildSelectReqMessage res endLoc isValueAddNP =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder res endLoc isValueAddNP
    }

tfOrder :: DSelect.DSelectRes -> Location.Location -> Bool -> Spec.Order
tfOrder res endLoc isValueAddNP =
  let orderBilling = Nothing
      orderCancellation = Nothing
      orderCancellationTerms = Nothing
      orderId = Nothing
      orderPayments = Nothing
      orderProvider = Nothing
      orderQuote = Nothing
      orderStatus = Nothing
      startLoc = res.searchRequest.fromLocation
      orderItem = tfOrderItem res isValueAddNP
      orderFulfillment = tfFulfillment res startLoc endLoc
      orderCreatedAt = Nothing
      orderUpdatedAt = Nothing
   in Spec.Order
        { orderFulfillments = Just [orderFulfillment],
          orderItems = Just [orderItem],
          ..
        }

tfFulfillment :: DSelect.DSelectRes -> Location.Location -> Location.Location -> Spec.Fulfillment
tfFulfillment res startLoc endLoc =
  let fulfillmentAgent = Nothing
      fulfillmentTags = Nothing
      fulfillmentState = Nothing
      fulfillmentCustomer = Nothing
      fulfillmentId = Just res.estimate.bppEstimateId.getId
      fulfillmentType = Just $ show Enums.DELIVERY
      fulfillmentStops = mkStops startLoc endLoc
      fulfillmentVehicle = tfVehicle res
   in Spec.Fulfillment
        { fulfillmentStops = fulfillmentStops,
          fulfillmentVehicle = Just fulfillmentVehicle,
          ..
        }

mkStops :: Location.Location -> Location.Location -> Maybe [Spec.Stop]
mkStops origin destination =
  let originGps = Gps.Gps {lat = origin.lat, lon = origin.lon}
      destinationGps = Gps.Gps {lat = destination.lat, lon = destination.lon}
   in Just
        [ Spec.Stop
            { stopLocation =
                Just $
                  Spec.Location
                    { locationAddress = Just $ UCommon.mkAddress origin.address,
                      locationAreaCode = origin.address.areaCode,
                      locationCity = Just $ Spec.City Nothing origin.address.city,
                      locationCountry = Just $ Spec.Country Nothing origin.address.country,
                      locationGps = A.decode $ A.encode originGps,
                      locationState = Just $ Spec.State origin.address.state,
                      locationId = Nothing,
                      locationUpdatedAt = Nothing
                    },
              stopType = Just $ show Enums.START,
              stopAuthorization = Nothing,
              stopTime = Nothing
            },
          Spec.Stop
            { stopLocation =
                Just $
                  Spec.Location
                    { locationAddress = Just $ UCommon.mkAddress destination.address,
                      locationAreaCode = destination.address.areaCode,
                      locationCity = Just $ Spec.City Nothing destination.address.city,
                      locationCountry = Just $ Spec.Country Nothing destination.address.country,
                      locationGps = A.decode $ A.encode destinationGps,
                      locationState = Just $ Spec.State destination.address.state,
                      locationId = Nothing,
                      locationUpdatedAt = Nothing
                    },
              stopType = Just $ show Enums.END,
              stopAuthorization = Nothing,
              stopTime = Nothing
            }
        ]

tfVehicle :: DSelect.DSelectRes -> Spec.Vehicle
tfVehicle res =
  let (category, variant) = UCommon.castVehicleVariant res.variant
      vehicleColor = Nothing
      vehicleMake = Nothing
      vehicleModel = Nothing
      vehicleRegistration = Nothing
      vehicleVariant = Just variant
      vehicleCategory = Just category
   in Spec.Vehicle {..}

tfOrderItem :: DSelect.DSelectRes -> Bool -> Spec.Item
tfOrderItem res isValueAddNP =
  let itemDescriptor = Nothing
      itemFulfillmentIds = Nothing
      itemLocationIds = Nothing
      itemPaymentIds = Nothing
      itemId = Just res.estimate.itemId
      itemTags =
        if isValueAddNP
          then Just $ mkItemTags res
          else Nothing
      itemPrice = tfPrice res
   in Spec.Item
        { itemPrice = Just itemPrice,
          ..
        }

mkItemTags :: DSelect.DSelectRes -> [Spec.TagGroup]
mkItemTags res =
  let itemTags = [mkAutoAssignEnabledTagGroup res]
      itemTags' = if isJust res.customerExtraFee then mkCustomerTipTagGroup res : itemTags else itemTags
   in itemTags'

mkCustomerTipTagGroup :: DSelect.DSelectRes -> Spec.TagGroup
mkCustomerTipTagGroup res =
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tags.CUSTOMER_TIP_INFO,
              descriptorName = Just "Customer Tip Info",
              descriptorShortDesc = Nothing
            },
      tagGroupList =
        Just
          [ Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = (\_ -> Just $ show Tags.CUSTOMER_TIP) =<< res.customerExtraFee,
                        descriptorName = (\_ -> Just "Customer Tip") =<< res.customerExtraFee,
                        descriptorShortDesc = Nothing
                      },
                tagDisplay = Just False,
                tagValue = (\charges -> Just $ show charges.getMoney) =<< res.customerExtraFee
              }
          ]
    }

mkAutoAssignEnabledTagGroup :: DSelect.DSelectRes -> Spec.TagGroup
mkAutoAssignEnabledTagGroup res =
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tags.AUTO_ASSIGN_ENABLED,
              descriptorName = Just "Auto Assign Enabled",
              descriptorShortDesc = Nothing
            },
      tagGroupList =
        Just
          [ Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tags.IS_AUTO_ASSIGN_ENABLED,
                        descriptorName = Just "Auto Assign Enabled",
                        descriptorShortDesc = Nothing
                      },
                tagDisplay = Just False,
                tagValue = Just $ show res.autoAssignEnabled
              }
          ]
    }

tfPrice :: DSelect.DSelectRes -> Spec.Price
tfPrice res =
  let priceCurrency = Just "INR"
      priceValue = Just $ show res.estimate.estimatedFare.getMoney
      priceComputedValue = Nothing
      priceMaximumValue = Nothing
      priceMinimumValue = Nothing
      priceOfferedValue = Nothing
   in Spec.Price {..}
