{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.FRFSTicketService where

import qualified Data.Maybe
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSTicket
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Station
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Juspay.Types.CreateOrder
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data FRFSBookingPaymentAPI = FRFSBookingPaymentAPI {paymentOrder :: Data.Maybe.Maybe Kernel.External.Payment.Juspay.Types.CreateOrder.CreateOrderResp, status :: API.Types.UI.FRFSTicketService.FRFSBookingPaymentStatusAPI}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FRFSBookingPaymentStatusAPI = NEW | PENDING | SUCCESS | FAILURE | REFUND_PENDING | REFUNDED deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data FRFSCanCancelStatus = FRFSCanCancelStatus
  { cancellationCharges :: Data.Maybe.Maybe Kernel.Types.Common.HighPrecMoney,
    isCancellable :: Data.Maybe.Maybe Kernel.Prelude.Bool,
    refundAmount :: Data.Maybe.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FRFSCancelStatus = FRFSCancelStatus {cancellationCharges :: Data.Maybe.Maybe Kernel.Types.Common.HighPrecMoney, refundAmount :: Data.Maybe.Maybe Kernel.Types.Common.HighPrecMoney}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FRFSConfigAPIRes = FRFSConfigAPIRes
  { bookingEndTime :: Kernel.Prelude.UTCTime,
    bookingStartTime :: Kernel.Prelude.UTCTime,
    customDates :: [Data.Text.Text],
    customEndTime :: Data.Text.Text,
    discount :: Kernel.Prelude.Int,
    metroStationTtl :: Kernel.Prelude.Int,
    oneWayTicketLimit :: Kernel.Prelude.Int,
    roundTripTicketLimit :: Kernel.Prelude.Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data FRFSQuoteAPIRes = FRFSQuoteAPIRes
  { _type :: Domain.Types.FRFSQuote.FRFSQuoteType,
    price :: Kernel.Types.Common.HighPrecMoney,
    priceWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    quantity :: Kernel.Prelude.Int,
    quoteId :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote,
    stations :: [API.Types.UI.FRFSTicketService.FRFSStationAPI],
    validTill :: Kernel.Prelude.UTCTime,
    vehicleType :: Domain.Types.Station.FRFSVehicleType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FRFSSearchAPIReq = FRFSSearchAPIReq {fromStationCode :: Data.Text.Text, quantity :: Kernel.Prelude.Int, toStationCode :: Data.Text.Text} deriving (Generic, ToJSON, FromJSON, ToSchema)

data FRFSSearchAPIRes = FRFSSearchAPIRes {searchId :: Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch} deriving (Generic, ToJSON, FromJSON, ToSchema)

data FRFSStationAPI = FRFSStationAPI
  { address :: Data.Maybe.Maybe Data.Text.Text,
    code :: Data.Text.Text,
    color :: Data.Maybe.Maybe Data.Text.Text,
    lat :: Data.Maybe.Maybe Kernel.Prelude.Double,
    lon :: Data.Maybe.Maybe Kernel.Prelude.Double,
    name :: Data.Text.Text,
    sequenceNum :: Data.Maybe.Maybe Kernel.Prelude.Int,
    stationType :: Data.Maybe.Maybe API.Types.UI.FRFSTicketService.StationType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data FRFSTicketAPI = FRFSTicketAPI {qrData :: Data.Text.Text, status :: Domain.Types.FRFSTicket.FRFSTicketStatus, ticketNumber :: Data.Text.Text, validTill :: Kernel.Prelude.UTCTime}
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data FRFSTicketBookingStatusAPIRes = FRFSTicketBookingStatusAPIRes
  { _type :: Domain.Types.FRFSQuote.FRFSQuoteType,
    bookingId :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking,
    city :: Kernel.Types.Beckn.Context.City,
    createdAt :: Kernel.Prelude.UTCTime,
    payment :: Data.Maybe.Maybe API.Types.UI.FRFSTicketService.FRFSBookingPaymentAPI,
    price :: Kernel.Types.Common.HighPrecMoney,
    priceWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    quantity :: Kernel.Prelude.Int,
    stations :: [API.Types.UI.FRFSTicketService.FRFSStationAPI],
    status :: Domain.Types.FRFSTicketBooking.FRFSTicketBookingStatus,
    tickets :: [API.Types.UI.FRFSTicketService.FRFSTicketAPI],
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleType :: Domain.Types.Station.FRFSVehicleType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data StationType = START | END | TRANSIT | INTERMEDIATE deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)
