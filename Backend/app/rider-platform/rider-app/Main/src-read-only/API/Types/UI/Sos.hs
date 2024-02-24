{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Sos where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Domain.Types.Sos
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data MarkAsSafeReq = MarkAsSafeReq
  { isMock :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MockSosReq = MockSosReq
  { onRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data SosDetailsRes = SosDetailsRes
  { sos :: Kernel.Prelude.Maybe Domain.Types.Sos.Sos
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data SosReq = SosReq
  { flow :: Domain.Types.Sos.SosType,
    rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data SosRes = SosRes
  { sosId :: Kernel.Types.Id.Id Domain.Types.Sos.Sos
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data SosUpdateReq = SosUpdateReq
  { comment :: Kernel.Prelude.Maybe Data.Text.Text,
    status :: Domain.Types.Sos.SosStatus
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
