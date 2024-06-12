{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSTrip where

import qualified Database.Beam as B
import qualified Domain.Types.FRFSTrip
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSTripT f = FRFSTripT
  { bppFulfillmentId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    quoteId :: B.C f Kernel.Prelude.Text,
    stationCode :: B.C f Kernel.Prelude.Text,
    stationName :: B.C f Kernel.Prelude.Text,
    stationType :: B.C f Domain.Types.FRFSTrip.StationType,
    stopSequence :: B.C f Kernel.Prelude.Int,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSTripT where
  data PrimaryKey FRFSTripT f = FRFSTripId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSTripId . id

type FRFSTrip = FRFSTripT Identity

$(enableKVPG ''FRFSTripT ['id] [['quoteId]])

$(mkTableInstances ''FRFSTripT "frfs_trip")

{-
	DSL Source Link: file://./../../../spec/Storage/FrfsTicket.yaml
-}
