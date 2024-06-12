{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BlackListOrg where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Domain
import Tools.Beam.UtilsTH

data BlackListOrgT f = BlackListOrgT {domain :: B.C f Kernel.Types.Beckn.Domain.Domain, id :: B.C f Kernel.Prelude.Text, subscriberId :: B.C f Kernel.Prelude.Text} deriving (Generic, B.Beamable)

instance B.Table BlackListOrgT where
  data PrimaryKey BlackListOrgT f = BlackListOrgId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BlackListOrgId . id

type BlackListOrg = BlackListOrgT Identity

$(enableKVPG ''BlackListOrgT ['id] [['subscriberId]])

$(mkTableInstancesWithTModifier ''BlackListOrgT "black_list_org" [("subscriberId", "subscriber_id")])

{-
	DSL Source Link: file://./../../../spec/Storage/BlackListOrg.yaml
-}
