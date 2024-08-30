{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Common
  ( module Domain.Types.Common,
    module Domain.Types,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import Domain.Types
import qualified Domain.Types.VehicleCategory as DTVC
import qualified Domain.Types.VehicleVariant as DTVV
import Tools.Beam.UtilsTH

$(mkBeamInstancesForEnum ''FareProductType)
$(mkBeamInstancesForEnumAndList ''ServiceTierType)
$(mkBeamInstancesForEnumAndList ''DTVV.VehicleVariant)
$(mkBeamInstancesForEnumAndList ''DTVC.VehicleCategory)
$(mkBeamInstancesForEnumAndList ''Enums.VehicleCategory)
$(mkBeamInstancesForEnum ''TripCategory)
$(mkBeamInstancesForEnum ''TripParty)
