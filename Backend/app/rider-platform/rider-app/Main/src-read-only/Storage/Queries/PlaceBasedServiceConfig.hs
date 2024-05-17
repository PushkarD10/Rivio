{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PlaceBasedServiceConfig (module Storage.Queries.PlaceBasedServiceConfig, module ReExport) where

import qualified Domain.Types.PlaceBasedServiceConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import Storage.Queries.PlaceBasedServiceConfigExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig] -> m ())
createMany = traverse_ create
