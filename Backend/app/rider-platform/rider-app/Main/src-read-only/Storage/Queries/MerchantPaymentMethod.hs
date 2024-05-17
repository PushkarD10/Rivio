{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantPaymentMethod (module Storage.Queries.MerchantPaymentMethod, module ReExport) where

import qualified Domain.Types.MerchantPaymentMethod
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import Storage.Queries.MerchantPaymentMethodExtra as ReExport
import Storage.Queries.Transformers.MerchantPaymentMethod

create :: KvDbFlow m r => (Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod] -> m ())
createMany = traverse_ create
