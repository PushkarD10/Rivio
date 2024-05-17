{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverQuote (module Storage.Queries.DriverQuote, module ReExport) where

import qualified Domain.Types.DriverQuote
import qualified Domain.Types.Person
import qualified Domain.Types.SearchTry
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverQuote as Beam
import Storage.Queries.DriverQuoteExtra as ReExport
import Storage.Queries.Transformers.DriverQuote

deleteByDriverId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByDriverId (Kernel.Types.Id.Id driverId) = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq driverId]

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.DriverQuote.DriverQuote -> m (Maybe Domain.Types.DriverQuote.DriverQuote))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findDriverQuoteBySTId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.SearchTry.SearchTry -> m (Maybe Domain.Types.DriverQuote.DriverQuote))
findDriverQuoteBySTId (Kernel.Types.Id.Id searchTryId) = do findOneWithKV [Se.Is Beam.searchTryId $ Se.Eq searchTryId]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.DriverQuote.DriverQuote -> m (Maybe Domain.Types.DriverQuote.DriverQuote))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]
