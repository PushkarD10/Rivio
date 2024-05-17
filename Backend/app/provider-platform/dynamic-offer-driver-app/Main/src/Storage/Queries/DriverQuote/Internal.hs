{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverQuote.Internal where

import Domain.Types.DriverQuote as DriverQuote
import Kernel.Beam.Functions (findAllWithKVAndConditionalDB)
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Sequelize as Se
import Storage.Beam.DriverQuote as BeamDQ
import Storage.Queries.OrphanInstances.DriverQuote ()
import Storage.Queries.OrphanInstances.Person ()

getDriverQuote ::
  (KvDbFlow m r, Log m) =>
  [Text] ->
  m [DriverQuote.DriverQuote]
getDriverQuote personKeys =
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is BeamDQ.driverId $ Se.In personKeys,
          Se.Is BeamDQ.status $ Se.Eq DriverQuote.Active
        ]
    ]
    Nothing
