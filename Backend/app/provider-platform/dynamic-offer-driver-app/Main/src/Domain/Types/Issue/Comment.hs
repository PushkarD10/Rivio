{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Issue.Comment where

import Data.Time
import qualified Domain.Types.Issue.IssueReport as D
import qualified Domain.Types.Person as D
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id

data Comment = Comment
  { id :: Id Comment,
    issueReportId :: Id D.IssueReport,
    authorId :: Id D.Person,
    comment :: Text,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, Eq)
