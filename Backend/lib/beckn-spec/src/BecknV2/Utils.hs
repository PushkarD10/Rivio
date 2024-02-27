{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module BecknV2.Utils where

import BecknV2.OnDemand.Tags
import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601
import EulerHS.Prelude

getTagV2 :: TagGroup -> Tag -> Maybe [Spec.TagGroup] -> Maybe Text
getTagV2 tagGroupCode tagCode mbTagGroups = do
  case mbTagGroups of
    Nothing -> Nothing
    Just tagGroups -> do
      tagGroup <- find (\tagGroup -> descriptorCode tagGroup.tagGroupDescriptor == Just (show tagGroupCode)) tagGroups
      case tagGroup.tagGroupList of
        Nothing -> Nothing
        Just tagGroupList -> do
          tag <- find (\tag -> descriptorCode tag.tagDescriptor == Just (show tagCode)) tagGroupList
          tag.tagValue
  where
    descriptorCode :: Maybe Spec.Descriptor -> Maybe Text
    descriptorCode (Just desc) = desc.descriptorCode
    descriptorCode Nothing = Nothing

parseISO8601Duration :: Text -> Maybe NominalDiffTime
parseISO8601Duration durationStr = do
  (calenderDiffernceTime :: CalendarDiffTime) <- iso8601ParseM $ T.unpack durationStr
  Just $ ctTime calenderDiffernceTime

formatTimeDifference :: NominalDiffTime -> Text
formatTimeDifference duration = T.pack $ iso8601Show $ calendarTimeTime duration

addDurationToUTCTime :: UTCTime -> NominalDiffTime -> UTCTime
addDurationToUTCTime time duration = addUTCTime duration time
