{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffer where

import Data.Aeson as DA
import qualified Data.List.NonEmpty as NE
import Data.Ord (comparing)
import Domain.Types.Common
import Kernel.Prelude
import Kernel.Utils.Common (Seconds)

data FPRentalDetailsDistanceBuffersD (s :: UsageSafety) = FPRentalDetailsDistanceBuffers
  { rideDuration :: Seconds,
    bufferKms :: Int,
    bufferMeters :: Int
  }
  deriving (Generic, Show, Eq, ToSchema)

type FPRentalDetailsDistanceBuffers = FPRentalDetailsDistanceBuffersD 'Safe

instance FromJSON (FPRentalDetailsDistanceBuffersD 'Unsafe)

instance ToJSON (FPRentalDetailsDistanceBuffersD 'Unsafe)

instance FromJSON (FPRentalDetailsDistanceBuffersD 'Safe)

instance ToJSON (FPRentalDetailsDistanceBuffersD 'Safe)

findFPRentalDetailsByDuration :: Int -> NonEmpty (FPRentalDetailsDistanceBuffersD s) -> FPRentalDetailsDistanceBuffersD s
findFPRentalDetailsByDuration duration slabList = do
  case NE.filter (\slab -> slab.rideDuration.getSeconds <= duration) $ NE.sortBy (comparing (.rideDuration)) slabList of
    [] -> error $ "Slab for duration = " <> show duration <> " not found. Non-emptiness supposed to be guaranteed by app logic."
    a -> last a

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

data FPRentalDetailsDistanceBuffersAPIEntity = FPRentalDetailsDistanceBuffersAPIEntity
  { rideDuration :: Seconds,
    bufferKms :: Int,
    bufferMeters :: Int
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

makeFPRentalDetailsDistanceBuffersList :: [FPRentalDetailsDistanceBuffersAPIEntity] -> [FPRentalDetailsDistanceBuffers]
makeFPRentalDetailsDistanceBuffersList = fmap makeFPRentalDetailsDistanceBuffers

makeFPRentalDetailsDistanceBuffersAPIEntity :: FPRentalDetailsDistanceBuffers -> FPRentalDetailsDistanceBuffersAPIEntity
makeFPRentalDetailsDistanceBuffersAPIEntity FPRentalDetailsDistanceBuffers {..} =
  FPRentalDetailsDistanceBuffersAPIEntity
    { rideDuration = rideDuration,
      bufferKms = bufferKms,
      bufferMeters = bufferMeters
    }

makeFPRentalDetailsDistanceBuffers :: FPRentalDetailsDistanceBuffersAPIEntity -> FPRentalDetailsDistanceBuffers
makeFPRentalDetailsDistanceBuffers FPRentalDetailsDistanceBuffersAPIEntity {..} =
  FPRentalDetailsDistanceBuffers
    { rideDuration = rideDuration,
      bufferKms = bufferKms,
      bufferMeters = bufferMeters
    }
