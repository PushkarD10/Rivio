{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Rating (buildRatingReq, buildRatingReqV2) where

import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.Rating as Rating
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils (computeTtlISO8601)
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import Control.Lens ((%~))
import qualified Data.Text as T
import qualified Domain.Action.UI.Feedback as DFeedback
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Storage.CachedQueries.BecknConfig as QBC
import Tools.Error

buildRatingReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DFeedback.FeedbackRes ->
  m (BecknReq Rating.RatingMessage)
buildRatingReq DFeedback.FeedbackRes {..} = do
  msgId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- buildTaxiContext Context.RATING msgId (Just transactionId) merchant.bapId bapUrl (Just providerId) (Just providerUrl) city merchant.country False
  let message =
        Rating.RatingMessage
          { id = bppBookingId.getId,
            value = ratingValue,
            feedback_form =
              [ Rating.FeedbackForm
                  { question = "Evaluate your ride experience.",
                    answer = feedbackDetails
                  },
                Rating.FeedbackForm
                  { question = "Was Assistance Offered?",
                    answer = case wasOfferedAssistance of
                      Just True -> Just "True"
                      Just False -> Just "False"
                      _ -> Nothing
                  },
                Rating.FeedbackForm
                  { question = "Get IssueId.",
                    answer = issueId
                  }
              ]
          }
  pure $ BecknReq context message

buildRatingReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl], CacheFlow m r, EsqDBFlow m r) =>
  DFeedback.FeedbackRes ->
  m Spec.RatingReq
buildRatingReqV2 res@DFeedback.FeedbackRes {..} = do
  msgId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle res.merchant.id "MOBILITY" (Utils.mapVariantToVehicle res.vehicleVariant) >>= fromMaybeM (InternalError "Beckn Config not found")
  ttl <- bapConfig.ratingTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  context <- ContextV2.buildContextV2 Context.RATING Context.MOBILITY msgId (Just transactionId) merchant.bapId bapUrl (Just providerId) (Just providerUrl) merchant.defaultCity merchant.country (Just ttl)
  let message = tfMessage res
  pure $
    Spec.RatingReq
      { ratingReqContext = context,
        ratingReqMessage = message
      }

tfMessage :: DFeedback.FeedbackRes -> Spec.RatingReqMessage
tfMessage res = do
  Spec.RatingReqMessage
    { ratingReqMessageRatings = Just [tfRating res]
    }

tfRating :: DFeedback.FeedbackRes -> Spec.Rating
tfRating res@DFeedback.FeedbackRes {..} = do
  Spec.Rating
    { ratingId = Just $ bppBookingId.getId,
      ratingValue = Just $ show ratingValue,
      ratingRatingCategory = Nothing,
      ratingFeedbackForm = Just $ tfFeedbackForm res,
      ratingTag = mkRatingTag res.shouldFavDriver
    }

tfFeedbackForm :: DFeedback.FeedbackRes -> [Spec.FeedbackForm]
tfFeedbackForm DFeedback.FeedbackRes {..} = do
  [ Spec.FeedbackForm
      { feedbackFormQuestion = "Evaluate your ride experience.",
        feedbackFormAnswer = feedbackDetails
      },
    Spec.FeedbackForm
      { feedbackFormQuestion = "Was Assistance Offered?",
        feedbackFormAnswer = case wasOfferedAssistance of
          Just True -> Just "True"
          Just False -> Just "False"
          _ -> Nothing
      },
    Spec.FeedbackForm
      { feedbackFormQuestion = "Get IssueId.",
        feedbackFormAnswer = issueId
      }
    ]

mkRatingTag :: Maybe Bool -> Maybe [Spec.TagGroup]
mkRatingTag mShouldFavDriver = mShouldFavDriver >>= mkRatingTags

mkRatingTags :: Bool -> Maybe [Spec.TagGroup]
mkRatingTags shouldFavDriver =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.RATING_TAGS,
                  descriptorName = Just "Rating Information",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList = mkPersonTag shouldFavDriver
        }
    ]

mkPersonTag :: Bool -> Maybe [Spec.Tag]
mkPersonTag shouldFavDriver =
  Just
    [ Spec.Tag
        { tagDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.SHOULD_FAVOURITE_DRIVER,
                  descriptorName = Just "Should Favourite Driver",
                  descriptorShortDesc = Nothing
                },
          tagDisplay = Just False,
          tagValue = Just $ show shouldFavDriver
        }
    ]
