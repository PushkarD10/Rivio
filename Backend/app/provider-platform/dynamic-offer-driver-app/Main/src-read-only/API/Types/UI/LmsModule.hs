{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.LmsModule where

import qualified Data.Maybe
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.DriverModuleCompletion
import qualified Domain.Types.LmsEnumTypes
import qualified Domain.Types.LmsModule
import qualified Domain.Types.LmsModuleVideoInformation
import qualified Domain.Types.QuestionInformation
import qualified Domain.Types.QuestionModuleMapping
import qualified Domain.Types.ReelsData
import qualified Domain.Types.Vehicle
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data LmsEntityCompletionStatus
  = ENTITY_COMPLETED
  | ENTITY_INCOMPLETE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsGetModuleRes = LmsGetModuleRes {completed :: [API.Types.UI.LmsModule.LmsModuleRes], remaining :: [API.Types.UI.LmsModule.LmsModuleRes]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsGetQuizRes = LmsGetQuizRes {questions :: [API.Types.UI.LmsModule.LmsQuestionRes], selectedModuleInfo :: API.Types.UI.LmsModule.LmsTranslatedModuleInfoRes}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsGetVideosRes = LmsGetVideosRes
  { completed :: [API.Types.UI.LmsModule.LmsVideoRes],
    pending :: [API.Types.UI.LmsModule.LmsVideoRes],
    quizEnabled :: Kernel.Prelude.Bool,
    quizStatus :: API.Types.UI.LmsModule.LmsEntityCompletionStatus,
    selectedModuleInfo :: API.Types.UI.LmsModule.LmsTranslatedModuleInfoRes
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsModuleRes = LmsModuleRes
  { category :: Domain.Types.LmsModule.LmsCategory,
    completedAt :: Data.Maybe.Maybe Kernel.Prelude.UTCTime,
    description :: Data.Text.Text,
    duration :: Kernel.Prelude.Int,
    languagesAvailableForQuiz :: [Kernel.External.Types.Language],
    languagesAvailableForVideos :: [Kernel.External.Types.Language],
    moduleCompletionCriteria :: Domain.Types.LmsModule.ModuleCompletionCriteria,
    moduleCompletionStatus :: Domain.Types.DriverModuleCompletion.ModuleCompletionStatus,
    moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
    name :: Data.Text.Text,
    noOfVideos :: Kernel.Prelude.Int,
    noOfVideosCompleted :: Kernel.Prelude.Int,
    rank :: Kernel.Prelude.Int,
    thumbnailImage :: Data.Text.Text,
    variant :: Data.Maybe.Maybe Domain.Types.Vehicle.Variant
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsQuestionRes = LmsQuestionRes
  { language :: Kernel.External.Types.Language,
    moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
    options :: API.Types.UI.LmsModule.QuizOptions,
    previousHistory :: Data.Maybe.Maybe API.Types.UI.LmsModule.LmsQuizHistory,
    question :: Domain.Types.LmsEnumTypes.QuizQuestion,
    questionId :: Kernel.Types.Id.Id Domain.Types.QuestionModuleMapping.QuestionModuleMapping
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsQuestionStatus
  = CORRECT
  | INCORRECT
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsQuizHistory = LmsQuizHistory {attemptNumber :: Kernel.Prelude.Int, selectedOptions :: [Data.Text.Text], status :: API.Types.UI.LmsModule.LmsQuestionStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsTranslatedModuleInfoRes = LmsTranslatedModuleInfoRes
  { category :: Domain.Types.LmsModule.LmsCategory,
    description :: Data.Text.Text,
    duration :: Kernel.Prelude.Int,
    languagesAvailableForQuiz :: [Kernel.External.Types.Language],
    languagesAvailableForVideos :: [Kernel.External.Types.Language],
    moduleCompletionCriteria :: Domain.Types.LmsModule.ModuleCompletionCriteria,
    moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
    name :: Data.Text.Text,
    noOfVideos :: Kernel.Prelude.Int,
    rank :: Kernel.Prelude.Int,
    thumbnailImage :: Data.Text.Text,
    variant :: Data.Maybe.Maybe Domain.Types.Vehicle.Variant
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LmsVideoRes = LmsVideoRes
  { attemptNumber :: Kernel.Prelude.Int,
    bottomButtonConfig :: [[Domain.Types.ReelsData.ReelButtonConfig]],
    completedAt :: Kernel.Prelude.UTCTime,
    completedThresholdInPercentage :: Data.Maybe.Maybe Kernel.Prelude.Int,
    completedWatchCount :: Kernel.Prelude.Int,
    description :: Data.Text.Text,
    duration :: Kernel.Prelude.Int,
    language :: Kernel.External.Types.Language,
    moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
    rank :: Kernel.Prelude.Int,
    sideButtonConfig :: [[Domain.Types.ReelsData.ReelButtonConfig]],
    startThresholdInPercentage :: Data.Maybe.Maybe Kernel.Prelude.Int,
    thresholdEnabled :: Kernel.Prelude.Bool,
    thumbnailImage :: Data.Text.Text,
    title :: Data.Text.Text,
    url :: Data.Text.Text,
    videoCompletionStatus :: API.Types.UI.LmsModule.LmsEntityCompletionStatus,
    videoId :: Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation,
    viewCount :: Kernel.Prelude.Int,
    ytVideoId :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Options = Options {options :: [Domain.Types.QuestionInformation.OptionEntity]}
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data QuestionConfirmReq = QuestionConfirmReq
  { language :: Kernel.External.Types.Language,
    moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
    questionId :: Kernel.Types.Id.Id Domain.Types.QuestionModuleMapping.QuestionModuleMapping,
    selectedOption :: API.Types.UI.LmsModule.SelectedOption
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data QuestionConfirmRes = QuestionConfirmRes {validation :: API.Types.UI.LmsModule.QuestionValidation, validationRes :: API.Types.UI.LmsModule.SelectedOptionValidation}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data QuestionValidation
  = CORRECT_ANSWER
  | INCORRECT_ANSWER
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data QuizOptions
  = SingleSelect API.Types.UI.LmsModule.Options
  | MultiSelect API.Types.UI.LmsModule.Options
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SelectedOption
  = SingleSelectedOption Data.Text.Text
  | MultiSelectedOption [Data.Text.Text]
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SelectedOptionValidation
  = SingleSelectedOptionValidation API.Types.UI.LmsModule.ValidationResult
  | MultiSelectedOptionValidation [API.Types.UI.LmsModule.ValidationResult]
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ValidationResult = ValidationResult {id :: Data.Text.Text, isCorrect :: Kernel.Prelude.Bool}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VideoUpdateAPIReq = VideoUpdateAPIReq
  { language :: Kernel.External.Types.Language,
    moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
    videoId :: Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
