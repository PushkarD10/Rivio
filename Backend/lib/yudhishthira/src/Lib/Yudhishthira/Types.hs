module Lib.Yudhishthira.Types
  ( module Reexport,
    YudhishthiraDecideReq (..),
    YudhishthiraDecideResp (..),
    ChakraQueriesAPIEntity (..),
    Source (..),
    SourceData,
    CreateNammaTagRequest (..),
    LogicDomain (..),
    AppDynamicLogicReq (..),
    AppDynamicLogicResp (..),
    RunLogicResp (..),
    RunKaalChakraJobReq (..),
    -- DynamicPricingResult (..),
  )
where

import Control.Lens.Operators hiding ((.=))
import Data.Aeson
import Data.OpenApi as OpenApi hiding (tags)
import qualified Data.Text as T
import Domain.Types.ServiceTierType as DST
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.HideSecrets
import Kernel.Types.TimeBound
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Lib.Yudhishthira.Types.Application as Reexport
import Lib.Yudhishthira.Types.Common as Reexport
import Lib.Yudhishthira.Types.KaalChakra as Reexport
import Lib.Yudhishthira.Types.Manual as Reexport
import Lib.Yudhishthira.Types.Tag as Reexport
import qualified Text.Show (show)

data Source
  = Application ApplicationEvent
  | KaalChakra Chakra
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

type SourceData = Value -- json to be decoded in the respective tag

data CreateNammaTagRequest
  = ApplicationTag NammaTagApplication
  | KaalChakraTag NammaTagChakra
  | ManualTag NammaTagManual
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets CreateNammaTagRequest where
  hideSecrets = identity

data ChakraQueriesAPIEntity = ChakraQueriesAPIEntity
  { chakra :: Chakra,
    queryName :: Text,
    queryResults :: [Text],
    queryText :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance HideSecrets ChakraQueriesAPIEntity where
  hideSecrets = identity

data YudhishthiraDecideReq = YudhishthiraDecideReq
  { source :: Source,
    sourceData :: SourceData
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

newtype YudhishthiraDecideResp = YudhishthiraDecideResp
  { tags :: [NammaTagResponse]
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data LogicDomain
  = POOLING
  | FARE_POLICY
  | DYNAMIC_PRICING DST.ServiceTierType
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

generateLogicDomainShowInstances :: [String]
generateLogicDomainShowInstances =
  [show POOLING]
    ++ [show FARE_POLICY]
    ++ [show (DYNAMIC_PRICING vehicleType) | vehicleType <- vehicleTypes]
  where
    vehicleTypes = [COMFY, ECO, PREMIUM, SUV, AUTO_RICKSHAW, HATCHBACK, SEDAN, TAXI, TAXI_PLUS, PREMIUM_SEDAN, BLACK, BLACK_XL, BIKE, AMBULANCE_TAXI, AMBULANCE_TAXI_OXY, AMBULANCE_AC, AMBULANCE_AC_OXY, AMBULANCE_VENTILATOR, SUV_PLUS, DELIVERY_BIKE]

instance ToParamSchema LogicDomain where
  toParamSchema _ =
    mempty
      & title ?~ "LogicDomain"
      & type_ ?~ OpenApiString
      & enum_
        ?~ map (String . T.pack) generateLogicDomainShowInstances

instance Show LogicDomain where
  show POOLING = "POOLING"
  show FARE_POLICY = "FARE-POLICY"
  show (DYNAMIC_PRICING vehicleType) =
    "DYNAMIC-PRICING_" ++ show vehicleType

instance Read LogicDomain where
  readsPrec _ s =
    let (prefx, rest) = break (== '_') s
     in case prefx of
          "POOLING" ->
            [(POOLING, drop 1 rest)]
          "FARE-POLICY" ->
            [(FARE_POLICY, drop 1 rest)]
          "DYNAMIC-PRICING" ->
            let (vehicleTypeStr, rest1) = break (== '_') (drop 1 rest)
             in case readMaybe vehicleTypeStr of
                  Just vehicleType ->
                    [(DYNAMIC_PRICING vehicleType, rest1)]
                  _ -> []
          _ -> []

$(mkBeamInstancesForEnumAndList ''LogicDomain)
$(mkHttpInstancesForEnum ''LogicDomain)

data AppDynamicLogicReq = AppDynamicLogicReq
  { rules :: [Value],
    inputData :: [Value],
    shouldUpdateRule :: Maybe Bool,
    updatePassword :: Maybe Text,
    timeBounds :: Maybe TimeBound,
    domain :: LogicDomain
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data AppDynamicLogicResp = AppDynamicLogicResp
  { result :: Value,
    isRuleUpdated :: Bool,
    errors :: [String]
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data RunLogicResp = RunLogicResp
  { result :: Value,
    errors :: [String]
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets AppDynamicLogicReq where
  hideSecrets = identity

newtype RunKaalChakraJobReq = RunKaalChakraJobReq
  { chakra :: Chakra
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets RunKaalChakraJobReq where
  hideSecrets = identity
