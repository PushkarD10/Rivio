module RemoteConfig.Types where

import Prelude
import Prelude (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Foreign.Generic (class Decode)
import Presto.Core.Utils.Encoding (defaultDecode)
import Data.Maybe (Maybe)
import Language.Types(STR(..))
import Data.Show.Generic (genericShow)


type SafetyVideoConfig
  = { videoId :: String
    , title :: String
    , coverImageUrl :: String
    , description :: Array DescriptionComponent
    }

newtype DescriptionComponent = DescriptionComponent {
  text :: String,
  color :: String,
  isBullet :: Boolean,
  marginLeft :: Int,
  marginTop :: Int,
  fontStyle :: String
}

derive instance genericDescriptionComponent :: Generic DescriptionComponent _
instance decodeDescriptionComponent :: Decode DescriptionComponent where decode = defaultDecode

type SpecialLocationsOb = {
  locations :: Array SpecialLocation
}

type SpecialLocation = {
  name :: String,
  gates :: Array Gate
}

type Gate = {
  gateName :: String,
  images :: Array PickupInstructions
}

type PickupInstructions = 
  { image :: String
  , title :: String
  }
  
newtype FamousDestination = FamousDestination {
  name :: String,
  address :: String,
  imageUrl :: String,
  lat :: Number,
  lon :: Number,
  description :: String,
  nameBasedOnLanguage :: String
}

derive instance genericFamousDestination :: Generic FamousDestination _
instance decodeFamousDestination :: Decode FamousDestination where decode = defaultDecode

type Service = {
  type :: ServiceType,
  name :: STR,
  image :: String,
  backgroundColor :: String
}

data ServiceType = INSTANT | TRANSIT | INTERCITY | RENTAL | DELIVERY | INTERCITY_BUS

derive instance genericServiceType :: Generic ServiceType _
instance eqServiceType :: Eq ServiceType where eq = genericEq
instance showServiceType :: Show ServiceType where show = genericShow