module Components.ChooseVehicle.Controller where

data Action
  = NoAction
  | OnSelect Config
  | OnImageClick

type Config
  = { vehicleImage :: String
    , isSelected :: Boolean
    , vehicleVariant :: String
    , vehicleType :: String
    , capacity :: String
    , price :: String
    , isCheckBox :: Boolean
    , isEnabled :: Boolean
    , index :: Int
    , activeIndex :: Int
    , id :: String
    }

config :: Config
config =
  { vehicleImage: "ic_sedan_ac,https://assets.juspay.in/nammayatri/images/user/ic_sedan_ac.png"
  , isSelected: false
  , vehicleVariant: "Non AC Taxi"
  , vehicleType: "Economical"
  , capacity: "4 people"
  , price: "246"
  , isCheckBox: false
  , isEnabled: true
  , activeIndex: 0
  , index: 0
  , id: ""
  }
