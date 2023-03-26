module Merchant.Utils where

import Common.Types.App (LazyCheck(..))
import Helpers.Utils (Merchant(..), getMerchant)
import Prelude ((<>))
import Screens.Types (FareTypes(..), Language)

foreign import getString' :: String -> String

foreign import getValueFromConfig :: String -> String

getInvoiceBreakUp :: LazyCheck -> Array FareTypes
getInvoiceBreakUp lazy = case (getMerchant FunctionCall) of
  NAMMAYATRI -> [ BASE_FARE, DEAD_KILOMETER_FARE, DRIVER_SELECTED_FARE, WAITING_CHARGES ]
  JATRISAATHI -> [ BASE_FARE, DEAD_KILOMETER_FARE, WAITING_CHARGES ]
  YATRI -> [ BASE_FARE, DEAD_KILOMETER_FARE, WAITING_CHARGES ]

type LanguageData
  = { languages :: Array Language
    }

getReferenceList :: LazyCheck -> Array String
getReferenceList lazy = case (getMerchant FunctionCall) of
  NAMMAYATRI -> [ "1.5" <> (getString' "DAYTIME_CHARGES_APPLICABLE_AT_NIGHT"), (getString' "DRIVERS_CAN_CHARGE_AN_ADDITIONAL_FARE_UPTO"), (getString' "WAITING_CHARGE_DESCRIPTION") ]
  JATRISAATHI -> [ "1.5" <> (getString' "DAYTIME_CHARGES_APPLICABLE_AT_NIGHT"), (getString' "WAITING_CHARGE_DESCRIPTION") ]
  YATRI -> [ "1.5" <> (getString' "DAYTIME_CHARGES_APPLICABLE_AT_NIGHT"), (getString' "WAITING_CHARGE_DESCRIPTION") ]

getLanguagesList :: LazyCheck -> Array Language
getLanguagesList lazy = case (getMerchant FunctionCall) of
  NAMMAYATRI ->
    [ { name: "English"
      , value: "EN_US"
      , subTitle: ""
      }
    , { name: "ಕನ್ನಡ"
      , value: "KN_IN"
      , subTitle: "Kannada"
      }
    , { name: "हिंदी"
      , value: "HI_IN"
      , subTitle: "Hindi"
      }
    ]
  JATRISAATHI ->
    [ { name: "English"
      , value: "EN_US"
      , subTitle: ""
      }
    , { name: "বাংলা"
      , value: "BN_IN"
      , subTitle: "Bengali"
      }
    , { name: "हिंदी"
      , value: "HI_IN"
      , subTitle: "Hindi"
      }
    ]
  YATRI ->
    [ { name: "English"
      , value: "EN_US"
      , subTitle: ""
      }
    , { name: "മലയാളം"
      , value: "ML_IN"
      , subTitle: "Malayalam"
      }
    ]
