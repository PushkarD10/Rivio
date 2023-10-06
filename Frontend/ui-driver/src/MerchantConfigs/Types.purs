module MerchantConfig.Types where

type AppConfig =
  {
    primaryTextColor :: String,
    primaryBackground :: String,
    fontType :: String,
    languageList :: Array Language,
    popupBackground :: String,
    defaultLanguage :: String,
    imageUploadOptional :: Boolean,
    showSavedCommission :: Boolean,
    leaderBoard :: LeaderBoard,
    subscriptionConfig :: SubscriptionConfig,
    rideActionModelConfig :: RideActionModelConfig 
  } 

type Language =  {
  name :: String,
  value :: String,
  subtitle :: String
 }

type LeaderBoard = {
  isMaskedName :: Boolean
}

type SubscriptionConfig =  {
  enableBlocking :: Boolean,
  completePaymentPopup :: Boolean,
  onBoardingSubscription :: Boolean
 }

type RideActionModelConfig = {
  showVehicleVariant :: Boolean
}