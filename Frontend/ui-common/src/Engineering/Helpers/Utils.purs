module Engineering.Helpers.Utils where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Presto.Core.Types.Language.Flow (delay)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (flowRunner, liftFlow, os)
import LoaderOverlay.Handler as UI
import Presto.Core.Types.Language.Flow (Flow, doAff, getState, modifyState)
import PrestoDOM.Core (terminateUI)
import Types.App (GlobalState(..))
import Debug (spy)
import Engineering.Helpers.Commons (os)
import Effect (Effect (..))
import Effect.Uncurried (EffectFn2(..), runEffectFn2, EffectFn1(..), runEffectFn1)

foreign import toggleLoaderIOS :: EffectFn1 Boolean Unit

foreign import loaderTextIOS :: EffectFn2 String String Unit

toggleLoader :: Boolean -> Flow GlobalState Unit
toggleLoader flag = do
  if os == "IOS" then do
    _ <- liftFlow $ runEffectFn1 toggleLoaderIOS flag
    pure unit
    else if flag then do
      state <- getState
      _ <- liftFlow $ launchAff $ flowRunner state UI.loaderScreen
      pure unit
      else do
        doAff $ liftEffect $ terminateUI $ Just "LoaderOverlay"

loaderText :: String -> String -> Flow GlobalState Unit
loaderText mainTxt subTxt = do
  if os == "IOS" then do
    _ <- liftFlow $ runEffectFn2 loaderTextIOS mainTxt subTxt
    pure unit
    else do 
      _ <- modifyState (\(GlobalState state) -> GlobalState state{loaderOverlay{data{title = mainTxt, subTitle = subTxt}}})
      pure unit

getSeparatorFactor :: Int
getSeparatorFactor = 8

defaultSeparatorCount :: Int
defaultSeparatorCount = 4

showAndHideLoader :: Number -> String -> String -> GlobalState -> Effect Unit
showAndHideLoader delayInMs title description state = do
  _ <- launchAff $ flowRunner state $ do
    _ <- loaderText title description
    _ <- toggleLoader true
    _ <- delay $ Milliseconds delayInMs
    _ <- toggleLoader false
    pure unit
  pure unit