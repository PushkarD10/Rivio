module Screens.SelectBusRoute.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (void, class Show, discard, pure, unit, bind, ($), not, (+), (-), (==), (*), (<>), show, (+), (==), (-), show, map)
import PrestoDOM (Eval, update, continue, exit, updateAndExit, continueWithCmd, continueWithCmd)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Types.Core (class Loggable)
import Helpers.Utils (compareDate, getCurrentDate, generateQR, getFirstRoute)
import Effect.Uncurried (runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Effect.Uncurried(runEffectFn4)
import Debug (spy)
import Data.Array (length, (!!), catMaybes)
import Data.Maybe (Maybe(..), maybe)
import Engineering.Helpers.Commons(getNewIDWithTag)
import JBridge (shareImageMessage, copyToClipboard, toast)
import Common.Types.App as Common
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.SelectBusRoute.ScreenData (SelectBusRouteScreenState)
import Services.API (FrfsQuote(..), FRFSRouteAPI(..))

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    _ -> pure unit
    
data Action = AfterRender
            | NoAction
            | BackPressed
            | GenericHeaderAC GenericHeader.Action
            | SeeRouteButtonAction PrimaryButton.Action
            | UpdateQuotes (Array FrfsQuote)
            | SelectQuote FrfsQuote

data ScreenOutput = TrackBus SelectBusRouteScreenState
                  | GoBack

eval :: Action -> SelectBusRouteScreenState -> Eval Action ScreenOutput SelectBusRouteScreenState

eval BackPressed state = exit GoBack

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

eval (SeeRouteButtonAction (PrimaryButton.OnClick)) state =
  exit $ TrackBus state 
  
eval (SelectQuote quote) state =
  continue state{ data{ selectedQuote = Just quote } }

eval (UpdateQuotes quotes) state = do
  let routes =
        catMaybes $  
          map (\quote ->
            getFirstRoute quote
          ) quotes
  continue state{ data{ quotes = quotes } }

eval _ state = continue state