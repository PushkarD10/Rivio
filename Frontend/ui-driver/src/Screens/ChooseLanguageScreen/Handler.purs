{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ChooseLanguageScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import Screens.ChooseLanguageScreen.Controller (ScreenOutput(..))
import Types.App (ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import MerchantConfig.Utils (getAppConfig)
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.ChooseLanguageScreen.View as ChooseLanguageScreen
import Types.App (FlowBT, GlobalState(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import MerchantConfig.Utils (getValueFromConfig)


chooseLanguage :: FlowBT String ScreenOutput
chooseLanguage = do
  (GlobalState state) <- getState
  config <- getAppConfig
  action <- lift $ lift $ runScreen $ ChooseLanguageScreen.screen state.chooseLanguageScreen{data{config = config},props{selectedLanguage = getValueFromConfig "defaultLanguage"}}
  case action of
    GoToEnterMobileScreen updateState -> do
      modifyScreenState $ ChooseLanguageScreenStateType (\chooseLanguageScreenScreen -> updateState)
      App.BackT $ App.BackPoint <$> pure action
