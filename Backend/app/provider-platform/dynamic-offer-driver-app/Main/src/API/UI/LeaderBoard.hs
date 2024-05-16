module API.UI.LeaderBoard where

import Data.Time
import qualified Domain.Action.UI.LeaderBoard as DLeaderBoard
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "driver" :> "leaderBoard"
    :> ( TokenAuth
           :> "daily"
           :> MandatoryQueryParam "date" Day
           :> QueryParam "fillData" Bool
           :> Get '[JSON] DLeaderBoard.LeaderBoardRes
           :<|> TokenAuth
           :> "weekly"
           :> MandatoryQueryParam "fromDate" Day
           :> MandatoryQueryParam "toDate" Day
           :> Get '[JSON] DLeaderBoard.LeaderBoardRes
           :<|> TokenAuth
           :> "monthly"
           :> MandatoryQueryParam "month" Int
           :> Get '[JSON] DLeaderBoard.LeaderBoardRes
       )

handler :: FlowServer API
handler =
  getDailyDriverLeaderBoard
    :<|> getWeeklyDriverLeaderBoard
    :<|> getMonthlyDriverLeaderBoard

getDailyDriverLeaderBoard :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Day -> Maybe Bool -> FlowHandler DLeaderBoard.LeaderBoardRes
getDailyDriverLeaderBoard (personId, merchantId, merchantOpCityId) date fillData = withFlowHandlerAPI $ DLeaderBoard.getDailyDriverLeaderBoard (personId, merchantId, merchantOpCityId) date fillData

getWeeklyDriverLeaderBoard :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Day -> Day -> FlowHandler DLeaderBoard.LeaderBoardRes
getWeeklyDriverLeaderBoard (personId, merchantId, merchantOpCityId) fromDate toDate = withFlowHandlerAPI $ DLeaderBoard.getWeeklyDriverLeaderBoard (personId, merchantId, merchantOpCityId) fromDate toDate

getMonthlyDriverLeaderBoard :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Int -> FlowHandler DLeaderBoard.LeaderBoardRes
getMonthlyDriverLeaderBoard (personId, merchantId, merchantOpCityId) month = withFlowHandlerAPI $ DLeaderBoard.getMonthlyDriverLeaderBoard (personId, merchantId, merchantOpCityId) month
