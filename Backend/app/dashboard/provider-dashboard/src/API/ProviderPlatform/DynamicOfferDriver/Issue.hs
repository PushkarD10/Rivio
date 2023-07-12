{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.Issue
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Issue as Common
import Domain.Types.AccessMatrix.BPP.IssueActionType
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Error (PersonError (..))
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, withFlowHandlerAPI)
import Kernel.Utils.Error (fromMaybeM)
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import qualified "lib-dashboard" Storage.Queries.Person as QP
import "lib-dashboard" Tools.Auth hiding (BECKN_TRANSPORT, CustomerActionType (..), MerchantActionType (..))
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "issue"
    :> ( IssueCategoryListAPI
           :<|> IssueListAPI
           :<|> IssueInfoAPI
           :<|> IssueUpdateAPI
           :<|> IssueAddCommentAPI
           :<|> IssueFetchMediaAPI
       )

type IssueCategoryListAPI =
  ApiAuth ('DriverOfferBPP ('ISSUE 'CATEGORY_LIST))
    :> Common.IssueCategoryListAPI

type IssueListAPI =
  ApiAuth ('DriverOfferBPP ('ISSUE 'LIST))
    :> Common.IssueListAPI

type IssueInfoAPI =
  ApiAuth ('DriverOfferBPP ('ISSUE 'INFO))
    :> Common.IssueInfoAPI

type IssueUpdateAPI =
  ApiAuth ('DriverOfferBPP ('ISSUE 'UPDATE))
    :> Common.IssueUpdateAPI

type IssueAddCommentAPI =
  ApiAuth ('DriverOfferBPP ('ISSUE 'ADD_COMMENT))
    :> Common.IssueAddCommentAPI

type IssueFetchMediaAPI =
  ApiAuth ('DriverOfferBPP ('ISSUE 'FETCH_MEDIA))
    :> Common.IssueFetchMediaAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  issueCategoryList merchantId
    :<|> issueList merchantId
    :<|> issueInfo merchantId
    :<|> issueUpdate merchantId
    :<|> issueAddComment merchantId
    :<|> issueFetchMedia merchantId

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.IssueEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo = T.buildTransaction (DT.IssueAPI endpoint) (Just DRIVER_OFFER_BPP) (Just apiTokenInfo) Nothing Nothing

issueCategoryList :: ShortId DM.Merchant -> ApiTokenInfo -> FlowHandler Common.IssueCategoryListRes
issueCategoryList merchantShortId apiTokenInfo = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.issue.issueCategoryList)

issueList :: ShortId DM.Merchant -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Common.IssueStatus -> Maybe (Id Common.IssueCategory) -> Maybe Text -> FlowHandler Common.IssueReportListResponse
issueList merchantShortId apiTokenInfo mbLimit mbOffset mbStatus mbCategoryId mbAssignee = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.issue.issueList) mbLimit mbOffset mbStatus mbCategoryId mbAssignee

issueInfo :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.IssueReport -> FlowHandler Common.IssueInfoRes
issueInfo merchantShortId apiTokenInfo issueReportId_ = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  addAuthorDetails =<< Client.callDriverOfferBPP checkedMerchantId (.issue.issueInfo) issueReportId_
  where
    mkAuthorDetail :: Common.IssueReportCommentItem -> Flow Common.IssueReportCommentItem
    mkAuthorDetail Common.IssueReportCommentItem {..} = do
      author <- Esq.runInReplica (QP.findById $ cast authorDetail.authorId) >>= fromMaybeM (PersonNotFound authorDetail.authorId.getId)
      let authorDetail_ =
            Common.AuthorDetail
              { authorId = cast author.id,
                firstName = Just author.firstName,
                lastName = Just author.lastName
              }
      pure $
        Common.IssueReportCommentItem
          { authorDetail = authorDetail_,
            ..
          }
    addAuthorDetails :: Common.IssueInfoRes -> Flow Common.IssueInfoRes
    addAuthorDetails Common.IssueInfoRes {..} = do
      comments_ <- mapM mkAuthorDetail comments
      pure $
        Common.IssueInfoRes
          { comments = comments_,
            ..
          }

issueUpdate :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.IssueReport -> Common.IssueUpdateReq -> FlowHandler APISuccess
issueUpdate merchantShortId apiTokenInfo issueReportId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.IssueUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.issue.issueUpdate) issueReportId (mkRequest req)
  where
    mkRequest Common.IssueUpdateReq {..} =
      Common.IssueUpdateByUserReq
        { userId = cast apiTokenInfo.personId,
          ..
        }

issueAddComment :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.IssueReport -> Common.IssueAddCommentReq -> FlowHandler APISuccess
issueAddComment merchantShortId apiTokenInfo issueReportId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.IssueAddCommentEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.issue.issueAddComment) issueReportId (mkRequest req)
  where
    mkRequest Common.IssueAddCommentReq {..} =
      Common.IssueAddCommentByUserReq
        { userId = cast apiTokenInfo.personId,
          ..
        }

issueFetchMedia :: ShortId DM.Merchant -> ApiTokenInfo -> Text -> FlowHandler Text
issueFetchMedia merchantShortId apiTokenInfo filePath = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.issue.issueFetchMedia) filePath
