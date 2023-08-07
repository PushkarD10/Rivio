{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Servant.HeaderAuth where

import Data.Typeable (typeRep)
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (HasCoreMetrics)
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Types.Flow
import Kernel.Utils.Common
import Kernel.Utils.IOLogging (HasLog)
import Kernel.Utils.Servant.HeaderAuth (VerificationMethod (..), addResponse401, addSecurityRequirement)
import Kernel.Utils.Servant.Server
import Network.Wai (Request (..))
import Servant hiding (ResponseHeader (..))
import Servant.Client
import qualified Servant.OpenApi as S
import qualified Servant.OpenApi.Internal as S
import Servant.Server.Internal.Delayed (addAuthCheck)
import Servant.Server.Internal.DelayedIO (DelayedIO, withRequest)

-- | Adds authentication via a header to API
--
-- Verify type argument defines what verification logic is supposed to do.
-- Normally you should define a type alias for this which fixes the
-- verification method.
-- Payload type argument define additional info for access checkings
data HeaderAuthWithPayload (header :: Symbol) (verify :: Type) (payload :: Type)

-- | How token verification with payload is performed.
class VerificationMethod verify => VerificationMethodWithPayload verify where
  -- | Verification payload type, phantom type will be converted
  -- to this usual data type and used in access checkings.
  type VerificationPayloadType verify

-- | Verification payload should have instance of this class for converting
-- phantom data types to usual types.
class VerificationPayload payloadType payload where
  toPayloadType :: Proxy payload -> payloadType

-- | Implementation of verification with payload.
data VerificationActionWithPayload verify m = VerificationMethodWithPayload verify =>
  VerificationActionWithPayload
  { -- | Check given header value and extract the information which
    -- identifies the current user.
    -- This is allowed to fail with 'ServantError'.
    runVerifyMethodWithPayload :: VerificationPayloadType verify -> Text -> m (VerificationResult verify)
  }

-- -- | This server part implementation accepts token in @token@header,
-- -- verifies it and puts @'VerificationResult'@to your endpoint.
instance
  ( HasServer api ctx,
    HasEnvEntry r ctx,
    HasContextEntry ctx (VerificationActionWithPayload verify (FlowR r)),
    VerificationMethodWithPayload verify,
    VerificationPayload (VerificationPayloadType verify) payload,
    KnownSymbol header,
    HasLog r,
    HasCoreMetrics r
  ) =>
  HasServer (HeaderAuthWithPayload header verify payload :> api) ctx
  where
  type
    ServerT (HeaderAuthWithPayload header verify payload :> api) m =
      VerificationResult verify -> ServerT api m

  route _ ctx subserver =
    route (Proxy @api) ctx $
      subserver `addAuthCheck` withRequest authCheck
    where
      authCheck :: Request -> DelayedIO (VerificationResult verify)
      authCheck req = runFlowRDelayedIO env . apiHandler $ do
        let headerName = fromString $ symbolVal (Proxy @header)
        requestHeaders req
          & (lookup headerName >>> fromMaybeM (MissingHeader headerName))
          >>= (parseHeader >>> fromEitherM (InvalidHeader headerName))
          >>= verifyMethod (toPayloadType (Proxy @payload))
      env = getEnvEntry ctx
      VerificationActionWithPayload verifyMethod = getContextEntry ctx :: VerificationActionWithPayload verify (FlowR r)

  hoistServerWithContext _ ctxp hst serv =
    hoistServerWithContext (Proxy @api) ctxp hst . serv

-- | This client part implementation simply accepts token and passes it to
-- the call.
instance
  (HasClient m api, KnownSymbol header) =>
  HasClient m (HeaderAuthWithPayload header verify payload :> api)
  where
  type Client m (HeaderAuthWithPayload header verify payload :> api) = RegToken -> Client m api

  clientWithRoute mp _ req =
    clientWithRoute
      mp
      (Proxy @(Header header Text :> api))
      req
      . Just

  hoistClientMonad mp _ hst cli = hoistClientMonad mp (Proxy @api) hst . cli

instance
  ( S.HasOpenApi api,
    VerificationMethodWithPayload verify,
    Typeable verify,
    KnownSymbol header
  ) =>
  S.HasOpenApi (HeaderAuthWithPayload header verify payload :> api)
  where
  toOpenApi _ =
    S.toOpenApi (Proxy @api)
      & addSecurityRequirement methodName (verificationDescription @verify) headerName
      & S.addDefaultResponse400 headerName
      & addResponse401
    where
      headerName = toText $ symbolVal (Proxy @header)
      methodName = show $ typeRep (Proxy @verify)
