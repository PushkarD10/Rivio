{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.PartnerOrganizationFRFS
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FRFSTicketService as DFRFSTypes
import qualified Domain.Action.UI.FRFSTicketService as DFRFSTicketService
import qualified Domain.Action.UI.PartnerOrganizationFRFS as DPOFRFS
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.PartnerOrgConfig as DPOC
import qualified Domain.Types.PartnerOrgStation as DPOS
import Domain.Types.PartnerOrganization
import Environment
import EulerHS.Prelude
import Kernel.Beam.Functions as B
import qualified Kernel.External.Maps as Maps
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import qualified Kernel.Types.Logging as Log
import Kernel.Utils.Common hiding (withLogTag)
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.PartnerOrgConfig as CQPOC
import qualified Storage.CachedQueries.Station as CQS
import Tools.Auth
import Tools.Error

type API =
  "frfs"
    :> ( "partnerOrganization"
           :> PartnerOrganizationAPIKey
           :> ( "upsertPersonAndGetFare"
                  :> ReqBody '[JSON] DPOFRFS.GetFareReq
                  :> Post '[JSON] DPOFRFS.GetFareResp
                  :<|> "getConfig"
                    :> ( "fromStation"
                           :> Capture "fromGMMStationId" (Id DPOS.PartnerOrgStation)
                           :> "toStation"
                           :> Capture "toGMMStationId" (Id DPOS.PartnerOrgStation)
                           :> Get '[JSON] DPOFRFS.GetConfigResp
                       )
              )
           :<|> "shareTicketInfo"
             :> Capture "ticketBookingId" (Id DFTB.FRFSTicketBooking)
             :> Get '[JSON] DPOFRFS.ShareTicketInfoResp
       )

handler :: FlowServer API
handler =
  handlerForAPIKey
    :<|> shareTicketInfo
  where
    handlerForAPIKey pOrg =
      upsertPersonAndGetFare pOrg
        :<|> getConfigByStationIds pOrg

upsertPersonAndGetFare :: PartnerOrganization -> DPOFRFS.GetFareReq -> FlowHandler DPOFRFS.GetFareResp
upsertPersonAndGetFare partnerOrg req = withFlowHandlerAPI . withLogTag $ do
  checkRateLimit partnerOrg.orgId getFareHitsCountKey

  fromStation <- B.runInReplica $ CQS.findByStationCode req.fromStationCode >>= fromMaybeM (StationDoesNotExist $ "StationCode:" +|| req.fromStationCode ||+ "")
  let fromStationOpCityId = fromStation.merchantOperatingCityId
      merchantId = fromStation.merchantId

  unless (merchantId == partnerOrg.merchantId) $
    throwError . InvalidRequest $ "apiKey of partnerOrgId:" +|| partnerOrg.orgId ||+ " not valid for merchantId:" +|| merchantId ||+ ""

  toStation <- B.runInReplica $ CQS.findByStationCode req.toStationCode >>= fromMaybeM (StationDoesNotExist $ "StationCode:" +|| req.toStationCode ||+ "")
  pOrgCfg <- B.runInReplica $ CQPOC.findByIdAndCfgType partnerOrg.orgId DPOC.REGISTRATION >>= fromMaybeM (PartnerOrgConfigNotFound partnerOrg.orgId.getId $ show DPOC.REGISTRATION)
  regPOCfg <- DPOC.getRegistrationConfig pOrgCfg.config

  let mbRegCoordinates = mkLatLong fromStation.lat fromStation.lon
  (personId, token) <- DPOFRFS.upsertPersonAndGetToken partnerOrg.orgId regPOCfg fromStationOpCityId merchantId mbRegCoordinates req

  Log.withLogTag ("FRFS:GetFare:PersonId:" <> personId.getId) $ do
    let frfsSearchReq = buildFRFSSearchReq fromStation.code toStation.code req.numberOfPassengers
        frfsVehicleType = fromStation.vehicleType
    res <- DFRFSTicketService.postFrfsSearchHandler (Just personId, merchantId) frfsVehicleType frfsSearchReq req.partnerOrgTransactionId (Just partnerOrg.orgId)

    return $ DPOFRFS.GetFareResp {searchId = res.searchId, ..}
  where
    withLogTag = Log.withLogTag ("FRFS:UpsertPersonAndGetFare:PartnerOrgId:" <> getId partnerOrg.orgId)

    getFareHitsCountKey :: Text
    getFareHitsCountKey = "BAP:FRFS:PartnerOrgId:" <> getId partnerOrg.orgId <> ":GetFare:hitsCount"

    mkLatLong :: Maybe Double -> Maybe Double -> Maybe Maps.LatLong
    mkLatLong mbLat mbLon = case (mbLat, mbLon) of
      (Just lat, Just lon) -> Just $ Maps.LatLong {..}
      _ -> Nothing

    buildFRFSSearchReq :: Text -> Text -> Int -> DFRFSTypes.FRFSSearchAPIReq
    buildFRFSSearchReq fromStationCode toStationCode quantity = DFRFSTypes.FRFSSearchAPIReq {..}

getConfigByStationIds :: PartnerOrganization -> Id DPOS.PartnerOrgStation -> Id DPOS.PartnerOrgStation -> FlowHandler DPOFRFS.GetConfigResp
getConfigByStationIds partnerOrg fromGMMStationId toGMMStationId = withFlowHandlerAPI . withLogTag $ do
  void $ checkRateLimit partnerOrg.orgId getConfigHitsCountKey

  DPOFRFS.getConfigByStationIds partnerOrg fromGMMStationId toGMMStationId
  where
    withLogTag = Log.withLogTag ("FRFS:GetConfig:PartnerOrgId:" <> getId partnerOrg.orgId)

    getConfigHitsCountKey :: Text
    getConfigHitsCountKey = "BAP:FRFS:PartnerOrgId:" <> getId partnerOrg.orgId <> ":GetConfig:hitsCount"

shareTicketInfo :: Id DFTB.FRFSTicketBooking -> FlowHandler DPOFRFS.ShareTicketInfoResp
shareTicketInfo bookingId = withFlowHandlerAPI . withLogTag $ do
  DPOFRFS.shareTicketInfo bookingId
  where
    withLogTag = Log.withLogTag ("FRFS:ShareTicketInfo:TicketBookingId:" <> bookingId.getId)

checkRateLimit ::
  ( Redis.HedisFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    DB.EsqDBReplicaFlow m r
  ) =>
  Id PartnerOrganization ->
  Text ->
  m ()
checkRateLimit partnerOrgId apiHitsCountKey = Log.withLogTag "checkRateLimit" $ do
  pOrgCfg <- B.runInReplica $ CQPOC.findByIdAndCfgType partnerOrgId DPOC.RATE_LIMIT >>= fromMaybeM (PartnerOrgConfigNotFound partnerOrgId.getId $ show DPOC.RATE_LIMIT)
  rateLimitPOCfg <- DPOC.getRateLimitConfig pOrgCfg.config
  checkSlidingWindowLimitWithOptions apiHitsCountKey rateLimitPOCfg.rateLimitOptions
