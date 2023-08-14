module Domain.Action.UI.DriverReferral
  ( createDriverReferral,
    ReferralLinkReq (..),
  )
where

import qualified Data.Text as T
import qualified Domain.Types.DriverReferral as D
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Text as TU
import Storage.CachedQueries.CacheConfig (HasCacheConfig)
import qualified Storage.CachedQueries.DriverReferral as CQD
import qualified Storage.CachedQueries.Merchant.TransporterConfig as QTC
import qualified Storage.Queries.DriverReferral as QRD
import Tools.Error

data ReferralLinkReq = ReferralLinkReq
  { referralCode :: Maybe Text,
    referralLinkPassword :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

createDriverReferral ::
  ( HasCacheConfig r,
    Redis.HedisFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    MonadTime m
  ) =>
  (Id SP.Person, Id DM.Merchant) ->
  Bool ->
  ReferralLinkReq ->
  m APISuccess
createDriverReferral (driverId, merchantId) isDashboard ReferralLinkReq {..} = do
  lastrefferalCode <- CQD.getLastRefferalCode
  let refferalCodeNumber = read (T.unpack (lastrefferalCode.getId)) :: Integer
  let referralCode' = T.pack (show (refferalCodeNumber + 1))
  unless (TU.validateAllDigitWithMinLength 6 referralCode') $
    throwError $ InvalidRequest "Referral Code must have 6 digits."
  transporterConfig <- QTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  when (transporterConfig.referralLinkPassword /= referralLinkPassword && not isDashboard) $
    throwError $ InvalidRequest "Invalid Password."
  mbLastReferralCodeWithDriver <- B.runInReplica $ QRD.findById driverId
  -- mbLastReferralCodeWithDriver <- QRD.findById driverId
  whenJust mbLastReferralCodeWithDriver $ \lastReferralCodeWithDriver ->
    unless (lastReferralCodeWithDriver.referralCode.getId == referralCode') $ throwError (InvalidRequest $ "DriverId: " <> driverId.getId <> " already linked with some referralCode.")
  mbReferralCodeAlreadyLinked <- B.runInReplica $ QRD.findByRefferalCode $ Id referralCode'
  -- mbReferralCodeAlreadyLinked <- QRD.findByRefferalCode $ Id referralCode
  whenJust mbReferralCodeAlreadyLinked $ \referralCodeAlreadyLinked ->
    unless (referralCodeAlreadyLinked.driverId == driverId) $ throwError (InvalidRequest $ "RefferalCode: " <> referralCode' <> " already linked with some other account.")
  driverRefferalRecord <- mkDriverRefferalType referralCode'
  when (all isNothing [mbReferralCodeAlreadyLinked, mbLastReferralCodeWithDriver]) $
    void (QRD.create driverRefferalRecord)
  pure Success
  where
    mkDriverRefferalType rc = do
      now <- getCurrentTime
      pure $
        D.DriverReferral
          { referralCode = Id rc,
            driverId = cast driverId,
            linkedAt = now
          }
