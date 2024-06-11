{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RiderDetails where

import Data.Aeson
import qualified Domain.Types.DriverReferral
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RiderDetailsE e = RiderDetails
  { cancellationDues :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    disputeChancesUsed :: Kernel.Prelude.Int,
    favDriverList :: [Kernel.Prelude.Text],
    hasTakenValidRide :: Kernel.Prelude.Bool,
    hasTakenValidRideAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    nightSafetyChecks :: Kernel.Prelude.Bool,
    otpCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    referralCode :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.DriverReferral.DriverReferral),
    referredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    referredByDriver :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type RiderDetails = RiderDetailsE 'AsEncrypted

type DecryptedRiderDetails = RiderDetailsE 'AsUnencrypted

instance EncryptedItem RiderDetails where
  type Unencrypted RiderDetails = (DecryptedRiderDetails, HashSalt)
  encryptItem (entity, salt) = do
    mobileNumber_ <- encryptItem (mobileNumber entity, salt)
    pure
      RiderDetails
        { cancellationDues = cancellationDues entity,
          createdAt = createdAt entity,
          currency = currency entity,
          disputeChancesUsed = disputeChancesUsed entity,
          favDriverList = favDriverList entity,
          hasTakenValidRide = hasTakenValidRide entity,
          hasTakenValidRideAt = hasTakenValidRideAt entity,
          id = id entity,
          merchantId = merchantId entity,
          mobileCountryCode = mobileCountryCode entity,
          mobileNumber = mobileNumber_,
          nightSafetyChecks = nightSafetyChecks entity,
          otpCode = otpCode entity,
          referralCode = referralCode entity,
          referredAt = referredAt entity,
          referredByDriver = referredByDriver entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    mobileNumber_ <- fst <$> decryptItem (mobileNumber entity)
    pure
      ( RiderDetails
          { cancellationDues = cancellationDues entity,
            createdAt = createdAt entity,
            currency = currency entity,
            disputeChancesUsed = disputeChancesUsed entity,
            favDriverList = favDriverList entity,
            hasTakenValidRide = hasTakenValidRide entity,
            hasTakenValidRideAt = hasTakenValidRideAt entity,
            id = id entity,
            merchantId = merchantId entity,
            mobileCountryCode = mobileCountryCode entity,
            mobileNumber = mobileNumber_,
            nightSafetyChecks = nightSafetyChecks entity,
            otpCode = otpCode entity,
            referralCode = referralCode entity,
            referredAt = referredAt entity,
            referredByDriver = referredByDriver entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' RiderDetails where
  type UnencryptedItem RiderDetails = DecryptedRiderDetails
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
