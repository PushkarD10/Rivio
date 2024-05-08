{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverSSN where

import Data.Aeson
import qualified Domain.Types.Person
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverSSNE e = DriverSSN
  { driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.DriverSSN.DriverSSN,
    ssn :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text
  }
  deriving (Generic)

type DriverSSN = DriverSSNE ('AsEncrypted)

type DecryptedDriverSSN = DriverSSNE ('AsUnencrypted)

instance EncryptedItem DriverSSN where
  type Unencrypted DriverSSN = (DecryptedDriverSSN, HashSalt)
  encryptItem (entity, salt) = do ssn_ <- encryptItem (ssn entity, salt); pure DriverSSN {driverId = driverId entity, id = id entity, ssn = ssn_}
  decryptItem entity = do ssn_ <- fst <$> decryptItem (ssn entity); pure (DriverSSN {driverId = driverId entity, id = id entity, ssn = ssn_}, "")

instance EncryptedItem' DriverSSN where
  type UnencryptedItem DriverSSN = DecryptedDriverSSN
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
