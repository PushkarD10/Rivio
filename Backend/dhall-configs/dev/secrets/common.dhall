let topSecret = ./top-secret.dhall

let globalCommon = ../../generic/common.dhall

let exotelCfg =
        { apiKey = "xxxxxxx"
        , apiToken = "xxxxxxx"
        , sid = "xxxxxxx"
        , callerId = "xxxxxxx"
        }
      : globalCommon.ExotelCfg

let mockS3Config1 =
      { baseLocalDirectory = "./s3/local"
      , bucketName = "test-bucket"
      , pathPrefix = ""
      }

let mockS3Config = globalCommon.S3Config.S3MockConf mockS3Config1

let InfoBIPConfig = { username = "xxxxx", password = "xxxxx", token = "xxxxx" }

in  { smsUserName = "xxxxxxx"
    , smsPassword = "yyyyyyy"
    , exotelCfg
    , s3Config = mockS3Config
    , googleKey = topSecret.googleKey
    , googleTranslateKey = topSecret.googleTranslateKey
    , slackToken = "xxxxxxx"
    , InfoBIPConfig
    }
