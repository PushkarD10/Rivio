{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App (startKafkaConsumers) where

import qualified Consumer.Flow as CF
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Supervisor
import Data.Function
import Environment
import EulerHS.Interpreters (runFlow)
import qualified EulerHS.Runtime as L
import qualified EulerHS.Runtime as R
import qualified Kafka.Consumer as Consumer
import Kernel.Beam.Connection.Flow (prepareConnectionRider)
import Kernel.Beam.Connection.Types (ConnectionConfigRider (..))
import Kernel.Prelude
import Kernel.Utils.Common hiding (id)
import Kernel.Utils.Dhall (readDhallConfigDefault)
import qualified Kernel.Utils.FlowLogging as L

runConsumer :: ConsumerType -> IO ()
runConsumer consumerType = do
  putStrLn (("Thread started " <> show consumerType <> "\n") :: Text)
  configFile <- CF.getConfigNameFromConsumertype consumerType
  appCfg :: AppCfg <- readDhallConfigDefault configFile
  appEnv <- buildAppEnv appCfg consumerType
  startConsumerWithEnv appCfg appEnv
  putStrLn ("Thread ended\n" :: Text)

startKafkaConsumers :: IO ()
startKafkaConsumers =
  bracketOnError
    ( do
        putStrLn ("Running consumers in thread" :: Text)
        baseConfigFile <- CF.getConfigNameFromConsumertype AVAILABILITY_TIME
        baseConfig :: AppCfg <- readDhallConfigDefault baseConfigFile
        let consumerTypes = baseConfig.consumerTypes
        supervisors <- mapM (\_ -> newSupervisor OneForOne) consumerTypes
        mapM_ (\(sup, consumerType) -> forkSupervised sup fibonacciRetryPolicy (runConsumer consumerType)) (zip supervisors consumerTypes)
        _ <- forkIO (go (eventStream (head supervisors)))
        return (head supervisors)
    )
    shutdownSupervisor
    (\_ -> Control.Concurrent.threadDelay 10000000000)
  where
    go eS = do
      newE <- atomically $ readTQueue eS
      print newE
      go eS

startConsumerWithEnv :: AppCfg -> AppEnv -> IO ()
startConsumerWithEnv appCfg appEnv@AppEnv {..} = do
  kafkaConsumer <- newKafkaConsumer
  let loggerRuntime = L.getEulerLoggerRuntime appEnv.hostname appEnv.loggerConfig
  R.withFlowRuntime (Just loggerRuntime) $ \flowRt' -> do
    managers <- managersFromManagersSettings appCfg.httpClientOptions.timeoutMs mempty -- default manager is created
    let flowRt = flowRt' {L._httpClientManagers = managers}
    runFlow
      flowRt
      ( prepareConnectionRider
          ( ConnectionConfigRider
              { esqDBCfg = appCfg.esqDBCfg,
                esqDBReplicaCfg = appCfg.esqDBReplicaCfg,
                hedisClusterCfg = appCfg.hedisClusterCfg
              }
          )
          appCfg.tables
      )
    CF.runConsumer flowRt appEnv consumerType kafkaConsumer
  where
    newKafkaConsumer =
      either (error . ("Unable to open a kafka consumer: " <>) . show) id
        <$> Consumer.newConsumer
          (kafkaConsumerCfg.consumerProperties)
          (Consumer.topics kafkaConsumerCfg.topicNames)
