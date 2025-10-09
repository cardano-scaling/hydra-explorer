{-# LANGUAGE OverloadedRecordDot #-}

module Hydra.Explorer where

import Hydra.Prelude

import Blammo.Logging (Logger, MonadLogger)
import Blammo.Logging.Logger (newLogger)
import Blammo.Logging.Setup (LoggingT, runLoggerLoggingT)
import Blammo.Logging.Simple (Message ((:#)), logError, logInfo, logDebug, withThreadContext, (.=))
import Control.Concurrent.Class.MonadSTM (modifyTVar', newTBQueueIO, newTVarIO, readTBQueue, readTVarIO, writeTBQueue)
import Hydra.Cardano.Api (NetworkId, chainPointToSlotNo)
import Hydra.Explorer.Env qualified as Env
import Hydra.Explorer.ExplorerState (ExplorerState (..), HeadState, TickState, aggregateObservation)
import Hydra.Explorer.ObservationApi (HydraVersion, NetworkParam (..), Observation, ObservationApi, observed, point)
import Hydra.Explorer.Options (Options (..))
import Hydra.Tx.Observe (HeadObservation (NoHeadTx))
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.Logging (requestLogger)
import Servant (hoistServer, serveDirectoryFileServer)
import Servant.API (Get, JSON, Raw, (:<|>) (..), (:>))
import Servant.Server (Application, Handler (..), serve)
import UnliftIO (race_)

-- * Observer-side API

-- | WAI application serving the 'ObservationApi'.
observationApiApp :: Logger -> PushObservation -> Application
observationApiApp logger pushObservation =
  serve api $
    hoistServer api nt server
 where
  nt :: LoggingT IO a -> Handler a
  nt = liftIO . runLoggerLoggingT logger

  api = Proxy @ObservationApi

  server =
    handlePostObservation pushObservation

handlePostObservation ::
  (MonadIO m, MonadLogger m) =>
  PushObservation ->
  NetworkParam ->
  HydraVersion ->
  Observation ->
  m ()
handlePostObservation pushObservation (NetworkParam networkId) version observation = do
  case observation.observed of
    NoHeadTx -> logDebug $ "Tick" :# ["network" .= networkId, "slot" .= chainPointToSlotNo observation.point]
    o -> logInfo $ "Observed" :# ["network" .= networkId, "version" .= version, "observation" .= o]
  liftIO $ pushObservation (networkId, version, observation)

-- * Client-side API

type ClientApi =
  "heads" :> Get '[JSON] [HeadState]
    :<|> "ticks" :> Get '[JSON] [TickState]
    :<|> Raw

-- | WAI application serving the 'ClientApi'.
clientApi :: FilePath -> GetExplorerState -> Application
clientApi staticPath getExplorerState =
  serve (Proxy @ClientApi) server
 where
  server =
    handleGetHeads getExplorerState
      :<|> handleGetTick getExplorerState
      :<|> serveDirectoryFileServer staticPath

handleGetHeads ::
  GetExplorerState ->
  Handler [HeadState]
handleGetHeads getExplorerState =
  liftIO getExplorerState <&> \ExplorerState{heads} -> heads

handleGetTick ::
  GetExplorerState ->
  Handler [TickState]
handleGetTick getExplorerState = do
  liftIO getExplorerState <&> \ExplorerState{ticks} -> ticks

-- * Agreggator

type GetExplorerState = IO ExplorerState

type ModifyExplorerState = (ExplorerState -> ExplorerState) -> IO ()

-- | In-memory 'ExplorerState' that can be queried or modified.
createExplorerState :: MonadIO m => m (GetExplorerState, ModifyExplorerState)
createExplorerState = liftIO $ do
  v <- newTVarIO (ExplorerState [] [])
  pure (getExplorerState v, modifyExplorerState v)
 where
  getExplorerState = readTVarIO
  modifyExplorerState v = atomically . modifyTVar' v

type PushObservation = (NetworkId, HydraVersion, Observation) -> IO ()

type PopObservation = IO (NetworkId, HydraVersion, Observation)

-- | Bounded queue to process observations.
createObservationQueue :: MonadIO m => m (PushObservation, PopObservation)
createObservationQueue = liftIO $ do
  q <- newTBQueueIO 10
  pure (pushObservation q, popObservation q)
 where
  pushObservation q = atomically . writeTBQueue q

  popObservation = atomically . readTBQueue

-- | Worker that continously pops observations and updates the in-memory
-- 'ExplorerState'.
aggregator :: MonadIO m => PopObservation -> ModifyExplorerState -> m ()
aggregator popObservation modifyExplorerState =
  liftIO . forever $ do
    -- XXX: STM would compose better here as IO does not ensure atomicity of the
    -- pop and modify. OTOH we don't have multiple producers/consumers and the
    -- whole explorer is going down in case of exceptions anyways.
    (network, version, observation) <- popObservation
    modifyExplorerState $ aggregateObservation network version observation

-- * Main

run :: Options -> IO ()
run opts = do
  logger <- newLogger =<< Env.parse
  (pushObservation, popObservation) <- createObservationQueue
  (getExplorerState, modifyExplorerState) <- createExplorerState
  race_ (observationServer logger pushObservation) $
    race_ (clientServer logger $ clientApi staticFilePath getExplorerState) $
      aggregator popObservation modifyExplorerState
 where
  Options{staticFilePath, clientPort, observerPort} = opts

  clientServer logger app = do
    let runLogger = runLoggerLoggingT logger . withThreadContext ["api" .= ("client" :: String)]
    let settings =
          Warp.defaultSettings
            & Warp.setPort (fromIntegral clientPort)
            & Warp.setHost "0.0.0.0"
            & Warp.setBeforeMainLoop (runLogger . logInfo $ "APIServerStarted" :# ["port" .= show @String clientPort])
            & Warp.setOnException (\_ e -> runLogger . logError $ "APIConnectionError" :# ["reason" .= show @String e])
    liftIO
      . Warp.runSettings settings
      . requestLogger logger
      . simpleCors
      $ app

  observationServer logger pushObservation = do
    let runLogger = runLoggerLoggingT logger . withThreadContext ["api" .= ("observer" :: String)]
    let settings =
          Warp.defaultSettings
            & Warp.setPort (fromIntegral observerPort)
            & Warp.setHost "0.0.0.0"
            & Warp.setBeforeMainLoop (runLogger . logInfo $ "APIServerStarted" :# ["port" .= show @String observerPort])
            & Warp.setOnException (\_ e -> runLogger . logError $ "APIConnectionError" :# ["reason" .= show @String e])
    liftIO
      . Warp.runSettings settings
      . requestLogger logger
      $ observationApiApp logger pushObservation
