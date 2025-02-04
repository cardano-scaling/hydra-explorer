module Hydra.Explorer where

import Hydra.Prelude

-- XXX: Depends on hydra-node
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)

import Control.Concurrent.Class.MonadSTM (modifyTVar', newTBQueueIO, newTVarIO, readTBQueue, readTVarIO, writeTBQueue)
import Hydra.Cardano.Api (NetworkId)
import Hydra.Explorer.ExplorerState (ExplorerState (..), HeadState, TickState, aggregateObservation)
import Hydra.Explorer.ObservationApi (HydraVersion, NetworkParam (..), Observation, ObservationApi)
import Hydra.Explorer.Options (Options (..))
import Network.Wai (Middleware, Request (..))
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Servant (serveDirectoryFileServer)
import Servant.API (Get, JSON, Raw, (:<|>) (..), (:>))
import Servant.Server (Application, Handler, serve)

-- * Observer-side API

-- | WAI application serving the 'ObservationApi'.
observationApi :: PushObservation -> Application
observationApi pushObservation =
  serve (Proxy @ObservationApi) server
 where
  server = handlePostObservation pushObservation

handlePostObservation ::
  PushObservation ->
  NetworkParam ->
  HydraVersion ->
  Observation ->
  Handler ()
handlePostObservation pushObservation (NetworkParam networkId) version observation =
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
createExplorerState :: IO (GetExplorerState, ModifyExplorerState)
createExplorerState = do
  v <- newTVarIO (ExplorerState [] [])
  pure (getExplorerState v, modifyExplorerState v)
 where
  getExplorerState = readTVarIO
  modifyExplorerState v = atomically . modifyTVar' v

type PushObservation = (NetworkId, HydraVersion, Observation) -> IO ()

type PopObservation = IO (NetworkId, HydraVersion, Observation)

-- | Bounded queue to process observations.
createObservationQueue :: IO (PushObservation, PopObservation)
createObservationQueue = do
  q <- newTBQueueIO 10
  pure (pushObservation q, popObservation q)
 where
  pushObservation q = atomically . writeTBQueue q

  popObservation = atomically . readTBQueue

-- | Worker that continously pops observations and updates the in-memory
-- 'ExplorerState'.
aggregator :: PopObservation -> ModifyExplorerState -> IO ()
aggregator popObservation modifyExplorerState =
  forever $ do
    -- TODO: IO does not compose as well as STM
    (network, version, observation) <- popObservation
    modifyExplorerState $ aggregateObservation network version observation

-- * Main

-- XXX: Depends on hydra-node for logging stuff, could replace with different
-- (structured) logging tools
-- TODO: distinguish servers when logging
run :: Options -> IO ()
run opts = do
  withTracer (Verbose "hydra-explorer") $ \tracer -> do
    (pushObservation, popObservation) <- createObservationQueue
    (getExplorerState, modifyExplorerState) <- createExplorerState
    race_ (observationServer tracer $ observationApi pushObservation) $
      race_ (clientServer tracer $ clientApi staticFilePath getExplorerState) $
        aggregator popObservation modifyExplorerState
 where
  Options{staticFilePath, clientPort, observerPort} = opts

  clientServer tracer app = do
    let settings =
          Warp.defaultSettings
            & Warp.setPort (fromIntegral clientPort)
            & Warp.setHost "0.0.0.0"
            & Warp.setBeforeMainLoop (traceWith tracer $ APIServerStarted clientPort)
            & Warp.setOnException (\_ e -> traceWith tracer $ APIConnectionError{reason = show e})
    Warp.runSettings settings
      . logMiddleware tracer
      . simpleCors
      $ app

  observationServer tracer app = do
    let settings =
          Warp.defaultSettings
            & Warp.setPort (fromIntegral observerPort)
            & Warp.setHost "0.0.0.0"
            & Warp.setBeforeMainLoop (traceWith tracer $ APIServerStarted observerPort)
            & Warp.setOnException (\_ e -> traceWith tracer $ APIConnectionError{reason = show e})
    Warp.runSettings settings
      . logMiddleware tracer
      $ app

-- * Logging

logMiddleware :: Tracer IO APIServerLog -> Middleware
logMiddleware tracer app' req sendResponse = do
  liftIO $
    traceWith tracer $
      APIHTTPRequestReceived
        { method = Method $ requestMethod req
        , path = PathInfo $ rawPathInfo req
        }
  app' req sendResponse
