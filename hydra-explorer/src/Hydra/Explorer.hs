module Hydra.Explorer where

import Hydra.Prelude

-- XXX: Depends on hydra-node
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)

import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Hydra.Explorer.ExplorerState (ExplorerState (..), HeadState, TickState, aggregateObservations, initialTickState)
import Hydra.Explorer.Observer.Api (Observation)
import Hydra.Explorer.Options (Options (..))
import Network.Wai (Middleware, Request (..))
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Servant (serveDirectoryFileServer)
import Servant.API (Get, JSON, Raw, (:<|>) (..), (:>))
import Servant.Server (Application, Handler, Server, serve)

type API :: Type
type API =
  "heads" :> Get '[JSON] [HeadState]
    :<|> "tick" :> Get '[JSON] TickState
    :<|> Raw

server ::
  FilePath ->
  GetExplorerState ->
  Server API
server staticPath getExplorerState =
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
  Handler TickState
handleGetTick getExplorerState = do
  liftIO getExplorerState <&> \ExplorerState{tick} -> tick

observerHandler :: ModifyExplorerState -> [Observation] -> IO ()
observerHandler modifyExplorerState observations = do
  modifyExplorerState $ aggregateObservations observations

type GetExplorerState = IO ExplorerState

type ModifyExplorerState = (ExplorerState -> ExplorerState) -> IO ()

createExplorerState :: IO (GetExplorerState, ModifyExplorerState)
createExplorerState = do
  v <- newTVarIO (ExplorerState [] initialTickState)
  pure (getExplorerState v, modifyExplorerState v)
 where
  getExplorerState = readTVarIO
  modifyExplorerState v = atomically . modifyTVar' v

-- XXX: Depends on hydra-node for logging stuff, could replace with different
-- (structured) logging tools

httpApp :: Tracer IO APIServerLog -> FilePath -> GetExplorerState -> Application
httpApp tracer staticPath getExplorerState =
  logMiddleware tracer
    . simpleCors
    . serve (Proxy @API)
    $ server staticPath getExplorerState

run :: Options -> IO ()
run opts = do
  withTracer (Verbose "hydra-explorer") $ \tracer -> do
    (getExplorerState, _modifyExplorerState) <- createExplorerState
    Warp.runSettings (settings tracer) (httpApp tracer staticFilePath getExplorerState)
 where
  Options{staticFilePath, clientPort} = opts

  settings tracer =
    Warp.defaultSettings
      & Warp.setPort (fromIntegral clientPort)
      & Warp.setHost "0.0.0.0"
      & Warp.setBeforeMainLoop (traceWith tracer $ APIServerStarted clientPort)
      & Warp.setOnException (\_ e -> traceWith tracer $ APIConnectionError{reason = show e})

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
