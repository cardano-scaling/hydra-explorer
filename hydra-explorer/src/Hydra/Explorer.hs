{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Explorer where

import Hydra.ChainObserver qualified
import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Control.Monad.Class.MonadAsync (forConcurrently_)
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
import Hydra.ChainObserver.NodeClient (ChainObservation)
import Hydra.Explorer.ExplorerState (ExplorerState (..), HeadState, TickState, aggregateHeadObservations, initialTickState)
import Hydra.Explorer.Options (BlockfrostOptions (..), DirectOptions (..), Options (..), toArgProjectPath, toArgStartChainFrom)
import Hydra.Explorer.ScriptsRegistry (HydraScriptRegistry (..), scriptsRegistryFromFile)
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)
import Hydra.Options qualified as Options
import Network.Wai (Middleware, Request (..))
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Servant (err404, errBody, serveDirectoryFileServer, throwError)
import Servant.API (Capture, Get, JSON, Raw, (:<|>) (..), (:>))
import Servant.Server (Application, Handler, Server, serve)
import System.Environment (withArgs)

type API :: Type
type API =
    "heads" :> Capture "hydraVersion" Text :> Get '[JSON] [HeadState]
        :<|> "tick" :> Capture "hydraVersion" Text :> Get '[JSON] TickState
        :<|> Raw

server ::
    FilePath ->
    [(Text, GetExplorerState)] ->
    Server API
server staticPath getExplorerStates = do
    handleGetHeads getExplorerStates
        :<|> handleGetTick getExplorerStates
        :<|> serveDirectoryFileServer staticPath

handleGetHeads ::
    [(Text, GetExplorerState)] ->
    Text ->
    Handler [HeadState]
handleGetHeads getExplorerStates hydraVersion =
    case mGetExplorerState of
        Just getExplorerState ->
            liftIO getExplorerState <&> \ExplorerState{heads} -> heads
        Nothing -> throwError err404{errBody = "Head version not found"}
  where
    mGetExplorerState = snd <$> find (\(i, _) -> i == hydraVersion) getExplorerStates

handleGetTick ::
    [(Text, GetExplorerState)] ->
    Text ->
    Handler TickState
handleGetTick getExplorerStates hydraVersion =
    case mGetExplorerState of
        Just getExplorerState ->
            liftIO getExplorerState <&> \ExplorerState{tick} -> tick
        Nothing -> throwError err404{errBody = "Head version not found"}
  where
    mGetExplorerState = snd <$> find (\(i, _) -> i == hydraVersion) getExplorerStates

logMiddleware :: Tracer IO APIServerLog -> Middleware
logMiddleware tracer app' req sendResponse = do
    liftIO $ putStrLn $ "Request received: " ++ show (rawPathInfo req)
    liftIO
        $ traceWith tracer
        $ APIHTTPRequestReceived
            { method = Method $ requestMethod req
            , path = PathInfo $ rawPathInfo req
            }
    app' req sendResponse

httpApp :: Tracer IO APIServerLog -> FilePath -> [(Text, GetExplorerState)] -> Application
httpApp tracer staticPath getExplorerStates =
    logMiddleware tracer
        . simpleCors
        . serve (Proxy @API)
        $ server staticPath getExplorerStates

observerHandler :: ModifyExplorerState -> [ChainObservation] -> IO ()
observerHandler modifyExplorerState observations = do
    modifyExplorerState
        $ aggregateHeadObservations observations

type GetExplorerState = IO ExplorerState

type ModifyExplorerState = (ExplorerState -> ExplorerState) -> IO ()

createExplorerState :: IO (GetExplorerState, ModifyExplorerState)
createExplorerState = do
    v <- newTVarIO (ExplorerState [] initialTickState)
    pure (getExplorerState v, modifyExplorerState v)
  where
    getExplorerState = readTVarIO
    modifyExplorerState v = atomically . modifyTVar' v

run :: Options -> IO ()
run opts = do
    withTracer (Verbose "hydra-explorer") $ \tracer -> do
        registries <- scriptsRegistryFromFile scriptsRegistryFilePath

        print registries

        let chainObserverArgs =
                case opts of
                    DirectOpts DirectOptions{networkId, nodeSocket, startChainFrom} ->
                        ["direct"]
                            <> Options.toArgNodeSocket nodeSocket
                            <> Options.toArgNetworkId networkId
                            <> toArgStartChainFrom startChainFrom
                    BlockfrostOpts BlockfrostOptions{projectPath, startChainFrom} ->
                        ["blockfrost"]
                            <> toArgProjectPath projectPath
                            <> toArgStartChainFrom startChainFrom

        stateRegistries <-
            mapM
                ( \registry -> do
                    (getExplorerState, modifyExplorerState) <- createExplorerState
                    pure (getExplorerState, modifyExplorerState, registry)
                )
                registries

        let getExplorerStates =
                fmap
                    (\(g, _, HydraScriptRegistry{version}) -> (version, g))
                    stateRegistries
        race_
            ( forConcurrently_ stateRegistries
                $ \(_, modifyExplorerState, HydraScriptRegistry{registry}) ->
                    do
                        withArgs chainObserverArgs
                        $ Hydra.ChainObserver.main registry
                        $ observerHandler modifyExplorerState
            )
            (Warp.runSettings (settings tracer) (httpApp tracer staticPath getExplorerStates))
  where
    staticPath =
        case opts of
            DirectOpts DirectOptions{staticFilePath} -> staticFilePath
            BlockfrostOpts BlockfrostOptions{staticFilePath} -> staticFilePath
    portToBind =
        case opts of
            DirectOpts DirectOptions{port} -> port
            BlockfrostOpts BlockfrostOptions{port} -> port
    scriptsRegistryFilePath =
        case opts of
            DirectOpts DirectOptions{scriptsRegistry} -> scriptsRegistry
            BlockfrostOpts BlockfrostOptions{scriptsRegistry} -> scriptsRegistry
    settings tracer =
        Warp.defaultSettings
            & Warp.setPort (fromIntegral portToBind)
            & Warp.setHost "0.0.0.0"
            & Warp.setBeforeMainLoop (traceWith tracer $ APIServerStarted portToBind)
            & Warp.setOnException (\_ e -> traceWith tracer $ APIConnectionError{reason = Hydra.Prelude.show e})
