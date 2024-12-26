{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Explorer where

import Hydra.ChainObserver qualified
import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Control.Monad.Class.MonadAsync (forConcurrently_)
import Data.Aeson (eitherDecodeFileStrict', withObject, (.:))
import Data.ByteString.Base16 qualified as Base16
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
import Hydra.ChainObserver.NodeClient (ChainObservation)
import Hydra.Explorer.ExplorerState (ExplorerState (..), HeadState, TickState, aggregateHeadObservations, initialTickState)
import Hydra.Explorer.Options (BlockfrostOptions (..), DirectOptions (..), Options (..), toArgProjectPath, toArgStartChainFrom)
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)
import Hydra.Options qualified as Options
import Hydra.SerialisedScriptRegistry (SerialisedScriptRegistry (..), cborHexToSerialisedScript, serialisedScriptFromText)
import Network.Wai (Middleware, Request (..))
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Servant (err404, errBody, serveDirectoryFileServer, throwError)
import Servant.API (Capture, Get, JSON, Raw, (:<|>) (..), (:>))
import Servant.Server (Application, Handler, Server, serve)
import System.Environment (withArgs)

type API :: Type
type API =
    "heads" :> Capture "headId" String :> Get '[JSON] [HeadState]
        :<|> "tick" :> Capture "headId" String :> Get '[JSON] TickState
        :<|> Raw

server ::
    FilePath ->
    [(String, GetExplorerState)] ->
    Server API
server staticPath getExplorerStates =
    handleGetHeads getExplorerStates
        :<|> handleGetTick getExplorerStates
        :<|> serveDirectoryFileServer staticPath

handleGetHeads ::
    [(String, GetExplorerState)] ->
    String ->
    Handler [HeadState]
handleGetHeads getExplorerStates headId =
    case mGetExplorerState of
        Just getExplorerState ->
            liftIO getExplorerState <&> \ExplorerState{heads} -> heads
        Nothing -> throwError err404{errBody = "Head ID not found"}
  where
    mGetExplorerState = snd <$> find (\(i, _) -> i == headId) getExplorerStates

handleGetTick ::
    [(String, GetExplorerState)] ->
    String ->
    Handler TickState
handleGetTick getExplorerStates headId = do
    case mGetExplorerState of
        Just getExplorerState ->
            liftIO getExplorerState <&> \ExplorerState{tick} -> tick
        Nothing -> throwError err404{errBody = "Head ID not found"}
  where
    mGetExplorerState = snd <$> find (\(i, _) -> i == headId) getExplorerStates

logMiddleware :: Tracer IO APIServerLog -> Middleware
logMiddleware tracer app' req sendResponse = do
    liftIO
        $ traceWith tracer
        $ APIHTTPRequestReceived
            { method = Method $ requestMethod req
            , path = PathInfo $ rawPathInfo req
            }
    app' req sendResponse

httpApp :: Tracer IO APIServerLog -> FilePath -> [(String, GetExplorerState)] -> Application
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

type ScriptsRegistry = [(String, SerialisedScriptRegistry)]

instance FromJSON SerialisedScriptRegistry where
    parseJSON = withObject "ScriptsRegistry" $ \o -> do
        initialScript <- o .: "initialScript"
        commitScript <- o .: "commitScript"
        headScript :: Text <- o .: "headScript"
        depositScript :: Text <- o .: "depositScript"
        headScriptBytes <- either fail pure $ Base16.decode $ encodeUtf8 headScript
        depositScriptBytes <- either fail pure $ Base16.decode $ encodeUtf8 depositScript
        pure
            $ SerialisedScriptRegistry
                { initialScriptValidator = serialisedScriptFromText initialScript
                , commitScriptValidator = serialisedScriptFromText commitScript
                , headScriptValidator = cborHexToSerialisedScript headScriptBytes
                , depositScriptValidator = cborHexToSerialisedScript depositScriptBytes
                }

scriptsRegistryFromFile :: FilePath -> IO ScriptsRegistry
scriptsRegistryFromFile fp = do
    putStrLn $ "Reading dataset from: " <> fp
    eitherDecodeFileStrict' fp >>= either (die . show) pure

run :: Options -> IO ()
run opts = do
    withTracer (Verbose "hydra-explorer") $ \tracer -> do
        let scriptsRegistryFilePath = case opts of
                DirectOpts DirectOptions{scriptsRegistry} -> scriptsRegistry
                BlockfrostOpts BlockfrostOptions{scriptsRegistry} -> scriptsRegistry

        registries <- scriptsRegistryFromFile scriptsRegistryFilePath

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

        let getExplorerStates = fmap (\(g, _, (i, _)) -> (i, g)) stateRegistries
        race_
            ( forConcurrently_ stateRegistries
                $ \(_, modifyExplorerState, (_, registry)) ->
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

    settings tracer =
        Warp.defaultSettings
            & Warp.setPort (fromIntegral portToBind)
            & Warp.setHost "0.0.0.0"
            & Warp.setBeforeMainLoop (traceWith tracer $ APIServerStarted portToBind)
            & Warp.setOnException (\_ e -> traceWith tracer $ APIConnectionError{reason = show e})
