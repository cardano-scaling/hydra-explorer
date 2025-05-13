{-# LANGUAGE DeriveAnyClass #-}

-- | Integration tests for the 'hydra-explorer' executable. These will run
-- also 'hydra-node' on a devnet and assert correct observation.
module Hydra.Explorer.IntegrationSpec where

import Hydra.Prelude hiding (get)
import Test.Hydra.Prelude

-- XXX: Depends on hydra-node for this
import Hydra.Logging (showLogsOnFailure)

import CardanoClient (RunningNode (..), queryTip)
import CardanoNode (NodeLog, withCardanoNodeDevnet)
import Control.Lens ((^.), (^?))
import Data.Aeson as Aeson
import Data.Aeson.Lens (key, nth, _Array, _Number, _String)
import Hydra.Cardano.Api (NetworkId (..), NetworkMagic (..), toNetworkMagic, unFile)
import Hydra.Cluster.Faucet (FaucetLog, publishHydraScriptsAs)
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Scenarios (EndToEndLog, singlePartyHeadFullLifeCycle, singlePartyOpenAHead)
import HydraNode (HydraNodeLog)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequestThrow)
import Network.Socket (PortNumber)
import System.Process.Typed (Process, ProcessConfig, createPipe, getStderr, proc, setEnv, setStderr, unsafeProcessHandle, withProcessTerm)
import Test.Network.Ports (withFreePort)

spec :: Spec
spec = do
  -- Simple end-to-end scenario with a cardano devnet and head opened/closed by
  -- a hydra-node, which a hydra-chain-observer reports into a single
  -- hydra-explorer.
  it "aggregates observations into /heads" $
    failAfter 60 $
      showLogsOnFailure "IntegrationSpec" $ \tracer -> do
        withTempDir "hydra-explorer-history" $ \tmpDir -> do
          withHydraExplorer $ \explorer -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node -> do
              scriptsTxId <- publishHydraScriptsAs node Faucet
              withChainObserver node explorer $ do
                -- Open and close a head
                singlePartyHeadFullLifeCycle (contramap FromScenario tracer) tmpDir node scriptsTxId
                -- Query client api
                allHeads <- getHeads explorer
                length (allHeads ^. _Array) `shouldBe` 1
                allHeads ^? nth 0 . key "network" `shouldBe` Just "Testnet"
                allHeads ^? nth 0 . key "networkMagic" . _Number `shouldBe` Just (fromIntegral . unNetworkMagic . toNetworkMagic $ networkId node)
                -- NOTE: This deliberately pins the latest version of hydra we test against.
                allHeads ^? nth 0 . key "version" `shouldBe` Just "0.20.0-0be61598fa556761fabab374a66649a37e402304"

  it "aggregates hydra transactions of multiple heads into /heads" $
    failAfter 60 $
      showLogsOnFailure "IntegrationSpec" $ \tracer -> do
        withHydraExplorer $ \explorer -> do
          withTempDir "hydra-explorer-history" $ \tmpDir -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node -> do
              withChainObserver node explorer $ do
                scriptsTxId <- publishHydraScriptsAs node Faucet
                -- Open two heads as alice (different head ids)
                let tr = contramap FromScenario tracer
                headId1 <- withTempDir "explorer-head1" $ \d -> singlePartyOpenAHead tr d node scriptsTxId $ \_ _ headId -> pure headId
                headId2 <- withTempDir "explorer-head2" $ \d -> singlePartyOpenAHead tr d node scriptsTxId $ \_ _ headId -> pure headId

                allHeads <- getHeads explorer
                length (allHeads ^. _Array) `shouldBe` 2
                allHeads ^? nth 0 . key "headId" `shouldBe` Just (toJSON headId1)
                allHeads ^? nth 0 . key "status" `shouldBe` Just "Open"
                allHeads ^? nth 1 . key "headId" `shouldBe` Just (toJSON headId2)
                allHeads ^? nth 1 . key "status" `shouldBe` Just "Open"

  it "aggregates latest point of observed chains" $
    failAfter 60 $
      showLogsOnFailure "IntegrationSpec" $ \tracer -> do
        withTempDir "hydra-explorer-get-tick" $ \tmpDir -> do
          withHydraExplorer $ \explorer -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node@RunningNode{nodeSocket, networkId} -> do
              withChainObserver node explorer $ do
                threadDelay 1
                tip <- toJSON <$> queryTip networkId nodeSocket

                allTicks <- getTicks explorer
                length (allTicks ^. _Array) `shouldBe` 1

                let tick = fromMaybe Null $ allTicks ^? nth 0
                let tipSlot = tip ^? key "slot" . _Number
                    tickSlot = tick ^? key "point" . key "slot" . _Number
                tickSlot `shouldBe` tipSlot

                let tipBlockHash = tip ^? key "blockHash" . _String
                    tickBlockHash = tick ^? key "point" . key "blockHash" . _String
                tickBlockHash `shouldBe` tipBlockHash

-- * Running hydra-explorer

data HydraExplorerClient = HydraExplorerClient
  { clientPort :: PortNumber
  , observerPort :: PortNumber
  , getHeads :: IO Value
  , getTicks :: IO Value
  }

-- | Starts a 'hydra-explorer'.
withHydraExplorer :: (HydraExplorerClient -> IO ()) -> IO ()
withHydraExplorer action =
  withFreePort $ \clientPort ->
    withFreePort $ \observerPort -> do
      let process =
            setEnv [("LOG_LEVEL", "debug")]
              . proc "hydra-explorer"
              $ ["--client-port", show clientPort]
                <> ["--observer-port", show observerPort]
      withProcessExpect process $ \_p -> do
        -- XXX: wait for the http server to be listening on port
        threadDelay 1
        action
          HydraExplorerClient
            { clientPort
            , observerPort
            , getHeads = getJSON clientPort "/heads"
            , getTicks = getJSON clientPort "/ticks"
            }
 where
  getJSON port path =
    parseRequestThrow ("http://127.0.0.1:" <> show port <> path)
      >>= httpJSON
      <&> getResponseBody

-- * Logging

data IntegrationTestLog
  = FromCardanoNode NodeLog
  | FromHydraNode HydraNodeLog
  | FromFaucet FaucetLog
  | FromScenario EndToEndLog
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- * Chain observer glue

-- TODO: DRY with hydra-chain-observer integration tests in hydra-cluster?

-- | Starts a 'hydra-chain-observer' on some Cardano network and have it connect to given 'hydra-explorer' port.
withChainObserver :: RunningNode -> HydraExplorerClient -> IO () -> IO ()
withChainObserver node HydraExplorerClient{observerPort} action =
  withProcessExpect process $ const action
 where
  process =
    proc "hydra-chain-observer" $
      ["--node-socket", unFile nodeSocket]
        <> case networkId of
          Mainnet -> ["--mainnet"]
          Testnet (NetworkMagic magic) -> ["--testnet-magic", show magic]
        <> ["--explorer", "http://127.0.0.1:" <> show observerPort]

  RunningNode{nodeSocket, networkId} = node

-- * Helpers

-- | Starts a process like 'withProcessTerm', but captures stderr and does
-- 'checkProcessHasNotDied'.
withProcessExpect ::
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout Handle -> IO ()) ->
  IO ()
withProcessExpect pc action =
  -- NOTE: Not using withProcessTerm_ as we want the sub-process to exit upon
  -- completion of acion, but not fail in that case.
  withProcessTerm (setStderr createPipe pc) $ \p ->
    race_ (check p) (action p)
 where
  check p = checkProcessHasNotDied (show pc) (unsafeProcessHandle p) (Just $ getStderr p)
