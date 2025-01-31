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
import Hydra.Cardano.Api (NetworkId (..), NetworkMagic (..), unFile)
import Hydra.Cluster.Faucet (FaucetLog, publishHydraScriptsAs, seedFromFaucet_)
import Hydra.Cluster.Fixture (Actor (..), aliceSk, bobSk, cperiod)
import Hydra.Cluster.Scenarios (EndToEndLog, singlePartyHeadFullLifeCycle)
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.Tx.DepositDeadline (DepositDeadline (..))
import HydraNode (HydraNodeLog, input, send, waitMatch, withHydraNode)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequestThrow)
import Network.Socket (PortNumber)
import System.Process.Typed (Process, ProcessConfig, createPipe, getStderr, proc, setStderr, unsafeProcessHandle, withProcessTerm)
import Test.Network.Ports (withFreePort)

spec :: Spec
spec = do
  -- Simple end-to-end scenario with a cardano devnet and head opened/closed by
  -- a hydra-node, which a hydra-chain-observer reports into a single
  -- hydra-explorer.
  it "aggregates observations" $
    failAfter 60 $
      showLogsOnFailure "IntegrationSpec" $ \tracer -> do
        withTempDir "hydra-explorer-history" $ \tmpDir -> do
          withHydraExplorer $ \explorer -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node -> do
              hydraScriptsTxId <- publishHydraScriptsAs node Faucet
              withChainObserver node explorer $ do
                -- Open and close a head
                singlePartyHeadFullLifeCycle (contramap FromScenario tracer) tmpDir node hydraScriptsTxId
                -- Query client api
                allHeads <- getHeads explorer
                -- FIXME: should assert version and network here or in a different test
                length (allHeads ^. _Array) `shouldBe` 1

  -- TODO: simplify scenarios! We are only interested in the end-to-end
  -- integration and not whether the hydra-chain-observer really works (that is
  -- to be covered in the main hydra repo)
  it "can observe hydra transactions created by multiple hydra-nodes" $
    failAfter 60 $
      showLogsOnFailure "IntegrationSpec" $ \tracer -> do
        withTempDir "hydra-explorer-history" $ \tmpDir -> do
          withHydraExplorer $ \explorer -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \cardanoNode@RunningNode{nodeSocket} -> do
              withChainObserver cardanoNode explorer $ do
                let hydraTracer = contramap FromHydraNode tracer
                hydraScriptsTxId <- publishHydraScriptsAs cardanoNode Faucet

                let initHead hydraNode = do
                      send hydraNode $ input "Init" []
                      waitMatch 5 hydraNode $ \v -> do
                        guard $ v ^? key "tag" == Just "HeadIsInitializing"
                        v ^? key "headId" . _String

                let depositDeadline = UnsafeDepositDeadline 200
                (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
                aliceChainConfig <- chainConfigFor Alice tmpDir nodeSocket hydraScriptsTxId [] cperiod depositDeadline
                seedFromFaucet_ cardanoNode aliceCardanoVk 25_000_000 (contramap FromFaucet tracer)
                aliceHeadId <- withHydraNode hydraTracer aliceChainConfig tmpDir 1 aliceSk [] [1] initHead

                (bobCardanoVk, _bobCardanoSk) <- keysFor Bob
                bobChainConfig <- chainConfigFor Bob tmpDir nodeSocket hydraScriptsTxId [] cperiod depositDeadline
                seedFromFaucet_ cardanoNode bobCardanoVk 25_000_000 (contramap FromFaucet tracer)
                bobHeadId <- withHydraNode hydraTracer bobChainConfig tmpDir 2 bobSk [] [2] initHead

                allHeads <- getHeads explorer
                length (allHeads ^. _Array) `shouldBe` 2
                allHeads ^. nth 0 . key "headId" . _String `shouldBe` aliceHeadId
                allHeads ^. nth 0 . key "status" . _String `shouldBe` "Initializing"
                allHeads ^. nth 1 . key "headId" . _String `shouldBe` bobHeadId
                allHeads ^. nth 1 . key "status" . _String `shouldBe` "Initializing"

  it "can query for all hydra heads observed" $
    failAfter 60 $
      showLogsOnFailure "IntegrationSpec" $ \tracer -> do
        withTempDir "hydra-explorer-get-heads" $ \tmpDir -> do
          withHydraExplorer $ \explorer -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \cardanoNode@RunningNode{nodeSocket} -> do
              withChainObserver cardanoNode explorer $ do
                let hydraTracer = contramap FromHydraNode tracer
                hydraScriptsTxId <- publishHydraScriptsAs cardanoNode Faucet
                let depositDeadline = UnsafeDepositDeadline 200
                (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
                aliceChainConfig <- chainConfigFor Alice tmpDir nodeSocket hydraScriptsTxId [] cperiod depositDeadline
                seedFromFaucet_ cardanoNode aliceCardanoVk 25_000_000 (contramap FromFaucet tracer)
                aliceHeadId <- withHydraNode hydraTracer aliceChainConfig tmpDir 1 aliceSk [] [1] $ \hydraNode -> do
                  send hydraNode $ input "Init" []

                  waitMatch 5 hydraNode $ \v -> do
                    guard $ v ^? key "tag" == Just "HeadIsInitializing"
                    v ^? key "headId" . _String

                (bobCardanoVk, _bobCardanoSk) <- keysFor Bob
                bobChainConfig <- chainConfigFor Bob tmpDir nodeSocket hydraScriptsTxId [] cperiod depositDeadline
                seedFromFaucet_ cardanoNode bobCardanoVk 25_000_000 (contramap FromFaucet tracer)
                bobHeadId <- withHydraNode hydraTracer bobChainConfig tmpDir 2 bobSk [] [2] $ \hydraNode -> do
                  send hydraNode $ input "Init" []

                  bobHeadId <- waitMatch 5 hydraNode $ \v -> do
                    guard $ v ^? key "tag" == Just "HeadIsInitializing"
                    v ^? key "headId" . _String

                  send hydraNode $ input "Abort" []

                  waitMatch 5 hydraNode $ \v -> do
                    guard $ v ^? key "tag" == Just "HeadIsAborted"
                    guard $ v ^? key "headId" . _String == Just bobHeadId

                  pure bobHeadId

                allHeads <- getHeads explorer
                length (allHeads ^. _Array) `shouldBe` 2
                allHeads ^. nth 0 . key "headId" . _String `shouldBe` aliceHeadId
                allHeads ^. nth 0 . key "status" . _String `shouldBe` "Initializing"
                allHeads ^. nth 1 . key "headId" . _String `shouldBe` bobHeadId
                allHeads ^. nth 1 . key "status" . _String `shouldBe` "Aborted"

  it "can query for latest point in time observed on chain" $
    failAfter 60 $
      showLogsOnFailure "IntegrationSpec" $ \tracer -> do
        withTempDir "hydra-explorer-get-tick" $ \tmpDir -> do
          withHydraExplorer $ \explorer -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node@RunningNode{nodeSocket, networkId} -> do
              withChainObserver node explorer $ do
                threadDelay 1
                tip <- toJSON <$> queryTip networkId nodeSocket
                tick <- getTick explorer

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
  , getTick :: IO Value
  }

-- | Starts a 'hydra-explorer'.
withHydraExplorer :: (HydraExplorerClient -> IO ()) -> IO ()
withHydraExplorer action =
  withFreePort $ \clientPort ->
    withFreePort $ \observerPort -> do
      let process =
            proc "hydra-explorer" $
              ["--client-port", show clientPort]
                <> ["--observer-port", show observerPort]
      withProcessExpect process $ \_p -> do
        -- XXX: wait for the http server to be listening on port
        threadDelay 1
        action
          HydraExplorerClient
            { clientPort
            , observerPort
            , getHeads = getJSON clientPort "/heads"
            , getTick = getJSON clientPort "/tick"
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
withChainObserver cardanoNode HydraExplorerClient{observerPort} action =
  withProcessExpect process $ const action
 where
  process =
    proc
      "hydra-chain-observer"
      $ ["--node-socket", unFile nodeSocket]
        <> case networkId of
          Mainnet -> ["--mainnet"]
          Testnet (NetworkMagic magic) -> ["--testnet-magic", show magic]
        <> ["--explorer", "http://127.0.0.1:" <> show observerPort]

  RunningNode{nodeSocket, networkId} = cardanoNode

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
