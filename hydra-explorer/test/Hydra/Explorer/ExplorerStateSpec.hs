-- | Unit tests for the aggregation logic in ExplorerState.
module Hydra.Explorer.ExplorerStateSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Explorer.ExplorerState (
  ExplorerState (..),
  HeadState (..),
  aggregateObservations,
  initialTickState,
 )
import Hydra.Explorer.Observer.Api (Observation (..))
import Test.QuickCheck (forAll, listOf1, (=/=))

spec :: Spec
spec = do
  describe "aggregate head observation into explorer state" $ do
    -- This ensures that the explorer always at least knows about the existence of a head.
    -- Even if we only observe a part of the life cycle of some head.
    prop "Any head observations (of some head id) must yield an entry of that head id" $
      forAll genObservations $ \observations ->
        let ExplorerState{heads} = aggregateObservations observations (ExplorerState [] initialTickState)
         in heads =/= []

    prop "Given any observations, the resulting list of head ids is a prefix of the original" $
      forAll genObservations $ \observations ->
        forAll arbitrary $ \initialHeads -> do
          let resultExplorerState = aggregateObservations observations (ExplorerState initialHeads initialTickState)
          getHeadIds initialHeads `isPrefixOf` getHeadIds (heads resultExplorerState)
 where
  genObservations :: Gen [Observation]
  genObservations =
    listOf1 $ Observation <$> arbitrary <*> arbitrary <*> arbitrary

  getHeadIds = fmap headId
