-- | Unit tests for the aggregation logic in ExplorerState.
module Hydra.Explorer.ExplorerStateSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Explorer.ExplorerState (
  ExplorerState (..),
  HeadState (..),
  aggregateObservation,
  initialTickState,
 )
import Test.QuickCheck (forAll, (=/=))

-- NOTE: Orphan instances for NetworkId
import Hydra.Cardano.Api ()

spec :: Spec
spec = do
  prop "Any observation of a head transaction must result in an entry of that head id" $ \network version observation ->
    observedTx observation
      /= Nothing
      ==> let ExplorerState{heads} = aggregateObservation network version observation (ExplorerState [] initialTickState)
           in heads =/= []

  prop "Given any observations, the resulting list of head ids is a prefix of the original" $ \network version observation ->
    forAll arbitrary $ \initialHeads -> do
      let resultExplorerState = aggregateObservations observations (ExplorerState initialHeads initialTickState)
      getHeadIds initialHeads `isPrefixOf` getHeadIds (heads resultExplorerState)
 where
  getHeadIds = fmap headId
