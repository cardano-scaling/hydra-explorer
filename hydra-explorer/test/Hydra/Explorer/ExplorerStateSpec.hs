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
import Hydra.Explorer.ObservationApi (Observation (..))
import Test.QuickCheck (counterexample, forAll, (=/=), (===), (==>))

-- NOTE: Orphan instances for NetworkId
import Hydra.Cardano.Api (toNetworkMagic)

spec :: Spec
spec = do
  prop "Any observation of a head transaction must result in an entry of that head id" $ \network version observation ->
    isJust (observedTx observation) ==>
      let ExplorerState{heads} = aggregateObservation network version observation (ExplorerState [] initialTickState)
       in heads =/= []

  prop "Given any observations, the resulting list of head ids is a prefix of the original" $ \network version observation ->
    forAll arbitrary $ \initialHeads -> do
      let resultState = aggregateObservation network version observation (ExplorerState initialHeads initialTickState)
      getHeadIds initialHeads `isPrefixOf` getHeadIds (heads resultState)

  prop "Network is not updated" $ \network1 network2 version observation ->
    isJust (observedTx observation) ==>
      let resultState =
            ExplorerState [] initialTickState
              & aggregateObservation network1 version observation
              & aggregateObservation network2 version observation
       in (networkMagic <$> heads resultState) === [toNetworkMagic network1]
            & counterexample (show resultState)

  prop "Version is not updated" $ \network version1 version2 observation ->
    isJust (observedTx observation) ==>
      let resultState =
            ExplorerState [] initialTickState
              & aggregateObservation network version1 observation
              & aggregateObservation network version2 observation
       in (version <$> heads resultState) === [version1]
            & counterexample (show resultState)
 where
  getHeadIds = fmap headId
