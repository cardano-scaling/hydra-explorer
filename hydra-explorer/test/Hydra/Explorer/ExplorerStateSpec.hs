-- | Unit tests for the aggregation logic in ExplorerState.
module Hydra.Explorer.ExplorerStateSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Explorer.ExplorerState (
  ExplorerState (..),
  HeadState (..),
  aggregateObservation,
 )
import Hydra.Explorer.ObservationApi (Observation (..))
import Hydra.Tx.Observe (HeadObservation (NoHeadTx))
import Test.QuickCheck (counterexample, forAll, (=/=), (===), (==>))

-- NOTE: Orphan instances for NetworkId
import Hydra.Cardano.Api (toNetworkMagic)

spec :: Spec
spec = do
  prop "Any observation of a head transaction must result in an entry of that head id" $ \network version observation ->
    observed observation /= NoHeadTx ==>
      let ExplorerState{heads} = aggregateObservation network version observation (ExplorerState [] [])
       in heads =/= []

  prop "Given any observations, the resulting list of head ids is a prefix of the original" $ \network version observation ->
    forAll arbitrary $ \initialHeads -> do
      let resultState = aggregateObservation network version observation (ExplorerState initialHeads [])
      getHeadIds initialHeads `isPrefixOf` getHeadIds (heads resultState)

  prop "Network is not updated" $ \network1 network2 version observation ->
    observed observation /= NoHeadTx ==>
      let resultState =
            ExplorerState [] []
              & aggregateObservation network1 version observation
              & aggregateObservation network2 version observation
       in (networkMagic <$> heads resultState) === [toNetworkMagic network1]
            & counterexample (show resultState)

  prop "Version is not updated" $ \network version1 version2 observation ->
    observed observation /= NoHeadTx ==>
      let resultState =
            ExplorerState [] []
              & aggregateObservation network version1 observation
              & aggregateObservation network version2 observation
       in (version <$> heads resultState) === [version1]
            & counterexample (show resultState)
 where
  getHeadIds = fmap headId
