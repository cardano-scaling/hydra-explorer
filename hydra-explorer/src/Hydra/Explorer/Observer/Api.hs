-- | Types for the observer-side REST API.
module Hydra.Explorer.Observer.Api where

import Hydra.Prelude

data Observation = Observation
  { point :: ChainPoint
  , blockNo :: BlockNo
  , observedTx :: Maybe (OnChainTx Tx)
  }
  deriving stock (Eq, Show, Generic)
