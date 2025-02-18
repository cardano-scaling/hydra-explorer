-- | Types for the observer-side REST API.
module Hydra.Explorer.ObservationApi where

import Hydra.Prelude

-- XXX: Depending on hydra-tx:testlib feels wrong here.
import Test.Hydra.Tx.Gen ()

import Hydra.Cardano.Api (BlockNo, ChainPoint, NetworkId (..), NetworkMagic (..), Tx, fromNetworkMagic)
import Hydra.Chain (OnChainTx)
import Servant (Capture, FromHttpApiData (..), JSON, Post, ReqBody, (:>))

data Observation = Observation
  { point :: ChainPoint
  , blockNo :: BlockNo
  , observedTx :: Maybe (OnChainTx Tx)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance Arbitrary Observation where
  arbitrary = Observation <$> arbitrary <*> arbitrary <*> arbitrary

-- | New type wrapper for defining API instances over a 'NetworkId'.
newtype NetworkParam = NetworkParam NetworkId

instance FromHttpApiData NetworkParam where
  parseUrlPiece = \case
    "mainnet" -> Right $ NetworkParam Mainnet
    t -> maybe (Left $ "failed to parse network magic: " <> show t) Right $ do
      magic <- readMaybe $ toString t
      pure $ NetworkParam $ fromNetworkMagic $ NetworkMagic magic

-- | Version of a hydra head as reported by chain observers.
newtype HydraVersion = HydraVersion Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Arbitrary, FromHttpApiData, Semigroup, Monoid)

-- TODO: test correspondence with openapi
type ObservationApi =
  "observations"
    :> Capture "network" NetworkParam
    :> Capture "version" HydraVersion
    :> ReqBody '[JSON] Observation
    :> Post '[JSON] ()
