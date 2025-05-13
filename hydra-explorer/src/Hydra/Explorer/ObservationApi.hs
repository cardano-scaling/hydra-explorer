-- | Types for the observer-side REST API.
module Hydra.Explorer.ObservationApi where

import Hydra.Prelude

-- XXX: Depending on hydra-tx:testlib feels wrong here.
import Test.Hydra.Tx.Gen ()

import Data.Aeson (Value (Object), withObject, (.:))
import Data.Aeson.KeyMap (fromMap, toMap)
import Data.Map qualified as Map
import Hydra.Cardano.Api (BlockNo, ChainPoint, NetworkId (..), NetworkMagic (..), fromNetworkMagic)
import Hydra.Tx.Observe (HeadObservation)
import Servant (Capture, FromHttpApiData (..), JSON, Post, ReqBody, (:>))

data Observation = Observation
  { point :: ChainPoint
  , blockNo :: BlockNo
  , observed :: HeadObservation
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON Observation where
  parseJSON = withObject "Observation" $ \o -> do
    point <- o .: "point"
    blockNo <- o .: "blockNo"
    observed <- o .: "observed" <|> (o .: "observedTx" >>= oldAPI)
    pure $ Observation{point, blockNo, observed}
   where
    oldAPI =
      withObject "OnChainTx (legacy)" $ \o ->
        parseJSON (Object . fromMap . Map.adjust mapTag "tag" $ toMap o)

    mapTag = \case
      "OnInitTx" -> "Init"
      s -> s

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
  deriving newtype (FromJSON, ToJSON, Arbitrary, FromHttpApiData)

-- TODO: test correspondence with openapi
type ObservationApi =
  "observations"
    :> Capture "network" NetworkParam
    :> Capture "version" HydraVersion
    :> ReqBody '[JSON] Observation
    :> Post '[JSON] ()
