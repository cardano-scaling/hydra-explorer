-- | Types for the observer-side REST API.
module Hydra.Explorer.ObservationApi where

import Hydra.Prelude

-- XXX: Depending on hydra-tx:testlib feels wrong here.
import Test.Hydra.Tx.Gen ()

import Data.Aeson (Value (Object), withObject, (.:))
import Data.Aeson.KeyMap (fromMap, toMap)
import Data.Aeson.Types (Parser)
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
    observed <- (o .: "observed") <|> (o .: "observedTx" >>= parseOldObservation)
    pure $ Observation{point, blockNo, observed}

-- FIXME: OnCollectComTx was not containing a utxoHash and contesters were not a thing
parseOldObservation :: Value -> Parser HeadObservation
parseOldObservation =
  withObject "OnChainTx (legacy)" $ \o ->
    parseJSON (Object . fromMap . Map.adjust mapTag "tag" $ toMap o)
 where
  mapTag = \case
    "OnInitTx" -> "Init"
    "OnAbortTx" -> "Abort"
    "OnCommitTx" -> "Commit"
    "OnCollectComTx" -> "CollectCom"
    "OnIncrementTx" -> "Increment"
    "OnDecrementTx" -> "Decrement"
    "OnCloseTx" -> "Close"
    "OnContestTx" -> "Contest"
    "OnFanoutTx" -> "Fanout"
    "OnDepositTx" -> "Deposit"
    "OnRecoverTx" -> "Recover"
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
