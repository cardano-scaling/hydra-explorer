{-# LANGUAGE RecordWildCards #-}

-- | Types for the observer-side REST API.
module Hydra.Explorer.ObservationApi where

import Hydra.Prelude

-- XXX: Depending on hydra-tx:testlib feels wrong here.
import Test.Hydra.Tx.Gen ()

import Data.Aeson (Value (..), withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, typeMismatch)
import Hydra.Cardano.Api (BlockNo, ChainPoint, NetworkId (..), NetworkMagic (..), fromNetworkMagic)
import Hydra.Tx.CollectCom (UTxOHash (..))
import Hydra.Tx.Observe (
  CollectComObservation (..),
  ContestObservation (..),
  DepositObservation (..),
  HeadObservation (..),
 )
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

-- | Backwards compatible parser for the old OnChainTx type that was used to
-- send observations to the explorer in hydra-chain-observer < 0.22
parseOldObservation :: Value -> Parser HeadObservation
parseOldObservation = \case
  Null -> pure NoHeadTx
  Object o -> do
    tag <- o .: "tag"
    case tag :: Text of
      "OnInitTx" -> Init <$> parseJSON (Object o)
      "OnAbortTx" -> Abort <$> parseJSON (Object o)
      "OnCommitTx" -> Commit <$> parseJSON (Object o)
      "OnCollectComTx" -> do
        headId <- o .: "headId"
        -- NOTE: OnChainTx did not have 'utxoHash'
        utxoHash <- o .:? "utxoHash" .!= UTxOHash "unknown"
        pure $ CollectCom CollectComObservation{..}
      "OnIncrementTx" -> Increment <$> parseJSON (Object o)
      "OnDecrementTx" -> Decrement <$> parseJSON (Object o)
      "OnCloseTx" -> Close <$> parseJSON (Object o)
      "OnContestTx" -> do
        headId <- o .: "headId"
        snapshotNumber <- o .: "snapshotNumber"
        contestationDeadline <- o .: "contestationDeadline"
        -- NOTE: OnChainTx does not have 'contesters'
        contesters <- o .:? "contesters" .!= []
        pure $ Contest ContestObservation{..}
      "OnFanoutTx" -> Fanout <$> parseJSON (Object o)
      "OnDepositTx" -> do
        headId <- o .: "headId"
        depositTxId <- o .: "depositTxId"
        deposited <- o .: "deposited"
        deadline <- o .: "deadline"
        -- NOTE: OnChainTx did not have 'created' < 0.22
        created <- o .:? "created" .!= 0
        pure $ Deposit DepositObservation{..}
      "OnRecoverTx" -> Recover <$> parseJSON (Object o)
      _ -> fail $ "unknown tag: " <> show tag
  v -> typeMismatch "HeadObservation (legacy)" v

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
