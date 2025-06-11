module Hydra.Explorer.ExplorerState where

import Hydra.Prelude

-- XXX: Need to depend on hydra-tx:testlib for generators?
import Test.Hydra.Tx.Gen (genUTxO)

import Data.Aeson (Value (..))
import Hydra.Cardano.Api (BlockNo, ChainPoint (..), Network, NetworkId, NetworkMagic (..), TxIn, UTxO, toNetworkMagic, toShelleyNetwork)
import Hydra.Explorer.ObservationApi (HydraVersion (..), Observation (..))
import Hydra.Tx.ContestationPeriod (ContestationPeriod, toNominalDiffTime)
import Hydra.Tx.HeadId (HeadId (..), HeadSeed, headSeedToTxIn)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Observe (
  AbortObservation (..),
  CloseObservation (..),
  CollectComObservation (..),
  CommitObservation (..),
  ContestObservation (..),
  DecrementObservation (..),
  DepositObservation (..),
  FanoutObservation (..),
  HeadObservation (..),
  IncrementObservation (..),
  InitObservation (..),
  RecoverObservation (..),
 )
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (SnapshotNumber (..))
import Test.QuickCheck (oneof)

data HeadMember = HeadMember
  { party :: Party
  , onChainId :: Observed OnChainId
  , commits :: Observed UTxO
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Arbitrary HeadMember where
  arbitrary = do
    commits <- oneof [pure Unknown, Seen <$> genUTxO]
    HeadMember <$> arbitrary <*> arbitrary <*> pure commits

data HeadStatus
  = Initializing
  | Aborted
  | Open
  | Closed
  | Finalized
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Arbitrary HeadStatus where
  arbitrary = genericArbitrary

data Observed a = Unknown | Seen a
  deriving stock (Eq, Show, Generic, Functor)

instance ToJSON a => ToJSON (Observed a) where
  toJSON Unknown = Null
  toJSON (Seen a) = toJSON a

instance FromJSON a => FromJSON (Observed a) where
  parseJSON Null = pure Unknown
  parseJSON value = Seen <$> parseJSON value

instance Arbitrary a => Arbitrary (Observed a) where
  arbitrary = genericArbitrary

-- | Represents the external appearance of a head state.
--
-- The decision to observe certain attributes or not is designed to address situations
-- where the explorer observes a head transaction on the chain without its
-- previously expected observation, preventing the loss of information during the transition.
-- Additionally, this simplifies the API for clients, eliminating the need to match against
-- different states.
data HeadState = HeadState
  { network :: Network
  , networkMagic :: NetworkMagic
  , version :: HydraVersion
  , headId :: HeadId
  , seedTxIn :: Observed TxIn
  , status :: HeadStatus
  , contestationPeriod :: Observed ContestationPeriod
  , members :: Observed [HeadMember]
  , contestations :: Observed Natural
  , snapshotNumber :: Observed Natural
  , contestationDeadline :: Observed UTCTime
  , point :: ChainPoint
  , blockNo :: BlockNo
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Arbitrary HeadState where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- | Represents the latest point in time observed on chain.
data TickState = TickState
  { network :: Network
  , networkMagic :: NetworkMagic
  , point :: ChainPoint
  , blockNo :: BlockNo
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Arbitrary TickState where
  arbitrary = genericArbitrary

data ExplorerState = ExplorerState
  { heads :: [HeadState]
  , ticks :: [TickState]
  }
  deriving (Eq, Show)

instance Arbitrary ExplorerState where
  arbitrary = ExplorerState <$> arbitrary <*> arbitrary

aggregateInitObservation ::
  NetworkId ->
  HydraVersion ->
  HeadId ->
  ChainPoint ->
  BlockNo ->
  HeadSeed ->
  HeadParameters ->
  [OnChainId] ->
  [HeadState] ->
  [HeadState]
aggregateInitObservation
  networkId
  version
  headId
  point
  blockNo
  headSeed
  HeadParameters{parties, contestationPeriod}
  participants
  currentHeads =
    case findHeadState headId currentHeads of
      Just headState -> replaceHeadState headState{status = Initializing} currentHeads
      Nothing -> currentHeads <> [newHeadState]
   where
    newHeadState =
      HeadState
        { network = toShelleyNetwork networkId
        , networkMagic = toNetworkMagic networkId
        , version
        , headId
        , seedTxIn = maybe Unknown Seen (headSeedToTxIn headSeed)
        , status = Initializing
        , contestationPeriod = Seen contestationPeriod
        , members =
            Seen $
              fmap
                ( \(party, onChainId) ->
                    HeadMember
                      { party
                      , onChainId = Seen onChainId
                      , commits = Unknown
                      }
                )
                (parties `zip` participants)
        , contestations = Seen 0
        , snapshotNumber = Seen 0
        , contestationDeadline = Unknown
        , point = point
        , blockNo = blockNo
        }

aggregateAbortObservation ::
  NetworkId ->
  HydraVersion ->
  HeadId ->
  ChainPoint ->
  BlockNo ->
  [HeadState] ->
  [HeadState]
aggregateAbortObservation networkId version headId point blockNo currentHeads =
  case findHeadState headId currentHeads of
    Just headState ->
      let newHeadState = headState{status = Aborted}
       in replaceHeadState newHeadState currentHeads
    Nothing -> currentHeads <> [newUnknownHeadState]
 where
  newUnknownHeadState =
    HeadState
      { network = toShelleyNetwork networkId
      , networkMagic = toNetworkMagic networkId
      , version
      , headId
      , seedTxIn = Unknown
      , status = Aborted
      , contestationPeriod = Unknown
      , members = Unknown
      , contestations = Seen 0
      , snapshotNumber = Seen 0
      , contestationDeadline = Unknown
      , point = point
      , blockNo = blockNo
      }

aggregateCommitObservation ::
  NetworkId ->
  HydraVersion ->
  HeadId ->
  ChainPoint ->
  BlockNo ->
  Party ->
  UTxO ->
  [HeadState] ->
  [HeadState]
aggregateCommitObservation networkId version headId point blockNo party committed currentHeads =
  case findHeadState headId currentHeads of
    Just headState ->
      let newHeadState = updateMember headState
       in replaceHeadState newHeadState currentHeads
    Nothing -> currentHeads <> [newUnknownHeadState]
 where
  updateMember headState@HeadState{members} =
    let members' = case members of
          Unknown -> []
          Seen ms -> ms
     in case find (\HeadMember{party = partyMember} -> partyMember == party) members' of
          Nothing -> headState{members = Seen $ newUnknownMember : members'}
          Just headMember@HeadMember{commits = currentCommits} ->
            let currentCommits' = case currentCommits of
                  Unknown -> mempty
                  Seen utxo -> utxo
                newMember = headMember{commits = Seen $ committed <> currentCommits'}
                newMembers = replaceMember members' newMember
             in headState{members = Seen newMembers}

  replaceMember members newMember@HeadMember{party = newHeadMember} =
    case members of
      [] -> [newMember]
      (headMember@HeadMember{party = currentHeadMember} : tailMembers) ->
        if newHeadMember == currentHeadMember
          then newMember : tailMembers
          else headMember : replaceMember tailMembers newMember

  newUnknownMember =
    HeadMember
      { party
      , onChainId = Unknown
      , commits = Seen committed
      }

  newUnknownHeadState =
    HeadState
      { network = toShelleyNetwork networkId
      , networkMagic = toNetworkMagic networkId
      , version
      , headId
      , seedTxIn = Unknown
      , status = Initializing
      , contestationPeriod = Unknown
      , members = Seen [newUnknownMember]
      , contestations = Seen 0
      , snapshotNumber = Seen 0
      , contestationDeadline = Unknown
      , point = point
      , blockNo = blockNo
      }

aggregateCollectComObservation ::
  NetworkId ->
  HydraVersion ->
  HeadId ->
  ChainPoint ->
  BlockNo ->
  [HeadState] ->
  [HeadState]
aggregateCollectComObservation networkId version headId point blockNo currentHeads =
  case findHeadState headId currentHeads of
    Just headState ->
      let newHeadState = headState{status = Open}
       in replaceHeadState newHeadState currentHeads
    Nothing -> currentHeads <> [newUnknownHeadState]
 where
  newUnknownHeadState =
    HeadState
      { network = toShelleyNetwork networkId
      , networkMagic = toNetworkMagic networkId
      , version
      , headId
      , seedTxIn = Unknown
      , status = Open
      , contestationPeriod = Unknown
      , members = Unknown
      , contestations = Seen 0
      , snapshotNumber = Seen 0
      , contestationDeadline = Unknown
      , point = point
      , blockNo = blockNo
      }

aggregateDepositObservation ::
  NetworkId ->
  HydraVersion ->
  HeadId ->
  ChainPoint ->
  BlockNo ->
  [HeadState] ->
  [HeadState]
aggregateDepositObservation networkId version headId point blockNo currentHeads =
  case findHeadState headId currentHeads of
    Just headState ->
      let newHeadState = headState{status = Open}
       in replaceHeadState newHeadState currentHeads
    Nothing -> currentHeads <> [newUnknownHeadState]
 where
  newUnknownHeadState =
    HeadState
      { network = toShelleyNetwork networkId
      , networkMagic = toNetworkMagic networkId
      , version
      , headId
      , seedTxIn = Unknown
      , status = Open
      , contestationPeriod = Unknown
      , members = Unknown
      , contestations = Seen 0
      , snapshotNumber = Seen 0
      , contestationDeadline = Unknown
      , point = point
      , blockNo = blockNo
      }

aggregateRecoverObservation ::
  NetworkId ->
  HydraVersion ->
  HeadId ->
  ChainPoint ->
  BlockNo ->
  [HeadState] ->
  [HeadState]
aggregateRecoverObservation networkId version headId point blockNo currentHeads =
  case findHeadState headId currentHeads of
    Just headState ->
      let newHeadState = headState{status = Open}
       in replaceHeadState newHeadState currentHeads
    Nothing -> currentHeads <> [newUnknownHeadState]
 where
  newUnknownHeadState =
    HeadState
      { network = toShelleyNetwork networkId
      , networkMagic = toNetworkMagic networkId
      , version
      , headId
      , seedTxIn = Unknown
      , status = Open
      , contestationPeriod = Unknown
      , members = Unknown
      , contestations = Seen 0
      , snapshotNumber = Seen 0
      , contestationDeadline = Unknown
      , point = point
      , blockNo = blockNo
      }

aggregateIncrementObservation ::
  NetworkId ->
  HydraVersion ->
  HeadId ->
  ChainPoint ->
  BlockNo ->
  [HeadState] ->
  [HeadState]
aggregateIncrementObservation networkId version headId point blockNo currentHeads =
  case findHeadState headId currentHeads of
    Just headState ->
      let newHeadState = headState{status = Open}
       in replaceHeadState newHeadState currentHeads
    Nothing -> currentHeads <> [newUnknownHeadState]
 where
  newUnknownHeadState =
    HeadState
      { network = toShelleyNetwork networkId
      , networkMagic = toNetworkMagic networkId
      , version
      , headId
      , seedTxIn = Unknown
      , status = Open
      , contestationPeriod = Unknown
      , members = Unknown
      , contestations = Seen 0
      , snapshotNumber = Seen 0
      , contestationDeadline = Unknown
      , point = point
      , blockNo = blockNo
      }

aggregateDecrementObservation ::
  NetworkId ->
  HydraVersion ->
  HeadId ->
  ChainPoint ->
  BlockNo ->
  [HeadState] ->
  [HeadState]
aggregateDecrementObservation networkId version headId point blockNo currentHeads =
  case findHeadState headId currentHeads of
    Just headState ->
      let newHeadState = headState{status = Open}
       in replaceHeadState newHeadState currentHeads
    Nothing -> currentHeads <> [newUnknownHeadState]
 where
  newUnknownHeadState =
    HeadState
      { network = toShelleyNetwork networkId
      , networkMagic = toNetworkMagic networkId
      , version
      , headId
      , seedTxIn = Unknown
      , status = Open
      , contestationPeriod = Unknown
      , members = Unknown
      , contestations = Seen 0
      , snapshotNumber = Seen 0
      , contestationDeadline = Unknown
      , point = point
      , blockNo = blockNo
      }

aggregateCloseObservation ::
  NetworkId ->
  HydraVersion ->
  HeadId ->
  ChainPoint ->
  BlockNo ->
  SnapshotNumber ->
  UTCTime ->
  [HeadState] ->
  [HeadState]
aggregateCloseObservation
  networkId
  version
  headId
  point
  blockNo
  (UnsafeSnapshotNumber sn)
  contestationDeadline
  currentHeads =
    case findHeadState headId currentHeads of
      Just headState ->
        let newHeadState =
              headState
                { status = Closed
                , contestations = Seen 0
                , snapshotNumber = Seen sn
                , contestationDeadline = Seen contestationDeadline
                }
         in replaceHeadState newHeadState currentHeads
      Nothing -> currentHeads <> [newUnknownHeadState]
   where
    newUnknownHeadState =
      HeadState
        { network = toShelleyNetwork networkId
        , networkMagic = toNetworkMagic networkId
        , version
        , headId
        , seedTxIn = Unknown
        , status = Closed
        , contestationPeriod = Unknown
        , members = Unknown
        , contestations = Seen 0
        , snapshotNumber = Seen sn
        , contestationDeadline = Seen contestationDeadline
        , point = point
        , blockNo = blockNo
        }

aggregateContestObservation ::
  NetworkId ->
  HydraVersion ->
  HeadId ->
  ChainPoint ->
  BlockNo ->
  SnapshotNumber ->
  [HeadState] ->
  [HeadState]
aggregateContestObservation networkId version headId point blockNo (UnsafeSnapshotNumber sn) currentHeads =
  case findHeadState headId currentHeads of
    Just headState@HeadState{contestations, contestationPeriod, contestationDeadline} ->
      let newHeadState =
            headState
              { contestations = (+ 1) <$> contestations
              , snapshotNumber = Seen sn
              , contestationDeadline =
                  case (contestationPeriod, contestationDeadline) of
                    (Seen cp, Seen cd) -> Seen $ addUTCTime (toNominalDiffTime cp) cd
                    _ -> Unknown
              }
       in replaceHeadState newHeadState currentHeads
    Nothing -> currentHeads <> [newUnknownHeadState]
 where
  newUnknownHeadState =
    HeadState
      { network = toShelleyNetwork networkId
      , networkMagic = toNetworkMagic networkId
      , version
      , headId
      , seedTxIn = Unknown
      , status = Closed
      , contestationPeriod = Unknown
      , members = Unknown
      , contestations = Seen 1
      , snapshotNumber = Seen sn
      , contestationDeadline = Unknown
      , point = point
      , blockNo = blockNo
      }

aggregateFanoutObservation ::
  NetworkId ->
  HydraVersion ->
  HeadId ->
  ChainPoint ->
  BlockNo ->
  [HeadState] ->
  [HeadState]
aggregateFanoutObservation networkId version headId point blockNo currentHeads =
  case findHeadState headId currentHeads of
    Just headState ->
      let newHeadState = headState{status = Finalized}
       in replaceHeadState newHeadState currentHeads
    Nothing -> currentHeads <> [newUnknownHeadState]
 where
  newUnknownHeadState =
    HeadState
      { network = toShelleyNetwork networkId
      , networkMagic = toNetworkMagic networkId
      , version
      , headId
      , seedTxIn = Unknown
      , status = Finalized
      , contestationPeriod = Unknown
      , members = Unknown
      , contestations = Unknown
      , snapshotNumber = Unknown
      , contestationDeadline = Unknown
      , point = point
      , blockNo = blockNo
      }

replaceHeadState :: HeadState -> [HeadState] -> [HeadState]
replaceHeadState newHeadState@HeadState{headId = newHeadStateId} currentHeads =
  case currentHeads of
    [] -> [newHeadState]
    (currentHeadState@HeadState{headId = currentHeadStateId} : tailStates) ->
      if newHeadStateId == currentHeadStateId
        then newHeadState : tailStates
        else currentHeadState : replaceHeadState newHeadState tailStates

replaceTickState :: TickState -> [TickState] -> [TickState]
replaceTickState newTickState@TickState{network = newNetwork, networkMagic = newNetworkMagic} currentTicks =
  case currentTicks of
    [] -> [newTickState]
    (currentTickState@TickState{network = currentNetwork, networkMagic = currentNetworkMagic} : tailStates) ->
      if newNetwork == currentNetwork && newNetworkMagic == currentNetworkMagic
        then newTickState : tailStates
        else currentTickState : replaceTickState newTickState tailStates

aggregateTickObservation ::
  NetworkId ->
  ChainPoint ->
  BlockNo ->
  [TickState] ->
  [TickState]
aggregateTickObservation networkId point blockNo currentTicks =
  case findTickState networkId currentTicks of
    Just _ ->
      let newTickState = newUnknownTickState
       in replaceTickState newTickState currentTicks
    Nothing -> currentTicks <> [newUnknownTickState]
 where
  newUnknownTickState =
    TickState
      { network = toShelleyNetwork networkId
      , networkMagic = toNetworkMagic networkId
      , point
      , blockNo
      }

-- XXX: Aggregation is very conservative and does ignore most information when
-- matching data is already present.
aggregateObservation ::
  NetworkId ->
  HydraVersion ->
  Observation ->
  ExplorerState ->
  ExplorerState
aggregateObservation networkId version Observation{point, blockNo, observed} ExplorerState{heads, ticks} =
  case observed of
    NoHeadTx ->
      ExplorerState
        { heads
        , ticks = aggregateTickObservation networkId point blockNo ticks
        }
    Init InitObservation{headId, headSeed, headParameters, participants} ->
      ExplorerState
        { heads = aggregateInitObservation networkId version headId point blockNo headSeed headParameters participants heads
        , ticks = aggregateTickObservation networkId point blockNo ticks
        }
    Abort AbortObservation{headId} ->
      ExplorerState
        { heads = aggregateAbortObservation networkId version headId point blockNo heads
        , ticks = aggregateTickObservation networkId point blockNo ticks
        }
    Commit CommitObservation{headId, party, committed} ->
      ExplorerState
        { heads = aggregateCommitObservation networkId version headId point blockNo party committed heads
        , ticks = aggregateTickObservation networkId point blockNo ticks
        }
    CollectCom CollectComObservation{headId} ->
      ExplorerState
        { heads = aggregateCollectComObservation networkId version headId point blockNo heads
        , ticks = aggregateTickObservation networkId point blockNo ticks
        }
    Deposit DepositObservation{headId} ->
      ExplorerState
        { heads = aggregateDepositObservation networkId version headId point blockNo heads
        , ticks = aggregateTickObservation networkId point blockNo ticks
        }
    Recover RecoverObservation{headId} ->
      ExplorerState
        { heads = aggregateRecoverObservation networkId version headId point blockNo heads
        , ticks = aggregateTickObservation networkId point blockNo ticks
        }
    Increment IncrementObservation{headId} ->
      ExplorerState
        { heads = aggregateIncrementObservation networkId version headId point blockNo heads
        , ticks = aggregateTickObservation networkId point blockNo ticks
        }
    Decrement DecrementObservation{headId} ->
      ExplorerState
        { heads = aggregateDecrementObservation networkId version headId point blockNo heads
        , ticks = aggregateTickObservation networkId point blockNo ticks
        }
    Close CloseObservation{headId, snapshotNumber, contestationDeadline} ->
      ExplorerState
        { heads = aggregateCloseObservation networkId version headId point blockNo snapshotNumber contestationDeadline heads
        , ticks = aggregateTickObservation networkId point blockNo ticks
        }
    Contest ContestObservation{headId, snapshotNumber} ->
      ExplorerState
        { heads = aggregateContestObservation networkId version headId point blockNo snapshotNumber heads
        , ticks = aggregateTickObservation networkId point blockNo ticks
        }
    Fanout FanoutObservation{headId} ->
      ExplorerState
        { heads = aggregateFanoutObservation networkId version headId point blockNo heads
        , ticks = aggregateTickObservation networkId point blockNo ticks
        }

findHeadState :: HeadId -> [HeadState] -> Maybe HeadState
findHeadState idToFind = find (\HeadState{headId} -> headId == idToFind)

findTickState :: NetworkId -> [TickState] -> Maybe TickState
findTickState networkId =
  find
    ( \TickState{network, networkMagic} ->
        toShelleyNetwork networkId == network && toNetworkMagic networkId == networkMagic
    )
