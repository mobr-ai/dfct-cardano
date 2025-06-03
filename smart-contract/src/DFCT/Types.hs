{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}

module DFCT.Types
    ( TopicStatus(..)
    , ContributionType(..)
    , ContributionStatus(..)
    , Topic(..)
    , Contribution(..)
    , TopicDatum(..)
    , ContributionDatum(..)
    , TopicAction(..)
    , ContributionAction(..)
    , AdminAction(..)
    , DFCTContrib(..)
    , RewardPoolInfo(..)
    , ReviewContent(..)
    , DisputeReason(..)
    , ReviewerMap
    , RewardMap
    ) where

import           GHC.Generics             (Generic)
import           PlutusLedgerApi.V3
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import qualified PlutusTx.AssocMap as Map

type ReviewerMap = Map.Map PubKeyHash Integer
type RewardMap   = Map.Map PubKeyHash Integer

data TopicStatus = TopicProposed
                 | TopicReviewed
                 | TopicActivated
                 | TopicClosed
                 | TopicRejected
                 deriving (Generic)

instance Eq TopicStatus where
    (==) TopicProposed TopicProposed = True
    (==) TopicReviewed TopicReviewed = True
    (==) TopicActivated TopicActivated = True
    (==) TopicClosed TopicClosed = True
    (==) TopicRejected TopicRejected = True
    (==) _ _ = False

PlutusTx.makeIsDataIndexed ''TopicStatus [('TopicProposed, 0), ('TopicReviewed, 1), ('TopicActivated, 2), ('TopicClosed, 3), ('TopicRejected, 4)]
PlutusTx.makeLift ''TopicStatus

data ContributionType = Evidence
                      | TagSelection
                      | VoteCasted
                      deriving (Generic)

instance Eq ContributionType where
    (==) Evidence Evidence = True
    (==) TagSelection TagSelection = True
    (==) VoteCasted VoteCasted = True
    (==) _ _ = False

PlutusTx.makeIsDataIndexed ''ContributionType [('Evidence, 0), ('TagSelection, 1), ('VoteCasted, 2)]
PlutusTx.makeLift ''ContributionType

data ContributionStatus = ContributionProposed
                        | ContributionReviewed
                        | ContributionDisputed
                        | ContributionUpdated
                        | ContributionRejected
                        | ContributionVerified
                        | ContributionEvaluated
                        | RewardsDistributed
                        | PoolEvaluated
                        deriving (Generic)

instance Eq ContributionStatus where
    (==) ContributionProposed ContributionProposed = True
    (==) ContributionReviewed ContributionReviewed = True
    (==) ContributionDisputed ContributionDisputed = True
    (==) ContributionUpdated ContributionUpdated = True
    (==) ContributionRejected ContributionRejected = True
    (==) ContributionVerified ContributionVerified = True
    (==) ContributionEvaluated ContributionEvaluated = True
    (==) RewardsDistributed RewardsDistributed = True
    (==) PoolEvaluated PoolEvaluated = True
    (==) _ _ = False

PlutusTx.makeIsDataIndexed ''ContributionStatus [
    ('ContributionProposed, 0),
    ('ContributionReviewed, 1),
    ('ContributionDisputed, 2),
    ('ContributionUpdated, 3),
    ('ContributionRejected, 4),
    ('ContributionVerified, 5),
    ('ContributionEvaluated, 6),
    ('RewardsDistributed, 7),
    ('PoolEvaluated, 8)]
PlutusTx.makeLift ''ContributionStatus

data Topic = Topic
    { topicId          :: BuiltinByteString
    , topicProposer    :: PubKeyHash
    , topicTimestamp   :: Integer
    } deriving (Generic)

PlutusTx.makeIsDataIndexed ''Topic [('Topic, 0)]
PlutusTx.makeLift ''Topic

data Contribution = Contribution
    { contributionId        :: BuiltinByteString
    , contributionTopicId   :: BuiltinByteString
    , contributionType      :: ContributionType
    , contributionCreator   :: PubKeyHash
    , contributionTimestamp :: Integer
    , contributionVersion   :: Integer            -- track version number
    , previousVersionId     :: BuiltinByteString  -- reference to previous version (empty if first version)
    } deriving (Generic)

PlutusTx.makeIsDataIndexed ''Contribution [('Contribution, 0)]
PlutusTx.makeLift ''Contribution

data ReviewContent = ReviewContent
    { reviewerPkh         :: PubKeyHash
    , reviewTimestamp     :: Integer
    } deriving (Generic)

PlutusTx.makeIsDataIndexed ''ReviewContent [('ReviewContent, 0)]
PlutusTx.makeLift ''ReviewContent

data DisputeReason = DisputeReason
    { disputeInitiator  :: PubKeyHash
    , disputeTimestamp  :: Integer
    } deriving (Generic)

PlutusTx.makeIsDataIndexed ''DisputeReason [('DisputeReason, 0)]
PlutusTx.makeLift ''DisputeReason

data RewardPoolInfo = RewardPoolInfo
    { totalAmount       :: Integer
    , allocatedAmount   :: Integer
    , tokenName         :: TokenName
    , creationTimestamp :: Integer      --  Track creation time for timeliness bonuses
    } deriving (Generic)

PlutusTx.makeIsDataIndexed ''RewardPoolInfo [('RewardPoolInfo, 0)]
PlutusTx.makeLift ''RewardPoolInfo

data TopicDatum = TopicDatum
    { topic               :: Topic
    , topicStatus         :: TopicStatus
    , rewardPool          :: RewardPoolInfo
    , authorizedReviewers :: ReviewerMap
    } deriving (Generic)

PlutusTx.makeIsDataIndexed ''TopicDatum [('TopicDatum, 0)]
PlutusTx.makeLift ''TopicDatum

data ContributionDatum = ContributionDatum
    { contribution       :: Contribution
    , cType              :: ContributionType
    , cStatus            :: ContributionStatus
    , relevance          :: Integer
    , accuracy           :: Integer
    , completeness       :: Integer
    , revContent         :: ReviewContent   --  Store review action
    , dispReason         :: DisputeReason   --  Store dispute action
    , timelinessScore    :: Integer         --  Track timeliness for bonus calculation
    } deriving (Generic)

PlutusTx.makeIsDataIndexed ''ContributionDatum [('ContributionDatum, 0)]
PlutusTx.makeLift ''ContributionDatum

data TopicAction = SubmitTopic Topic RewardPoolInfo
                 | ReviewTopic BuiltinByteString Integer
                 | ActivateTopic BuiltinByteString
                 | CloseTopic BuiltinByteString
                 | RejectTopic BuiltinByteString
                 deriving (Generic)

PlutusTx.makeIsDataIndexed ''TopicAction [
    ('SubmitTopic, 0),
    ('ReviewTopic, 1),
    ('ActivateTopic, 2),
    ('CloseTopic, 3),
    ('RejectTopic, 4)]
PlutusTx.makeLift ''TopicAction

data ContributionAction = SubmitContribution Contribution
                        | ReviewContribution BuiltinByteString Integer Integer Integer ReviewContent
                        | VerifyContribution BuiltinByteString
                        | DisputeContribution BuiltinByteString DisputeReason
                        | UpdateContribution BuiltinByteString BuiltinByteString
                        | RejectContribution BuiltinByteString
                        | EvaluateContribution BuiltinByteString
                        deriving (Generic)

PlutusTx.makeIsDataIndexed ''ContributionAction [
    ('SubmitContribution, 0),
    ('ReviewContribution, 1),
    ('VerifyContribution, 2),
    ('DisputeContribution, 3),
    ('UpdateContribution, 4),
    ('RejectContribution, 5),
    ('EvaluateContribution, 6)]
PlutusTx.makeLift ''ContributionAction

data AdminAction = CheckPool BuiltinByteString
                 | DistributeRewards BuiltinByteString RewardMap
                 | UpdateReviewers ReviewerMap
                 deriving (Generic)

PlutusTx.makeIsDataIndexed ''AdminAction [
    ('CheckPool, 0),
    ('DistributeRewards, 1),
    ('UpdateReviewers, 2)]
PlutusTx.makeLift ''AdminAction

data DFCTContrib = TopicAction TopicAction
                 | ContributionAction ContributionAction
                 | AdminAction AdminAction
                 deriving (Generic)

PlutusTx.makeIsDataIndexed ''DFCTContrib [
    ('TopicAction, 0),
    ('ContributionAction, 1),
    ('AdminAction, 2)]
PlutusTx.makeLift ''DFCTContrib