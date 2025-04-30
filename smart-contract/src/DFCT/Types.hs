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

type ReviewerMap = Map.Map PubKeyHash Bool
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

data ContributionStatus = EvidenceProposed
                        | VerdictTagSelected
                        | VoteCasted
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
    (==) EvidenceProposed EvidenceProposed = True
    (==) VerdictTagSelected VerdictTagSelected = True
    (==) VoteCasted VoteCasted = True
    (==) ContributionReviewed ContributionReviewed = True
    (==) ContributionDisputed ContributionDisputed = True
    (==) ContributionUpdated ContributionUpdated = True
    (==) ContributionRejected ContributionRejected = True
    (==) ContributionVerified ContributionVerified = True
    (==) ContributionEvaluated ContributionEvaluated = True
    (==) RewardsDistributed RewardsDistributed = True
    (==) PoolEvaluated PoolEvaluated = True
    (==) _ _ = False

PlutusTx.makeIsDataIndexed ''ContributionStatus [('EvidenceProposed, 0), ('VerdictTagSelected, 1), ('VoteCasted, 2), 
                                               ('ContributionReviewed, 3), ('ContributionDisputed, 4), ('ContributionUpdated, 5),
                                               ('ContributionRejected, 6), ('ContributionVerified, 7), ('ContributionEvaluated, 8),
                                               ('RewardsDistributed, 9), ('PoolEvaluated, 10)]
PlutusTx.makeLift ''ContributionStatus

data Topic = Topic
    { topicId          :: BuiltinByteString
    , topicTitle       :: BuiltinByteString
    , topicDescription :: BuiltinByteString
    , topicProposer    :: PubKeyHash
    , topicTimestamp   :: Integer
    } deriving (Generic)

PlutusTx.makeIsDataIndexed ''Topic [('Topic, 0)]
PlutusTx.makeLift ''Topic

data Contribution = Contribution
    { contributionId        :: BuiltinByteString
    , contributionTopicId   :: BuiltinByteString
    , contributionType      :: BuiltinByteString
    , contributionContent   :: BuiltinByteString
    , contributionCreator   :: PubKeyHash
    , contributionTimestamp :: Integer
    , contributionVersion   :: Integer            -- track version number
    , previousVersionId     :: BuiltinByteString  -- reference to previous version (empty if first version)
    } deriving (Generic)

PlutusTx.makeIsDataIndexed ''Contribution [('Contribution, 0)]
PlutusTx.makeLift ''Contribution

--  Store review content with reasoning
data ReviewContent = ReviewContent
    { reviewerPkh         :: PubKeyHash
    , refCntribId         :: BuiltinByteString  -- reference to the contribution being reviewed
    , relevanceReason     :: BuiltinByteString
    , accuracyReason      :: BuiltinByteString
    , completenessReason  :: BuiltinByteString
    , reviewTimestamp     :: Integer
    } deriving (Generic)

PlutusTx.makeIsDataIndexed ''ReviewContent [('ReviewContent, 0)]
PlutusTx.makeLift ''ReviewContent

--  Store dispute reasons
data DisputeReason = DisputeReason
    { disputeInitiator  :: PubKeyHash
    , disputeContent    :: BuiltinByteString
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
    , cStatus            :: ContributionStatus
    , relevance          :: Integer
    , accuracy           :: Integer
    , completeness       :: Integer
    , reviewContents     :: [ReviewContent]   --  Store detailed review reasoning
    , disputeReasons     :: [DisputeReason]   --  Store dispute history
    , timelinessScore    :: Integer           --  Track timeliness for bonus calculation
    } deriving (Generic)

PlutusTx.makeIsDataIndexed ''ContributionDatum [('ContributionDatum, 0)]
PlutusTx.makeLift ''ContributionDatum

data TopicAction = SubmitTopic Topic RewardPoolInfo
                 | ReviewTopic BuiltinByteString Bool
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

data ContributionAction = SubmitEvidence Contribution
                        | CastVote Contribution
                        | SelectTag Contribution
                        | ReviewContribution BuiltinByteString Integer Integer Integer ReviewContent -- Enhanced with ReviewContent
                        | VerifyContribution BuiltinByteString
                        | DisputeContribution BuiltinByteString DisputeReason -- Enhanced with DisputeReason
                        | UpdateContribution BuiltinByteString BuiltinByteString
                        | RejectContribution BuiltinByteString
                        | EvaluateContribution BuiltinByteString
                        deriving (Generic)

PlutusTx.makeIsDataIndexed ''ContributionAction [
    ('SubmitEvidence, 0),
    ('CastVote, 1),
    ('SelectTag, 2),
    ('ReviewContribution, 3),
    ('VerifyContribution, 4),
    ('DisputeContribution, 5),
    ('UpdateContribution, 6),
    ('RejectContribution, 7),
    ('EvaluateContribution, 8)]
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