{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-} -- make any type constructor into a type
{-# LANGUAGE TemplateHaskell     #-} -- allows embedding domain-specific language into the Haskell host language

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import DFCT.Types ()
import TestTopic
import TestContrib
import TestRewardMinting
import TestStateTransition
import TestEdge
import TestGovernance

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "DFCT Smart Contract Tests"
    [ testGroup "Core Functionality"
        [ testCase "Topic submission with valid data succeeds" testTopicSubmissionValid
        , testCase "Topic submission with invalid topic ID fails" testTopicSubmissionInvalidId
        , testCase "Topic review with authorized reviewer succeeds" testTopicReviewAuthorized
        , testCase "Topic review with unauthorized reviewer fails" testTopicReviewUnauthorized
        , testCase "Topic activation from reviewed state succeeds" testTopicActivationValid
        , testCase "Topic closing from activated state succeeds" testTopicClosingValid
        , testCase "Topic rejection from proposed state succeeds" testTopicRejectionValid
        , testCase "Reviewer update with owner signature succeeds" testReviewerUpdateOwner
        , testCase "Reviewer update with non-owner signature fails" testReviewerUpdateNonOwner
        ]
    , testGroup "Contribution Handling"
        [ testCase "Contribution submission in active topic succeeds" testContributionSubmissionValid
        , testCase "Different types of contribution submission succeeds" testContributionSubmissionValidWithDifferentTypes
        , testCase "Contribution review with valid scores succeeds" testContributionReviewValid
        , testCase "Contribution dispute with reason succeeds" testContributionDisputeValid
        , testCase "Contribution rejection in active topic succeeds" testContributionRejectValid
        , testCase "Invalid review content is rejected" testInvalidReviewContent
        , testCase "Invalid timeliness score is rejected" testInvalidTimeliness
        ]
    , testGroup "Reward and Minting Logic"
        [ testCase "Reward distribution with valid amounts succeeds" testRewardDistributionValid
        , testCase "Reward distribution with overflow fails" testRewardDistributionOverflow
        , testCase "Token minting with authority succeeds" testTokenMintingValid
        , testCase "Token minting with zero amount fails" testTokenMintingZero
        , testCase "Token minting exceeding max amount fails" testTokenMintingRefTx
        , testCase "Token burning (negative amount) is rejected" testTokenBurningValid
        ]
    , testGroup "State Transitions"
        [ testCase "Invalid transition from proposed to activated fails" testStateTransitionInvalid
        , testCase "Valid transition from reviewed to activated succeeds" testStateTransitionValid
        ]
    , testGroup "Edge Cases"
        [ testCase "Empty reviewer list is rejected" testEmptyReviewerList
        , testCase "Invalid topic datum is rejected" testInvalidTopicDatum
        , testCase "Contribution with empty ID is rejected" testInvalidContribution
        , testCase "Reward pool with negative total amount is rejected" testInvalidRewardPool
        ]
    , testGroup "Admin Functions"
        [ testCase "Check pool with valid topic ID succeeds" testCheckPoolValid
        , testCase "Check pool with invalid topic ID fails" testCheckPoolInvalidTopic
        ]
    , testGroup "Governance Core"
      [ testCase "Proposal submission by owner succeeds" testProposalSubmissionOwner
      , testCase "Proposal submission by authorized PKH succeeds" testProposalSubmissionAuthorized
      , testCase "Proposal submission with invalid ID fails" testProposalSubmissionInvalidId
      , testCase "Proposal submission by unauthorized PKH fails" testProposalSubmissionUnauthorized
      ]
    , testGroup "Governance Voting Logic"
        [ testCase "Vote by eligible voter succeeds" testVoteValid
        , testCase "Vote with insufficient tokens fails" testVoteInsufficientTokens
        , testCase "Duplicate vote fails" testVoteDuplicate
        , testCase "Vote outside voting period fails" testVoteOutsidePeriod
        ]
    , testGroup "Governance Parameter Updates"
        [ testCase "Update authorized PKHs by owner succeeds" testUpdateAuthorizedPKHsValid
        , testCase "Update authorized PKHs by non-owner fails" testUpdateAuthorizedPKHsNonOwner
        , testCase "Update min voting tokens by owner succeeds" testUpdateMinVotingTokensValid
        , testCase "Update min voting tokens with zero fails" testUpdateMinVotingTokensInvalid
        ]
    , testGroup "Governance State Transitions"
        [ testCase "Set voting period for proposed proposal succeeds" testSetVotingPeriodValid
        , testCase "Finalize proposal as approved succeeds" testFinalizeProposalApproved
        , testCase "Execute approved proposal succeeds" testExecuteProposalValid
        , testCase "Invalid transition from proposed to executed fails" testInvalidStateTransition
        ]
    , testGroup "Governance Edge Cases"
        [ testCase "Empty authorized PKHs map fails" testEmptyAuthorizedPKHs
        , testCase "Invalid proposal datum is rejected" testInvalidProposalDatum
        , testCase "Vote with invalid value fails" testInvalidVoteValue
        ]
    ]