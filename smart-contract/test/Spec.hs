{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-} -- make any type constructor into a type
{-# LANGUAGE TemplateHaskell     #-} -- allows embedding domain-specific language into the Haskell host language

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import DFCT.Types ()
import Topic
import Contrib
import RewardMinting
import StateTransition
import Edge


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
        [ testCase "Evidence submission in active topic succeeds" testEvidenceSubmissionValid
        , testCase "Vote casting in active topic succeeds" testVoteCastingValid
        , testCase "Tag selection in active topic succeeds" testTagSelectionValid
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
    ]