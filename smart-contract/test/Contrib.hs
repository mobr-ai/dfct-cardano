{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Contrib where

import DFCT.Provenance
import DFCT.Types
import qualified PlutusLedgerApi.V1.Value as Value
import qualified PlutusLedgerApi.V1.Interval as Interval
import PlutusLedgerApi.V3
import qualified PlutusLedgerApi.V3.Contexts as Contexts
import qualified PlutusTx.Prelude as PlutusTx hiding (Semigroup(..), unless)
import qualified PlutusTx (toBuiltinData)
import qualified Data.ByteString.Char8 as BS
import Control.Exception (evaluate, catch, SomeException)
import Test.Tasty.HUnit
import qualified PlutusTx.AssocMap as AssocMap

import TestFixtures

testEvidenceSubmissionValid :: Assertion
testEvidenceSubmissionValid = do
    let contributor = mkTestPubKeyHash 6
        contrib = mkTestContribution contributor 2000
        datum = mkTestTopicDatum defaultTopic TopicActivated defaultPool AssocMap.empty
        uContrib = ContributionAction (SubmitEvidence contrib)
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
        outputDatum = mkTestContributionDatum contrib
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
        ctx = mkTestScriptContext
            [contributor]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData uContrib)

        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
            `catch` \(_ :: SomeException) -> return False

    assertBool "Valid evidence submission should succeed" result

testVoteCastingValid :: Assertion
testVoteCastingValid = do
    let voter = mkTestPubKeyHash 6
        vote = (mkTestContribution voter 2000) { contributionType = PlutusTx.toBuiltin (BS.pack "vote") }
        datum = mkTestTopicDatum defaultTopic TopicActivated defaultPool AssocMap.empty
        contrib = ContributionAction (CastVote vote)
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
        outputDatum = mkTestContributionDatum vote
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
        ctx = mkTestScriptContext
            [voter]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData contrib)
        
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
            `catch` \(_ :: SomeException) -> return False

    assertBool "Valid vote casting should succeed" result

testTagSelectionValid :: Assertion
testTagSelectionValid = do
    let tagger = mkTestPubKeyHash 6
        tag = (mkTestContribution tagger 2000) { contributionType = PlutusTx.toBuiltin (BS.pack "verdict_tag") }
        datum = mkTestTopicDatum defaultTopic TopicActivated defaultPool AssocMap.empty
        contrib = ContributionAction (SelectTag tag)
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
        outputDatum = mkTestContributionDatum tag
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
        ctx = mkTestScriptContext
            [tagger]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData contrib)
        
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
            `catch` \(_ :: SomeException) -> return False

    assertBool "Valid tag selection should succeed" result

testContributionReviewValid :: Assertion
testContributionReviewValid = do
    let reviewers = mkTestReviewerMap [defaultReviewer]
        contrib = mkTestContribution defaultReviewer 2000
        contribDatum = mkTestContributionDatum contrib
        reviewContent = mkTestReviewContent defaultReviewer (PlutusTx.toBuiltin (BS.pack "contrib1")) 2001
        uContrib = ContributionAction (ReviewContribution (PlutusTx.toBuiltin (BS.pack "contrib1")) 8 9 7 reviewContent)
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData contribDatum)))

        -- Create a proper topic datum in the active state
        topicDatum = mkTestTopicDatum defaultTopic TopicActivated defaultPool reviewers
        topicTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
        
        ctx = mkTestScriptContext
            [defaultReviewer]
            [TxInInfo (TxOutRef defaultTxId 0) topicTxOut, TxInInfo (TxOutRef defaultTxId 1) inputTxOut]
            []
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData uContrib)
        
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
            `catch` \(_ :: SomeException) -> return False

    assertBool "Valid contribution review should succeed" result

testContributionReviewInvalid :: Assertion
testContributionReviewInvalid = do
    let contribDatum = mkTestContributionDatum (mkTestContribution defaultReviewer 2000)
        reviewContent = mkTestReviewContent defaultReviewer (PlutusTx.toBuiltin (BS.pack "contrib1")) 2001
        contrib = ContributionAction (ReviewContribution (PlutusTx.toBuiltin (BS.pack "contrib1")) (-1) 9 7 reviewContent)
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton (CurrencySymbol PlutusTx.emptyByteString) (Value.TokenName PlutusTx.emptyByteString) 0)
            (OutputDatum (Datum (PlutusTx.toBuiltinData contribDatum)))
        ctx = mkTestScriptContext
            [defaultReviewer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            []
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData contrib)
        
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return False)
              `catch` \(_ :: SomeException) -> return True

    assertBool "Invalid scores should fail" result

testContributionVerificationValid :: Assertion
testContributionVerificationValid = do
    let reviewers = mkTestReviewerMap [defaultReviewer]
        contrib = mkTestContribution defaultProposer 2000
        uContrib = ContributionAction (VerifyContribution (PlutusTx.toBuiltin (BS.pack "contrib1")))
        
        -- create a topic datum in activated state
        topicDatum = mkTestTopicDatum defaultTopic TopicActivated defaultPool reviewers
        
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
        
        -- add the contribution datum
        contribDatum = mkTestContributionDatum contrib
        contribTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 50)
            (OutputDatum (Datum (PlutusTx.toBuiltinData contribDatum)))
            
        ctx = mkTestScriptContext
            [defaultProposer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut, TxInInfo (TxOutRef defaultTxId 1) contribTxOut]
            []
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData uContrib)
        
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
            `catch` \(_ :: SomeException) -> return False

    assertBool "Valid contribution verification should succeed" result

testContributionDisputeValid :: Assertion
testContributionDisputeValid = do
    let reviewers = mkTestReviewerMap [defaultReviewer]
        contrib = mkTestContribution defaultProposer 2000
        disputeReason = mkTestDisputeReason defaultProposer 2001
        uContrib = ContributionAction (DisputeContribution (PlutusTx.toBuiltin (BS.pack "contrib1")) disputeReason)
        
        -- create a topic datum in activated state
        topicDatum = mkTestTopicDatum defaultTopic TopicActivated defaultPool reviewers
        
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
        
        -- add the contribution datum
        contribDatum = mkTestContributionDatum contrib
        contribTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 50)
            (OutputDatum (Datum (PlutusTx.toBuiltinData contribDatum)))
            
        ctx = mkTestScriptContext
            [defaultProposer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut, TxInInfo (TxOutRef defaultTxId 1) contribTxOut]
            []
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData uContrib)
        
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
            `catch` \(_ :: SomeException) -> return False

    assertBool "Valid contribution dispute should succeed" result

--  tests for additional contribution actions

testContributionUpdateValid :: Assertion
testContributionUpdateValid = do
    let contributor = mkTestPubKeyHash 6
        contrib = mkTestContribution contributor 2000
        datum = mkTestTopicDatum defaultTopic TopicActivated defaultPool AssocMap.empty
        newContent = PlutusTx.toBuiltin (BS.pack "Updated content")
        uContrib = ContributionAction (UpdateContribution (PlutusTx.toBuiltin (BS.pack "contrib1")) newContent)
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
        
        -- Create a proper contribution datum
        contribDatum = mkTestContributionDatum contrib
        contribTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 50)
            (OutputDatum (Datum (PlutusTx.toBuiltinData contribDatum)))
            
        ctx = mkTestScriptContext
            [contributor]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut, TxInInfo (TxOutRef defaultTxId 1) contribTxOut]
            []
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData uContrib)
        
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
            `catch` \(_ :: SomeException) -> return False

    assertBool "Valid contribution update should succeed" result

testContributionRejectValid :: Assertion
testContributionRejectValid = do
    let reviewer = defaultReviewer
        reviewers = mkTestReviewerMap [reviewer]
        contrib = mkTestContribution defaultProposer 2000
        uContrib = ContributionAction (RejectContribution (PlutusTx.toBuiltin (BS.pack "contrib1")))
        
        -- Create a topic datum in activated state
        topicDatum = mkTestTopicDatum defaultTopic TopicActivated defaultPool reviewers
        
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
        
        -- Add the contribution datum
        contribDatum = mkTestContributionDatum contrib
        contribTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 50)
            (OutputDatum (Datum (PlutusTx.toBuiltinData contribDatum)))
            
        ctx = mkTestScriptContext
            [reviewer] 
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut, TxInInfo (TxOutRef defaultTxId 1) contribTxOut]
            []
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData uContrib)
        
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
            `catch` \(_ :: SomeException) -> return False

    assertBool "Valid contribution rejection should succeed" result

testContributionEvaluateValid :: Assertion
testContributionEvaluateValid = do
    let evaluator = mkTestPubKeyHash 5
        reviewers = mkTestReviewerMap [defaultReviewer]
        contrib = mkTestContribution defaultProposer 2000
        uContrib = ContributionAction (EvaluateContribution (PlutusTx.toBuiltin (BS.pack "contrib1")))
        
        -- Create a topic datum in activated state
        topicDatum = mkTestTopicDatum defaultTopic TopicActivated defaultPool reviewers
        
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
        
        -- Add the contribution datum
        contribDatum = mkTestContributionDatum contrib
        contribTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 50)
            (OutputDatum (Datum (PlutusTx.toBuiltinData contribDatum)))
            
        ctx = mkTestScriptContext
            [evaluator]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut, TxInInfo (TxOutRef defaultTxId 1) contribTxOut]
            []
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData uContrib)
        
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
            `catch` \(_ :: SomeException) -> return False

    assertBool "Valid contribution evaluation should succeed" result

testInvalidTimeliness :: Assertion
testInvalidTimeliness = do
    let contributor = mkTestPubKeyHash 6
        contrib = mkTestContribution contributor 2000
        
        -- Mock a contribution with an invalid timeliness calculation
        -- For this, we'll create a setup where the function validateContributionTimeliness would fail
        -- by setting up a negative timeScore
        
        -- Create a topic datum with a very recent creationTimestamp
        futureTimestamp = 2000 + (60 * 60 * 24 * 365) -- One year in the future
        modifiedPool = defaultPool { creationTimestamp = futureTimestamp }
        datum = mkTestTopicDatum defaultTopic TopicActivated modifiedPool AssocMap.empty
        
        -- Set the contribution timestamp to be before the topic creation
        pastContrib = contrib { contributionTimestamp = 1000 }  -- In the past
        
        uContrib = ContributionAction (SubmitEvidence pastContrib)
        
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
            
        outputDatum = mkTestContributionDatum pastContrib
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
            
        -- Create a context with a validRange that would produce a negative timeliness score
        -- We need to set up the ScriptContext manually to ensure proper validation failure
        txInfo = (scriptContextTxInfo $ mkTestScriptContext
            [contributor]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData uContrib))
            -- Override the validRange to a time that would cause timeliness calculation to fail
            { txInfoValidRange = Interval.from (POSIXTime 1000) }  -- Very early time
            
        ctx = ScriptContext
            { scriptContextTxInfo = txInfo
            , scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData uContrib
            , scriptContextScriptInfo = Contexts.SpendingScript 
                                       (TxOutRef defaultTxId 0) 
                                       Nothing
            }
        
        ctxData = PlutusTx.toBuiltinData ctx

    -- We expect this to fail due to invalid timeliness
    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return False)
            `catch` \(_ :: SomeException) -> return True

    assertBool "Invalid timeliness should fail" result

testInvalidReviewContent :: Assertion
testInvalidReviewContent = do
    let reviewers = mkTestReviewerMap [defaultReviewer]
        -- Create review content with empty reasoning fields
        invalidReviewContent = (mkTestReviewContent defaultReviewer (PlutusTx.toBuiltin (BS.pack "contrib1")) 2001)
            { relevanceReason = PlutusTx.emptyByteString }
        
        uContrib = ContributionAction (ReviewContribution 
                       (PlutusTx.toBuiltin (BS.pack "contrib1")) 
                       8 9 7 
                       invalidReviewContent)
        
        -- Create a topic datum in activated state
        topicDatum = mkTestTopicDatum defaultTopic TopicActivated defaultPool reviewers
        
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
            
        ctx = mkTestScriptContext
            [defaultReviewer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            []
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData uContrib)
        
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return False)
              `catch` \(_ :: SomeException) -> return True

    assertBool "Invalid review content should fail" result