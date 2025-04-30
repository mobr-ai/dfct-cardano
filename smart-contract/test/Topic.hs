{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Topic where

import DFCT.Provenance
import DFCT.Types
import qualified PlutusLedgerApi.V1.Value as Value
import PlutusLedgerApi.V3
import qualified PlutusTx.Prelude as PlutusTx hiding (Semigroup(..), unless)
import qualified PlutusTx (toBuiltinData)
import qualified Data.ByteString.Char8 as BS
import Control.Exception (evaluate, catch, SomeException)
import Test.Tasty.HUnit
import qualified PlutusTx.AssocMap as AssocMap

import TestFixtures

-- Core Functionality
testTopicSubmissionValid :: Assertion
testTopicSubmissionValid = do
    let datum = mkTestTopicDatum defaultTopic TopicProposed defaultPool AssocMap.empty
        contrib = TopicAction (SubmitTopic defaultTopic defaultPool)
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
        ctx = mkTestScriptContext
            [defaultProposer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            (mkTestMintValue defaultCurrencySymbol defaultTokenName 100)
            (Redeemer $ PlutusTx.toBuiltinData contrib)
        
        -- Convert the context to BuiltinData for the new validator
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
            `catch` \(_ :: SomeException) -> return False

    assertBool "Valid topic submission should succeed" result

testTopicSubmissionInvalidId :: Assertion
testTopicSubmissionInvalidId = do
    let invalidTopic = defaultTopic { topicId = PlutusTx.toBuiltin (BS.pack "") }
        datum = mkTestTopicDatum invalidTopic TopicProposed defaultPool AssocMap.empty
        contrib = TopicAction (SubmitTopic invalidTopic defaultPool)
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
        ctx = mkTestScriptContext
            [defaultProposer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            (mkTestMintValue defaultCurrencySymbol defaultTokenName 100)
            (Redeemer $ PlutusTx.toBuiltinData contrib)
        
        -- Convert the context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx
    
    -- Evaluate the validator expression and catch the expected exception
    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return False)
              `catch` \(_ :: SomeException) -> return True

    assertBool "Invalid topic ID should fail" result

testTopicReviewAuthorized :: Assertion
testTopicReviewAuthorized = do
    let reviewers = mkTestReviewerMap [defaultReviewer]
        contrib = TopicAction (ReviewTopic (PlutusTx.toBuiltin (BS.pack "topic1")) True)
        datum = mkTestTopicDatum defaultTopic TopicProposed defaultPool reviewers
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
        outputDatum = mkTestTopicDatum defaultTopic TopicReviewed defaultPool reviewers
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
        ctx = mkTestScriptContext
            [defaultReviewer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData contrib)
        
        -- Convert the context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
        `catch` \(_ :: SomeException) -> return False

    assertBool "Authorized reviewer should succeed" result

testTopicReviewUnauthorized :: Assertion
testTopicReviewUnauthorized = do
    let unauthorized = mkTestPubKeyHash 5
        reviewers = mkTestReviewerMap [defaultReviewer]
        datum = mkTestTopicDatum defaultTopic TopicProposed defaultPool reviewers
        contrib = TopicAction (ReviewTopic (PlutusTx.toBuiltin (BS.pack "topic1")) False) -- False - no signed parameter
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
        outputDatum = mkTestTopicDatum defaultTopic TopicReviewed defaultPool reviewers
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
        ctx = mkTestScriptContext
            [unauthorized]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData contrib)
        
        -- Convert the context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return False)
            `catch` \(_ :: SomeException) -> return True

    assertBool "Unauthorized reviewer should fail" result

testTopicActivationValid :: Assertion
testTopicActivationValid =
    mkStateTransitionTest
        TopicReviewed
        TopicActivated
        (TopicAction (ActivateTopic (PlutusTx.toBuiltin (BS.pack "topic1"))))
        [defaultProposer]
        True

testTopicClosingValid :: Assertion
testTopicClosingValid =
    mkStateTransitionTest
        TopicActivated
        TopicClosed
        (TopicAction (CloseTopic (PlutusTx.toBuiltin (BS.pack "topic1"))))
        [defaultProposer]
        True

testTopicRejectionValid :: Assertion
testTopicRejectionValid =
    mkStateTransitionTest
        TopicProposed
        TopicRejected
        (TopicAction (RejectTopic (PlutusTx.toBuiltin (BS.pack "topic1"))))
        [defaultReviewer]
        True

testReviewerUpdateOwner :: Assertion
testReviewerUpdateOwner = do
    let reviewers = mkTestReviewerMap [defaultReviewer]
        newReviewers = mkTestReviewerMap [defaultReviewer, mkTestPubKeyHash 5]
        inputDatum = mkTestTopicDatum defaultTopic TopicProposed defaultPool reviewers
        outputDatum = mkTestTopicDatum defaultTopic TopicProposed defaultPool newReviewers
        contrib = mkTestUpdateReviewers [defaultReviewer, mkTestPubKeyHash 5]
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData inputDatum)))
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
        ctx = mkTestScriptContext
            [defaultAuth]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData contrib)
        
        -- Convert the context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
            `catch` \(_ :: SomeException) -> return False

    assertBool "Valid reviewer update should succeed" result

testReviewerUpdateNonOwner :: Assertion
testReviewerUpdateNonOwner = do
    let reviewers = mkTestReviewerMap [defaultReviewer]
        newReviewers = AssocMap.empty  -- Empty map will cause failure as expected in smart contract
        inputDatum = mkTestTopicDatum defaultTopic TopicProposed defaultPool reviewers
        outputDatum = mkTestTopicDatum defaultTopic TopicProposed defaultPool newReviewers
        contrib = mkTestUpdateReviewers []  -- Empty reviewers list
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData inputDatum)))
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
        ctx = mkTestScriptContext
            [defaultProposer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData contrib)
        
        -- Convert the context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx
    
    -- Evaluate the validator expression and catch the expected exception
    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return False)
              `catch` \(_ :: SomeException) -> return True
    
    assertBool "Non-owner reviewer update should fail" result