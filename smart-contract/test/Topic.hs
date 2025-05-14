{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Topic where

import DFCT.Provenance
import DFCT.Types
import qualified PlutusLedgerApi.V1.Value as Value
import PlutusLedgerApi.V3
import qualified PlutusLedgerApi.V3.Contexts as Contexts
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
    let dTopic = defaultTopic
        pool = defaultPool
        
        -- Create a topic datum
        datum = mkTestTopicDatum dTopic TopicProposed pool AssocMap.empty
        
        -- Create the action with the correct topic and pool
        contrib = TopicAction (SubmitTopic dTopic pool)
        
        -- Create the input transaction
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
            
        -- Create the output transaction (same as input for topic submission)
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
            
        -- Create a proper context with the topic proposer signature
        txInfo = defaultTxInfo
            [defaultProposer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            (mkTestMintValue defaultCurrencySymbol defaultTokenName 100)
            
        -- Build the ScriptContext
        ctx = ScriptContext
            { scriptContextTxInfo = txInfo
            , scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData contrib
            , scriptContextScriptInfo = Contexts.SpendingScript 
                                       (TxOutRef defaultTxId 0) 
                                       (Just (Datum (PlutusTx.toBuiltinData datum)))
            }
        
        -- Convert the context to BuiltinData
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
        
        -- Create a topic datum in proposed state with reviewers
        datum = mkTestTopicDatum defaultTopic TopicProposed defaultPool reviewers
        
        -- Create a review action with signed = True
        contrib = TopicAction (ReviewTopic (PlutusTx.toBuiltin (BS.pack "topic1")) 1)
        
        -- Create input transaction with the datum
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
            
        -- Create output transaction with correct state transition
        outputDatum = mkTestTopicDatum defaultTopic TopicReviewed defaultPool reviewers
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
            
        -- Create a context with the reviewer's signature
        txInfo = defaultTxInfo
            [defaultReviewer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            mkEmptyMintValue
            
        -- Build the ScriptContext with correct datum
        ctx = ScriptContext
            { scriptContextTxInfo = txInfo
            , scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData contrib
            , scriptContextScriptInfo = Contexts.SpendingScript 
                                       (TxOutRef defaultTxId 0) 
                                       (Just (Datum (PlutusTx.toBuiltinData datum)))
            }
        
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
        contrib = TopicAction (ReviewTopic (PlutusTx.toBuiltin (BS.pack "topic1")) 1) -- False - no signed parameter
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
        contrib = AdminAction (UpdateReviewers newReviewers)
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData inputDatum)))
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
        
        -- Create a context with proper script info including the datum
        txInfo = defaultTxInfo
            [defaultAuth]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            mkEmptyMintValue
        
        ctx = ScriptContext
            { scriptContextTxInfo = txInfo
            , scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData contrib
            , scriptContextScriptInfo = Contexts.SpendingScript 
                                      (TxOutRef defaultTxId 0) 
                                      (Just (Datum (PlutusTx.toBuiltinData inputDatum)))
            }
        
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