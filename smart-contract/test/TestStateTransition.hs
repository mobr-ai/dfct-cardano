{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module TestStateTransition where

import DFCT.Provenance
import DFCT.Types hiding (Contribution)
import qualified PlutusLedgerApi.V1.Value as Value
import PlutusLedgerApi.V3
import qualified PlutusLedgerApi.V3.Contexts as Contexts
import qualified PlutusTx
import PlutusTx.Prelude
import Control.Exception (evaluate, catch, SomeException)
import Test.Tasty.HUnit

import TestFixtures

-- State Transitions
testStateTransitionInvalid :: Assertion
testStateTransitionInvalid = do
    let reviewers = mkTestReviewerMap [defaultReviewer]
        inputDatum = mkTestTopicDatum defaultTopic TopicProposed defaultPool reviewers
        outputDatum = mkTestTopicDatum defaultTopic TopicActivated defaultPool reviewers
        contrib = TopicAction (ActivateTopic "topic1")

        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData inputDatum)))
            
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
            
        -- Build the transaction info
        txInfo = defaultTxInfo
            [defaultProposer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            mkEmptyMintValue
            
        -- Build the script context with proper datum
        ctx = Contexts.ScriptContext
            { Contexts.scriptContextTxInfo = txInfo
            , Contexts.scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData contrib
            , Contexts.scriptContextScriptInfo = Contexts.SpendingScript 
                                      (TxOutRef defaultTxId 0) 
                                      (Just (Datum (PlutusTx.toBuiltinData inputDatum)))
            }
            
        ctxData = PlutusTx.toBuiltinData ctx
        
    -- This is an invalid transition, so it should fail
    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return False)
            `catch` \(_ :: SomeException) -> return True

    assertBool "Invalid state transition should fail" result

testStateTransitionValid :: Assertion
testStateTransitionValid = do
    let reviewers = mkTestReviewerMap [defaultReviewer]
        inputDatum = mkTestTopicDatum defaultTopic TopicReviewed defaultPool reviewers
        outputDatum = mkTestTopicDatum defaultTopic TopicActivated defaultPool reviewers
        contrib = TopicAction (ActivateTopic "topic1")

        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData inputDatum)))
            
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
            
        -- Build the transaction info
        txInfo = defaultTxInfo
            [defaultProposer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            mkEmptyMintValue
            
        -- Build the script context with proper datum
        ctx = Contexts.ScriptContext
            { Contexts.scriptContextTxInfo = txInfo
            , Contexts.scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData contrib
            , Contexts.scriptContextScriptInfo = Contexts.SpendingScript 
                                      (TxOutRef defaultTxId 0) 
                                      (Just (Datum (PlutusTx.toBuiltinData inputDatum)))
            }
            
        ctxData = PlutusTx.toBuiltinData ctx
        
    -- This is a valid transition, so it should succeed
    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
            `catch` \(_ :: SomeException) -> return False

    assertBool "Valid state transition should succeed" result