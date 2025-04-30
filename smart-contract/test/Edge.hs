{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Edge where

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

-- Edge Cases
testEmptyReviewerList :: Assertion
testEmptyReviewerList = do
    let reviewers = mkTestReviewerMap [defaultReviewer]
        emptyReviewers = AssocMap.empty
        inputDatum = mkTestTopicDatum defaultTopic TopicProposed defaultPool reviewers
        outputDatum = mkTestTopicDatum defaultTopic TopicProposed defaultPool emptyReviewers
        contrib = mkTestUpdateReviewers []
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
        
        -- Convert context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx

    -- Evaluate the validator expression and catch the expected exception
    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return False)
              `catch` \(_ :: SomeException) -> return True

    assertBool "Empty reviewer list should fail" result

testInvalidTopicDatum :: Assertion
testInvalidTopicDatum = do
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
        
        -- Convert context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx

    -- Evaluate the validator expression and catch the expected exception
    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return False)
              `catch` \(_ :: SomeException) -> return True

    assertBool "Invalid topic datum should fail" result

testInvalidContribution :: Assertion
testInvalidContribution = do
    let contributor = mkTestPubKeyHash 6
        invalidContrib = (mkTestContribution contributor 2000) { contributionId = PlutusTx.toBuiltin (BS.pack "") }
        datum = mkTestTopicDatum defaultTopic TopicActivated defaultPool AssocMap.empty
        contrib = ContributionAction (SubmitEvidence invalidContrib)
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
        outputDatum = mkTestContributionDatum invalidContrib
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
        ctx = mkTestScriptContext
            [contributor]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData contrib)
        
        -- Convert context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx

    -- Evaluate the validator expression and catch the expected exception
    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return False)
              `catch` \(_ :: SomeException) -> return True

    assertBool "Invalid contribution should fail" result

testInvalidRewardPool :: Assertion
testInvalidRewardPool = do
    let invalidPool = RewardPoolInfo (-100) 0 defaultTokenName 0
        datum = mkTestTopicDatum defaultTopic TopicProposed invalidPool AssocMap.empty
        contrib = TopicAction (SubmitTopic defaultTopic invalidPool)
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
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData contrib)
        
        -- Convert context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx

    -- Evaluate the validator expression and catch the expected exception
    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return False)
              `catch` \(_ :: SomeException) -> return True

    assertBool "Negative reward pool should fail" result
