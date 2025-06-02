{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestContrib where

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

-- Contribution Handling
testContributionSubmissionValid :: Assertion
testContributionSubmissionValid = do
    let contributer = mkTestPubKeyHash 6
        -- Create a topic datum in activated state
        topicDatum = mkTestTopicDatum defaultTopic TopicActivated defaultPool (mkTestReviewerMap [defaultReviewer])
        
        -- Create a valid contribution with Evidence type
        tContribution = mkTestContribution contributer 5000
        
        -- Create the action
        contrib = ContributionAction (SubmitContribution tContribution)
        
        -- Input transaction with the topic datum
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
            
        -- Output transaction with same topic datum (not changing topic state)
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
            
        -- Build a transaction with the contributor's signature
        txInfo = defaultTxInfo
            [contributer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            mkEmptyMintValue
            
        -- Build the script context
        ctx = ScriptContext
            { scriptContextTxInfo = txInfo
            , scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData contrib
            , scriptContextScriptInfo = Contexts.SpendingScript 
                                      (TxOutRef defaultTxId 0) 
                                      (Just (Datum (PlutusTx.toBuiltinData topicDatum)))
            }
        
        -- Convert the context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx
    
    -- This should succeed
    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
            `catch` \(_ :: SomeException) -> return False
    
    assertBool "Valid contribution submission should succeed" result

testContributionSubmissionValidWithDifferentTypes :: Assertion
testContributionSubmissionValidWithDifferentTypes = do
    let contributer = mkTestPubKeyHash 6
        -- Create a topic datum in activated state
        topicDatum = mkTestTopicDatum defaultTopic TopicActivated defaultPool (mkTestReviewerMap [defaultReviewer])
        
        -- Create a valid contribution with VoteCasted type
        tContribution = (mkTestContribution contributer 5000) { contributionType = VoteCasted }
        
        -- Create the action
        contrib = ContributionAction (SubmitContribution tContribution)
        
        -- Input transaction with the topic datum
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
            
        -- Output transaction with same topic datum
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
            
        -- Build a transaction with the contributor's signature
        txInfo = defaultTxInfo
            [contributer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            mkEmptyMintValue
            
        -- Build the script context
        ctx = ScriptContext
            { scriptContextTxInfo = txInfo
            , scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData contrib
            , scriptContextScriptInfo = Contexts.SpendingScript 
                                      (TxOutRef defaultTxId 0) 
                                      (Just (Datum (PlutusTx.toBuiltinData topicDatum)))
            }
        
        -- Convert the context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx
    
    -- This should succeed
    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
            `catch` \(_ :: SomeException) -> return False
    
    assertBool "Valid contribution submission with VoteCasted type should succeed" result

testContributionReviewValid :: Assertion
testContributionReviewValid = do
    let contribId = PlutusTx.toBuiltin (BS.pack "contrib1")
        reviewer = defaultReviewer
        reviewContent = mkTestReviewContent reviewer contribId 5100
        
        -- Create a contribution with matching ID
        tContribution = mkTestContribution (mkTestPubKeyHash 9) 5000
        contribDatum = ContributionDatum
            { contribution = tContribution
            , cType = Evidence
            , cStatus = ContributionProposed
            , relevance = 0
            , accuracy = 0
            , completeness = 0
            , revContent = mkTestReviewContent reviewer contribId 0
            , dispReason = mkTestDisputeReason (mkTestPubKeyHash 0) 0
            , timelinessScore = 10
            }
        
        -- Create the review action with scores
        reviewAction = ContributionAction (ReviewContribution contribId 8 7 9 reviewContent)
        
        -- Set up the active topic as a reference input
        topicDatum = mkTestTopicDatum defaultTopic TopicActivated defaultPool (mkTestReviewerMap [reviewer])
        refTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
            
        -- Input transaction with the contribution datum
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData contribDatum)))
            
        -- Output with the updated contribution status, scores, and review content
        outputContribDatum = contribDatum { 
            cStatus = ContributionReviewed,
            relevance = 8,
            accuracy = 7,
            completeness = 9,
            revContent = reviewContent
        }
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputContribDatum)))
            
        -- Build the transaction info with reference to the active topic
        txInfo = TxInfo
            { txInfoInputs = [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            , txInfoReferenceInputs = [TxInInfo (TxOutRef defaultTxId 1) refTxOut]
            , txInfoOutputs = [outputTxOut]
            , txInfoFee = Lovelace 0
            , txInfoMint = mkEmptyMintValue
            , txInfoValidRange = Interval.from (POSIXTime 4600)
            , txInfoSignatories = [reviewer]
            , txInfoData = AssocMap.empty
            , txInfoRedeemers = AssocMap.empty
            , txInfoId = defaultTxId
            , txInfoWdrl = AssocMap.empty
            , txInfoTxCerts = []
            , txInfoVotes = AssocMap.empty
            , txInfoProposalProcedures = []
            , txInfoCurrentTreasuryAmount = Nothing
            , txInfoTreasuryDonation = Nothing
            }
            
        -- Build the script context with contribution datum
        ctx = ScriptContext
            { scriptContextTxInfo = txInfo
            , scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData reviewAction
            , scriptContextScriptInfo = Contexts.SpendingScript 
                                      (TxOutRef defaultTxId 0) 
                                      (Just (Datum (PlutusTx.toBuiltinData contribDatum)))
            }
        
        -- Convert the context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx
    
    -- This should succeed
    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
            `catch` \(_ :: SomeException) -> return False
    
    assertBool "Valid contribution review should succeed" result

testContributionDisputeValid :: Assertion
testContributionDisputeValid = do
    let disputer = mkTestPubKeyHash 10
        contribId = PlutusTx.toBuiltin (BS.pack "contrib1")
        disputeReason = mkTestDisputeReason disputer 5200
        
        -- Create a contribution with matching ID
        tContribution = mkTestContribution (mkTestPubKeyHash 9) 5000
        contribDatum = ContributionDatum
            { contribution = tContribution
            , cType = Evidence
            , cStatus = ContributionProposed
            , relevance = 0
            , accuracy = 0
            , completeness = 0
            , revContent = mkTestReviewContent defaultReviewer contribId 0
            , dispReason = mkTestDisputeReason (mkTestPubKeyHash 0) 0
            , timelinessScore = 10
            }
        
        -- Create the dispute action
        disputeAction = ContributionAction (DisputeContribution contribId disputeReason)
        
        -- Set up the active topic as a reference input
        topicDatum = mkTestTopicDatum defaultTopic TopicActivated defaultPool (mkTestReviewerMap [defaultReviewer])
        refTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
            
        -- Input transaction with the contribution datum
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData contribDatum)))
            
        -- Output with the updated contribution status and dispute reason
        outputContribDatum = contribDatum { 
            cStatus = ContributionDisputed,
            dispReason = disputeReason
        }
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputContribDatum)))
            
        -- Build the transaction info with reference to the active topic
        txInfo = TxInfo
            { txInfoInputs = [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            , txInfoReferenceInputs = [TxInInfo (TxOutRef defaultTxId 1) refTxOut]
            , txInfoOutputs = [outputTxOut]
            , txInfoFee = Lovelace 0
            , txInfoMint = mkEmptyMintValue
            , txInfoValidRange = Interval.from (POSIXTime 4600)
            , txInfoSignatories = [disputer]
            , txInfoData = AssocMap.empty
            , txInfoRedeemers = AssocMap.empty
            , txInfoId = defaultTxId
            , txInfoWdrl = AssocMap.empty
            , txInfoTxCerts = []
            , txInfoVotes = AssocMap.empty
            , txInfoProposalProcedures = []
            , txInfoCurrentTreasuryAmount = Nothing
            , txInfoTreasuryDonation = Nothing
            }
            
        -- Build the script context with contribution datum
        ctx = ScriptContext
            { scriptContextTxInfo = txInfo
            , scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData disputeAction
            , scriptContextScriptInfo = Contexts.SpendingScript 
                                      (TxOutRef defaultTxId 0) 
                                      (Just (Datum (PlutusTx.toBuiltinData contribDatum)))
            }
        
        -- Convert the context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx
    
    -- This should succeed
    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
            `catch` \(_ :: SomeException) -> return False
    
    assertBool "Valid contribution dispute should succeed" result

testContributionRejectValid :: Assertion
testContributionRejectValid = do
    let reviewer = defaultReviewer
        contribId = PlutusTx.toBuiltin (BS.pack "contrib1")
        
        -- Create a contribution with matching ID
        tContribution = mkTestContribution (mkTestPubKeyHash 11) 5000
        contribDatum = ContributionDatum
            { contribution = tContribution
            , cType = Evidence
            , cStatus = ContributionProposed
            , relevance = 0
            , accuracy = 0
            , completeness = 0
            , revContent = mkTestReviewContent reviewer contribId 0
            , dispReason = mkTestDisputeReason (mkTestPubKeyHash 0) 0
            , timelinessScore = 10
            }
        
        -- Create the reject action
        rejectAction = ContributionAction (RejectContribution contribId)
        
        -- Set up the active topic as a reference input
        topicDatum = mkTestTopicDatum defaultTopic TopicActivated defaultPool (mkTestReviewerMap [reviewer])
        refTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
            
        -- Input transaction with the contribution datum
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData contribDatum)))
            
        -- Output with the updated contribution status
        outputContribDatum = contribDatum { cStatus = ContributionRejected }
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputContribDatum)))
            
        -- Build the transaction info with reference to the active topic
        txInfo = TxInfo
            { txInfoInputs = [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            , txInfoReferenceInputs = [TxInInfo (TxOutRef defaultTxId 1) refTxOut]
            , txInfoOutputs = [outputTxOut]
            , txInfoFee = Lovelace 0
            , txInfoMint = mkEmptyMintValue
            , txInfoValidRange = Interval.from (POSIXTime 4600)
            , txInfoSignatories = [reviewer]
            , txInfoData = AssocMap.empty
            , txInfoRedeemers = AssocMap.empty
            , txInfoId = defaultTxId
            , txInfoWdrl = AssocMap.empty
            , txInfoTxCerts = []
            , txInfoVotes = AssocMap.empty
            , txInfoProposalProcedures = []
            , txInfoCurrentTreasuryAmount = Nothing
            , txInfoTreasuryDonation = Nothing
            }
            
        -- Build the script context with contribution datum
        ctx = ScriptContext
            { scriptContextTxInfo = txInfo
            , scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData rejectAction
            , scriptContextScriptInfo = Contexts.SpendingScript 
                                      (TxOutRef defaultTxId 0) 
                                      (Just (Datum (PlutusTx.toBuiltinData contribDatum)))
            }
        
        -- Convert the context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx
    
    -- This should succeed
    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
            `catch` \(_ :: SomeException) -> return False
    
    assertBool "Valid contribution rejection should succeed" result

testInvalidReviewContent :: Assertion
testInvalidReviewContent = do
    let contribId = PlutusTx.toBuiltin (BS.pack "contrib1")
        -- Create an invalid review content with empty accuracy reason
        invalidReviewContent = ReviewContent { 
            reviewerPkh = defaultReviewer,
            refCntribId = contribId,
            relevanceReason = PlutusTx.toBuiltin (BS.pack "Good relevance"),
            accuracyReason = PlutusTx.emptyByteString, -- Invalid empty reason
            completenessReason = PlutusTx.toBuiltin (BS.pack "Complete submission"),
            reviewTimestamp = 5100
        }
        
        -- Create a contribution with matching ID
        tContribution = mkTestContribution (mkTestPubKeyHash 9) 5000
        contribDatum = ContributionDatum
            { contribution = tContribution
            , cType = Evidence
            , cStatus = ContributionProposed
            , relevance = 0
            , accuracy = 0
            , completeness = 0
            , revContent = mkTestReviewContent defaultReviewer contribId 0
            , dispReason = mkTestDisputeReason (mkTestPubKeyHash 0) 0
            , timelinessScore = 10
            }
        
        -- Create the review action with scores and invalid content
        reviewAction = ContributionAction (ReviewContribution contribId 8 7 9 invalidReviewContent)
        
        -- Set up the active topic as a reference input
        topicDatum = mkTestTopicDatum defaultTopic TopicActivated defaultPool (mkTestReviewerMap [defaultReviewer])
        refTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
            
        -- Input transaction with the contribution datum
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData contribDatum)))
            
        -- Output with the updated contribution status, scores, and invalid review content
        outputContribDatum = contribDatum { 
            cStatus = ContributionReviewed,
            relevance = 8,
            accuracy = 7,
            completeness = 9,
            revContent = invalidReviewContent
        }
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputContribDatum)))
            
        -- Build the transaction info with reference to the active topic
        txInfo = TxInfo
            { txInfoInputs = [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            , txInfoReferenceInputs = [TxInInfo (TxOutRef defaultTxId 1) refTxOut]
            , txInfoOutputs = [outputTxOut]
            , txInfoFee = Lovelace 0
            , txInfoMint = mkEmptyMintValue
            , txInfoValidRange = Interval.from (POSIXTime 4600)
            , txInfoSignatories = [defaultReviewer]
            , txInfoData = AssocMap.empty
            , txInfoRedeemers = AssocMap.empty
            , txInfoId = defaultTxId
            , txInfoWdrl = AssocMap.empty
            , txInfoTxCerts = []
            , txInfoVotes = AssocMap.empty
            , txInfoProposalProcedures = []
            , txInfoCurrentTreasuryAmount = Nothing
            , txInfoTreasuryDonation = Nothing
            }
            
        -- Build the script context with contribution datum
        ctx = ScriptContext
            { scriptContextTxInfo = txInfo
            , scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData reviewAction
            , scriptContextScriptInfo = Contexts.SpendingScript 
                                      (TxOutRef defaultTxId 0) 
                                      (Just (Datum (PlutusTx.toBuiltinData contribDatum)))
            }
        
        -- Convert the context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx
    
    -- This should fail due to invalid review content
    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return False)
            `catch` \(_ :: SomeException) -> return True
    
    assertBool "Invalid review content should be rejected" result

testInvalidTimeliness :: Assertion
testInvalidTimeliness = do
    let contribId = PlutusTx.toBuiltin (BS.pack "contrib1")
        -- Create a valid review content
        reviewContent = mkTestReviewContent defaultReviewer contribId 5100
        
        -- Create a contribution datum
        tContribution = mkTestContribution (mkTestPubKeyHash 9) 5000
        contribDatum = mkTestContributionDatum tContribution
        
        -- Create the review action with invalid timeliness score (-1)
        reviewAction = ContributionAction (ReviewContribution contribId 8 7 (-1) reviewContent)
        
        -- Set up the active topic as a reference input
        topicDatum = mkTestTopicDatum defaultTopic TopicActivated defaultPool (mkTestReviewerMap [defaultReviewer])
        refTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
            
        -- Input transaction with the contribution datum
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData contribDatum)))
            
        -- Output with the updated contribution status and invalid timeliness
        outputContribDatum = contribDatum { 
            cStatus = ContributionReviewed,
            relevance = 8,
            accuracy = 7,
            completeness = -1, -- Negative value is invalid
            revContent = reviewContent
        }
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputContribDatum)))
            
        -- Build the transaction info with reference to the active topic
        txInfo = TxInfo
            { txInfoInputs = [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            , txInfoReferenceInputs = [TxInInfo (TxOutRef defaultTxId 1) refTxOut]
            , txInfoOutputs = [outputTxOut]
            , txInfoFee = Lovelace 0
            , txInfoMint = mkEmptyMintValue
            , txInfoValidRange = Interval.from (POSIXTime 4600)
            , txInfoSignatories = [defaultReviewer]
            , txInfoData = AssocMap.empty
            , txInfoRedeemers = AssocMap.empty
            , txInfoId = defaultTxId
            , txInfoWdrl = AssocMap.empty
            , txInfoTxCerts = []
            , txInfoVotes = AssocMap.empty
            , txInfoProposalProcedures = []
            , txInfoCurrentTreasuryAmount = Nothing
            , txInfoTreasuryDonation = Nothing
            }
            
        -- Build the script context with contribution datum
        ctx = ScriptContext
            { scriptContextTxInfo = txInfo
            , scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData reviewAction
            , scriptContextScriptInfo = Contexts.SpendingScript 
                                      (TxOutRef defaultTxId 0) 
                                      (Just (Datum (PlutusTx.toBuiltinData contribDatum)))
            }
        
        -- Convert the context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx
    
    -- This should fail due to invalid timeliness score
    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return False)
            `catch` \(_ :: SomeException) -> return True
    
    assertBool "Invalid timeliness score should be rejected" result

testInvalidContribution :: Assertion
testInvalidContribution = do
    let contributer = mkTestPubKeyHash 6
        -- Create a topic datum in activated state
        topicDatum = mkTestTopicDatum defaultTopic TopicActivated defaultPool (mkTestReviewerMap [defaultReviewer])
        
        -- Create an invalid contribution with empty ID
        invalidContribution = (mkTestContribution contributer 5000) { 
            contributionId = PlutusTx.emptyByteString -- Invalid empty ID
        }
        
        -- Create the action
        contrib = ContributionAction (SubmitContribution invalidContribution)
        
        -- Input transaction with the topic datum
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
            
        -- Output transaction with same topic datum
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
            
        -- Build a transaction with the contributor's signature
        txInfo = defaultTxInfo
            [contributer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            mkEmptyMintValue
            
        -- Build the script context
        ctx = ScriptContext
            { scriptContextTxInfo = txInfo
            , scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData contrib
            , scriptContextScriptInfo = Contexts.SpendingScript 
                                      (TxOutRef defaultTxId 0) 
                                      (Just (Datum (PlutusTx.toBuiltinData topicDatum)))
            }
        
        -- Convert the context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx
    
    -- This should fail due to invalid contribution ID
    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return False)
            `catch` \(_ :: SomeException) -> return True

    assertBool "Contribution with empty ID should be rejected" result