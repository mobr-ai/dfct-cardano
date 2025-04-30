{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RewardMinting where

import DFCT.MintingPolicy
import DFCT.Provenance
import DFCT.Types hiding (Contribution)

import qualified PlutusLedgerApi.V1.Interval as Interval
import qualified PlutusLedgerApi.V1.Value as Value
import PlutusLedgerApi.V3
import qualified PlutusTx.Prelude as PlutusTx hiding (Semigroup(..), unless)
import qualified PlutusTx (toBuiltinData)
import qualified Data.ByteString.Char8 as BS
import Control.Exception (evaluate, catch, SomeException)
import Test.Tasty.HUnit
import qualified PlutusTx.AssocMap as Map
import qualified PlutusLedgerApi.V3.Contexts as Contexts

import TestFixtures

-- Reward and Minting Logic
testRewardDistributionValid :: Assertion
testRewardDistributionValid = do
    let contrib1 = mkTestPubKeyHash 4
        contrib2 = mkTestPubKeyHash 5
        topicId1 = PlutusTx.toBuiltin (BS.pack "topic1")
        
        -- Create a topic datum in activated state with a reward pool
        pool = mkTestRewardPool 100
        topicDatum = mkTestTopicDatum (defaultTopic { topicId = topicId1 }) TopicActivated pool Map.empty
        
        contrib = mkTestDistributeRewards topicId1 [(contrib1, 40), (contrib2, 30)]
        
        -- Input transaction containing the topic datum
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
        
        -- Output transactions for the reward recipients
        outs = [ mkTestTxOut
                    (Address (PubKeyCredential contrib1) Nothing)
                    (Value.singleton defaultCurrencySymbol defaultTokenName 40)
                    NoOutputDatum
               , mkTestTxOut
                    (Address (PubKeyCredential contrib2) Nothing)
                    (Value.singleton defaultCurrencySymbol defaultTokenName 30)
                    NoOutputDatum
               ]
        
        ctx = mkTestScriptContext
            [defaultProposer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            outs
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData contrib)
        
        -- Convert context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
              `catch` \(_ :: SomeException) -> return False

    assertBool "Valid reward distribution should succeed" result

testRewardDistributionOverflow :: Assertion
testRewardDistributionOverflow = do
    let contrib1 = mkTestPubKeyHash 4
        contrib2 = mkTestPubKeyHash 5
        smallPool = mkTestRewardPool 50
        emptyReviewers = Map.empty
        datum = mkTestTopicDatum defaultTopic TopicActivated smallPool emptyReviewers
        contrib = mkTestDistributeRewards (PlutusTx.toBuiltin (BS.pack "topic1")) [(contrib1, 40), (contrib2, 30)]
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 50)
            (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
        ctx = mkTestScriptContext
            [defaultProposer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            []
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData contrib)
        
        -- Convert context to BuiltinData
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return False)
              `catch` \(_ :: SomeException) -> return True

    assertBool "Reward overflow should fail" result

testTokenMintingValid :: Assertion
testTokenMintingValid = do
    let redeemerData = PlutusTx.toBuiltinData ()
        txOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 500)
            NoOutputDatum
        mintValue = mkTestMintValue defaultCurrencySymbol defaultTokenName 500
        scriptPurpose = Contexts.Minting defaultCurrencySymbol
        scriptInfo = Contexts.MintingScript defaultCurrencySymbol
        txInfo = TxInfo
            { txInfoInputs = []
            , txInfoReferenceInputs = []
            , txInfoOutputs = [txOut]
            , txInfoFee = Lovelace 0
            , txInfoMint = mintValue
            , txInfoSignatories = [defaultAuth]
            , txInfoValidRange = Interval.from (POSIXTime 4600)
            , txInfoData = Map.empty
            , txInfoRedeemers = Map.singleton scriptPurpose (Redeemer redeemerData)
            , txInfoId = defaultTxId
            , txInfoWdrl = Map.empty
            , txInfoTxCerts = []
            , txInfoVotes = Map.empty
            , txInfoProposalProcedures = []
            , txInfoCurrentTreasuryAmount = Nothing
            , txInfoTreasuryDonation = Nothing
            }
        ctx = ScriptContext
            { scriptContextTxInfo = txInfo
            , scriptContextRedeemer = Redeemer redeemerData
            , scriptContextScriptInfo = scriptInfo
            }
        
        -- Convert context to BuiltinData for the MintingPolicy
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCMintingPolicy defaultParams ctxData) >> return True)
              `catch` \(_ :: SomeException) -> return False

    assertBool "Valid token minting should succeed" result

testTokenMintingZero :: Assertion
testTokenMintingZero = do
    let redeemerData = PlutusTx.toBuiltinData ()
        scriptPurpose = Contexts.Minting defaultCurrencySymbol
        scriptInfo = Contexts.MintingScript defaultCurrencySymbol
        mintValue = mkTestMintValue defaultCurrencySymbol defaultTokenName 0
        txInfo = TxInfo
            { txInfoInputs = []
            , txInfoReferenceInputs = []
            , txInfoOutputs = []
            , txInfoFee = Lovelace 0
            , txInfoValidRange = Interval.from (POSIXTime 4600)
            , txInfoMint = mintValue
            , txInfoSignatories = [defaultAuth]
            , txInfoData = Map.empty
            , txInfoRedeemers = Map.singleton scriptPurpose (Redeemer redeemerData)
            , txInfoId = defaultTxId
            , txInfoWdrl = Map.empty
            , txInfoTxCerts = []
            , txInfoVotes = Map.empty
            , txInfoProposalProcedures = []
            , txInfoCurrentTreasuryAmount = Nothing
            , txInfoTreasuryDonation = Nothing
            }
        ctx = ScriptContext
            { scriptContextTxInfo = txInfo
            , scriptContextRedeemer = Redeemer redeemerData
            , scriptContextScriptInfo = scriptInfo
            }
        
        -- Convert context to BuiltinData for the MintingPolicy
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCMintingPolicy defaultParams ctxData) >> return False)
              `catch` \(_ :: SomeException) -> return True

    assertBool "Zero token minting should fail" result

testTokenMintingRefTx :: Assertion
testTokenMintingRefTx = do
    let redeemerData = PlutusTx.toBuiltinData ()
        recipient = mkTestPubKeyHash 10
        referenceTxId = TxId $ PlutusTx.toBuiltin $ BS.pack "bef157d27751fe9aeb9c7158daaae10044c2e011c6fcb466e2d78e8c2058672c"
        referenceTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 1)
            NoOutputDatum
        txOut = mkTestTxOut
            (Address (PubKeyCredential recipient) Nothing)
            (Value.singleton defaultCurrencySymbol defaultTokenName 10000)
            NoOutputDatum
        mintValue = mkTestMintValue defaultCurrencySymbol defaultTokenName 10000
        scriptPurpose = Contexts.Minting defaultCurrencySymbol
        scriptInfo = Contexts.MintingScript defaultCurrencySymbol
        txInfo = TxInfo
            { txInfoInputs = []
            , txInfoReferenceInputs = [TxInInfo (TxOutRef referenceTxId 0) referenceTxOut]
            , txInfoOutputs = [txOut]
            , txInfoFee = Lovelace 0
            , txInfoMint = mintValue
            , txInfoValidRange = Interval.from (POSIXTime 4600)
            , txInfoSignatories = [defaultAuth]
            , txInfoData = Map.empty
            , txInfoRedeemers = Map.singleton scriptPurpose (Redeemer redeemerData)
            , txInfoId = defaultTxId
            , txInfoWdrl = Map.empty
            , txInfoTxCerts = []
            , txInfoVotes = Map.empty
            , txInfoProposalProcedures = []
            , txInfoCurrentTreasuryAmount = Nothing
            , txInfoTreasuryDonation = Nothing
            }
        ctx = ScriptContext
            { scriptContextTxInfo = txInfo
            , scriptContextRedeemer = Redeemer redeemerData
            , scriptContextScriptInfo = scriptInfo
            }
        
        -- Convert context to BuiltinData for the MintingPolicy
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCMintingPolicy defaultParams ctxData) >> return True)
              `catch` \(_ :: SomeException) -> return False

    assertBool "Minting 10000 tokens with reference input should succeed" result

-- token burning (negative minting)
testTokenBurningValid :: Assertion
testTokenBurningValid = do
    let redeemerData = PlutusTx.toBuiltinData ()
        burnAmount = -100  -- Negative amount for burning
        txOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 500)
            NoOutputDatum
        mintValue = mkTestMintValue defaultCurrencySymbol defaultTokenName burnAmount
        scriptPurpose = Contexts.Minting defaultCurrencySymbol
        scriptInfo = Contexts.MintingScript defaultCurrencySymbol
        txInfo = TxInfo
            { txInfoInputs = []
            , txInfoReferenceInputs = []
            , txInfoOutputs = [txOut]
            , txInfoFee = Lovelace 0
            , txInfoMint = mintValue
            , txInfoSignatories = [defaultAuth]
            , txInfoValidRange = Interval.from (POSIXTime 4600)
            , txInfoData = Map.empty
            , txInfoRedeemers = Map.singleton scriptPurpose (Redeemer redeemerData)
            , txInfoId = defaultTxId
            , txInfoWdrl = Map.empty
            , txInfoTxCerts = []
            , txInfoVotes = Map.empty
            , txInfoProposalProcedures = []
            , txInfoCurrentTreasuryAmount = Nothing
            , txInfoTreasuryDonation = Nothing
            }
        ctx = ScriptContext
            { scriptContextTxInfo = txInfo
            , scriptContextRedeemer = Redeemer redeemerData
            , scriptContextScriptInfo = scriptInfo
            }
        
        -- Convert context to BuiltinData for the MintingPolicy
        ctxData = PlutusTx.toBuiltinData ctx

    -- The minting policy should reject burning tokens (negative amounts)
    result <- (evaluate (mkDFCMintingPolicy defaultParams ctxData) >> return False)
              `catch` \(_ :: SomeException) -> return True

    assertBool "Token burning should fail" result

--  tests for admin actions
testCheckPoolValid :: Assertion
testCheckPoolValid = do
    let topicId1 = PlutusTx.toBuiltin (BS.pack "topic1")
        
        -- Create a topic datum with the same topic ID
        topicDatum = mkTestTopicDatum (defaultTopic { topicId = topicId1 }) TopicActivated defaultPool Map.empty
        
        checkPoolAction = AdminAction (CheckPool topicId1)
        
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
            
        ctx = mkTestScriptContext
            [defaultProposer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            []
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData checkPoolAction)
        
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
            `catch` \(_ :: SomeException) -> return False

    assertBool "Valid pool check should succeed" result

testCheckPoolInvalidTopic :: Assertion
testCheckPoolInvalidTopic = do
    let topicId1 = PlutusTx.toBuiltin (BS.pack "topic1")
        invalidTopicId = PlutusTx.toBuiltin (BS.pack "topic2") -- Different ID
        
        -- Create a topic datum with a different topic ID
        topicDatum = mkTestTopicDatum (defaultTopic { topicId = topicId1 }) TopicActivated defaultPool Map.empty
        
        checkPoolAction = AdminAction (CheckPool invalidTopicId)
        
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData topicDatum)))
            
        ctx = mkTestScriptContext
            [defaultProposer]
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            []
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData checkPoolAction)
        
        ctxData = PlutusTx.toBuiltinData ctx

    result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return False)
            `catch` \(_ :: SomeException) -> return True

    assertBool "Invalid topic ID for pool check should fail" result