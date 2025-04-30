{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestFixtures where

import DFCT.MintingPolicy
import DFCT.Provenance
import DFCT.Types

import qualified PlutusLedgerApi.V1.Interval as Interval
import qualified PlutusLedgerApi.V1.Value as Value
import PlutusLedgerApi.V3
import qualified PlutusLedgerApi.V3.MintValue as V3
import qualified PlutusTx.Prelude as PlutusTx hiding (Semigroup(..), unless)
import qualified PlutusTx (toBuiltinData)
import qualified Data.ByteString.Char8 as BS
import qualified PlutusLedgerApi.V3.Contexts as Contexts
import qualified PlutusTx.AssocMap as AssocMap
import Test.Tasty.HUnit
import Control.Exception (evaluate, catch, SomeException)

-- Helper functions for string representation of types
topicStatusToString :: TopicStatus -> String
topicStatusToString TopicProposed = "TopicProposed"
topicStatusToString TopicReviewed = "TopicReviewed"
topicStatusToString TopicActivated = "TopicActivated" 
topicStatusToString TopicClosed = "TopicClosed"
topicStatusToString TopicRejected = "TopicRejected"

-- Constants
defaultTokenName :: TokenName
defaultTokenName = TokenName "DFC"

defaultCurrencySymbol :: CurrencySymbol
defaultCurrencySymbol = CurrencySymbol (PlutusTx.toBuiltin (BS.pack "test_symbol"))

defaultScriptHash :: Address
defaultScriptHash = Address (PubKeyCredential (mkTestPubKeyHash 1)) Nothing

defaultTxId :: TxId
defaultTxId = TxId (PlutusTx.toBuiltin (BS.pack "tx1"))

defaultAuth :: PubKeyHash
defaultAuth = PubKeyHash "Authority"

defaultProposer :: PubKeyHash
defaultProposer = mkTestPubKeyHash 3

defaultReviewer :: PubKeyHash
defaultReviewer = mkTestPubKeyHash 4

-- Helper to create a ReviewerMap
mkTestReviewerMap :: [PubKeyHash] -> ReviewerMap
mkTestReviewerMap pks = foldr (\pk -> AssocMap.insert pk True) AssocMap.empty pks

-- Helper to create a RewardMap
mkTestRewardMap :: [(PubKeyHash, Integer)] -> RewardMap
mkTestRewardMap pairs = foldr (\(pk, amt) -> AssocMap.insert pk amt) AssocMap.empty pairs

extractPkh :: PubKeyHash -> BuiltinByteString
extractPkh (PubKeyHash bs) = bs

defaultTopic :: Topic
defaultTopic = mkTestTopic defaultProposer 1000

defaultPool :: RewardPoolInfo
defaultPool = mkTestRewardPool 100

defaultParams :: MintingParams
defaultParams = mkMintingParams defaultAuth defaultTokenName

-- Fixture creators
mkTestPubKeyHash :: Integer -> PubKeyHash
mkTestPubKeyHash n = PubKeyHash (PlutusTx.toBuiltin (BS.pack ("pk" ++ show n)))

mkTestTopic :: PubKeyHash -> Integer -> Topic
mkTestTopic proposer timestamp = Topic
    { topicId = PlutusTx.toBuiltin (BS.pack "topic1")
    , topicTitle = PlutusTx.toBuiltin (BS.pack "Test Topic")
    , topicDescription = PlutusTx.toBuiltin (BS.pack "A test topic")
    , topicProposer = proposer
    , topicTimestamp = timestamp
    }

mkTestRewardPool :: Integer -> RewardPoolInfo
mkTestRewardPool amount = RewardPoolInfo
    { totalAmount       = amount
    , allocatedAmount   = 0
    , tokenName         = defaultTokenName
    , creationTimestamp = 0
    }

mkTestTopicDatum :: Topic -> TopicStatus -> RewardPoolInfo -> ReviewerMap -> TopicDatum
mkTestTopicDatum t status pool reviewers = TopicDatum
    { topic = t
    , topicStatus = status
    , rewardPool = pool
    , authorizedReviewers = reviewers
    }

mkTestContribution :: PubKeyHash -> Integer -> Contribution
mkTestContribution creator timestamp = Contribution
    { contributionId = PlutusTx.toBuiltin (BS.pack "contrib1")
    , contributionTopicId = PlutusTx.toBuiltin (BS.pack "topic1")
    , contributionType = PlutusTx.toBuiltin (BS.pack "evidence")
    , contributionContent = PlutusTx.toBuiltin (BS.pack "Test evidence")
    , contributionCreator = creator
    , contributionTimestamp = timestamp
    , contributionVersion = 1
    , previousVersionId = PlutusTx.emptyByteString
    }

mkTestContributionDatum :: Contribution -> ContributionDatum
mkTestContributionDatum c = ContributionDatum
    { contribution = c
    , cStatus = EvidenceProposed
    , relevance = 0
    , accuracy = 0
    , completeness = 0
    , reviewContents = []
    , disputeReasons = []
    , timelinessScore = 0
    }

mkTestReviewContent :: PubKeyHash -> BuiltinByteString -> Integer -> ReviewContent
mkTestReviewContent reviewer contribId timestamp = ReviewContent
    { reviewerPkh = reviewer
    , refCntribId = contribId
    , relevanceReason = PlutusTx.toBuiltin (BS.pack "Good relevance")
    , accuracyReason = PlutusTx.toBuiltin (BS.pack "Accurate information")
    , completenessReason = PlutusTx.toBuiltin (BS.pack "Complete submission")
    , reviewTimestamp = timestamp
    }

mkTestDisputeReason :: PubKeyHash -> Integer -> DisputeReason
mkTestDisputeReason disputer timestamp = DisputeReason
    { disputeInitiator = disputer
    , disputeContent = PlutusTx.toBuiltin (BS.pack "Invalid data")
    , disputeTimestamp = timestamp
    }

mkTestTxOut :: Address -> Value -> OutputDatum -> TxOut
mkTestTxOut addr val datum = TxOut
    { txOutAddress = addr
    , txOutValue = val
    , txOutDatum = datum
    , txOutReferenceScript = Nothing
    }

-- Helper to create a MintValue for testing
mkEmptyMintValue :: V3.MintValue
mkEmptyMintValue = V3.emptyMintValue

-- Create a MintValue with tokens
mkTestMintValue :: CurrencySymbol -> TokenName -> Integer -> V3.MintValue
mkTestMintValue cs tn amount = 
    let m1 = AssocMap.singleton tn amount
        m2 = AssocMap.singleton cs m1
    in V3.UnsafeMintValue m2

-- Create a ScriptContext for testing
mkTestScriptContext :: [PubKeyHash] -> [TxInInfo] -> [TxOut] -> V3.MintValue -> Redeemer -> ScriptContext
mkTestScriptContext signers ins outs mint redeemer = ScriptContext
    { scriptContextTxInfo = TxInfo
        { txInfoInputs = ins
        , txInfoReferenceInputs = []
        , txInfoOutputs = outs
        , txInfoFee = Lovelace 0
        , txInfoMint = mint
        , txInfoValidRange = Interval.from (POSIXTime 4600)
        , txInfoSignatories = signers
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
    , scriptContextRedeemer = redeemer
    , scriptContextScriptInfo = Contexts.SpendingScript 
                                  (TxOutRef defaultTxId 0) 
                                  Nothing
    }

-- Common test helpers
mkStateTransitionTest :: TopicStatus -> TopicStatus -> DFCTContrib -> [PubKeyHash] -> Bool -> Assertion
mkStateTransitionTest fromStatus toStatus contrib signers shouldSucceed = do
    let reviewers = mkTestReviewerMap [defaultReviewer]
        datum = mkTestTopicDatum defaultTopic fromStatus defaultPool reviewers
        outputDatum = mkTestTopicDatum defaultTopic toStatus defaultPool reviewers
        inputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
        outputTxOut = mkTestTxOut
            defaultScriptHash
            (Value.singleton defaultCurrencySymbol defaultTokenName 100)
            (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
        ctx = mkTestScriptContext
            signers
            [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
            [outputTxOut]
            mkEmptyMintValue
            (Redeemer $ PlutusTx.toBuiltinData contrib)
        
        -- Create the BuiltinData for the script context
        ctxData = PlutusTx.toBuiltinData ctx
    
    if shouldSucceed
    then do
        -- For success cases, we expect validation to complete without error
        result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return True)
               `catch` \(_ :: SomeException) -> return False

        assertBool ("State transition should succeed") result

    else do
        -- For failing cases, we expect the validation to throw an error
        result <- (evaluate (mkDFCTValidator defaultCurrencySymbol ctxData) >> return False)
              `catch` \(_ :: SomeException) -> return True
        assertBool ("State transition should fail") result

-- Create test contribution actions
mkTestDistributeRewards :: BuiltinByteString -> [(PubKeyHash, Integer)] -> DFCTContrib
mkTestDistributeRewards tid rewards = AdminAction (DistributeRewards tid (mkTestRewardMap rewards))

mkTestUpdateReviewers :: [PubKeyHash] -> DFCTContrib
mkTestUpdateReviewers reviewers = AdminAction (UpdateReviewers (mkTestReviewerMap reviewers))