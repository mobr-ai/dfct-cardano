{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestGovernance where

import DFCT.Governance
import qualified PlutusLedgerApi.V1.Value as Value
import PlutusLedgerApi.V3
import qualified PlutusLedgerApi.V3.Contexts as Contexts
import qualified PlutusTx.Prelude as PlutusTx hiding (Semigroup(..), unless)
import qualified PlutusTx (toBuiltinData)
import qualified Data.ByteString.Char8 as BS
import Control.Exception (evaluate, catch, SomeException)
import Test.Tasty.HUnit
import qualified PlutusTx.AssocMap as AssocMap
import PlutusLedgerApi.V1.Interval as Interval
import TestFixtures

-- Helper Functions for Governance Testing
defaultGovernanceParams :: DFCTGovernanceParams
defaultGovernanceParams = DFCTGovernanceParams
  { gpOwner = defaultAuth
  , gpMinVotingTokens = 50
  , gpAuthorizedPKHs = AssocMap.safeFromList [(defaultReviewer, 1)]
  }

defaultProposal :: DFCTProposal
defaultProposal = DFCTProposal
  { proposalId = PlutusTx.toBuiltin (BS.pack "proposal1")
  , proposer = defaultAuth
  , votingStart = 1000
  , votingEnd = 2000
  , voteTally = AssocMap.empty
  , proposalOutcome = 2
  }

mkTestGovernanceDatum :: DFCTProposalStatus -> Maybe DFCTProposal -> DFCTGovernanceParams -> DFCTGovernanceDatum
mkTestGovernanceDatum status dfctProposal params = DFCTGovernanceDatum
  { govParams = params
  , proposal = dfctProposal
  , proposalStatus = status
  }

-- Core Functionality Tests
testProposalSubmissionOwner :: Assertion
testProposalSubmissionOwner = do
  let params = defaultGovernanceParams
      datum = mkTestGovernanceDatum Proposed Nothing params
      action = SubmitProposal defaultProposal
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputDatum = mkTestGovernanceDatum Proposed (Just defaultProposal) params
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      txInfo = defaultTxInfo
        [defaultAuth]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
        [outputTxOut]
        mkEmptyMintValue
      ctx = ScriptContext
        { scriptContextTxInfo = txInfo
        , scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData action
        , scriptContextScriptInfo = Contexts.SpendingScript
            (TxOutRef defaultTxId 0)
            (Just (Datum (PlutusTx.toBuiltinData datum)))
        }
      ctxData = PlutusTx.toBuiltinData ctx
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return True)
    `catch` \(_ :: SomeException) -> return False
  assertBool "Owner proposal submission should succeed" result

testProposalSubmissionAuthorized :: Assertion
testProposalSubmissionAuthorized = do
  let params = defaultGovernanceParams
      dfctProposal = defaultProposal { proposer = defaultReviewer }
      datum = mkTestGovernanceDatum Proposed Nothing params
      action = SubmitProposal dfctProposal
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputDatum = mkTestGovernanceDatum Proposed (Just dfctProposal) params
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      txInfo = defaultTxInfo
        [defaultReviewer]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
        [outputTxOut]
        mkEmptyMintValue
      ctx = ScriptContext
        { scriptContextTxInfo = txInfo
        , scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData action
        , scriptContextScriptInfo = Contexts.SpendingScript
            (TxOutRef defaultTxId 0)
            (Just (Datum (PlutusTx.toBuiltinData datum)))
        }
      ctxData = PlutusTx.toBuiltinData ctx
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return True)
    `catch` \(_ :: SomeException) -> return False
  assertBool "Authorized PKH proposal submission should succeed" result

testProposalSubmissionInvalidId :: Assertion
testProposalSubmissionInvalidId = do
  let params = defaultGovernanceParams
      invalidProposal = defaultProposal { proposalId = PlutusTx.toBuiltin (BS.pack "") }
      datum = mkTestGovernanceDatum Proposed Nothing params
      action = SubmitProposal invalidProposal
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputDatum = mkTestGovernanceDatum Proposed (Just invalidProposal) params
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      ctx = mkTestScriptContext
        [defaultAuth]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
        [outputTxOut]
        mkEmptyMintValue
        (Redeemer $ PlutusTx.toBuiltinData action)
      ctxData = PlutusTx.toBuiltinData ctx
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return False)
    `catch` \(_ :: SomeException) -> return True
  assertBool "Invalid proposal ID should fail" result

testProposalSubmissionUnauthorized :: Assertion
testProposalSubmissionUnauthorized = do
  let params = defaultGovernanceParams
      unauthorized = mkTestPubKeyHash 5
      dfctProposal = defaultProposal { proposer = unauthorized }
      datum = mkTestGovernanceDatum Proposed Nothing params
      action = SubmitProposal dfctProposal
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputDatum = mkTestGovernanceDatum Proposed (Just dfctProposal) params
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      ctx = mkTestScriptContext
        [unauthorized]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
        [outputTxOut]
        mkEmptyMintValue
        (Redeemer $ PlutusTx.toBuiltinData action)
      ctxData = PlutusTx.toBuiltinData ctx
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return False)
    `catch` \(_ :: SomeException) -> return True
  assertBool "Unauthorized proposal submission should fail" result

-- PSVoting Logic Tests
testVoteValid :: Assertion
testVoteValid = do
  let params = defaultGovernanceParams
      voter = mkTestPubKeyHash 6
      datum = mkTestGovernanceDatum PSVoting (Just defaultProposal) params
      action = VoteOnProposal (proposalId defaultProposal) 1 50
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputProposal = defaultProposal { voteTally = AssocMap.safeFromList [(voter, 50)] }
      outputDatum = mkTestGovernanceDatum PSVoting (Just outputProposal) params
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      voterInput = mkTestTxOut
        (Address (PubKeyCredential voter) Nothing)
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        NoOutputDatum
      txInfo = defaultTxInfo
        [voter]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut, TxInInfo (TxOutRef defaultTxId 1) voterInput]
        [outputTxOut]
        mkEmptyMintValue
      ctx = ScriptContext
        { scriptContextTxInfo = txInfo { txInfoValidRange = Interval.interval 1500 1500 }
        , scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData action
        , scriptContextScriptInfo = Contexts.SpendingScript
            (TxOutRef defaultTxId 0)
            (Just (Datum (PlutusTx.toBuiltinData datum)))
        }
      ctxData = PlutusTx.toBuiltinData ctx
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return True)
    `catch` \(_ :: SomeException) -> return False
  assertBool "Valid vote should succeed" result

testVoteInsufficientTokens :: Assertion
testVoteInsufficientTokens = do
  let params = defaultGovernanceParams
      voter = mkTestPubKeyHash 6
      datum = mkTestGovernanceDatum PSVoting (Just defaultProposal) params
      action = VoteOnProposal (proposalId defaultProposal) 1 50
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputProposal = defaultProposal { voteTally = AssocMap.safeFromList [(voter, 50)] }
      outputDatum = mkTestGovernanceDatum PSVoting (Just outputProposal) params
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      voterInput = mkTestTxOut
        (Address (PubKeyCredential voter) Nothing)
        (Value.singleton defaultCurrencySymbol defaultTokenName 10) -- Below minVotingTokens
        NoOutputDatum
      ctx = mkTestScriptContext
        [voter]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut, TxInInfo (TxOutRef defaultTxId 1) voterInput]
        [outputTxOut]
        mkEmptyMintValue
        (Redeemer $ PlutusTx.toBuiltinData action)
      ctxData = PlutusTx.toBuiltinData ctx
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return False)
    `catch` \(_ :: SomeException) -> return True
  assertBool "Vote with insufficient tokens should fail" result

testVoteDuplicate :: Assertion
testVoteDuplicate = do
  let params = defaultGovernanceParams
      voter = mkTestPubKeyHash 6
      dfctProposal = defaultProposal { voteTally = AssocMap.safeFromList [(voter, 50)] }
      datum = mkTestGovernanceDatum PSVoting (Just dfctProposal) params
      action = VoteOnProposal (proposalId defaultProposal) 1 50
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputDatum = mkTestGovernanceDatum PSVoting (Just dfctProposal) params
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      voterInput = mkTestTxOut
        (Address (PubKeyCredential voter) Nothing)
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        NoOutputDatum
      ctx = mkTestScriptContext
        [voter]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut, TxInInfo (TxOutRef defaultTxId 1) voterInput]
        [outputTxOut]
        mkEmptyMintValue
        (Redeemer $ PlutusTx.toBuiltinData action)
      ctxData = PlutusTx.toBuiltinData ctx
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return False)
    `catch` \(_ :: SomeException) -> return True
  assertBool "Duplicate vote should fail" result

testVoteOutsidePeriod :: Assertion
testVoteOutsidePeriod = do
  let params = defaultGovernanceParams
      voter = mkTestPubKeyHash 6
      datum = mkTestGovernanceDatum PSVoting (Just defaultProposal) params
      action = VoteOnProposal (proposalId defaultProposal) 1 50
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputProposal = defaultProposal { voteTally = AssocMap.safeFromList [(voter, 50)] }
      outputDatum = mkTestGovernanceDatum PSVoting (Just outputProposal) params
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      voterInput = mkTestTxOut
        (Address (PubKeyCredential voter) Nothing)
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        NoOutputDatum
      ctx = mkTestScriptContext
        [voter]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut, TxInInfo (TxOutRef defaultTxId 1) voterInput]
        [outputTxOut]
        mkEmptyMintValue
        (Redeemer $ PlutusTx.toBuiltinData action)
      ctxData = PlutusTx.toBuiltinData ctx
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return False)
    `catch` \(_ :: SomeException) -> return True
  assertBool "Vote outside period should fail" result

-- Parameter Updates Tests
testUpdateAuthorizedPKHsValid :: Assertion
testUpdateAuthorizedPKHsValid = do
  let params = defaultGovernanceParams
      newPKHs = AssocMap.safeFromList [(defaultReviewer, 1), (mkTestPubKeyHash 7, 1)]
      datum = mkTestGovernanceDatum Proposed Nothing params
      action = UpdateAuthorizedPKHs newPKHs
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputParams = params { gpAuthorizedPKHs = newPKHs }
      outputDatum = mkTestGovernanceDatum Proposed Nothing outputParams
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      txInfo = defaultTxInfo
        [defaultAuth]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
        [outputTxOut]
        mkEmptyMintValue
      ctx = ScriptContext
        { scriptContextTxInfo = txInfo
        , scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData action
        , scriptContextScriptInfo = Contexts.SpendingScript
            (TxOutRef defaultTxId 0)
            (Just (Datum (PlutusTx.toBuiltinData datum)))
        }
      ctxData = PlutusTx.toBuiltinData ctx
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return True)
    `catch` \(_ :: SomeException) -> return False
  assertBool "Valid authorized PKHs update should succeed" result

testUpdateAuthorizedPKHsNonOwner :: Assertion
testUpdateAuthorizedPKHsNonOwner = do
  let params = defaultGovernanceParams
      newPKHs = AssocMap.safeFromList [(defaultReviewer, 1), (mkTestPubKeyHash 7, 1)]
      datum = mkTestGovernanceDatum Proposed Nothing params
      action = UpdateAuthorizedPKHs newPKHs
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputParams = params { gpAuthorizedPKHs = newPKHs }
      outputDatum = mkTestGovernanceDatum Proposed Nothing outputParams
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      ctx = mkTestScriptContext
        [defaultReviewer]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
        [outputTxOut]
        mkEmptyMintValue
        (Redeemer $ PlutusTx.toBuiltinData action)
      ctxData = PlutusTx.toBuiltinData ctx
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return False)
    `catch` \(_ :: SomeException) -> return True
  assertBool "Non-owner authorized PKHs update should fail" result

testUpdateMinVotingTokensValid :: Assertion
testUpdateMinVotingTokensValid = do
  let params = defaultGovernanceParams
      newMinTokens = 100
      datum = mkTestGovernanceDatum Proposed Nothing params
      action = UpdateMinVotingTokens newMinTokens
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputParams = params { gpMinVotingTokens = newMinTokens }
      outputDatum = mkTestGovernanceDatum Proposed Nothing outputParams
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      ctx = mkTestScriptContext
        [defaultAuth]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
        [outputTxOut]
        mkEmptyMintValue
        (Redeemer $ PlutusTx.toBuiltinData action)
      ctxData = PlutusTx.toBuiltinData ctx
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return True)
    `catch` \(_ :: SomeException) -> return False
  assertBool "Valid min voting tokens update should succeed" result

testUpdateMinVotingTokensInvalid :: Assertion
testUpdateMinVotingTokensInvalid = do
  let params = defaultGovernanceParams
      newMinTokens = 0
      datum = mkTestGovernanceDatum Proposed Nothing params
      action = UpdateMinVotingTokens newMinTokens
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputParams = params { gpMinVotingTokens = newMinTokens }
      outputDatum = mkTestGovernanceDatum Proposed Nothing outputParams
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      ctx = mkTestScriptContext
        [defaultAuth]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
        [outputTxOut]
        mkEmptyMintValue
        (Redeemer $ PlutusTx.toBuiltinData action)
      ctxData = PlutusTx.toBuiltinData ctx
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return False)
    `catch` \(_ :: SomeException) -> return True
  assertBool "Invalid min voting tokens should fail" result

-- State Transitions Tests
testSetVotingPeriodValid :: Assertion
testSetVotingPeriodValid = do
  let params = defaultGovernanceParams
      datum = mkTestGovernanceDatum Proposed (Just defaultProposal) params
      action = SetVotingPeriod (proposalId defaultProposal) 3000 4000
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputProposal = defaultProposal { votingStart = 3000, votingEnd = 4000 }
      outputDatum = mkTestGovernanceDatum Proposed (Just outputProposal) params
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      txInfo = defaultTxInfo
        [defaultAuth]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
        [outputTxOut]
        mkEmptyMintValue
      ctx = ScriptContext
        { scriptContextTxInfo = txInfo { txInfoValidRange = Interval.from 1000 }
        , scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData action
        , scriptContextScriptInfo = Contexts.SpendingScript (TxOutRef defaultTxId 0) (Just (Datum (PlutusTx.toBuiltinData datum)))
        }
      ctxData = PlutusTx.toBuiltinData ctx
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return True)
    `catch` \(_ :: SomeException) -> return False
  assertBool "Valid voting period set should succeed" result

testFinalizeProposalApproved :: Assertion
testFinalizeProposalApproved = do
  let params = defaultGovernanceParams
      dfctProposal = defaultProposal { votingEnd = 1000 }
      datum = mkTestGovernanceDatum PSVoting (Just dfctProposal) params
      action = FinalizeProposal (proposalId defaultProposal) 1
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputProposal = dfctProposal { proposalOutcome = 1 }
      outputDatum = mkTestGovernanceDatum Approved (Just outputProposal) params
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      ctx = mkTestScriptContext
        [defaultAuth]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
        [outputTxOut]
        mkEmptyMintValue
        (Redeemer $ PlutusTx.toBuiltinData action)
      ctx' = ctx { scriptContextTxInfo = (scriptContextTxInfo ctx) { txInfoValidRange = Interval.interval 1001 2000 } }
      ctxData = PlutusTx.toBuiltinData ctx'
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return True)
    `catch` \(_ :: SomeException) -> return False
  assertBool "Finalize as approved should succeed" result

testExecuteProposalValid :: Assertion
testExecuteProposalValid = do
  let params = defaultGovernanceParams
      dfctProposal = defaultProposal { proposalOutcome = 1 }
      datum = mkTestGovernanceDatum Approved (Just dfctProposal) params
      action = ExecuteProposal (proposalId defaultProposal)
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputDatum = mkTestGovernanceDatum Executed (Just dfctProposal) params
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      ctx = mkTestScriptContext
        [defaultAuth]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
        [outputTxOut]
        mkEmptyMintValue
        (Redeemer $ PlutusTx.toBuiltinData action)
      ctxData = PlutusTx.toBuiltinData ctx
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return True)
    `catch` \(_ :: SomeException) -> return False
  assertBool "Execute approved proposal should succeed" result

testInvalidStateTransition :: Assertion
testInvalidStateTransition = do
  let params = defaultGovernanceParams
      datum = mkTestGovernanceDatum Proposed (Just defaultProposal) params
      action = ExecuteProposal (proposalId defaultProposal)
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputDatum = mkTestGovernanceDatum Executed (Just defaultProposal) params
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      ctx = mkTestScriptContext
        [defaultAuth]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
        [outputTxOut]
        mkEmptyMintValue
        (Redeemer $ PlutusTx.toBuiltinData action)
      ctxData = PlutusTx.toBuiltinData ctx
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return False)
    `catch` \(_ :: SomeException) -> return True
  assertBool "Invalid state transition should fail" result

-- Edge Cases Tests
testEmptyAuthorizedPKHs :: Assertion
testEmptyAuthorizedPKHs = do
  let params = defaultGovernanceParams
      newPKHs = AssocMap.empty
      datum = mkTestGovernanceDatum Proposed Nothing params
      action = UpdateAuthorizedPKHs newPKHs
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputParams = params { gpAuthorizedPKHs = newPKHs }
      outputDatum = mkTestGovernanceDatum Proposed Nothing outputParams
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      txInfo = defaultTxInfo
        [defaultAuth]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
        [outputTxOut]
        mkEmptyMintValue
      ctx = ScriptContext
        { scriptContextTxInfo = txInfo
        , scriptContextRedeemer = Redeemer $ PlutusTx.toBuiltinData action
        , scriptContextScriptInfo = Contexts.SpendingScript
            (TxOutRef defaultTxId 0)
            (Just (Datum (PlutusTx.toBuiltinData datum)))
        }
      ctxData = PlutusTx.toBuiltinData ctx
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return False)
    `catch` \(_ :: SomeException) -> return True
  assertBool "Empty authorized PKHs should fail" result

testInvalidProposalDatum :: Assertion
testInvalidProposalDatum = do
  let params = defaultGovernanceParams
      invalidProposal = defaultProposal { proposalId = PlutusTx.toBuiltin (BS.pack "") }
      datum = mkTestGovernanceDatum Proposed (Just invalidProposal) params
      action = VoteOnProposal (proposalId invalidProposal) 1 50
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputDatum = mkTestGovernanceDatum PSVoting (Just invalidProposal) params
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      ctx = mkTestScriptContext
        [defaultAuth]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut]
        [outputTxOut]
        mkEmptyMintValue
        (Redeemer $ PlutusTx.toBuiltinData action)
      ctxData = PlutusTx.toBuiltinData ctx
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return False)
    `catch` \(_ :: SomeException) -> return True
  assertBool "Invalid proposal datum should fail" result

testInvalidVoteValue :: Assertion
testInvalidVoteValue = do
  let params = defaultGovernanceParams
      voter = mkTestPubKeyHash 6
      datum = mkTestGovernanceDatum PSVoting (Just defaultProposal) params
      action = VoteOnProposal (proposalId defaultProposal) 2 50 -- Invalid vote
      inputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData datum)))
      outputProposal = defaultProposal { voteTally = AssocMap.safeFromList [(voter, 50)] }
      outputDatum = mkTestGovernanceDatum PSVoting (Just outputProposal) params
      outputTxOut = mkTestTxOut
        defaultScriptHash
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        (OutputDatum (Datum (PlutusTx.toBuiltinData outputDatum)))
      voterInput = mkTestTxOut
        (Address (PubKeyCredential voter) Nothing)
        (Value.singleton defaultCurrencySymbol defaultTokenName 100)
        NoOutputDatum
      ctx = mkTestScriptContext
        [voter]
        [TxInInfo (TxOutRef defaultTxId 0) inputTxOut, TxInInfo (TxOutRef defaultTxId 1) voterInput]
        [outputTxOut]
        mkEmptyMintValue
        (Redeemer $ PlutusTx.toBuiltinData action)
      ctxData = PlutusTx.toBuiltinData ctx
  result <- (evaluate (mkGovernanceValidator defaultCurrencySymbol ctxData) >> return False)
    `catch` \(_ :: SomeException) -> return True
  assertBool "Invalid vote value should fail" result