{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}

module DFCT.Governance where

import GHC.Generics (Generic)

import PlutusTx.Prelude
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts as Contexts
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import PlutusLedgerApi.V1.Interval as Interval

-- Governance Parameters
data DFCTGovernanceParams = DFCTGovernanceParams
  { gpOwner            :: PubKeyHash           -- Contract owner
  , gpTokenSymbol      :: CurrencySymbol       -- DFC token currency symbol
  , gpTokenName        :: TokenName            -- DFC token name
  , gpMinVotingTokens  :: Integer              -- Minimum DFC tokens required to vote
  , gpAuthorizedPKHs   :: AssocMap.Map PubKeyHash Integer -- Map of authorized PKHs for proposal submission
  } deriving (Generic)

PlutusTx.makeIsDataIndexed ''DFCTGovernanceParams [('DFCTGovernanceParams, 0)]
PlutusTx.makeLift ''DFCTGovernanceParams

-- DFCTProposal Status
data DFCTProposalStatus = Proposed | PSVoting | Approved | Rejected | Executed
  deriving (Generic)

instance Eq DFCTProposalStatus where
  (==) Proposed Proposed = True
  (==) PSVoting PSVoting = True
  (==) Approved Approved = True
  (==) Rejected Rejected = True
  (==) Executed Executed = True
  (==) _ _ = False

PlutusTx.makeIsDataIndexed ''DFCTProposalStatus [('Proposed, 0), ('PSVoting, 1), ('Approved, 2), ('Rejected, 3), ('Executed, 4)]
PlutusTx.makeLift ''DFCTProposalStatus

-- DFCTProposal Data
data DFCTProposal = DFCTProposal
  { proposalId         :: BuiltinByteString    -- Unique proposal ID
  , proposer           :: PubKeyHash           -- Proposer’s public key hash
  , votingStart        :: POSIXTime            -- Voting start time
  , votingEnd          :: POSIXTime            -- Voting end time
  , voteTally          :: AssocMap.Map PubKeyHash Integer -- Map of voter PKH to vote weight
  , proposalOutcome    :: Maybe Integer        -- 1 for approve, 0 for reject, Nothing if undecided
  } deriving (Generic)

PlutusTx.makeIsDataIndexed ''DFCTProposal [('DFCTProposal, 0)]
PlutusTx.makeLift ''DFCTProposal

-- Governance Datum
data DFCTGovernanceDatum = DFCTGovernanceDatum
  { govParams          :: DFCTGovernanceParams     -- Governance parameters
  , proposal           :: Maybe DFCTProposal       -- Current proposal (if any)
  , proposalStatus     :: DFCTProposalStatus        -- Current proposal status
  } deriving (Generic)

PlutusTx.makeIsDataIndexed ''DFCTGovernanceDatum [('DFCTGovernanceDatum, 0)]
PlutusTx.makeLift ''DFCTGovernanceDatum

-- Governance Actions
data DFCTGovernanceAction
  = SubmitProposal DFCTProposal
  | VoteOnProposal BuiltinByteString Integer Integer -- DFCTProposal ID, vote (1=approve, 0=reject), vote weight
  | UpdateAuthorizedPKHs (AssocMap.Map PubKeyHash Integer) -- Update map of authorized submitters
  | UpdateMinVotingTokens Integer                -- Update minimum DFC tokens for voting
  | SetVotingPeriod BuiltinByteString POSIXTime POSIXTime -- DFCTProposal ID, start, end
  | FinalizeProposal BuiltinByteString Integer   -- DFCTProposal ID, outcome (1=approve, 0=reject)
  | ExecuteProposal BuiltinByteString            -- Execute approved proposal
  deriving (Generic)

PlutusTx.makeIsDataIndexed ''DFCTGovernanceAction [
  ('SubmitProposal, 0),
  ('VoteOnProposal, 1),
  ('UpdateAuthorizedPKHs, 2),
  ('UpdateMinVotingTokens, 3),
  ('SetVotingPeriod, 4),
  ('FinalizeProposal, 5),
  ('ExecuteProposal, 6)]
PlutusTx.makeLift ''DFCTGovernanceAction

-- Governance Validator
{-# INLINABLE mkGovernanceValidator #-}
mkGovernanceValidator :: BuiltinData -> BuiltinUnit
mkGovernanceValidator rawData =
  case fromBuiltinData rawData of
    Nothing -> traceError "1" -- Failed: ScriptContext
    Just scriptContext ->
      let txInfo = scriptContextTxInfo scriptContext
          scriptInfo = scriptContextScriptInfo scriptContext
          govDatum = case scriptInfo of
            SpendingScript _ dt -> case dt of
              Just d -> fromBuiltinData (getDatum d) :: Maybe DFCTGovernanceDatum
              Nothing -> Nothing
            _ -> Nothing
          action = case fromBuiltinData $ getRedeemer (scriptContextRedeemer scriptContext) of
            Just a -> a :: DFCTGovernanceAction
            Nothing -> traceError "2" -- Failed: DFCTGovernanceAction
      in case govDatum of
        Nothing -> traceError "3" -- Failed: DFCTGovernanceDatum
        Just datum -> case action of
          SubmitProposal prop ->
            if not (validateProposalSubmission prop (govParams datum) txInfo)
              then traceError "4" -- Invalid proposal submission
              else if not (ensureProperTransition Proposed scriptContext)
                then traceError "5" -- Invalid state transition
                else toOpaque ()

          VoteOnProposal pid vote weight ->
            if not (validateVote pid vote weight datum (govParams datum) txInfo)
              then traceError "6" -- Invalid vote
              else if not (ensureProperTransition PSVoting scriptContext)
                then traceError "7" -- Invalid state transition
                else toOpaque ()

          UpdateAuthorizedPKHs newPKHs ->
            if not (Contexts.txSignedBy txInfo (gpOwner (govParams datum)))
              then traceError "8" -- Unauthorized: only owner
              else if AssocMap.null newPKHs
                then traceError "9" -- Empty PKH map
              else if not (validateAuthorizedPKHs newPKHs)
                then traceError "10" -- Invalid PKH map values
              else if not (ensureParamUpdate scriptContext newPKHs (gpMinVotingTokens (govParams datum)))
                then traceError "11" -- Invalid param update
                else toOpaque ()

          UpdateMinVotingTokens minTokens ->
            if not (Contexts.txSignedBy txInfo (gpOwner (govParams datum)))
              then traceError "12" -- Unauthorized: only owner
              else if minTokens <= 0
                then traceError "13" -- Invalid minimum tokens
              else if not (ensureParamUpdate scriptContext (gpAuthorizedPKHs (govParams datum)) minTokens)
                then traceError "14" -- Invalid param update
                else toOpaque ()

          SetVotingPeriod pid start end ->
            if not (validateVotingPeriodUpdate pid start end datum (govParams datum) txInfo)
              then traceError "15" -- Invalid voting period update
              else if not (ensureProperTransition Proposed scriptContext)
                then traceError "16" -- Invalid state transition
                else toOpaque ()

          FinalizeProposal pid outcome ->
            if not (validateProposalFinalization pid outcome datum (govParams datum) txInfo)
              then traceError "17" -- Invalid finalization
              else if not (ensureProperTransition (if outcome == 1 then Approved else Rejected) scriptContext)
                then traceError "18" -- Invalid state transition
                else toOpaque ()

          ExecuteProposal pid ->
            if not (validateProposalExecution pid datum (govParams datum) txInfo)
              then traceError "19" -- Invalid execution
              else if not (ensureProperTransition Executed scriptContext)
                then traceError "20" -- Invalid state transition
                else toOpaque ()

-- Validate DFCTProposal Submission
{-# INLINABLE validateProposalSubmission #-}
validateProposalSubmission :: DFCTProposal -> DFCTGovernanceParams -> TxInfo -> Bool
validateProposalSubmission prop params info =
  lengthOfByteString (proposalId prop) > 0 &&
  Contexts.txSignedBy info (proposer prop) &&
  (proposer prop == gpOwner params || isAuthorizedSubmitter (proposer prop) (gpAuthorizedPKHs params))

-- Validate Authorized PKHs Map
{-# INLINABLE validateAuthorizedPKHs #-}
validateAuthorizedPKHs :: AssocMap.Map PubKeyHash Integer -> Bool
validateAuthorizedPKHs pkhMap =
  all (\(_, value) -> value > 0) (AssocMap.toList pkhMap)

-- Check if PKH is Authorized Submitter
{-# INLINABLE isAuthorizedSubmitter #-}
isAuthorizedSubmitter :: PubKeyHash -> AssocMap.Map PubKeyHash Integer -> Bool
isAuthorizedSubmitter pkh submitters =
  case AssocMap.lookup pkh submitters of
    Just v -> v > 0
    Nothing -> False

-- Validate Vote
{-# INLINABLE validateVote #-}
validateVote :: BuiltinByteString -> Integer -> Integer -> DFCTGovernanceDatum -> DFCTGovernanceParams -> TxInfo -> Bool
validateVote pid vote weight datum params info =
  case proposal datum of
    Nothing -> False
    Just prop ->
      let voter = head (txInfoSignatories info) -- Assume single voter for simplicity
          currentTime = txInfoValidRange info
      in proposalId prop == pid &&
         proposalStatus datum == PSVoting &&
         Interval.contains (Interval.from (votingStart prop)) currentTime &&
         Interval.contains (Interval.to (votingEnd prop)) currentTime &&
         (vote == 1 || vote == 0) && -- Ensure valid vote value
         weight > 0 &&
         checkVoterEligibility voter weight params info &&
         not (AssocMap.member voter (voteTally prop)) -- Prevent duplicate voting

-- Check Voter Eligibility (Minimum DFC Tokens)
{-# INLINABLE checkVoterEligibility #-}
checkVoterEligibility :: PubKeyHash -> Integer -> DFCTGovernanceParams -> TxInfo -> Bool
checkVoterEligibility voter weight params info =
  let inputs = txInfoInputs info
      voterAddress = Address (PubKeyCredential voter) Nothing
      voterInputs = filter (\txIn -> txOutAddress (txInInfoResolved txIn) == voterAddress) inputs
      voterValue = foldl (\acc txIn -> acc <> txOutValue (txInInfoResolved txIn)) mempty voterInputs
      valueMap = getValue voterValue
      tokenMap = fromMaybe AssocMap.empty (AssocMap.lookup (gpTokenSymbol params) valueMap)
      dfcTokens = fromMaybe 0 (AssocMap.lookup (gpTokenName params) tokenMap)
  in dfcTokens >= gpMinVotingTokens params && weight <= dfcTokens

-- Validate Voting Period Update
{-# INLINABLE validateVotingPeriodUpdate #-}
validateVotingPeriodUpdate :: BuiltinByteString -> POSIXTime -> POSIXTime -> DFCTGovernanceDatum -> DFCTGovernanceParams -> TxInfo -> Bool
validateVotingPeriodUpdate pid start end datum params info =
  case proposal datum of
    Nothing -> False
    Just prop ->
      proposalId prop == pid &&
      proposalStatus datum == Proposed &&
      start < end &&
      getPOSIXTime start > getLowerTime (txInfoValidRange info) &&
      (Contexts.txSignedBy info (gpOwner params) || isAuthorizedSubmitter (head (txInfoSignatories info)) (gpAuthorizedPKHs params))

-- Validate DFCTProposal Finalization
{-# INLINABLE validateProposalFinalization #-}
validateProposalFinalization :: BuiltinByteString -> Integer -> DFCTGovernanceDatum -> DFCTGovernanceParams -> TxInfo -> Bool
validateProposalFinalization pid outcome datum params info =
  case proposal datum of
    Nothing -> False
    Just prop ->
      proposalId prop == pid &&
      proposalStatus datum == PSVoting &&
      not (Interval.contains (Interval.to (votingEnd prop)) (txInfoValidRange info)) &&
      (outcome == 1 || outcome == 0) && -- Ensure valid outcome
      Contexts.txSignedBy info (gpOwner params)

-- Validate DFCTProposal Execution
{-# INLINABLE validateProposalExecution #-}
validateProposalExecution :: BuiltinByteString -> DFCTGovernanceDatum -> DFCTGovernanceParams -> TxInfo -> Bool
validateProposalExecution pid datum params info =
  case proposal datum of
    Nothing -> False
    Just prop ->
      proposalId prop == pid &&
      proposalStatus datum == Approved &&
      Contexts.txSignedBy info (gpOwner params)

-- Ensure Proper State Transition
{-# INLINABLE ensureProperTransition #-}
ensureProperTransition :: DFCTProposalStatus -> ScriptContext -> Bool
ensureProperTransition newStatus scriptCtx =
  case Contexts.findOwnInput scriptCtx of
    Nothing -> False
    Just _ -> case Contexts.getContinuingOutputs scriptCtx of
      [o] -> case txOutDatum o of
        OutputDatum d ->
          case PlutusTx.fromBuiltinData (getDatum d) of
            Just (newDatum :: DFCTGovernanceDatum) -> proposalStatus newDatum == newStatus
            Nothing -> False
        _ -> False
      _ -> False

-- Ensure Governance Parameter Update
{-# INLINABLE ensureParamUpdate #-}
ensureParamUpdate :: ScriptContext -> AssocMap.Map PubKeyHash Integer -> Integer -> Bool
ensureParamUpdate scriptCtx newPKHs newMinTokens =
  case Contexts.findOwnInput scriptCtx of
    Nothing -> False
    Just _ -> case Contexts.getContinuingOutputs scriptCtx of
      [o] -> case txOutDatum o of
        OutputDatum d ->
          case PlutusTx.fromBuiltinData (getDatum d) of
            Just (newDatum :: DFCTGovernanceDatum) ->
              let params = govParams newDatum
              in reviewerMapsEqual (gpAuthorizedPKHs params) newPKHs && gpMinVotingTokens params == newMinTokens
            Nothing -> False
        _ -> False
      _ -> False

-- Helper function to check equality of two PKH maps
{-# INLINABLE reviewerMapsEqual #-}
reviewerMapsEqual :: AssocMap.Map PubKeyHash Integer -> AssocMap.Map PubKeyHash Integer -> Bool
reviewerMapsEqual a b =
  let aKeys = AssocMap.keys a
      bKeys = AssocMap.keys b
  in setEqual aKeys bKeys && all (\k -> AssocMap.member k b) aKeys

-- Helper function to check set equality
{-# INLINABLE setEqual #-}
setEqual :: [PubKeyHash] -> [PubKeyHash] -> Bool
setEqual as bs = lenEqual as bs && all (\a -> any (\b -> a == b) bs) as

-- Helper function to check if two lists have the same length
{-# INLINABLE lenEqual #-}
lenEqual :: [a] -> [b] -> Bool
lenEqual [] [] = True
lenEqual (_:xs) (_:ys) = lenEqual xs ys
lenEqual _ _ = False

-- Helper: Extract POSIXTime from LowerBound
{-# INLINABLE getLowerTime #-}
getLowerTime :: POSIXTimeRange -> Integer
getLowerTime timeInterval =
  case Interval.ivFrom timeInterval of
    Interval.LowerBound (Interval.Finite t) _ -> getPOSIXTime t
    _ -> 0 -- Default for unbounded intervals