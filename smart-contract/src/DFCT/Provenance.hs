{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module DFCT.Provenance where

import DFCT.Types
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map
import qualified PlutusTx.Prelude as Maybe
import           PlutusTx.Prelude           hiding (Semigroup (..), unless)
import           PlutusLedgerApi.V3
import qualified PlutusLedgerApi.V3.Contexts as Contexts
import qualified PlutusLedgerApi.V1.Interval as Interval

-- d-FCT Validator Logic
{-# INLINABLE mkDFCTValidator #-}
mkDFCTValidator :: CurrencySymbol -> BuiltinData -> BuiltinUnit
mkDFCTValidator symbol rawData = 
    let scriptContext = unsafeFromBuiltinData rawData :: ScriptContext
        txInfo = scriptContextTxInfo scriptContext
        ownInput = case Contexts.findOwnInput scriptContext of
                     Just i  -> i
                     Nothing -> traceError "Input not found"
        rawDatum = case txOutDatum (txInInfoResolved ownInput) of
                     OutputDatum d -> getDatum d
                     _             -> traceError "Datum not found"
        datum = unsafeFromBuiltinData rawDatum :: TopicDatum
        
        redeemer = scriptContextRedeemer scriptContext
        contrib = unsafeFromBuiltinData (getRedeemer redeemer) :: DFCTContrib
    in
        if not (validateDatum datum) 
        then traceError "Invalid datum"
        else case contrib of
            TopicAction ta -> case ta of
                SubmitTopic topic' pool ->
                    if not (validateTopicData topic' txInfo)
                        then traceError "Invalid topic data"
                    else if lengthOfByteString (topicId topic') == 0
                        then traceError "Topic ID cannot be empty"
                    else if not (validateRewardPool pool)
                        then traceError "Invalid reward pool allocation"
                    else if not (checkTokenAllocation pool txInfo symbol)
                        then traceError "Insufficient token allocation"
                    else toOpaque ()

                ReviewTopic tid signed ->
                    if topicStatus datum /= TopicProposed
                        then traceError "Topic not in proposed state"
                    else if lengthOfByteString tid == 0
                        then traceError "Topic ID cannot be empty"
                    else if not (checkReviewerAuth signed datum txInfo)
                        then traceError "Unauthorized reviewer"
                    else if not (ensureProperTransition TopicReviewed scriptContext)
                        then traceError "Failed to transition topic state"
                    else toOpaque ()

                ActivateTopic tid ->
                    if lengthOfByteString tid == 0
                        then traceError "Topic ID cannot be empty"
                    else if topicStatus datum /= TopicReviewed
                        then traceError "Topic not in reviewed state"
                    else if not (ensureProperTransition TopicActivated scriptContext)
                        then traceError "Failed to transition topic state"
                    else toOpaque ()

                CloseTopic tid ->
                    if lengthOfByteString tid == 0
                        then traceError "Topic ID cannot be empty"
                    else if topicStatus datum /= TopicActivated
                        then traceError "Topic not in active state"
                    else if not (ensureProperTransition TopicClosed scriptContext)
                        then traceError "Failed to transition topic state"
                    else toOpaque ()

                RejectTopic tid ->
                    if lengthOfByteString tid == 0
                        then traceError "Topic ID cannot be empty"
                    else if topicStatus datum /= TopicProposed && topicStatus datum /= TopicReviewed
                        then traceError "Topic not in proposed or reviewed state"
                    else if not (ensureProperTransition TopicRejected scriptContext)
                        then traceError "Failed to transition topic state"
                    else toOpaque ()

            ContributionAction ca -> case ca of
                SubmitEvidence sEvContrib ->
                    if topicStatus datum /= TopicActivated
                        then traceError "Topic not in active state"
                    else if lengthOfByteString (contributionId sEvContrib) == 0
                        then traceError "Contribution ID cannot be empty"
                    else if not (validateContributionData sEvContrib txInfo)
                        then traceError "Invalid contribution data"
                    else 
                        -- Calculate timeliness score for new contributions
                        let currentTime = txInfoValidRange txInfo
                            timeScore = calculateTimelinessScore 
                                        (creationTimestamp (rewardPool datum)) 
                                        (getLowerTime currentTime)
                        in if not (validateContributionTimeliness sEvContrib timeScore)
                            then traceError "Invalid timeliness calculation"
                            else toOpaque ()

                CastVote cvContrib ->
                    if topicStatus datum /= TopicActivated
                        then traceError "Topic not in active state"
                    else if not (validateContributionData cvContrib txInfo)
                        then traceError "Invalid vote data"
                    else toOpaque ()

                SelectTag stContrib ->
                    if topicStatus datum /= TopicActivated
                        then traceError "Topic not in active state"
                    else if not (validateContributionData stContrib txInfo)
                        then traceError "Invalid tag selection"
                    else toOpaque ()

                ReviewContribution cid rel acc comp reviewContent ->
                    if lengthOfByteString cid == 0
                        then traceError "Contribution ID cannot be empty"
                    else if topicStatus datum /= TopicActivated
                        then traceError "Topic not in active state"
                    else if rel < 0 || rel > 10 || acc < 0 || acc > 10 || comp < 0 || comp > 10
                        then traceError "Invalid review scores"
                    else if not (checkReviewerAuth False datum txInfo)
                        then traceError "Unauthorized reviewer"
                    else if not (validateReviewContent reviewContent txInfo)
                        then traceError "Invalid review content"
                    else if cid /= refCntribId reviewContent
                        then traceError "Review content must reference the specified contribution"
                    else toOpaque ()

                VerifyContribution cid ->
                    if lengthOfByteString cid == 0
                        then traceError "Contribution ID cannot be empty"
                    else if topicStatus datum /= TopicActivated
                        then traceError "Topic not in active state"
                    else toOpaque ()

                DisputeContribution cid disputeReason ->
                    if lengthOfByteString cid == 0
                        then traceError "Contribution ID cannot be empty"
                    else if topicStatus datum /= TopicActivated
                        then traceError "Topic not in active state"
                    else if lengthOfByteString (disputeContent disputeReason) <= 0
                        then traceError "Dispute reason required"
                    else if not (validateDisputeReason disputeReason txInfo)
                        then traceError "Invalid dispute reason"
                    else toOpaque ()

                UpdateContribution cid newContent ->
                    if lengthOfByteString cid == 0
                        then traceError "Contribution ID cannot be empty"
                    else if topicStatus datum /= TopicActivated
                        then traceError "Topic not in active state"
                    else if lengthOfByteString newContent <= 0
                        then traceError "Updated content required"
                    else toOpaque ()

                RejectContribution cid ->
                    if lengthOfByteString cid == 0
                        then traceError "Contribution ID cannot be empty"
                    else if topicStatus datum /= TopicActivated
                        then traceError "Topic not in active state"
                    else toOpaque ()

                EvaluateContribution cid ->
                    if lengthOfByteString cid == 0
                        then traceError "Contribution ID cannot be empty"
                    else if topicStatus datum /= TopicActivated
                        then traceError "Topic not in active state"
                    else toOpaque ()

            AdminAction aa -> case aa of
                CheckPool tid ->
                    if lengthOfByteString tid == 0
                        then traceError "Topic ID cannot be empty"
                    else if tid /= topicId (topic datum)
                        then traceError "Topic does not exist"
                    else toOpaque ()

                DistributeRewards tid rewards ->
                    if lengthOfByteString tid == 0
                        then traceError "Topic ID cannot be empty"
                    else if topicStatus datum /= TopicActivated
                        then traceError "Topic not in active state"
                    else if not (validateRewardDistribution rewards)
                        then traceError "Invalid reward distribution"
                    else if sumRewards rewards > totalAmount (rewardPool datum)
                        then traceError "Reward overflow"
                    else if not (validateOutputsForRewards rewards txInfo symbol)
                        then traceError "Invalid reward outputs"
                    else toOpaque ()

                UpdateReviewers newReviewers ->
                    if Map.null newReviewers
                        then traceError "Empty reviewer list not allowed"
                    else if not (ensureReviewerUpdate newReviewers scriptContext)
                        then traceError "Failed to update reviewers"
                    else toOpaque ()

-- Helper functions

-- helper function to check if a PKH is in a ReviewerMap
{-# INLINABLE isAuthorizedReviewer #-}
isAuthorizedReviewer :: PubKeyHash -> ReviewerMap -> Bool
isAuthorizedReviewer pkh reviewers = Map.member pkh reviewers

-- helper function to check equality of two ReviewerMaps
{-# INLINABLE reviewerMapsEqual #-}
reviewerMapsEqual :: ReviewerMap -> ReviewerMap -> Bool
reviewerMapsEqual a b = 
    let aKeys = Map.keys a
        bKeys = Map.keys b
    in setEqual aKeys bKeys && all (\k -> Map.member k b) aKeys

-- helper function to check set equality
{-# INLINABLE setEqual #-}
setEqual :: [PubKeyHash] -> [PubKeyHash] -> Bool
setEqual as bs = lenEqual as bs && all (\a -> any (\b -> a == b) bs) as

-- helper function to check if two lists have the same length
{-# INLINABLE lenEqual #-}
lenEqual :: [a] -> [b] -> Bool
lenEqual [] [] = True
lenEqual (_:xs) (_:ys) = lenEqual xs ys
lenEqual _ _ = False

-- Helper function to extract POSIXTime from LowerBound
{-# INLINABLE getLowerTime #-}
getLowerTime :: POSIXTimeRange -> Integer
getLowerTime interval = 
    case Interval.ivFrom interval of
        Interval.LowerBound (Interval.Finite t) _ -> getPOSIXTime t
        _                                         -> 0 -- Default to 0 for unbounded intervals

{-# INLINABLE calculateTimelinessScore #-}
calculateTimelinessScore :: Integer -> Integer -> Integer
calculateTimelinessScore creationTime currentTime =
    let timeDifference :: Integer = (currentTime - creationTime)
        hourInMillis :: Integer = (60 * 60 * 1000)
        twoHours :: Integer = (2 * hourInMillis)
        twelveHours :: Integer = (12 * hourInMillis)
    -- First check if the time difference is negative
    -- A negative time difference means the contribution timestamp is 
    -- before the topic creation, which is invalid
    in if currentTime < creationTime
       then traceError "Invalid timeliness: current time less than creation time"
       else if timeDifference <= twoHours
            then 10  -- First 2 hours: maximum bonus
            else if timeDifference <= twelveHours
                 then 5   -- First 12 hours: medium bonus
                 else 1   -- Later: minimum valid score

{-# INLINABLE validateDatum #-}
validateDatum :: TopicDatum -> Bool
validateDatum d = (lengthOfByteString (topicId $ topic d) /= 0) &&
                    totalAmount (rewardPool d) >= 0 &&
                    allocatedAmount (rewardPool d) >= 0

{-# INLINABLE validateTopicData #-}
validateTopicData :: Topic -> TxInfo -> Bool
validateTopicData t info =
    lengthOfByteString (topicId t) > 0 &&
    lengthOfByteString (topicTitle t) > 0 &&
    Contexts.txSignedBy info (topicProposer t)

{-# INLINABLE validateRewardPool #-}
validateRewardPool :: RewardPoolInfo -> Bool
validateRewardPool pool = totalAmount pool > 0 && allocatedAmount pool == 0

{-# INLINABLE checkTokenAllocation #-}
checkTokenAllocation :: RewardPoolInfo -> TxInfo -> CurrencySymbol -> Bool
checkTokenAllocation pool info symbol =
    let value = Contexts.valueSpent info
        getValue' = getValue value
        tokenMap = Maybe.fromMaybe Map.empty (Map.lookup symbol getValue')
        inputTokens = Maybe.fromMaybe 0 (Map.lookup (TokenName "DFC") tokenMap)
    in inputTokens >= totalAmount pool

{-# INLINABLE validateContributionData #-}
validateContributionData :: Contribution -> TxInfo -> Bool
validateContributionData c info =
    lengthOfByteString (contributionId c) > 0 &&
    lengthOfByteString (contributionTopicId c) > 0 &&
    lengthOfByteString (contributionType c) > 0 &&
    Contexts.txSignedBy info (contributionCreator c)

-- validate contribution timeliness
{-# INLINABLE validateContributionTimeliness #-}
validateContributionTimeliness :: Contribution -> Integer -> Bool
validateContributionTimeliness _ timeScore = timeScore >= 0 && timeScore <= 10

-- validate review content
{-# INLINABLE validateReviewContent #-}
validateReviewContent :: ReviewContent -> TxInfo -> Bool
validateReviewContent rc info =
    lengthOfByteString (refCntribId rc) > 0 &&
    lengthOfByteString (relevanceReason rc) > 0 &&
    lengthOfByteString (accuracyReason rc) > 0 &&
    lengthOfByteString (completenessReason rc) > 0 &&
    Contexts.txSignedBy info (reviewerPkh rc)

-- validate dispute reason
{-# INLINABLE validateDisputeReason #-}
validateDisputeReason :: DisputeReason -> TxInfo -> Bool
validateDisputeReason dr info =
    lengthOfByteString (disputeContent dr) > 0 &&
    Contexts.txSignedBy info (disputeInitiator dr)

{-# INLINABLE checkReviewerAuth #-}
checkReviewerAuth :: Bool -> TopicDatum -> TxInfo -> Bool
checkReviewerAuth signed datum info =
    let signatories = txInfoSignatories info
    in any (\pkh -> isAuthorizedReviewer pkh (authorizedReviewers datum)) signatories || signed

{-# INLINABLE ensureProperTransition #-}
ensureProperTransition :: TopicStatus -> ScriptContext -> Bool
ensureProperTransition newStatus scriptCtx =
    case Contexts.findOwnInput scriptCtx of
        Nothing -> False
        Just _  -> case Contexts.getContinuingOutputs scriptCtx of
            [o] -> case txOutDatum o of
                OutputDatum d ->
                    case PlutusTx.fromBuiltinData (getDatum d) of
                        Just (newDatum :: TopicDatum) -> topicStatus newDatum == newStatus
                        Nothing -> False
                _ -> False
            _   -> False

{-# INLINABLE ensureReviewerUpdate #-}
ensureReviewerUpdate :: ReviewerMap -> ScriptContext -> Bool
ensureReviewerUpdate newReviewers scriptCtx =
    case Contexts.findOwnInput scriptCtx of
        Nothing -> False
        Just _  -> case Contexts.getContinuingOutputs scriptCtx of
            [o] -> case txOutDatum o of
                OutputDatum d ->
                    case PlutusTx.fromBuiltinData (getDatum d) of
                        Just (updatedDatum :: TopicDatum) ->
                            let updatedReviewers = authorizedReviewers updatedDatum
                            in reviewerMapsEqual updatedReviewers newReviewers
                        Nothing -> False
                _ -> False
            _   -> False

-- calculate sum of rewards in RewardMap
{-# INLINABLE sumRewards #-}
sumRewards :: RewardMap -> Integer
sumRewards = foldl (\total (_, amt) -> total + amt) 0 . Map.toList

{-# INLINABLE validateRewardDistribution #-}
validateRewardDistribution :: RewardMap -> Bool
validateRewardDistribution rewards =
    all (\(_, amount) -> amount > 0) (Map.toList rewards)

{-# INLINABLE validateOutputsForRewards #-}
validateOutputsForRewards :: RewardMap -> TxInfo -> CurrencySymbol -> Bool
validateOutputsForRewards rewardMap info symbol =
    all (validateSingleOutput info symbol) (Map.toList rewardMap)

{-# INLINABLE validateSingleOutput #-}
validateSingleOutput :: TxInfo -> CurrencySymbol -> (PubKeyHash, Integer) -> Bool
validateSingleOutput info symbol (pkh, reward) =
    any (\o -> txOutAddress o == pubKeyHashAddress pkh &&
            let getValue' = getValue (txOutValue o)
                tokenMap = Maybe.fromMaybe Map.empty (Map.lookup symbol getValue')
                quantity = Maybe.fromMaybe 0 (Map.lookup (TokenName "DFC") tokenMap)
            in quantity >= reward) (txInfoOutputs info)

{-# INLINABLE pubKeyHashAddress #-}
pubKeyHashAddress :: PubKeyHash -> Address
pubKeyHashAddress pkh = Address (PubKeyCredential pkh) Nothing