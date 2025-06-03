{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module DFCT.Provenance where

import DFCT.Types
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import           PlutusTx.Prelude
import           PlutusLedgerApi.V3
import qualified PlutusLedgerApi.V3.Contexts as Contexts
import qualified PlutusLedgerApi.V1.Interval as Interval

-- d-FCT Validator Logic
{-# INLINABLE mkDFCTValidator #-}
mkDFCTValidator :: CurrencySymbol -> BuiltinData -> BuiltinUnit
mkDFCTValidator symbol rawData = 
    case fromBuiltinData rawData of
        Nothing -> traceError "1" --Failed: ScriptContext
        Just scriptContext -> 
            let txInfo = scriptContextTxInfo scriptContext
                scriptInfo = scriptContextScriptInfo scriptContext

                -- Try to decode as TopicDatum
                topicDatum = case scriptInfo of
                    SpendingScript _ dt -> case dt of
                        Just d -> fromBuiltinData (getDatum d) :: Maybe TopicDatum
                        Nothing -> Nothing
                    _ -> Nothing

                -- Try to decode as ContributionDatum if TopicDatum fails
                contribDatum = case scriptInfo of
                    SpendingScript _ dt -> case dt of
                        Just d -> fromBuiltinData (getDatum d) :: Maybe ContributionDatum
                        Nothing -> Nothing
                    _ -> Nothing

                contrib = case fromBuiltinData $ getRedeemer (scriptContextRedeemer scriptContext) of
                        Just c -> c :: DFCTContrib
                        Nothing -> traceError "2" -- Failed: DFCTContrib
            in
                -- Handle based on which datum type we successfully decoded
                case (topicDatum, contribDatum) of
                    (Just datum, _) -> 
                        -- Topic datum path
                        if not (validateDatum datum)
                        then traceError "3" -- Invalid datum
                        else case contrib of
                            TopicAction ta -> case ta of
                                SubmitTopic topic' pool ->
                                    if not (validateTopicData topic' txInfo)
                                        then traceError "4" -- invalid topic data
                                    else if lengthOfByteString (topicId topic') == 0
                                        then traceError "5" -- invalid topic_id is empty
                                    else if not (validateRewardPool pool)
                                        then traceError "6" -- invalid reward pool allocation
                                    -- else if not (checkTokenAllocation pool txInfo symbol)
                                    --     then traceError "SubmitTopic: Insufficient token allocation"
                                    else toOpaque ()

                                ReviewTopic tid signed ->
                                    if topicStatus datum /= TopicProposed
                                        then traceError "7" -- Topic isnt in proposed state
                                    else if lengthOfByteString tid == 0
                                        then traceError "8" -- invalid topic_id is empty
                                    else if not (checkReviewerAuth signed datum txInfo)
                                        then traceError "9" -- unauthorized reviewer
                                    else if not (ensureProperTransition TopicReviewed scriptContext)
                                        then traceError "10" -- invalid transition
                                    else toOpaque ()

                                ActivateTopic tid ->
                                    if lengthOfByteString tid == 0
                                        then traceError "11" -- invalid topic_id is empty
                                    else if topicStatus datum /= TopicReviewed
                                        then traceError "12" -- Topic isnt in reviewed state
                                    else if not (ensureProperTransition TopicActivated scriptContext)
                                        then traceError "13" -- invalid transition
                                    else toOpaque ()

                                CloseTopic tid ->
                                    if lengthOfByteString tid == 0
                                        then traceError "14" -- invalid topic_id is empty
                                    else if topicStatus datum /= TopicActivated
                                        then traceError "15" -- Topic isnt in activated state
                                    else if not (ensureProperTransition TopicClosed scriptContext)
                                        then traceError "16" -- invalid transition
                                    else toOpaque ()

                                RejectTopic tid ->
                                    if lengthOfByteString tid == 0
                                        then traceError "17" -- invalid topic_id is empty
                                    else if topicStatus datum /= TopicProposed && topicStatus datum /= TopicReviewed
                                        then traceError "18" -- Topic isnt in proposed nor reviewed state
                                    else if not (ensureProperTransition TopicRejected scriptContext)
                                        then traceError "19" -- invalid transition
                                    else toOpaque ()

                            ContributionAction ca -> case ca of
                                SubmitContribution sContrib ->
                                    if topicStatus datum /= TopicActivated
                                        then traceError "20" -- topic isnt in activated state
                                    else if lengthOfByteString (contributionId sContrib) == 0
                                        then traceError "21" -- invalid contribution_id is empty
                                    else if not (validateContributionData sContrib txInfo)
                                        then traceError "22" -- Invalid contrib data
                                    else 
                                        -- Calculate timeliness score for new contributions
                                        let currentTime = txInfoValidRange txInfo
                                            timeScore = calculateTimelinessScore 
                                                        (creationTimestamp (rewardPool datum)) 
                                                        (getLowerTime currentTime)
                                        in if not (validateContributionTimeliness sContrib timeScore)
                                            then traceError "24" -- Invalid timeliness calc
                                            else toOpaque ()

                                _ -> traceError "25" -- Invalid contribution action

                            AdminAction aa -> case aa of
                                CheckPool tid ->
                                    if lengthOfByteString tid == 0
                                        then traceError "26" -- invalid topic_id is empty
                                    else if tid /= topicId (topic datum)
                                        then traceError "27" -- invalid topic_id
                                    else toOpaque ()

                                DistributeRewards tid rewards ->
                                    if lengthOfByteString tid == 0
                                        then traceError "28" -- invalid topic_id is empty
                                    else if topicStatus datum /= TopicActivated
                                        then traceError "29" -- topic isnt in activated state
                                    else if not (validateRewardDistribution rewards)
                                        then traceError "30" -- invalid reward distribution
                                    else if sumRewards rewards > totalAmount (rewardPool datum)
                                        then traceError "31" -- reward overflow
                                    else if not (validateOutputsForRewards rewards txInfo symbol)
                                        then traceError "32" -- invalid reward outputs
                                    else toOpaque ()

                                UpdateReviewers newReviewers ->
                                    if AssocMap.null newReviewers
                                        then traceError "33" -- invalid empty reviewer list
                                    else if not (ensureReviewerUpdate newReviewers scriptContext)
                                        then traceError "34" -- invalid reviewer update
                                    else toOpaque ()

                    (Nothing, Just _) -> 
                        -- Contribution datum path - Handle ContributionAction operations
                        case contrib of
                            ContributionAction ca -> case ca of
                                ReviewContribution cid rel acc comp reviewContent ->
                                    if lengthOfByteString cid == 0
                                        then traceError "35" -- invalid: contribution id is empty
                                    else if rel < 0 || rel > 10 || acc < 0 || acc > 10 || comp < 0 || comp > 10
                                        then traceError "36" -- invalid review scores
                                    else if not (findTopicInReferences txInfo)
                                        then traceError "37" -- reference to topic is required
                                    else if not (isTopicActive txInfo)
                                        then traceError "38" -- topic isn't in activated state
                                    else if not (checkReviewerInReferences txInfo)
                                        then traceError "39" -- reviewer is not authorized
                                    else if not (validateReviewContent reviewContent txInfo)
                                        then traceError "40" -- invalid content
                                    else if not (ensureContributionProperTransition ContributionReviewed scriptContext)
                                        then traceError "42" -- invalid transition
                                    else toOpaque ()

                                VerifyContribution cid ->
                                    if lengthOfByteString cid == 0
                                        then traceError "43" -- invalid: contribution id is empty
                                    else if not (findTopicInReferences txInfo)
                                        then traceError "44" -- reference to topic is required
                                    else if not (isTopicActive txInfo)
                                        then traceError "45" -- topic isn't in activated state
                                    else if not (ensureContributionProperTransition ContributionVerified scriptContext)
                                        then traceError "46" -- invalid transition
                                    else toOpaque ()

                                DisputeContribution cid disputeReason ->
                                    if lengthOfByteString cid == 0
                                        then traceError "47" -- invalid: contribution id is empty
                                    else if not (findTopicInReferences txInfo)
                                        then traceError "48" -- reference to topic is required
                                    else if not (isTopicActive txInfo)
                                        then traceError "49" -- topic isn't in activated state
                                    else if not (validateDisputeReason disputeReason txInfo)
                                        then traceError "51" -- invalid reason
                                    else if not (ensureContributionProperTransition ContributionDisputed scriptContext)
                                        then traceError "52" -- invalid transition
                                    else toOpaque ()

                                UpdateContribution cid newContent ->
                                    if lengthOfByteString cid == 0
                                        then traceError "53" -- invalid: contribution id is empty
                                    else if not (findTopicInReferences txInfo)
                                        then traceError "54" -- reference to topic is required
                                    else if not (isTopicActive txInfo)
                                        then traceError "55" -- topic isn't in activated state
                                    else if lengthOfByteString newContent <= 0
                                        then traceError "56"
                                    else if not (ensureContributionProperTransition ContributionUpdated scriptContext)
                                        then traceError "57" -- invalid transition
                                    else toOpaque ()

                                RejectContribution cid ->
                                    if lengthOfByteString cid == 0
                                        then traceError "58" -- invalid: contribution id is empty
                                    else if not (findTopicInReferences txInfo)
                                        then traceError "59" -- reference to topic is required
                                    else if not (isTopicActive txInfo)
                                        then traceError "60" -- topic isn't in activated state
                                    else if not (ensureContributionProperTransition ContributionRejected scriptContext)
                                        then traceError "61" -- invalid transition
                                    else toOpaque ()

                                EvaluateContribution cid ->
                                    if lengthOfByteString cid == 0
                                        then traceError "62" -- invalid: contribution id is empty
                                    else if not (findTopicInReferences txInfo)
                                        then traceError "63" -- reference to topic is required
                                    else if not (isTopicActive txInfo)
                                        then traceError "64" -- topic isn't in activated state
                                    else if not (ensureContributionProperTransition ContributionEvaluated scriptContext)
                                        then traceError "65" -- invalid transition
                                    else toOpaque ()

                                SubmitContribution sContrib ->
                                    if lengthOfByteString (contributionId sContrib) == 0
                                        then traceError "66" -- invalid: contribution id is empty
                                    else if not (findTopicInReferences txInfo)
                                        then traceError "67" -- reference to topic is required
                                    else if not (isTopicActive txInfo)
                                        then traceError "68" -- topic isn't in activated state
                                    else if not (Contexts.txSignedBy txInfo (contributionCreator sContrib))
                                        then traceError "69" -- creator signature required
                                    else if not (ensureContributionProperTransition ContributionProposed scriptContext)
                                        then traceError "70" -- invalid transition
                                    else toOpaque ()

                            -- Other actions (TopicAction, AdminAction) aren't allowed for Contribution UTxOs
                            _ -> traceError "71" -- missing topic datum

                    -- If we couldn't decode either datum type
                    (Nothing, Nothing) -> traceError "72" -- failed decoding either datum type

-- Check if there's a topic reference input in active state
{-# INLINABLE findTopicInReferences #-}
findTopicInReferences :: TxInfo -> Bool
findTopicInReferences info =
    any isTopicDatumInRef (txInfoReferenceInputs info)

{-# INLINABLE isTopicDatumInRef #-}
isTopicDatumInRef :: TxInInfo -> Bool
isTopicDatumInRef txInInfo =
    case txOutDatum (txInInfoResolved txInInfo) of
    OutputDatum d ->
        case PlutusTx.fromBuiltinData (getDatum d) of
        Just (_ :: TopicDatum) -> True
        Nothing -> False
    _ -> False

-- Check if the referenced topic is in active state
{-# INLINABLE isTopicActive #-}
isTopicActive :: TxInfo -> Bool
isTopicActive info =
    any checkTopicActive (txInfoReferenceInputs info)

{-# INLINABLE checkTopicActive #-}
checkTopicActive :: TxInInfo -> Bool
checkTopicActive txInInfo =
    case txOutDatum (txInInfoResolved txInInfo) of
    OutputDatum d ->
        case PlutusTx.fromBuiltinData (getDatum d) of
        Just (topic_datum :: TopicDatum) -> topicStatus topic_datum == TopicActivated
        Nothing -> False
    _ -> False

-- Check if a reviewer is authorized by looking at reference inputs
{-# INLINABLE checkReviewerInReferences #-}
checkReviewerInReferences :: TxInfo -> Bool
checkReviewerInReferences info =
    let signatories = txInfoSignatories info
    in any (checkSignatory signatories) (txInfoReferenceInputs info)

{-# INLINABLE checkSignatory #-}
checkSignatory :: [PubKeyHash] -> TxInInfo -> Bool
checkSignatory sigs txInInfo =
    case txOutDatum (txInInfoResolved txInInfo) of
    OutputDatum d ->
        case PlutusTx.fromBuiltinData (getDatum d) of
        Just (topic_datum :: TopicDatum) -> 
            any (\pkh -> isAuthorizedReviewer pkh (authorizedReviewers topic_datum)) sigs
        Nothing -> False
    _ -> False

-- Check transition of ContributionStatus
{-# INLINABLE ensureContributionProperTransition #-}
ensureContributionProperTransition :: ContributionStatus -> ScriptContext -> Bool
ensureContributionProperTransition newStatus scriptCtx =
    case Contexts.findOwnInput scriptCtx of
        Nothing -> False
        Just _  -> case Contexts.getContinuingOutputs scriptCtx of
            [o] -> case txOutDatum o of
                OutputDatum d ->
                    case PlutusTx.fromBuiltinData (getDatum d) of
                        Just (newDatum :: ContributionDatum) -> cStatus newDatum == newStatus
                        Nothing -> False
                _ -> False
            _   -> False

-- Helper function to check if a PKH is in a ReviewerMap
{-# INLINABLE isAuthorizedReviewer #-}
isAuthorizedReviewer :: PubKeyHash -> ReviewerMap -> Bool
isAuthorizedReviewer pkh reviewers = 
    case AssocMap.lookup pkh reviewers of
        Just v -> v > 0
        Nothing -> False

-- helper function to check equality of two ReviewerMaps
{-# INLINABLE reviewerMapsEqual #-}
reviewerMapsEqual :: ReviewerMap -> ReviewerMap -> Bool
reviewerMapsEqual a b = 
    let aKeys = AssocMap.keys a
        bKeys = AssocMap.keys b
    in setEqual aKeys bKeys && all (\k -> AssocMap.member k b) aKeys

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
       then traceError "Inv timeliness: current time less than creation time"
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
    Contexts.txSignedBy info (topicProposer t)

{-# INLINABLE validateRewardPool #-}
validateRewardPool :: RewardPoolInfo -> Bool
validateRewardPool pool = totalAmount pool > 0 && allocatedAmount pool == 0

{-# INLINABLE checkTokenAllocation #-}
checkTokenAllocation :: RewardPoolInfo -> TxInfo -> CurrencySymbol -> Bool
checkTokenAllocation pool info symbol =
    let value = Contexts.valueSpent info
        valueMap = getValue value
        tokenMap = case (AssocMap.lookup symbol valueMap) of
            Nothing -> traceError ("DFC CurrencySymbol not found in valueMap. symbol: " <> decodeUtf8 (unCurrencySymbol symbol))
            Just tMap -> tMap
        inputTokens = fromMaybe 0 (AssocMap.lookup (tokenName pool) tokenMap)
    in if inputTokens >= (totalAmount pool)
       then True
       else traceError $ "Insufficient tokens"

{-# INLINABLE validateContributionData #-}
validateContributionData :: Contribution -> TxInfo -> Bool
validateContributionData c info =
    lengthOfByteString (contributionId c) > 0 &&
    lengthOfByteString (contributionTopicId c) > 0 &&
    Contexts.txSignedBy info (contributionCreator c)

-- validate contribution timeliness
{-# INLINABLE validateContributionTimeliness #-}
validateContributionTimeliness :: Contribution -> Integer -> Bool
validateContributionTimeliness _ timeScore = timeScore >= 0 && timeScore <= 10

-- validate review content
{-# INLINABLE validateReviewContent #-}
validateReviewContent :: ReviewContent -> TxInfo -> Bool
validateReviewContent rc info =
    (reviewTimestamp rc) > 0 &&
    Contexts.txSignedBy info (reviewerPkh rc)

-- validate dispute reason
{-# INLINABLE validateDisputeReason #-}
validateDisputeReason :: DisputeReason -> TxInfo -> Bool
validateDisputeReason dr info =
    Contexts.txSignedBy info (disputeInitiator dr)

{-# INLINABLE checkReviewerAuth #-}
checkReviewerAuth :: Integer -> TopicDatum -> TxInfo -> Bool
checkReviewerAuth _ datum info =
    let signatories = txInfoSignatories info
    in any (\pkh -> isAuthorizedReviewer pkh (authorizedReviewers datum)) signatories

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
sumRewards = foldl (\total (_, amt) -> total + amt) 0 . AssocMap.toList

{-# INLINABLE validateRewardDistribution #-}
validateRewardDistribution :: RewardMap -> Bool
validateRewardDistribution rewards =
    all (\(_, amount) -> amount > 0) (AssocMap.toList rewards)

{-# INLINABLE validateOutputsForRewards #-}
validateOutputsForRewards :: RewardMap -> TxInfo -> CurrencySymbol -> Bool
validateOutputsForRewards rewardMap info symbol =
    all (validateSingleOutput info symbol) (AssocMap.toList rewardMap)

{-# INLINABLE validateSingleOutput #-}
validateSingleOutput :: TxInfo -> CurrencySymbol -> (PubKeyHash, Integer) -> Bool
validateSingleOutput info symbol (pkh, reward) =
    any (\o -> txOutAddress o == pubKeyHashAddress pkh &&
            let getValue' = getValue (txOutValue o)
                tokenMap = fromMaybe AssocMap.empty (AssocMap.lookup symbol getValue')
                quantity = fromMaybe 0 (AssocMap.lookup (TokenName "DFC") tokenMap)
            in quantity >= reward) (txInfoOutputs info)

{-# INLINABLE pubKeyHashAddress #-}
pubKeyHashAddress :: PubKeyHash -> Address
pubKeyHashAddress pkh = Address (PubKeyCredential pkh) Nothing