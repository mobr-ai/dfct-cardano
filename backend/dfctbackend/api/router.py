from fastapi import APIRouter, HTTPException
from typing import Any
import logging

from dfctbackend.models import (
    TopicSubmitRequest, TopicReviewRequest, TopicActivateRequest,
    ContributionSubmitRequest, ContributionReviewRequest, ContributionDisputeRequest,
    ProposalSubmitRequest, VoteRequest, FinalizeProposalRequest,
    TransactionResponse
)
from dfctbackend.cardano.provenance_contract import ProvenanceContract
from dfctbackend.cardano.governance_contract import GovernanceContract
from dfctbackend.cardano.transaction import TransactionError
from dfctbackend.cardano.wallet import get_wallet_by_name, local_wallets

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/v1")

# Create contract instances
provenance_contract = ProvenanceContract()
governance_contract = GovernanceContract()

@router.post("/topic", response_model=TransactionResponse)
async def submit_topic(request: TopicSubmitRequest):
    """
    Submit a new topic to the provenance contract.
    
    Args:
        request: Topic submission request.
        
    Returns:
        TransactionResponse: Transaction response.
    """
    try:
        wallet = local_wallets["proposer"]
        result = provenance_contract.submit_topic(
            proposer=wallet,
            title=request.title,
            description=request.description,
            lovelace_amount=request.lovelace_amount,
            reward_amount=request.reward_amount
        )
        return TransactionResponse(
            transaction_hash=result["transaction_hash"],
            topic_id=result["topic_id"]
        )
    except TransactionError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error submitting topic: {str(e)}")

@router.post("/topic/review", response_model=TransactionResponse)
async def review_topic(request: TopicReviewRequest):
    """
    Review a topic in the provenance contract.
    
    Args:
        request: Topic review request.
        
    Returns:
        TransactionResponse: Transaction response.
    """
    try:
        wallet = local_wallets["reviewer1"]
        result = provenance_contract.review_topic(
            wallet=wallet,
            topic_id=request.topic_id,
            approved=request.approved
        )
        return TransactionResponse(
            transaction_hash=result["transaction_hash"],
            topic_id=request.topic_id
        )
    except TransactionError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error reviewing topic: {str(e)}")

@router.post("/topic/{wallet_name}/review", response_model=TransactionResponse)
async def review_topic_with_wallet(wallet_name: str, request: TopicReviewRequest):
    """
    Review a topic using a specific wallet.
    
    Args:
        wallet_name: Name of the wallet to use.
        request: Topic review request.
        
    Returns:
        TransactionResponse: Transaction response.
    """
    wallet = get_wallet_by_name(wallet_name)
    if not wallet:
        raise HTTPException(status_code=404, detail=f"Wallet '{wallet_name}' not found")
    
    try:
        result = provenance_contract.review_topic(
            wallet=wallet,
            topic_id=request.topic_id,
            approved=request.approved
        )
        return TransactionResponse(
            transaction_hash=result["transaction_hash"],
            topic_id=request.topic_id
        )
    except TransactionError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error reviewing topic: {str(e)}")

@router.post("/topic/activate", response_model=TransactionResponse)
async def activate_topic(request: TopicActivateRequest):
    """
    Activate a reviewed topic.
    
    Args:
        request: Topic activation request.
        
    Returns:
        TransactionResponse: Transaction response.
    """
    try:
        wallet = local_wallets["owner"]
        result = provenance_contract.activate_topic(
            wallet=wallet,
            topic_id=request.topic_id
        )
        return TransactionResponse(
            transaction_hash=result["transaction_hash"],
            topic_id=request.topic_id
        )
    except TransactionError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error activating topic: {str(e)}")

@router.get("/topics", response_model=list[dict])
async def get_all_topics():
    """
    Get all topics from the blockchain.
    
    Returns:
        List of topics with their details.
    """
    try:
        topics = provenance_contract.get_topics()
        return topics
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error fetching topics: {str(e)}")

@router.get("/topic/{topic_id}", response_model=dict)
async def get_topic(topic_id: str):
    """
    Get a specific topic by ID.
    
    Args:
        topic_id: ID of the topic.
        
    Returns:
        Topic details.
    """
    try:
        topic = provenance_contract.get_topic(topic_id)
        if not topic:
            raise HTTPException(status_code=404, detail=f"Topic {topic_id} not found")
        return topic
    except HTTPException as e:
        raise e
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error fetching topic: {str(e)}")

@router.post("/contribution", response_model=TransactionResponse)
async def submit_contribution(request: ContributionSubmitRequest):
    """
    Submit a new contribution to a topic.
    
    Args:
        request: Contribution submission request.
        
    Returns:
        TransactionResponse: Transaction response.
    """
    try:
        wallet = local_wallets["proposer"]
        result = provenance_contract.submit_contribution(
            topic_id=request.topic_id,
            content=request.content,
            contribution_type=request.contribution_type.value,
            contributor=wallet
        )
        return TransactionResponse(
            transaction_hash=result["transaction_hash"],
            topic_id=request.topic_id,
            contribution_id=result["contribution_id"]
        )
    except TransactionError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error submitting contribution: {str(e)}")

@router.post("/contribution/review", response_model=TransactionResponse)
async def review_contribution(request: ContributionReviewRequest):
    """
    Review a contribution.
    
    Args:
        request: Contribution review request.
        
    Returns:
        TransactionResponse: Transaction response.
    """
    try:
        wallet = local_wallets["reviewer1"]
        review_content = request.comment or "Review completed"
        result = provenance_contract.review_contribution(
            contribution_id=request.contribution_id,
            reviewer=wallet,
            relevance=request.relevance,
            accuracy=request.accuracy,
            completeness=request.completeness,
            review_content=review_content,
            approved=request.approved
        )
        return TransactionResponse(
            transaction_hash=result["transaction_hash"],
            contribution_id=request.contribution_id
        )
    except TransactionError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error reviewing contribution: {str(e)}")

@router.post("/contribution/{wallet_name}/review", response_model=TransactionResponse)
async def review_contribution_with_wallet(wallet_name: str, request: ContributionReviewRequest):
    """
    Review a contribution with a specific wallet.
    
    Args:
        wallet_name: Name of the wallet to use
        request: Contribution review request.
        
    Returns:
        TransactionResponse: Transaction response.
    """
    wallet = get_wallet_by_name(wallet_name)
    if not wallet:
        raise HTTPException(status_code=404, detail=f"Wallet '{wallet_name}' not found")
    
    try:
        review_content = request.comment or "Review completed"
        result = provenance_contract.review_contribution(
            contribution_id=request.contribution_id,
            reviewer=wallet,
            relevance=request.relevance,
            accuracy=request.accuracy,
            completeness=request.completeness,
            review_content=review_content,
            approved=request.approved
        )
        return TransactionResponse(
            transaction_hash=result["transaction_hash"],
            contribution_id=request.contribution_id
        )
    except TransactionError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error reviewing contribution: {str(e)}")

@router.get("/topic/{topic_id}/contributions", response_model=list[dict])
async def get_topic_contributions(topic_id: str):
    """
    Get all contributions for a specific topic.
    
    Args:
        topic_id: ID of the topic.
        
    Returns:
        List of contributions for the topic.
    """
    try:
        contributions = provenance_contract.get_contributions_for_topic(topic_id)
        return contributions
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error fetching topic contributions: {str(e)}")

@router.get("/contribution/{contribution_id}", response_model=dict)
async def get_contribution(contribution_id: str):
    """
    Get a specific contribution by ID.
    
    Args:
        contribution_id: ID of the contribution.
        
    Returns:
        Contribution details.
    """
    try:
        contribution = provenance_contract.get_contribution(contribution_id)
        if not contribution:
            raise HTTPException(status_code=404, detail=f"Contribution {contribution_id} not found")
        return contribution
    except HTTPException as e:
        raise e
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error fetching contribution: {str(e)}")

@router.post("/contribution/{contribution_id}/dispute", response_model=TransactionResponse)
async def dispute_contribution(contribution_id: str, request: ContributionDisputeRequest):
    """
    Dispute a reviewed contribution.
    
    Args:
        contribution_id: ID of the contribution to dispute.
        request: Dispute request containing the reason.
        
    Returns:
        TransactionResponse: Transaction response.
    """
    try:
        wallet = local_wallets["proposer"]
        result = provenance_contract.dispute_contribution(
            contribution_id=contribution_id,
            dispute_reason=request.dispute_reason,
            contributor=wallet
        )
        return TransactionResponse(
            transaction_hash=result["transaction_hash"],
            contribution_id=contribution_id
        )
    except TransactionError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error disputing contribution: {str(e)}")

@router.post("/topic/{topic_id}/close", response_model=TransactionResponse)
async def close_topic(topic_id: str):
    """
    Close a topic.
    
    Args:
        topic_id: ID of the topic to close.
        
    Returns:
        TransactionResponse: Transaction response.
    """
    try:
        wallet = local_wallets["owner"]
        result = provenance_contract.close_topic(
            topic_id=topic_id,
            wallet=wallet
        )
        return TransactionResponse(
            transaction_hash=result["transaction_hash"],
            topic_id=topic_id
        )
    except TransactionError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error closing topic: {str(e)}")

@router.get("/status/{tx_hash}", response_model=dict[str, Any])
async def check_transaction_status(tx_hash: str):
    """
    Check the status of a transaction.
    
    Args:
        tx_hash: Transaction hash.
        
    Returns:
        dict[str, Any]: Transaction status.
    """
    try:
        tx_status = provenance_contract.tx_handler.get_transaction_status(tx_hash)
        return {
            "status": tx_status["status"],
            "confirmed_block": tx_status.get("confirmed_block"),
            "confirmation_time": tx_status.get("confirmation_time"),
            "transaction_hash": tx_hash
        }
    except TransactionError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error checking transaction status: {str(e)}")

@router.get("/reviewers", response_model=list[dict])
async def get_all_reviewers():
    """
    Get all authorized reviewers from the blockchain.
    
    Returns:
        List of reviewers with their details.
    """
    try:
        topics = provenance_contract.get_topics()
        all_reviewers = []
        reviewer_set = set()
        for topic in topics:
            if "reviewers" in topic:
                for reviewer in topic["reviewers"]:
                    pkh = reviewer.get("pkh")
                    if pkh and pkh not in reviewer_set:
                        reviewer_set.add(pkh)
                        all_reviewers.append(reviewer)
        return all_reviewers
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error fetching reviewers: {str(e)}")

@router.post("/governance/proposal", response_model=TransactionResponse)
async def submit_proposal(request: ProposalSubmitRequest):
    """
    Submit a new governance proposal.
    """
    try:
        wallet = local_wallets["proposer"]
        result = governance_contract.submit_proposal(
            proposer=wallet,
            title=request.title,
            description=request.description,
            lovelace_amount=request.lovelace_amount,
            reward_amount=request.reward_amount
        )
        return TransactionResponse(
            transaction_hash=result["transaction_hash"],
            proposal_id=result["proposal_id"]
        )
    except TransactionError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error submitting proposal: {str(e)}")

@router.post("/governance/proposal/vote", response_model=TransactionResponse)
async def vote_on_proposal(request: VoteRequest):
    """
    Cast a vote on a governance proposal.
    """
    try:
        wallet = local_wallets["proposer"]
        result = governance_contract.vote_on_proposal(
            proposal_id=request.proposal_id,
            voter=wallet,
            vote=request.vote,
            dfc_amount=request.dfc_amount
        )
        return TransactionResponse(
            transaction_hash=result["transaction_hash"],
            proposal_id=request.proposal_id
        )
    except TransactionError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error voting on proposal: {str(e)}")

@router.post("/governance/proposal/finalize", response_model=TransactionResponse)
async def finalize_proposal(request: FinalizeProposalRequest):
    """
    Finalize a governance proposal.
    """
    try:
        wallet = local_wallets["owner"]
        result = governance_contract.finalize_proposal(
            proposal_id=request.proposal_id,
            wallet=wallet
        )
        return TransactionResponse(
            transaction_hash=result["transaction_hash"],
            proposal_id=request.proposal_id
        )
    except TransactionError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error finalizing proposal: {str(e)}")

@router.get("/governance/proposals", response_model=list[dict])
async def get_all_proposals():
    """
    Get all governance proposals from the blockchain.
    """
    try:
        proposals = governance_contract.get_proposals()
        return proposals
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error fetching proposals: {str(e)}")

@router.get("/governance/proposal/{proposal_id}", response_model=dict)
async def get_proposal(proposal_id: str):
    """
    Get a specific governance proposal by ID.
    """
    try:
        proposal = governance_contract.get_proposal(proposal_id)
        if not proposal:
            raise HTTPException(status_code=404, detail=f"Proposal {proposal_id} not found")
        return proposal
    except HTTPException as e:
        raise e
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error fetching proposal: {str(e)}")