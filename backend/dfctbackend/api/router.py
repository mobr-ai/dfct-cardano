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
from dfctbackend.cardano.wallet import local_wallets, CardanoWallet

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
        proposer = CardanoWallet.create_wallet(
            request.proposer_wallet_info.name,
            request.proposer_wallet_info.pub_key_hash,
            request.proposer_wallet_info.address,
            request.proposer_wallet_info.skey
        )
        result = provenance_contract.submit_topic(
            proposer=proposer,
            topic_id=request.topic_id,
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
        reviewer = CardanoWallet.create_wallet(
            request.reviewer_wallet_info.name,
            request.reviewer_wallet_info.pub_key_hash,
            request.reviewer_wallet_info.address,
            request.reviewer_wallet_info.skey
        )
        result = provenance_contract.review_topic(
            wallet=reviewer,
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
        request: Topic activation request. Currently only the owner can activate a topic

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
        wallet = CardanoWallet.create_wallet(
            request.contributor_wallet_info.name,
            request.contributor_wallet_info.pub_key_hash,
            request.contributor_wallet_info.address,
            request.contributor_wallet_info.skey
        )

        result = provenance_contract.submit_contribution(
            topic_id=request.topic_id,
            contribution_id=request.contribution_id,
            lovelace_amount=request.lovelace_amount,
            contribution_type=request.contribution_type,
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
        wallet = CardanoWallet.create_wallet(
            request.reviewer_wallet_info.name,
            request.reviewer_wallet_info.pub_key_hash,
            request.reviewer_wallet_info.address,
            request.reviewer_wallet_info.skey
        )
        result = provenance_contract.review_contribution(
            contribution_id=request.contribution_id,
            reviewer=wallet,
            relevance=request.relevance,
            accuracy=request.accuracy,
            completeness=request.completeness,
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
        wallet = CardanoWallet.create_wallet(
            request.proposer_wallet_info.name,
            request.proposer_wallet_info.pub_key_hash,
            request.proposer_wallet_info.address,
            request.proposer_wallet_info.skey
        )
        result = provenance_contract.dispute_contribution(
            contribution_id=contribution_id,
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
        topic_id: ID of the topic to close. Currently only the owner can close a topic
        
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
        wallet = CardanoWallet.create_wallet(
            request.proposer_wallet_info.name,
            request.proposer_wallet_info.pub_key_hash,
            request.proposer_wallet_info.address,
            request.proposer_wallet_info.skey
        )
        result = governance_contract.submit_proposal(
            proposer=wallet,
            proposal_id=request.proposal_id,
            lovelace_amount=request.lovelace_amount
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
        wallet = CardanoWallet.create_wallet(
            request.voter_wallet_info.name,
            request.voter_wallet_info.pub_key_hash,
            request.voter_wallet_info.address,
            request.voter_wallet_info.skey
        )
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
    Finalize a governance proposal. Currently only the owner can finalize a proposal.
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