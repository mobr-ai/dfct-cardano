from fastapi import APIRouter, HTTPException
from typing import Any
import logging

from dfctbackend.models import (
    TopicSubmitRequest, TopicReviewRequest, TopicActivateRequest,
    ContributionSubmitRequest, ContributionReviewRequest,
    TransactionResponse
)
from dfctbackend.cardano.contract import ProvenanceContract
from dfctbackend.cardano.transaction import TransactionError
from dfctbackend.cardano.wallet import get_wallet_by_name, local_wallets

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/v1")

# Create contract instance
provenance_contract = ProvenanceContract()

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
        # Use proposer wallet
        wallet = local_wallets["proposer"]

        # Submit the transaction
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
        raise HTTPException(
            status_code=400, 
            detail=str(e)
        )
    except Exception as e:
        raise HTTPException(
            status_code=500, 
            detail=f"Error submitting topic: {str(e)}"
        )

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
        # Use reviewer1 by default
        wallet = local_wallets["reviewer1"]
        
        # Submit the transaction
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
        raise HTTPException(
            status_code=400, 
            detail=str(e)
        )
    except Exception as e:
        raise HTTPException(
            status_code=500, 
            detail=f"Error reviewing topic: {str(e)}"
        )

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
        raise HTTPException(
            status_code=404,
            detail=f"Wallet '{wallet_name}' not found"
        )
    
    try:
        # Submit the transaction
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
        raise HTTPException(
            status_code=400, 
            detail=str(e)
        )
    except Exception as e:
        raise HTTPException(
            status_code=500, 
            detail=f"Error reviewing topic: {str(e)}"
        )

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
        # Use owner wallet for activation
        wallet = local_wallets["owner"]
        
        # Submit the transaction
        result = provenance_contract.activate_topic(
            wallet=wallet,
            topic_id=request.topic_id
        )
        
        return TransactionResponse(
            transaction_hash=result["transaction_hash"],
            topic_id=request.topic_id
        )
    
    except TransactionError as e:
        raise HTTPException(
            status_code=400, 
            detail=str(e)
        )
    except Exception as e:
        raise HTTPException(
            status_code=500, 
            detail=f"Error activating topic: {str(e)}"
        )

@router.get("/topics", response_model=list[dict])
async def get_all_topics():
    """
    Get all topics from the blockchain.
    
    Returns:
        List of topics with their details.
    """
    try:
        # Use contract to get topics
        topics = provenance_contract.get_topics()
        return topics
    
    except Exception as e:
        raise HTTPException(
            status_code=500,
            detail=f"Error fetching topics: {str(e)}"
        )

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
        # Use contract to get topic by ID
        topic = provenance_contract.get_topic(topic_id)
        if not topic:
            raise HTTPException(
                status_code=404,
                detail=f"Topic {topic_id} not found"
            )
        return topic
    
    except HTTPException as e:
        raise e
    except Exception as e:
        raise HTTPException(
            status_code=500,
            detail=f"Error fetching topic: {str(e)}"
        )

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
        # Use a default contributor wallet (can be modified to accept wallet param)
        wallet = local_wallets["proposer"]  # Using proposer as default contributor
        
        # Submit the transaction
        result = provenance_contract.submit_contribution(
            topic_id=request.topic_id,
            content=request.content,
            contributor=wallet
        )

        return TransactionResponse(
            transaction_hash=result["transaction_hash"],
            topic_id=request.topic_id
        )
    
    except TransactionError as e:
        raise HTTPException(
            status_code=400, 
            detail=str(e)
        )
    except Exception as e:
        raise HTTPException(
            status_code=500, 
            detail=f"Error submitting contribution: {str(e)}"
        )

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
        # Use reviewer1 by default
        wallet = local_wallets["reviewer1"]
        
        # Default values for new fields required by the smart contract
        relevance = 5  # Default midpoint score
        accuracy = 5   # Default midpoint score
        completeness = 5  # Default midpoint score
        review_content = request.comment or "Review completed"
        
        # Submit the transaction
        result = provenance_contract.review_contribution(
            contribution_id=request.contribution_id,
            reviewer=wallet,
            relevance=relevance,
            accuracy=accuracy,
            completeness=completeness,
            review_content=review_content
        )
        
        return TransactionResponse(
            transaction_hash=result["transaction_hash"]
        )
    
    except TransactionError as e:
        raise HTTPException(
            status_code=400, 
            detail=str(e)
        )
    except Exception as e:
        raise HTTPException(
            status_code=500, 
            detail=f"Error reviewing contribution: {str(e)}"
        )

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
        raise HTTPException(
            status_code=404,
            detail=f"Wallet '{wallet_name}' not found"
        )
    
    try:
        # Default values for new fields required by the smart contract
        relevance = 5  # Default midpoint score
        accuracy = 5   # Default midpoint score
        completeness = 5  # Default midpoint score
        review_content = request.comment or "Review completed"
        
        # Submit the transaction
        result = provenance_contract.review_contribution(
            contribution_id=request.contribution_id,
            reviewer=wallet,
            relevance=relevance,
            accuracy=accuracy,
            completeness=completeness,
            review_content=review_content
        )
        
        return TransactionResponse(
            transaction_hash=result["transaction_hash"]
        )
    
    except TransactionError as e:
        raise HTTPException(
            status_code=400, 
            detail=str(e)
        )
    except Exception as e:
        raise HTTPException(
            status_code=500, 
            detail=f"Error reviewing contribution: {str(e)}"
        )

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
        # Use contract to get topic contributions
        contributions = provenance_contract.get_contributions_for_topic(topic_id)
        return contributions
    
    except Exception as e:
        raise HTTPException(
            status_code=500,
            detail=f"Error fetching topic contributions: {str(e)}"
        )

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
        # Use contract to get contribution by ID
        contribution = provenance_contract.get_contribution(contribution_id)
        if not contribution:
            raise HTTPException(
                status_code=404,
                detail=f"Contribution {contribution_id} not found"
            )
        return contribution
    
    except HTTPException as e:
        raise e
    except Exception as e:
        raise HTTPException(
            status_code=500,
            detail=f"Error fetching contribution: {str(e)}"
        )

@router.post("/contribution/{contribution_id}/dispute", response_model=TransactionResponse)
async def dispute_contribution(contribution_id: str, dispute_reason: str):
    """
    Dispute a reviewed contribution.
    
    Args:
        contribution_id: ID of the contribution to dispute.
        dispute_reason: Reason for the dispute.
        
    Returns:
        TransactionResponse: Transaction response.
    """
    try:
        # Use proposer wallet as default contributor for disputes
        wallet = local_wallets["proposer"]
        
        # Submit the transaction
        result = provenance_contract.dispute_contribution(
            contribution_id=contribution_id,
            dispute_reason=dispute_reason,
            contributor=wallet
        )
        
        return TransactionResponse(
            transaction_hash=result["transaction_hash"]
        )
    
    except TransactionError as e:
        raise HTTPException(
            status_code=400, 
            detail=str(e)
        )
    except Exception as e:
        raise HTTPException(
            status_code=500, 
            detail=f"Error disputing contribution: {str(e)}"
        )

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
        # Only the owner can close topics
        wallet = local_wallets["owner"]
        
        # Submit the transaction
        result = provenance_contract.close_topic(
            topic_id=topic_id,
            wallet=wallet
        )
        
        return TransactionResponse(
            transaction_hash=result["transaction_hash"]
        )
    
    except TransactionError as e:
        raise HTTPException(
            status_code=400, 
            detail=str(e)
        )
    except Exception as e:
        raise HTTPException(
            status_code=500, 
            detail=f"Error closing topic: {str(e)}"
        )

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
        # Get the transaction status
        tx_status = provenance_contract.tx_handler.get_transaction_status(tx_hash)
        
        return {
            "status": tx_status["status"],
            "confirmed_block": tx_status.get("confirmed_block"),
            "confirmation_time": tx_status.get("confirmation_time"),
            "transaction_hash": tx_hash
        }
    
    except TransactionError as e:
        raise HTTPException(
            status_code=400, 
            detail=str(e)
        )
    except Exception as e:
        raise HTTPException(
            status_code=500, 
            detail=f"Error checking transaction status: {str(e)}"
        )

@router.get("/reviewers", response_model=list[dict])
async def get_all_reviewers():
    """
    Get all authorized reviewers from the blockchain.
    
    Returns:
        List of reviewers with their details.
    """
    try:
        # Use contract to get reviewers from the topics
        topics = provenance_contract.get_topics()
        all_reviewers = []
        
        # Extract unique reviewers from all topics
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
        raise HTTPException(
            status_code=500,
            detail=f"Error fetching reviewers: {str(e)}"
        )