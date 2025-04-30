from fastapi import APIRouter, HTTPException
from typing import Any

from dfctbackend.models import (
    TopicSubmitRequest, TopicReviewRequest, TopicActivateRequest,
    ContributionSubmitRequest, ContributionReviewRequest,
    TransactionResponse
)
from dfctbackend.cardano.contract import ProvenanceContract
from dfctbackend.cardano.transaction import TransactionError
from dfctbackend.cardano.wallet import get_wallet_by_name, local_wallets


import logging

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
            wallet=wallet,
            title=request.title,
            description=request.description,
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
            wallet=wallet,
            topic_id=request.topic_id,
            content=request.content,
            evidence_links=request.evidence_links
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
        
        # Submit the transaction
        result = provenance_contract.review_contribution(
            wallet=wallet,
            contribution_id=request.contribution_id,
            approved=request.approved,
            comment=request.comment
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