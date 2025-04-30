import pytest
from pydantic import ValidationError

from src.models import (
    TopicSubmitRequest, TopicReviewRequest, TopicActivateRequest,
    ContributionSubmitRequest, ContributionReviewRequest,
    TransactionResponse, ErrorResponse, WalletInfo
)

def test_topic_submit_request():
    """Test TopicSubmitRequest model."""
    # Valid request
    valid_data = {
        "title": "Test Topic",
        "description": "This is a test topic description.",
        "reward_amount": 100
    }
    request = TopicSubmitRequest(**valid_data)
    assert request.title == "Test Topic"
    assert request.description == "This is a test topic description."
    assert request.reward_amount == 100
    
    # Invalid request - missing title
    invalid_data = {
        "description": "This is a test topic description.",
        "reward_amount": 100
    }
    with pytest.raises(ValidationError):
        TopicSubmitRequest(**invalid_data)
    
    # Invalid request - negative reward amount
    invalid_data = {
        "title": "Test Topic",
        "description": "This is a test topic description.",
        "reward_amount": -10
    }
    with pytest.raises(ValidationError):
        TopicSubmitRequest(**invalid_data)

def test_topic_review_request():
    """Test TopicReviewRequest model."""
    # Valid request
    valid_data = {
        "topic_id": "topic-1234567890",
        "approved": True
    }
    request = TopicReviewRequest(**valid_data)
    assert request.topic_id == "topic-1234567890"
    assert request.approved is True
    
    # Invalid request - missing topic_id
    invalid_data = {
        "approved": True
    }
    with pytest.raises(ValidationError):
        TopicReviewRequest(**invalid_data)

def test_topic_activate_request():
    """Test TopicActivateRequest model."""
    # Valid request
    valid_data = {
        "topic_id": "topic-1234567890"
    }
    request = TopicActivateRequest(**valid_data)
    assert request.topic_id == "topic-1234567890"
    
    # Invalid request - missing topic_id
    invalid_data = {}
    with pytest.raises(ValidationError):
        TopicActivateRequest(**invalid_data)

def test_contribution_submit_request():
    """Test ContributionSubmitRequest model."""
    # Valid request
    valid_data = {
        "topic_id": "topic-1234567890",
        "content": "This is my contribution.",
        "evidence_links": ["https://example.com/evidence1", "https://example.com/evidence2"]
    }
    request = ContributionSubmitRequest(**valid_data)
    assert request.topic_id == "topic-1234567890"
    assert request.content == "This is my contribution."
    assert len(request.evidence_links) == 2
    
    # Invalid request - missing topic_id
    invalid_data = {
        "content": "This is my contribution.",
        "evidence_links": ["https://example.com/evidence1"]
    }
    with pytest.raises(ValidationError):
        ContributionSubmitRequest(**invalid_data)
    
    # Valid request - empty evidence_links
    valid_data = {
        "topic_id": "topic-1234567890",
        "content": "This is my contribution.",
        "evidence_links": []
    }
    request = ContributionSubmitRequest(**valid_data)
    assert request.evidence_links == []

def test_contribution_review_request():
    """Test ContributionReviewRequest model."""
    # Valid request
    valid_data = {
        "contribution_id": "contrib-1234567890",
        "approved": False,
        "comment": "Rejected due to insufficient evidence."
    }
    request = ContributionReviewRequest(**valid_data)
    assert request.contribution_id == "contrib-1234567890"
    assert request.approved is False
    assert request.comment == "Rejected due to insufficient evidence."
    
    # Invalid request - missing contribution_id
    invalid_data = {
        "approved": True
    }
    with pytest.raises(ValidationError):
        ContributionReviewRequest(**invalid_data)
    
    # Valid request - optional comment
    valid_data = {
        "contribution_id": "contrib-1234567890",
        "approved": True
    }
    request = ContributionReviewRequest(**valid_data)
    assert request.contribution_id == "contrib-1234567890"
    assert request.approved is True
    assert request.comment is None

def test_transaction_response():
    """Test TransactionResponse model."""
    # Valid response
    valid_data = {
        "transaction_hash": "abcdef1234567890",
        "topic_id": "topic-1234567890"
    }
    response = TransactionResponse(**valid_data)
    assert response.transaction_hash == "abcdef1234567890"
    assert response.topic_id == "topic-1234567890"
    
    # Valid response - optional topic_id
    valid_data = {
        "transaction_hash": "abcdef1234567890"
    }
    response = TransactionResponse(**valid_data)
    assert response.transaction_hash == "abcdef1234567890"
    assert response.topic_id is None
    
    # Invalid response - missing transaction_hash
    invalid_data = {
        "topic_id": "topic-1234567890"
    }
    with pytest.raises(ValidationError):
        TransactionResponse(**invalid_data)

def test_error_response():
    """Test ErrorResponse model."""
    # Valid response
    valid_data = {
        "status": "error",
        "message": "Something went wrong."
    }
    response = ErrorResponse(**valid_data)
    assert response.status == "error"
    assert response.message == "Something went wrong."
    
    # Invalid response - missing message
    invalid_data = {
        "status": "error"
    }
    with pytest.raises(ValidationError):
        ErrorResponse(**invalid_data)

def test_wallet_info():
    """Test WalletInfo model."""
    # Valid wallet info
    valid_data = {
        "name": "proposer",
        "address": "addr_test1qp0al5vhh9572qrh3h3fs3j3sev8mspx3jgyfzyw8vd7wr47fwgjlneh7pgj928xypgs9q3fwg40dua05tsjp7jwk4rqfqnelk",
        "payment_key_hash": "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
    }
    wallet_info = WalletInfo(**valid_data)
    assert wallet_info.name == "proposer"
    assert wallet_info.address == "addr_test1qp0al5vhh9572qrh3h3fs3j3sev8mspx3jgyfzyw8vd7wr47fwgjlneh7pgj928xypgs9q3fwg40dua05tsjp7jwk4rqfqnelk"
    assert wallet_info.payment_key_hash == "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
    
    # Invalid wallet info - missing name
    invalid_data = {
        "address": "addr_test1qp0al5vhh9572qrh3h3fs3j3sev8mspx3jgyfzyw8vd7wr47fwgjlneh7pgj928xypgs9q3fwg40dua05tsjp7jwk4rqfqnelk",
        "payment_key_hash": "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
    }
    with pytest.raises(ValidationError):
        WalletInfo(**invalid_data)