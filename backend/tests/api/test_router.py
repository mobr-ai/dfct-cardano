import pytest
from fastapi.testclient import TestClient
from unittest.mock import patch, MagicMock

from pycardano import UTxO

from dfctbackend.main import app
from dfctbackend.cardano.contract import ProvenanceContract
from dfctbackend.cardano.wallet import CardanoWallet

client = TestClient(app)

@pytest.fixture
def mock_wallet():
    """Mock wallet fixture."""
    wallet = MagicMock(spec=CardanoWallet)
    wallet.name = "test_wallet"
    wallet.address = MagicMock()  # Use a MagicMock instead of string
    wallet.address_str = "addr_test1234567890"
    wallet.payment_key_hash = "abcdef1234567890"
    wallet.skey_path = "/mock/path/test_wallet.skey"
    wallet.to_dict.return_value = {
        "name": "test_wallet",
        "address": "addr_test1234567890",
        "payment_key_hash": "abcdef1234567890"
    }
    # Add local_wallets attribute to mock wallet
    wallet.local_wallets = {
        "reviewer1": MagicMock(spec=CardanoWallet, payment_key_hash="reviewer1hash"),
        "reviewer2": MagicMock(spec=CardanoWallet, payment_key_hash="reviewer2hash")
    }
    return wallet

@pytest.fixture
def mock_provenance_contract():
    """Mock provenance contract fixture."""
    contract = MagicMock()
    
    # Mock required methods with proper return values
    contract.submit_topic.return_value = {
        "transaction_hash": "mock_tx_hash",
        "topic_id": "mock_topic_id"
    }
    
    contract.review_topic.return_value = {
        "transaction_hash": "mock_tx_hash_review",
        "topic_id": "mock_topic_id"
    }
    
    contract.activate_topic.return_value = {
        "transaction_hash": "mock_tx_hash_activate",
        "topic_id": "mock_topic_id"
    }
    
    contract.submit_contribution.return_value = {
        "transaction_hash": "mock_tx_hash_contrib",
        "topic_id": "mock_topic_id",
        "contribution_id": "mock_contrib_id"
    }
    
    contract.review_contribution.return_value = {
        "transaction_hash": "mock_tx_hash_contrib_review",
        "contribution_id": "mock_contrib_id"
    }
    
    # Mock tx_handler
    contract.tx_handler = MagicMock()
    contract.tx_handler.get_transaction_status.return_value = {
        "status": "confirmed",
        "confirmed_block": 12345,
        "confirmation_time": "2023-01-01T12:00:00Z",  # String, not int
        "transaction_hash": "mock_tx_hash"
    }
    
    # Apply the patch
    with patch("dfctbackend.api.router.provenance_contract", contract):
        yield contract

@pytest.fixture
def mock_local_wallets(mock_wallet):
    """Mock local_wallets fixture."""
    with patch("dfctbackend.api.router.local_wallets", {
        "owner": mock_wallet,
        "proposer": mock_wallet,
        "reviewer1": mock_wallet,
        "reviewer2": mock_wallet
    }):
        yield

@pytest.fixture
def mock_get_wallet_by_name(mock_wallet):
    """Mock get_wallet_by_name function."""
    with patch("dfctbackend.api.router.get_wallet_by_name", return_value=mock_wallet):
        yield

def test_health_check():
    """Test health check endpoint."""
    response = client.get("/health")
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "healthy"
    assert "timestamp" in data
    # Don't try to manipulate the timestamp

def test_get_config():
    """Test config endpoint."""
    response = client.get("/config")
    assert response.status_code == 200
    data = response.json()
    assert "policy_id" in data
    assert "validator_address" in data
    assert "token_name" in data

def test_submit_topic(mock_local_wallets, mock_provenance_contract):
    """Test submit topic endpoint."""
    request_data = {
        "title": "Test Topic",
        "description": "This is a test topic",
        "reward_amount": 100
    }
    
    response = client.post("/api/v1/topic", json=request_data)
    assert response.status_code == 200
    data = response.json()
    assert data["transaction_hash"] == "mock_tx_hash"
    assert data["topic_id"] == "mock_topic_id"
    
    # Verify that submit_topic was called with correct arguments
    mock_provenance_contract.submit_topic.assert_called_once()
    call_args = mock_provenance_contract.submit_topic.call_args[1]
    assert "wallet" in call_args
    assert call_args["title"] == "Test Topic"
    assert call_args["description"] == "This is a test topic"
    assert call_args["reward_amount"] == 100

def test_review_topic(mock_local_wallets, mock_provenance_contract):
    """Test review topic endpoint."""
    request_data = {
        "topic_id": "mock_topic_id",
        "approved": True
    }
    
    response = client.post("/api/v1/topic/review", json=request_data)
    assert response.status_code == 200
    data = response.json()
    assert data["transaction_hash"] == "mock_tx_hash_review"
    assert data["topic_id"] == "mock_topic_id"
    
    # Verify that review_topic was called with correct arguments
    mock_provenance_contract.review_topic.assert_called_once()
    call_args = mock_provenance_contract.review_topic.call_args[1]
    assert "wallet" in call_args
    assert call_args["topic_id"] == "mock_topic_id"
    assert call_args["approved"] is True

def test_review_topic_with_wallet(mock_get_wallet_by_name, mock_provenance_contract):
    """Test review topic with specific wallet endpoint."""
    request_data = {
        "topic_id": "mock_topic_id",
        "approved": True
    }

    response = client.post("/api/v1/topic/reviewer2/review", json=request_data)
    assert response.status_code == 200
    data = response.json()
    assert data["transaction_hash"] == "mock_tx_hash_review"
    assert data["topic_id"] == "mock_topic_id"
    
    # Verify that review_topic was called with correct arguments
    mock_provenance_contract.review_topic.assert_called_once()
    call_args = mock_provenance_contract.review_topic.call_args[1]
    assert "wallet" in call_args
    assert call_args["topic_id"] == "mock_topic_id"
    assert call_args["approved"] is True

def test_review_topic_with_nonexistent_wallet(mock_provenance_contract):
    """Test review topic with nonexistent wallet."""
    request_data = {
        "topic_id": "mock_topic_id",
        "approved": True
    }
    
    with patch("dfctbackend.api.router.get_wallet_by_name", return_value=None):
        response = client.post("/api/v1/topic/nonexistent/review", json=request_data)
        assert response.status_code == 404
        data = response.json()
        assert "not found" in data["detail"].lower()

def test_transaction_status(mock_provenance_contract):
    """Test transaction status endpoint."""
    response = client.get("/api/v1/status/mock_tx_hash")
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "confirmed"
    assert data["confirmed_block"] == 12345
    assert data["confirmation_time"] == "2023-01-01T12:00:00Z"
    assert data["transaction_hash"] == "mock_tx_hash"
    
    mock_provenance_contract.tx_handler.get_transaction_status.assert_called_once_with("mock_tx_hash")

def test_transaction_error_handling(mock_local_wallets, mock_provenance_contract):
    """Test handling of transaction errors."""
    # Set up the mock to raise an exception
    mock_provenance_contract.submit_topic.side_effect = Exception("Test error")
    
    request_data = {
        "title": "Test Topic",
        "description": "This is a test topic",
        "reward_amount": 100
    }
    
    response = client.post("/api/v1/topic", json=request_data)
    assert response.status_code == 500
    data = response.json()
    assert "error" in data["detail"].lower()

def test_activate_topic(mock_local_wallets, mock_provenance_contract):
    """Test activate topic endpoint."""
    request_data = {
        "topic_id": "mock_topic_id"
    }
    
    response = client.post("/api/v1/topic/activate", json=request_data)
    assert response.status_code == 200
    data = response.json()
    assert data["transaction_hash"] == "mock_tx_hash_activate"
    assert data["topic_id"] == "mock_topic_id"
    
    # Verify that activate_topic was called with correct arguments
    mock_provenance_contract.activate_topic.assert_called_once()
    call_args = mock_provenance_contract.activate_topic.call_args[1]
    assert "wallet" in call_args
    assert call_args["topic_id"] == "mock_topic_id"