import pytest
from unittest.mock import MagicMock, patch
from pathlib import Path

from dfctbackend.cardano.contract import ProvenanceContract, TransactionError
from dfctbackend.cardano.wallet import CardanoWallet
from pycardano import Address, TransactionOutput, Value, MultiAsset, ScriptHash, AuxiliaryData, UTxO, PlutusData


@pytest.fixture
def mock_tx_handler():
    """Create a mock transaction handler."""
    tx_handler = MagicMock()
    
    # Mock find_token_utxos to return a valid UTXO
    mock_utxo = MagicMock(spec=UTxO)
    mock_utxo.output = MagicMock()
    mock_utxo.output.address = MagicMock(spec=Address)
    mock_utxo.output.amount = MagicMock()
    tx_handler.find_token_utxos.return_value = [mock_utxo]
    
    # Mock find_utxos_at_address to return a valid UTXO
    fee_utxo = MagicMock(spec=UTxO)
    fee_utxo.output = MagicMock()
    fee_utxo.output.address = MagicMock(spec=Address)
    fee_utxo.output.amount = MagicMock()
    tx_handler.find_utxos_at_address.return_value = [fee_utxo]
    
    # Mock build_transaction to return bytes
    tx_handler.build_transaction.return_value = b"signed_tx"
    
    # Mock submit_transaction to return a hash
    tx_handler.submit_transaction.return_value = "tx_hash_123"
    
    return tx_handler


@pytest.fixture
def mock_plutus_data():
    """Mock PlutusData to avoid instantiation issues."""
    with patch('dfctbackend.cardano.contract.create_plutus_data_from_json') as mock:
        # Configure the mock to return a MagicMock with PlutusData spec
        mock.return_value = MagicMock(spec=PlutusData)
        yield mock


@pytest.fixture
def contract(mock_tx_handler, mock_plutus_data):
    """Create a testable contract instance."""
    # Create the contract with patches to avoid external dependencies
    with patch('dfctbackend.cardano.contract.Address.from_primitive') as mock_from_primitive, \
         patch('dfctbackend.cardano.contract.MultiAsset') as mock_multi_asset, \
         patch('dfctbackend.cardano.contract.TransactionOutput') as mock_output, \
         patch('dfctbackend.cardano.contract.ScriptHash') as mock_script_hash, \
         patch('dfctbackend.cardano.contract.AuxiliaryData') as mock_aux_data:
        
        # Configure the mocks
        mock_from_primitive.return_value = MagicMock(spec=Address)
        mock_multi_asset.return_value = MagicMock(spec=MultiAsset)
        mock_output.return_value = MagicMock(spec=TransactionOutput)
        mock_script_hash.side_effect = lambda x: MagicMock(spec=ScriptHash)
        mock_aux_data.return_value = MagicMock(spec=AuxiliaryData)
        
        # Create the contract
        contract = ProvenanceContract()
        
        # Override internal attributes
        contract.tx_handler = mock_tx_handler
        contract.policy_id = "81126953b8144b242640e84b3d52f54364601d29bc6ebc14cf30db20"
        contract.validator_address = "addr_test1vqv7hkpkefcm50hf3eum205w7zpm522xg8v33dutj8ausjgwgd4x2"
        contract.token_name = "DFC"
        contract.token_name_hex = "444643"  # Hex for "DFC"
        
        # Override internal methods to avoid external dependencies
        contract._prepare_topic_datum = MagicMock(return_value=({"mock": "datum"}, "test-topic-123"))
        contract._prepare_submit_topic_redeemer = MagicMock(return_value={"mock": "redeemer"})
        contract._prepare_review_topic_redeemer = MagicMock(return_value={"mock": "review_redeemer"})
        
        yield contract


@pytest.fixture
def mock_wallet():
    """Create a mock wallet with necessary attributes."""
    wallet = MagicMock(spec=CardanoWallet)
    wallet.name = "proposer"
    wallet.address = MagicMock(spec=Address)
    wallet.address_str = "addr_test1vqv7hkpkefcm50hf3eum205w7zpm522xg8v33dutj8ausjgwgd4x2"
    wallet.payment_key_hash = "abcdef1234567890"
    wallet.skey_path = Path("/mock/assets/proposer.skey")
    
    # Set up two reviewer wallets
    reviewer1 = MagicMock(spec=CardanoWallet)
    reviewer1.payment_key_hash = "reviewer1hash"
    reviewer2 = MagicMock(spec=CardanoWallet)
    reviewer2.payment_key_hash = "reviewer2hash"
    
    wallet.local_wallets = {
        "reviewer1": reviewer1,
        "reviewer2": reviewer2
    }
    
    return wallet


def test_submit_topic(contract, mock_wallet):
    """Test submit_topic method."""
    # Call the method
    result = contract.submit_topic(
        wallet=mock_wallet,
        title="Test Topic",
        description="This is a test",
        reward_amount=100
    )
    
    # Check the result
    assert "transaction_hash" in result
    assert "topic_id" in result
    assert result["transaction_hash"] == "tx_hash_123"
    assert result["topic_id"] == "test-topic-123"
    
    # Verify methods were called
    assert contract._prepare_topic_datum.called
    assert contract._prepare_submit_topic_redeemer.called
    assert contract.tx_handler.find_token_utxos.called
    assert contract.tx_handler.find_utxos_at_address.called
    assert contract.tx_handler.build_transaction.called
    assert contract.tx_handler.submit_transaction.called


def test_submit_topic_no_wallet_address(contract):
    """Test submit_topic with no wallet address."""
    wallet = MagicMock(spec=CardanoWallet)
    wallet.name = "proposer"
    wallet.address = None
    
    with pytest.raises(TransactionError) as exc_info:
        contract.submit_topic(
            wallet=wallet,
            title="Test Topic",
            description="This is a test",
            reward_amount=100
        )
    
    # Check that the error message is about the wallet address
    assert "no address" in str(exc_info.value).lower()
    assert wallet.name in str(exc_info.value)


def test_submit_topic_no_payment_key_hash(contract):
    """Test submit_topic with no payment key hash."""
    wallet = MagicMock(spec=CardanoWallet)
    wallet.name = "proposer"
    wallet.address = MagicMock(spec=Address)
    wallet.payment_key_hash = None
    
    with pytest.raises(TransactionError) as exc_info:
        contract.submit_topic(
            wallet=wallet,
            title="Test Topic",
            description="This is a test",
            reward_amount=100
        )
    
    # Check that the error message is about the payment key hash
    assert "no payment key hash" in str(exc_info.value).lower()
    assert wallet.name in str(exc_info.value)


def test_submit_topic_no_reviewers(contract):
    """Test submit_topic with no reviewers."""
    wallet = MagicMock(spec=CardanoWallet)
    wallet.name = "proposer"
    wallet.address = MagicMock(spec=Address)
    wallet.payment_key_hash = "abcdef1234567890"
    wallet.local_wallets = {}
    
    with pytest.raises(TransactionError) as exc_info:
        contract.submit_topic(
            wallet=wallet,
            title="Test Topic",
            description="This is a test",
            reward_amount=100
        )
    
    # Check that the error message is about no reviewers
    assert "no reviewer wallets found" in str(exc_info.value).lower()


def test_submit_topic_no_token_utxos(contract, mock_wallet):
    """Test submit_topic with no token UTXOs."""
    # Override the mock to return empty list
    contract.tx_handler.find_token_utxos.return_value = []
    
    with pytest.raises(TransactionError) as exc_info:
        contract.submit_topic(
            wallet=mock_wallet,
            title="Test Topic",
            description="This is a test",
            reward_amount=100
        )
    
    # Check that the error message is about token UTXOs
    assert "no utxos with sufficient" in str(exc_info.value).lower()
    assert contract.token_name in str(exc_info.value)
    assert mock_wallet.name in str(exc_info.value)


def test_submit_topic_no_fee_utxos(contract, mock_wallet):
    """Test submit_topic with no fee UTXOs."""
    # Override the mock to return empty list for fee UTXOs
    contract.tx_handler.find_utxos_at_address.return_value = []
    
    with pytest.raises(TransactionError) as exc_info:
        contract.submit_topic(
            wallet=mock_wallet,
            title="Test Topic",
            description="This is a test",
            reward_amount=100
        )
    
    # Check that the error message is about fee UTXOs
    assert "no utxos with sufficient ada" in str(exc_info.value).lower()
    assert mock_wallet.name in str(exc_info.value)


def test_review_topic(contract, mock_wallet):
    """Test review_topic method."""
    # Call the method
    result = contract.review_topic(
        wallet=mock_wallet,
        topic_id="test-topic-123",
        approved=True
    )
    
    # Check the result
    assert "transaction_hash" in result
    assert "topic_id" in result
    assert result["transaction_hash"] == "tx_hash_123"
    assert result["topic_id"] == "test-topic-123"
    
    # Verify methods were called
    assert contract._prepare_review_topic_redeemer.called
    assert contract.tx_handler.find_utxos_at_address.called
    assert contract.tx_handler.build_transaction.called
    assert contract.tx_handler.submit_transaction.called