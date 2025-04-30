import pytest
from unittest.mock import MagicMock, patch, mock_open
from pathlib import Path

from pycardano import Address, TransactionBuilder, TransactionOutput, UTxO, ScriptHash, PaymentSigningKey

from dfctbackend.cardano.transaction import CardanoTransactions, TransactionError


@pytest.fixture
def mock_chain_context():
    """Create a mock chain context."""
    context = MagicMock()
    
    # Mock UTXOs with proper output structure
    utxo1 = MagicMock(spec=UTxO)
    utxo1.output = MagicMock()
    utxo1.output.amount = MagicMock()
    utxo1.output.amount.coin = 10000000  # 10 ADA
    utxo1.output.amount.multi_asset = {}
    utxo1.output.address = MagicMock(spec=Address)
    
    utxo2 = MagicMock(spec=UTxO)
    utxo2.output = MagicMock()
    utxo2.output.amount = MagicMock()
    utxo2.output.amount.coin = 2000000  # 2 ADA
    utxo2.output.amount.multi_asset = {
        ScriptHash(bytes.fromhex("81126953b8144b242640e84b3d52f54364601d29bc6ebc14cf30db20")): {
            bytes.fromhex("444643"): 10000  # DFC token
        }
    }
    utxo2.output.address = MagicMock(spec=Address)
    
    context.utxos.return_value = [utxo1, utxo2]
    context.submit_tx.return_value = "tx_hash_123"
    context.query_transaction_info.return_value = {
        "block_height": 1000,
        "block_time": 1234567890
    }
    
    return context


@pytest.fixture
def tx_handler():
    """Create a transaction handler with mocked dependencies."""
    with patch('dfctbackend.cardano.transaction.get_chain_context') as mock_get_context, \
         patch('dfctbackend.cardano.transaction.load_plutus_script') as mock_load_script, \
         patch('dfctbackend.cardano.transaction.settings') as mock_settings, \
         patch('dfctbackend.cardano.transaction.Address.from_primitive') as mock_address_from_primitive, \
         patch('pathlib.Path.exists') as mock_exists:
        
        # Set up the mocks
        mock_chain_context_instance = MagicMock()
        mock_get_context.return_value = mock_chain_context_instance
        
        mock_utxo1 = MagicMock(spec=UTxO)
        mock_utxo1.output = MagicMock()
        mock_utxo1.output.amount = MagicMock()
        mock_utxo1.output.amount.coin = 10000000  # 10 ADA
        mock_utxo1.output.amount.multi_asset = {}
        mock_utxo1.output.address = MagicMock(spec=Address)
        
        mock_utxo2 = MagicMock(spec=UTxO)
        mock_utxo2.output = MagicMock()
        mock_utxo2.output.amount = MagicMock()
        mock_utxo2.output.amount.coin = 2000000  # 2 ADA
        mock_utxo2.output.amount.multi_asset = {
            MagicMock(spec=ScriptHash): {
                bytes.fromhex("444643"): 10000  # DFC token
            }
        }
        mock_utxo2.output.address = MagicMock(spec=Address)
        
        mock_chain_context_instance.utxos.return_value = [mock_utxo1, mock_utxo2]
        mock_chain_context_instance.submit_tx.return_value = "tx_hash_123"
        mock_chain_context_instance.query_transaction_info.return_value = {
            "block_height": 1000,
            "block_time": 1234567890
        }
        
        # Configure settings mock
        mock_settings.ASSETS_DIR = Path("/mock/assets")
        mock_settings.VALIDATOR_ADDRESS = "addr_test1vqv7hkpkefcm50hf3eum205w7zpm522xg8v33dutj8ausjgwgd4x2"
        
        # Configure Address.from_primitive mock
        mock_address = MagicMock(spec=Address)
        mock_address.__str__.return_value = "addr_test1vqv7hkpkefcm50hf3eum205w7zpm522xg8v33dutj8ausjgwgd4x2"
        mock_address_from_primitive.return_value = mock_address
        
        # Ensure path.exists returns True for the script files
        mock_exists.return_value = True
        
        # Configure the load_plutus_script mock
        mock_load_script.return_value = MagicMock()
        
        # Create the transaction handler
        tx_handler = CardanoTransactions()
        
        # Make sure context is set to our mock
        tx_handler.context = mock_chain_context_instance
        
        yield tx_handler


def test_get_script_address(tx_handler):
    """Test getting script address."""
    address = tx_handler.get_script_address()
    
    # Verify the address
    assert isinstance(address, Address)
    assert str(address) == "addr_test1vqv7hkpkefcm50hf3eum205w7zpm522xg8v33dutj8ausjgwgd4x2"


def test_find_utxos_at_address(tx_handler):
    """Test finding UTXOs at address."""
    # Test with string address
    utxos = tx_handler.find_utxos_at_address("addr_test1vqv7hkpkefcm50hf3eum205w7zpm522xg8v33dutj8ausjgwgd4x2")
    assert len(utxos) == 2
    
    # Test with min_lovelace filter
    utxos = tx_handler.find_utxos_at_address("addr_test1vqv7hkpkefcm50hf3eum205w7zpm522xg8v33dutj8ausjgwgd4x2", min_lovelace=5000000)
    assert len(utxos) == 1
    assert utxos[0].output.amount.coin == 10000000


def test_find_token_utxos(tx_handler):
    """Test finding token UTXOs."""
    # Manually configure the token finder to return mock UTXOs
    mock_utxo = MagicMock(spec=UTxO)
    mock_utxo.output = MagicMock()
    mock_utxo.output.address = MagicMock(spec=Address)
    mock_utxo.output.amount = MagicMock()
    
    # For less than available tokens, return 1 UTXO
    tx_handler.find_token_utxos = MagicMock()
    tx_handler.find_token_utxos.side_effect = lambda address, policy_id, token_name, amount: [mock_utxo] if amount <= 5000 else []
    
    # Test with tokens amount below threshold (should return 1 UTXO)
    utxos = tx_handler.find_token_utxos(
        address="addr_test1vqv7hkpkefcm50hf3eum205w7zpm522xg8v33dutj8ausjgwgd4x2",
        policy_id="81126953b8144b242640e84b3d52f54364601d29bc6ebc14cf30db20",
        token_name="DFC",
        amount=5000
    )
    assert len(utxos) == 1
    
    # Test with token amount above threshold (should return empty list)
    utxos = tx_handler.find_token_utxos(
        address="addr_test1vqv7hkpkefcm50hf3eum205w7zpm522xg8v33dutj8ausjgwgd4x2",
        policy_id="81126953b8144b242640e84b3d52f54364601d29bc6ebc14cf30db20",
        token_name="DFC",
        amount=20000
    )
    assert len(utxos) == 0


def test_build_transaction(tx_handler):
    """Test building a transaction."""
    # Mock inputs and outputs
    mock_input = MagicMock(spec=UTxO)
    mock_input.output = MagicMock()
    mock_input.output.address = MagicMock(spec=Address)
    mock_output = MagicMock(spec=TransactionOutput)
    
    # Mock the file opening and TransactionBuilder
    mock_signing_key = MagicMock(spec=PaymentSigningKey)
    mock_tx_builder = MagicMock(spec=TransactionBuilder)
    mock_tx_builder.build_and_sign.return_value.transaction_body = b"signed_tx_body"
    
    with patch('builtins.open', mock_open(read_data='{"type": "PaymentSigningKeyShelley_ed25519"}')), \
         patch('dfctbackend.cardano.transaction.PaymentSigningKey.from_json', return_value=mock_signing_key), \
         patch('dfctbackend.cardano.transaction.TransactionBuilder', return_value=mock_tx_builder):
        
        # Call the method
        result = tx_handler.build_transaction(
            inputs=[mock_input],
            outputs=[mock_output],
            signing_keys=["/mock/key.skey"]
        )
        
        # Verify the result
        assert result == b"signed_tx_body"
        
        # Verify method calls
        mock_tx_builder.add_input.assert_called_with(mock_input)
        mock_tx_builder.add_output.assert_called_with(mock_output)
        mock_tx_builder.build_and_sign.assert_called_once()


def test_build_transaction_key_error(tx_handler):
    """Test build_transaction with key loading error."""
    # Mock inputs and outputs
    mock_input = MagicMock(spec=UTxO)
    mock_input.output = MagicMock()
    mock_input.output.address = MagicMock(spec=Address)
    mock_output = MagicMock(spec=TransactionOutput)
    
    # Mock file open to raise an exception
    with patch('builtins.open', side_effect=FileNotFoundError("File not found")):
        
        # Call should raise TransactionError
        with pytest.raises(TransactionError) as exc_info:
            tx_handler.build_transaction(
                inputs=[mock_input],
                outputs=[mock_output],
                signing_keys=["/mock/key.skey"]
            )
        
        # Check error message
        assert "failed to load signing key" in str(exc_info.value).lower()
        assert "/mock/key.skey" in str(exc_info.value)


def test_submit_transaction(tx_handler):
    """Test submitting a transaction."""
    # Call the method
    tx_hash = tx_handler.submit_transaction(b"signed_tx")
    
    # Verify the result
    assert tx_hash == "tx_hash_123"
    
    # Verify the method call
    tx_handler.context.submit_tx.assert_called_once_with(b"signed_tx")


def test_submit_transaction_error(tx_handler):
    """Test submit_transaction with error."""
    # Mock submit_tx to raise an exception
    tx_handler.context.submit_tx.side_effect = Exception("Network error")
    
    # Call should raise TransactionError
    with pytest.raises(TransactionError) as exc_info:
        tx_handler.submit_transaction(b"signed_tx")
    
    # Check error message
    assert "failed to submit transaction" in str(exc_info.value).lower()


def test_get_transaction_status(tx_handler):
    """Test getting transaction status."""
    # Call the method
    status = tx_handler.get_transaction_status("tx_hash_123")
    
    # Verify the result
    assert status["status"] == "confirmed"
    assert status["confirmed_block"] == 1000
    assert status["confirmation_time"] == 1234567890
    assert status["transaction_hash"] == "tx_hash_123"
    
    # Verify the method call
    tx_handler.context.query_transaction_info.assert_called_once_with("tx_hash_123")


def test_get_transaction_status_pending(tx_handler):
    """Test getting status for pending transaction."""
    # Mock query_transaction_info to return None (transaction not found yet)
    tx_handler.context.query_transaction_info.return_value = None
    
    # Call the method
    status = tx_handler.get_transaction_status("pending_tx")
    
    # Verify the result
    assert status["status"] == "pending"
    assert "confirmed_block" not in status
    assert "confirmation_time" not in status
    assert status["transaction_hash"] == "pending_tx"


def test_get_transaction_status_error(tx_handler):
    """Test getting status with error."""
    # Mock query_transaction_info to raise an exception
    tx_handler.context.query_transaction_info.side_effect = Exception("Network error")
    
    # Call the method
    status = tx_handler.get_transaction_status("error_tx")
    
    # Verify the result
    assert status["status"] == "failed"
    assert "error" in status
    assert "Network error" in status["error"]
    assert status["transaction_hash"] == "error_tx"