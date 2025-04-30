import pytest
from unittest.mock import patch, mock_open, MagicMock
from pathlib import Path

from src.cardano.wallet import CardanoWallet, get_wallet_by_name, local_wallets
from pycardano import Address

@pytest.fixture
def mock_address_file():
    """Mock address file content."""
    return "addr_test1qp0al5vhh9572qrh3h3fs3j3sev8mspx3jgyfzyw8vd7wr47fwgjlneh7pgj928xypgs9q3fwg40dua05tsjp7jwk4rqfqnelk"

@pytest.fixture
def mock_pkh_file():
    """Mock payment key hash file content."""
    return "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

@pytest.fixture
def mock_settings():
    """Mock settings for testing."""
    with patch("src.cardano.wallet.settings") as mock_settings:
        mock_settings.ASSETS_DIR = Path("/mock/assets")
        mock_settings.CARDANO_NETWORK = "testnet"
        yield mock_settings

def test_cardano_wallet_initialization(mock_settings, mock_address_file, mock_pkh_file):
    """Test CardanoWallet initialization."""
    with patch("builtins.open") as mock_open_func, \
         patch("pathlib.Path.exists", return_value=True), \
         patch("pycardano.Address.from_primitive", return_value=MagicMock(spec=Address)):
        
        # Create multiple mock_open instances for different file reads
        mock_open_func.side_effect = [
            mock_open(read_data=mock_address_file).return_value,
            mock_open(read_data=mock_pkh_file).return_value
        ]
        
        # Patch Path.__truediv__ to control the result of Path operations
        with patch.object(Path, "__truediv__", return_value=Path("/mock/assets/test_wallet.addr")):
            wallet = CardanoWallet("test_wallet")
        
            # Check that data was loaded
            assert wallet.payment_key_hash == mock_pkh_file
            
            # Check mock was used correctly
            mock_open_func.assert_called()

def test_wallet_to_dict(mock_settings, mock_address_file, mock_pkh_file):
    """Test wallet to_dict method."""
    with patch("builtins.open") as mock_open_func, \
         patch("pathlib.Path.exists", return_value=True), \
         patch("pycardano.Address.from_primitive") as mock_from_primitive:
        
        # Create multiple mock_open instances for different file reads
        mock_open_func.side_effect = [
            mock_open(read_data=mock_address_file).return_value,
            mock_open(read_data=mock_pkh_file).return_value
        ]
        
        mock_address = MagicMock(spec=Address)
        mock_address.__str__.return_value = mock_address_file
        mock_from_primitive.return_value = mock_address
        
        wallet = CardanoWallet("test_wallet")
        wallet._address = mock_address  # Set address directly for the test
        wallet_dict = wallet.to_dict()
        
        assert wallet_dict["name"] == "test_wallet"
        assert wallet_dict["address"] == mock_address_file
        assert wallet_dict["payment_key_hash"] == mock_pkh_file

def test_get_wallet_by_name():
    """Test get_wallet_by_name function."""
    # Mock local_wallets
    mock_wallet = MagicMock(spec=CardanoWallet)
    mock_wallet.name = "test_wallet"
    
    # Create local instance for the test
    with patch.dict("src.cardano.wallet.local_wallets", {"test_wallet": mock_wallet}, clear=True):
        # Test getting existing wallet
        result = get_wallet_by_name("test_wallet")
        assert result is mock_wallet
        
        # Test getting non-existent wallet
        result = get_wallet_by_name("nonexistent_wallet")
        assert result is None

def test_local_wallets_initialization():
    """Test that local_wallets is initialized with expected wallets."""
    expected_wallets = ["owner", "proposer", "reviewer1", "reviewer2"]
    
    # Since we can't easily reload the module and reset the local_wallets variable,
    # we'll just check that the local_wallets dictionary has the expected keys
    for wallet_name in expected_wallets:
        assert wallet_name in local_wallets