import pytest
from unittest.mock import patch, MagicMock
from pathlib import Path

from src.cardano.wallet import CardanoWallet
from pycardano import Address, Network

@pytest.fixture
def mock_settings():
    """Global mock settings fixture."""
    with patch("src.config.settings") as mock_settings:
        mock_settings.ASSETS_DIR = Path("/mock/assets")
        mock_settings.CARDANO_NETWORK = "preview"
        mock_settings.CARDANO_NODE_HOST = "localhost"
        mock_settings.CARDANO_NODE_PORT = 3001
        mock_settings.OGMIOS_PORT = 1337
        mock_settings.TESTNET_MAGIC = 2
        mock_settings.POLICY_ID = "81126953b8144b242640e84b3d52f54364601d29bc6ebc14cf30db20"
        mock_settings.VALIDATOR_ADDRESS = "addr_test_validator"
        mock_settings.TOKEN_NAME = "DFC"
        yield mock_settings

@pytest.fixture
def mock_wallet():
    """Mock wallet fixture."""
    wallet = MagicMock(spec=CardanoWallet)
    wallet.name = "test_wallet"
    wallet.address = MagicMock(spec=Address)
    wallet.address_str = "addr_test1234567890"
    wallet.payment_key_hash = "abcdef1234567890"
    wallet.skey_path = Path("/mock/assets/test_wallet.skey")
    wallet.to_dict.return_value = {
        "name": "test_wallet",
        "address": "addr_test1234567890",
        "payment_key_hash": "abcdef1234567890"
    }
    # Add local_wallets attribute
    wallet.local_wallets = {
        "reviewer1": MagicMock(spec=CardanoWallet, payment_key_hash="reviewer1hash"),
        "reviewer2": MagicMock(spec=CardanoWallet, payment_key_hash="reviewer2hash")
    }
    return wallet

@pytest.fixture
def mock_local_wallets(mock_wallet):
    """Mock local_wallets fixture."""
    wallets = {
        "owner": mock_wallet,
        "proposer": mock_wallet,
        "reviewer1": mock_wallet,
        "reviewer2": mock_wallet
    }
    
    with patch("src.cardano.wallet.local_wallets", wallets):
        yield wallets

@pytest.fixture
def mock_get_wallet_by_name(mock_wallet):
    """Mock get_wallet_by_name function."""
    def _get_wallet(name):
        if name.lower() in ["owner", "proposer", "reviewer1", "reviewer2", "test_wallet"]:
            return mock_wallet
        return None

    with patch("src.cardano.wallet.get_wallet_by_name", _get_wallet):
        yield _get_wallet