import pytest
from unittest.mock import patch, MagicMock

from src.cardano.context import get_chain_context
from pycardano import OgmiosChainContext

@pytest.fixture
def mock_settings():
    """Mock settings for testing."""
    with patch("src.cardano.context.settings") as mock_settings:
        mock_settings.CARDANO_NETWORK = "preview"
        mock_settings.CARDANO_NODE_HOST = "localhost"
        mock_settings.CARDANO_NODE_PORT = 3001
        mock_settings.OGMIOS_PORT = 1337
        yield mock_settings

def test_get_chain_context(mock_settings):
    """Test get_chain_context with preview."""
    with patch("src.cardano.context.OgmiosChainContext") as mock_context_cls:
        mock_context = MagicMock(spec=OgmiosChainContext)
        mock_context_cls.return_value = mock_context
        
        # Call the function 
        context = get_chain_context()
        
        # Check context is created
        mock_context_cls.assert_called_once()
        
        # Check result matches expected mock
        assert context is mock_context

def test_get_chain_context_error(mock_settings):
    """Test get_chain_context with error."""
    error_message = "Connection failed"
    with patch("src.cardano.context.OgmiosChainContext", side_effect=Exception(error_message)):
        with pytest.raises(ValueError) as excinfo:
            get_chain_context()
        
        assert "Failed to create chain context" in str(excinfo.value)
        assert error_message in str(excinfo.value)