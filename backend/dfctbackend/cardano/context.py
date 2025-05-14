from pycardano import OgmiosChainContext
from dfctbackend.config import settings

def get_chain_context():
    """
    Create a chain context for interacting with the Cardano node via Ogmios.
    """
    try:
        context = OgmiosChainContext(
            host="localhost",
            port=1337,
            network="preview"
        )
        return context

    except Exception as e:
        raise ValueError(f"Failed to create chain context: {str(e)}. Ensure Ogmios is running.")