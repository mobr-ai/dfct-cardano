from pycardano import Network, OgmiosChainContext
from dfctbackend.config import settings

def get_chain_context():
    """
    Create a chain context for interacting with the Cardano node via Ogmios running in Docker.
    
    Returns:
        OgmiosChainContext: The chain context.
    """
    
    try:
        
        # Initialize OgmiosChainContext
        context = OgmiosChainContext()
        return context

    except Exception as e:
        raise ValueError(f"Failed to create chain context: {str(e)}. Ensure Ogmios is running and the Cardano node socket is accessible.")