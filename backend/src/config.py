from pydantic_settings import BaseSettings, SettingsConfigDict
from pathlib import Path
from typing import Optional
from binascii import Error as HexError

class Settings(BaseSettings):
    """Application settings loaded from environment variables with fallbacks."""
    
    # Pydantic V2 config using SettingsConfigDict
    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        extra="ignore"
    )
    
    # Base paths
    ASSETS_DIR: Path = Path("assets")
    
    # Cardano node configuration
    CARDANO_NETWORK: str = "preview"
    CARDANO_NODE_HOST: str = "localhost"
    CARDANO_NODE_PORT: int = 3001
    OGMIOS_PORT: int = 1337
    
    # Magic number for testnet
    TESTNET_MAGIC: int = 2
    
    # Contract information
    POLICY_ID: Optional[str] = None
    VALIDATOR_ADDRESS: Optional[str] = None
    TOKEN_NAME: str = "DFC"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        if not self.POLICY_ID:
            policy_id_path = self.ASSETS_DIR / "dfct-minting-policy.id"
            if policy_id_path.exists():
                with open(policy_id_path, "r") as f:
                    self.POLICY_ID = f.read().strip()
                    try:
                        bytes.fromhex(self.POLICY_ID)
                    except HexError:
                        raise ValueError(f"Invalid POLICY_ID in {policy_id_path}: not a hex string")
            else:
                raise ValueError('Policy id is a requirement')

        if not self.VALIDATOR_ADDRESS:
            validator_addr_path = self.ASSETS_DIR / "dfct-provenance.addr"
            if validator_addr_path.exists():
                with open(validator_addr_path, "r") as f:
                    self.VALIDATOR_ADDRESS = f.read().strip()
            else:
                raise ValueError('Validator address is a requirement')


# Create a global settings instance
settings = Settings()