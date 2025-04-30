from typing import Optional, Union, Any
from pathlib import Path

from pycardano import (
    Address, TransactionBuilder, TransactionOutput,
    ScriptHash, AuxiliaryData, UTxO,
    PaymentSigningKey
)

from dfctbackend.config import settings
from dfctbackend.cardano.context import get_chain_context
from dfctbackend.cardano.utils import (
    load_plutus_script, str_to_hex
)

class TransactionError(Exception):
    """Exception raised for transaction errors."""
    pass

class CardanoTransactions:
    """
    Class for building and submitting Cardano transactions using PyCardano.
    """
    
    def __init__(self):
        """Initialize CardanoTransactions with the chain context."""
        self.context = get_chain_context()
        
        # Load contract scripts
        self.validator_path = settings.ASSETS_DIR / "dfct-provenance.plutus"
        self.policy_path = settings.ASSETS_DIR / "dfct-minting-policy.plutus"
        
        # Load scripts
        self._load_scripts()
    
    def _load_scripts(self):
        """Load Plutus scripts."""
        if self.validator_path.exists():
            self.validator_script = load_plutus_script(self.validator_path)
        else:
            raise TransactionError(f"Validator script not found at {self.validator_path}")
        
        if self.policy_path.exists():
            self.policy_script = load_plutus_script(self.policy_path)
        else:
            raise TransactionError(f"Policy script not found at {self.policy_path}")
    
    def get_script_address(self) -> Address:
        """
        Get the address of the validator script.
        
        Returns:
            Address: The validator script address.
        """
        return Address.from_primitive(settings.VALIDATOR_ADDRESS)
    
    def find_utxos_at_address(self, address: Union[Address, str], min_lovelace: int = 0) -> list[UTxO]:
        """
        Find UTXOs at an address with a minimum amount of lovelace.
        
        Args:
            address: The address to query.
            min_lovelace: Minimum amount of lovelace required.
            
        Returns:
            list[UTxO]: list of UTXOs found at the address.
        """
        if isinstance(address, str):
            address = Address.from_primitive(address)
        
        # Query UTXOs
        utxos = self.context.utxos(str(address))
        
        # Filter by minimum lovelace if specified
        if min_lovelace > 0:
            utxos = [utxo for utxo in utxos if utxo.output.amount.coin >= min_lovelace]
        
        return utxos
    
    def find_token_utxos(self, address: Union[Address, str], policy_id: str, token_name: str, amount: int = 1) -> list[UTxO]:
        """
        Find UTXOs containing a specific token.
        
        Args:
            address: The address to query.
            policy_id: The policy ID of the token.
            token_name: The name of the token.
            amount: Minimum amount of tokens required.
            
        Returns:
            list[UTxO]: list of UTXOs containing the token.
        """
        if isinstance(address, str):
            address = Address.from_primitive(address)
        
        # Query UTXOs
        utxos = self.context.utxos(str(address))
        
        # Token name to hex
        token_name_hex = "444643"
        
        # Filter UTXOs by token
        result = []
        for utxo in utxos:
            # Check if the UTXO contains the token
            ma = utxo.output.amount.multi_asset
            policy = ScriptHash(bytes.fromhex(policy_id))
            if policy in ma:
                token_amount = ma[policy].get(bytes.fromhex(token_name_hex), 0)
                if token_amount >= amount:
                    result.append(utxo)
        
        return result
    
    def build_transaction(
        self,
        inputs: list[UTxO],
        outputs: list[TransactionOutput],
        signing_keys: list[str],
        metadata: Optional[dict[str, Any]] = None,
        auxiliary_data: Optional[AuxiliaryData] = None,
    ) -> bytes:
        """
        Build a transaction.
        
        Args:
            inputs: list of inputs.
            outputs: list of outputs.
            signing_keys: list of signing key file paths.
            metadata: Optional metadata.
            auxiliary_data: Optional auxiliary data.
            
        Returns:
            bytes: The signed transaction.
        """
        # Create a transaction builder
        builder = TransactionBuilder(self.context)
        
        # Add inputs
        for utxo in inputs:
            builder.add_input(utxo)
        
        # Add outputs
        for output in outputs:
            builder.add_output(output)
        
        # Add metadata if provided
        if metadata:
            builder.auxiliary_data = AuxiliaryData(data=metadata)
        
        # Add auxiliary data if provided
        if auxiliary_data:
            builder.auxiliary_data = auxiliary_data
        
        # Load signing keys
        skeys = []
        for key_path in signing_keys:
            try:
                with open(Path(key_path), 'r') as f:
                    skeys.append(PaymentSigningKey.from_json(f.read()))
            except Exception as e:
                raise TransactionError(f"Failed to load signing key from {key_path}: {str(e)}")
        
        # Build and sign transaction
        signed_tx = builder.build_and_sign(signing_keys=skeys, change_address=inputs[0].output.address)
        
        return signed_tx.transaction_body
    
    def submit_transaction(self, signed_tx: bytes) -> str:
        """
        Submit a transaction to the network.
        
        Args:
            signed_tx: The signed transaction.
            
        Returns:
            str: The transaction hash.
        """
        try:
            tx_id = self.context.submit_tx(signed_tx)
            return tx_id
        except Exception as e:
            raise TransactionError(f"Failed to submit transaction: {str(e)}")

    def get_transaction_status(self, tx_hash: str) -> dict[str, Any]:
        """
        Get the status of a transaction.
        
        Args:
            tx_hash: Transaction hash.
            
        Returns:
            dict[str, Any]: Transaction status information.
        """
        try:
            # Query transaction information from the node
            tx_info = self.context.query_transaction_info(tx_hash)
            
            if not tx_info:
                return {
                    "status": "pending",
                    "transaction_hash": tx_hash
                }
            
            confirmed_block = tx_info.get("block_height")
            confirmation_time = tx_info.get("block_time")
            
            # Check if transaction is confirmed
            if confirmed_block:
                return {
                    "status": "confirmed",
                    "confirmed_block": confirmed_block,
                    "confirmation_time": confirmation_time,
                    "transaction_hash": tx_hash
                }
            else:
                return {
                    "status": "pending",
                    "transaction_hash": tx_hash
                }
        except Exception as e:
            # If we can't find the transaction, assume it's not submitted or failed
            return {
                "status": "failed",
                "error": str(e),
                "transaction_hash": tx_hash
            }