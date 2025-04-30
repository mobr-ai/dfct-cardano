import time
from typing import Any, Optional
from binascii import Error as HexError

from pycardano import (
    Address, TransactionOutput, 
    Value, MultiAsset, ScriptHash, AuxiliaryData
)

from dfctbackend.config import settings
from dfctbackend.cardano.wallet import CardanoWallet
from dfctbackend.cardano.transaction import CardanoTransactions, TransactionError
from dfctbackend.cardano.utils import (
    create_plutus_data_from_json, generate_topic_id, str_to_hex
)

class ProvenanceContract:
    """
    Class for interacting with the DFCT Provenance Contract.
    """
    def __init__(self):
        """Initialize the contract with transaction handler."""
        self.tx_handler = CardanoTransactions()
        self.policy_id = settings.POLICY_ID
        self.validator_address = settings.VALIDATOR_ADDRESS
        self.token_name = settings.TOKEN_NAME
        self.token_name_hex = str_to_hex(settings.TOKEN_NAME)

        # Validate hex strings
        if self.policy_id:
            try:
                bytes.fromhex(self.policy_id)
            except HexError:
                raise ValueError(f"Invalid policy_id: {self.policy_id} is not a valid hex string")
        if self.token_name_hex:
            try:
                bytes.fromhex(self.token_name_hex)
            except HexError:
                raise ValueError(f"Invalid token_name_hex: {self.token_name_hex} is not a valid hex string")
    
    def _prepare_topic_datum(
        self,
        title: str,
        description: str,
        proposer_pkh: str,
        reward_amount: int,
        reviewers: list[str]
    ) -> tuple[dict[str, Any], str]:
        """
        Prepare a topic datum for the contract.
        
        Args:
            title: Topic title.
            description: Topic description.
            proposer_pkh: Proposer's payment key hash.
            reward_amount: Reward amount.
            reviewers: list of reviewer payment key hashes.
            
        Returns:
            Tuple[dict[str, Any], str]: The prepared datum and topic ID.
        """
        topic_id = generate_topic_id()
        
        reviewer_map = [
            {
                "k": {"bytes": pkh},
                "v": {"int": 1}
            }
            for pkh in reviewers
        ]
        
        datum = {
            "constructor": 0,
            "fields": [
                {
                    "constructor": 0,
                    "fields": [
                        {"bytes": str_to_hex(topic_id)},
                        {"bytes": str_to_hex(title)},
                        {"bytes": str_to_hex(description)},
                        {"bytes": proposer_pkh},
                        {"int": int(time.time())}
                    ]
                },
                {
                    "constructor": 0,
                    "fields": []
                },
                {
                    "constructor": 0,
                    "fields": [
                        {"int": reward_amount},
                        {"int": 0},
                        {"bytes": self.token_name_hex},
                        {"int": int(time.time())}
                    ]
                },
                {
                    "map": reviewer_map
                }
            ]
        }
        
        return datum, topic_id
    
    def _prepare_submit_topic_redeemer(
        self,
        topic_id: str,
        title: str,
        description: str,
        proposer_pkh: str,
        reward_amount: int
    ) -> dict[str, Any]:
        """
        Prepare a submit topic redeemer for the contract.
        
        Args:
            topic_id: Topic ID.
            title: Topic title.
            description: Topic description.
            proposer_pkh: Proposer's payment key hash.
            reward_amount: Reward amount.
            
        Returns:
            dict[str, Any]: The prepared redeemer.
        """
        redeemer = {
            "constructor": 0,
            "fields": [
                {
                    "constructor": 0,
                    "fields": [
                        {
                            "constructor": 0,
                            "fields": [
                                {"bytes": str_to_hex(topic_id)},
                                {"bytes": str_to_hex(title)},
                                {"bytes": str_to_hex(description)},
                                {"bytes": proposer_pkh},
                                {"int": 0}
                            ]
                        },
                        {
                            "constructor": 0,
                            "fields": [
                                {"int": reward_amount},
                                {"int": 0},
                                {"bytes": self.token_name_hex},
                                {"int": 0}
                            ]
                        }
                    ]
                }
            ]
        }
        
        return redeemer
    
    def submit_topic(
        self,
        wallet: CardanoWallet,
        title: str,
        description: str,
        reward_amount: int
    ) -> dict[str, Any]:
        """
        Submit a new topic to the contract.
        
        Args:
            wallet: The wallet to use for the transaction.
            title: Topic title.
            description: Topic description.
            reward_amount: Reward amount.
            
        Returns:
            dict[str, Any]: Transaction result including the transaction hash and topic ID.
        """
        if not wallet.address:
            raise TransactionError(f"Wallet {wallet.name} has no address")
        
        if not wallet.payment_key_hash:
            raise TransactionError(f"Wallet {wallet.name} has no payment key hash")
        
        reviewer_pkhs = [
            reviewer.payment_key_hash for name, reviewer in wallet.local_wallets.items() 
            if name.startswith("reviewer") and reviewer.payment_key_hash
        ]
        
        if not reviewer_pkhs:
            raise TransactionError("No reviewer wallets found")
        
        datum, topic_id = self._prepare_topic_datum(
            title=title,
            description=description,
            proposer_pkh=wallet.payment_key_hash,
            reward_amount=reward_amount,
            reviewers=reviewer_pkhs
        )
        
        redeemer = self._prepare_submit_topic_redeemer(
            topic_id=topic_id,
            title=title,
            description=description,
            proposer_pkh=wallet.payment_key_hash,
            reward_amount=reward_amount
        )
        
        token_utxos = self.tx_handler.find_token_utxos(
            address=wallet.address,
            policy_id=self.policy_id,
            token_name=self.token_name,
            amount=reward_amount
        )
        
        if not token_utxos:
            raise TransactionError(f"No UTXOs with sufficient {self.token_name} tokens found for {wallet.name}")
        
        fee_utxos = self.tx_handler.find_utxos_at_address(
            address=wallet.address,
            min_lovelace=5000000
        )
        
        if not fee_utxos:
            raise TransactionError(f"No UTXOs with sufficient ADA found for {wallet.name}")
        
        inputs = [token_utxos[0], fee_utxos[0]]
        
        plutus_datum = create_plutus_data_from_json(datum)
        
        multi_asset = MultiAsset({
            ScriptHash(bytes.fromhex(self.policy_id)): {
                bytes.fromhex(self.token_name_hex): reward_amount
            }
        })
        
        validator_output = TransactionOutput(
            address=Address.from_primitive(self.validator_address),
            amount=Value(coin=2000000, multi_asset=multi_asset),
            datum=plutus_datum
        )
        
        signed_tx = self.tx_handler.build_transaction(
            inputs=inputs,
            outputs=[validator_output],
            signing_keys=[str(wallet.skey_path)],
            auxiliary_data=AuxiliaryData(data=redeemer)
        )
        
        tx_hash = self.tx_handler.submit_transaction(signed_tx)
        
        return {
            "transaction_hash": tx_hash,
            "topic_id": topic_id
        }
    
    def _prepare_review_topic_redeemer(self, topic_id: str, approved: bool) -> dict[str, Any]:
        """
        Prepare a review topic redeemer for the contract.
        
        Args:
            topic_id: Topic ID.
            approved: Whether the topic is approved.
            
        Returns:
            dict[str, Any]: The prepared redeemer.
        """
        redeemer = {
            "constructor": 0,
            "fields": [
                {
                    "constructor": 1,
                    "fields": [
                        {"bytes": str_to_hex(topic_id)},
                        {"int": 1 if approved else 0}
                    ]
                }
            ]
        }
        
        return redeemer
    
    def review_topic(
        self,
        wallet: CardanoWallet,
        topic_id: str,
        approved: bool
    ) -> dict[str, Any]:
        """
        Review a topic in the contract.
        
        Args:
            wallet: The wallet to use for the transaction.
            topic_id: Topic ID.
            approved: Whether the topic is approved.
            
        Returns:
            dict[str, Any]: Transaction result including the transaction hash.
        """
        if not wallet.address:
            raise TransactionError(f"Wallet {wallet.name} has no address")
        
        redeemer = self._prepare_review_topic_redeemer(topic_id, approved)
        
        validator_utxos = self.tx_handler.find_utxos_at_address(
            address=self.validator_address,
            min_lovelace=2000000
        )
        
        if not validator_utxos:
            raise TransactionError(f"No UTXOs found at validator address for topic {topic_id}")
        
        fee_utxos = self.tx_handler.find_utxos_at_address(
            address=wallet.address,
            min_lovelace=5000000
        )
        
        if not fee_utxos:
            raise TransactionError(f"No UTXOs with sufficient ADA found for {wallet.name}")
        
        inputs = [validator_utxos[0], fee_utxos[0]]
        
        # For simplicity, return tokens to proposer (in production, this would be more complex)
        output = TransactionOutput(
            address=wallet.address,
            amount=validator_utxos[0].output.amount
        )
        
        signed_tx = self.tx_handler.build_transaction(
            inputs=inputs,
            outputs=[output],
            signing_keys=[str(wallet.skey_path)],
            auxiliary_data=AuxiliaryData(data=redeemer)
        )
        
        tx_hash = self.tx_handler.submit_transaction(signed_tx)
        
        return {
            "transaction_hash": tx_hash,
            "topic_id": topic_id
        }

def _prepare_activate_topic_redeemer(self, topic_id: str) -> dict[str, Any]:
    """
    Prepare an activate topic redeemer for the contract.
    
    Args:
        topic_id: Topic ID.
        
    Returns:
        dict[str, Any]: The prepared redeemer.
    """
    redeemer = {
        "constructor": 0,
        "fields": [
            {
                "constructor": 2,
                "fields": [
                    {"bytes": str_to_hex(topic_id)}
                ]
            }
        ]
    }
    
    return redeemer

def activate_topic(
    self,
    wallet: CardanoWallet,
    topic_id: str
) -> dict[str, Any]:
    """
    Activate a topic that has been reviewed.
    
    Args:
        wallet: The wallet to use for the transaction.
        topic_id: Topic ID to activate.
        
    Returns:
        dict[str, Any]: Transaction result including the transaction hash.
    """
    if not wallet.address:
        raise TransactionError(f"Wallet {wallet.name} has no address")
    
    redeemer = self._prepare_activate_topic_redeemer(topic_id)
    
    # Find the validator UTXO containing the topic
    validator_utxos = self.tx_handler.find_utxos_at_address(
        address=self.validator_address,
        min_lovelace=2000000
    )
    
    if not validator_utxos:
        raise TransactionError(f"No UTXOs found at validator address for topic {topic_id}")
    
    # For simplicity, we'll use the first UTXO we find
    topic_utxo = validator_utxos[0]
    
    fee_utxos = self.tx_handler.find_utxos_at_address(
        address=wallet.address,
        min_lovelace=5000000
    )
    
    if not fee_utxos:
        raise TransactionError(f"No UTXOs with sufficient ADA found for {wallet.name}")
    
    inputs = [topic_utxo, fee_utxos[0]]
    
    # Create a new output with the activated topic status
    output = TransactionOutput(
        address=Address.from_primitive(self.validator_address),
        amount=topic_utxo.output.amount
    )
    
    signed_tx = self.tx_handler.build_transaction(
        inputs=inputs,
        outputs=[output],
        signing_keys=[str(wallet.skey_path)],
        auxiliary_data=AuxiliaryData(data=redeemer)
    )
    
    tx_hash = self.tx_handler.submit_transaction(signed_tx)
    
    return {
        "transaction_hash": tx_hash,
        "topic_id": topic_id
    }

def _prepare_contribution_datum(
    self,
    topic_id: str,
    content: str,
    evidence_links: list[str],
    contributor_pkh: str
) -> tuple[dict[str, Any], str]:
    """
    Prepare a contribution datum for the contract.
    
    Args:
        topic_id: Topic ID.
        content: Contribution content.
        evidence_links: Evidence links.
        contributor_pkh: Contributor's payment key hash.
        
    Returns:
        Tuple[dict[str, Any], str]: The prepared datum and contribution ID.
    """
    contribution_id = generate_contribution_id()
    
    # Convert evidence links to bytes fields
    evidence_bytes = [
        {"bytes": str_to_hex(link)} for link in evidence_links
    ]
    
    datum = {
        "constructor": 1,
        "fields": [
            {
                "constructor": 0,
                "fields": [
                    {"bytes": str_to_hex(contribution_id)},
                    {"bytes": str_to_hex(topic_id)},
                    {"bytes": str_to_hex(content)},
                    {"bytes": contributor_pkh},
                    {"int": int(time.time())}
                ]
            },
            {
                "constructor": 0,
                "fields": evidence_bytes
            },
            {
                "map": []  # No reviews yet
            }
        ]
    }
    
    return datum, contribution_id

def _prepare_submit_contribution_redeemer(
    self,
    topic_id: str,
    contribution_id: str,
    content: str,
    evidence_links: list[str]
) -> dict[str, Any]:
    """
    Prepare a submit contribution redeemer for the contract.
    
    Args:
        topic_id: Topic ID.
        contribution_id: Contribution ID.
        content: Contribution content.
        evidence_links: Evidence links.
        
    Returns:
        dict[str, Any]: The prepared redeemer.
    """
    # Convert evidence links to bytes fields
    evidence_bytes = [
        {"bytes": str_to_hex(link)} for link in evidence_links
    ]
    
    redeemer = {
        "constructor": 0,
        "fields": [
            {
                "constructor": 3,
                "fields": [
                    {"bytes": str_to_hex(topic_id)},
                    {"bytes": str_to_hex(contribution_id)},
                    {"bytes": str_to_hex(content)},
                    {
                        "list": evidence_bytes
                    }
                ]
            }
        ]
    }
    
    return redeemer

def submit_contribution(
    self,
    wallet: CardanoWallet,
    topic_id: str,
    content: str,
    evidence_links: list[str]
) -> dict[str, Any]:
    """
    Submit a new contribution to a topic.
    
    Args:
        wallet: The wallet to use for the transaction.
        topic_id: Topic ID.
        content: Contribution content.
        evidence_links: Evidence links.
        
    Returns:
        dict[str, Any]: Transaction result including the transaction hash and contribution ID.
    """
    if not wallet.address:
        raise TransactionError(f"Wallet {wallet.name} has no address")
    
    if not wallet.payment_key_hash:
        raise TransactionError(f"Wallet {wallet.name} has no payment key hash")
    
    # Find the topic in the contract
    validator_utxos = self.tx_handler.find_utxos_at_address(
        address=self.validator_address,
        min_lovelace=2000000
    )
    
    if not validator_utxos:
        raise TransactionError(f"No UTXOs found at validator address for topic {topic_id}")
    
    # In a real implementation, we would need to search for the specific UTXO
    # containing the topic by parsing the datum.
    # For simplicity, we'll use the first UTXO we find
    topic_utxo = validator_utxos[0]
    
    # Prepare contribution datum and redeemer
    datum, contribution_id = self._prepare_contribution_datum(
        topic_id=topic_id,
        content=content,
        evidence_links=evidence_links,
        contributor_pkh=wallet.payment_key_hash
    )
    
    redeemer = self._prepare_submit_contribution_redeemer(
        topic_id=topic_id,
        contribution_id=contribution_id,
        content=content,
        evidence_links=evidence_links
    )
    
    fee_utxos = self.tx_handler.find_utxos_at_address(
        address=wallet.address,
        min_lovelace=5000000
    )
    
    if not fee_utxos:
        raise TransactionError(f"No UTXOs with sufficient ADA found for {wallet.name}")
    
    inputs = [topic_utxo, fee_utxos[0]]
    
    plutus_datum = create_plutus_data_from_json(datum)
    
    # Create a new output with the contribution added to the topic
    # In a real implementation, we would need to modify the topic datum to include
    # the contribution reference
    output = TransactionOutput(
        address=Address.from_primitive(self.validator_address),
        amount=topic_utxo.output.amount,
        datum=plutus_datum
    )
    
    signed_tx = self.tx_handler.build_transaction(
        inputs=inputs,
        outputs=[output],
        signing_keys=[str(wallet.skey_path)],
        auxiliary_data=AuxiliaryData(data=redeemer)
    )
    
    tx_hash = self.tx_handler.submit_transaction(signed_tx)
    
    return {
        "transaction_hash": tx_hash,
        "topic_id": topic_id,
        "contribution_id": contribution_id
    }

def _prepare_review_contribution_redeemer(
    self,
    contribution_id: str,
    approved: bool,
    comment: Optional[str] = None
) -> dict[str, Any]:
    """
    Prepare a review contribution redeemer for the contract.
    
    Args:
        contribution_id: Contribution ID.
        approved: Whether the contribution is approved.
        comment: Optional comment.
        
    Returns:
        dict[str, Any]: The prepared redeemer.
    """
    comment_field = {"bytes": str_to_hex(comment)} if comment else {"bytes": ""}
    
    redeemer = {
        "constructor": 0,
        "fields": [
            {
                "constructor": 4,
                "fields": [
                    {"bytes": str_to_hex(contribution_id)},
                    {"int": 1 if approved else 0},
                    comment_field
                ]
            }
        ]
    }
    
    return redeemer

def review_contribution(
    self,
    wallet: CardanoWallet,
    contribution_id: str,
    approved: bool,
    comment: Optional[str] = None
) -> dict[str, Any]:
    """
    Review a contribution.
    
    Args:
        wallet: The wallet to use for the transaction.
        contribution_id: Contribution ID.
        approved: Whether the contribution is approved.
        comment: Optional comment.
        
    Returns:
        dict[str, Any]: Transaction result including the transaction hash.
    """
    if not wallet.address:
        raise TransactionError(f"Wallet {wallet.name} has no address")
    
    if not wallet.payment_key_hash:
        raise TransactionError(f"Wallet {wallet.name} has no payment key hash")
    
    # Find the contribution in the contract
    validator_utxos = self.tx_handler.find_utxos_at_address(
        address=self.validator_address,
        min_lovelace=2000000
    )
    
    if not validator_utxos:
        raise TransactionError(f"No UTXOs found at validator address for contribution {contribution_id}")
    
    # In a real implementation, we would need to search for the specific UTXO
    # containing the contribution by parsing the datum.
    # For simplicity, we'll use the first UTXO we find
    contribution_utxo = validator_utxos[0]
    
    redeemer = self._prepare_review_contribution_redeemer(
        contribution_id=contribution_id,
        approved=approved,
        comment=comment
    )
    
    fee_utxos = self.tx_handler.find_utxos_at_address(
        address=wallet.address,
        min_lovelace=5000000
    )
    
    if not fee_utxos:
        raise TransactionError(f"No UTXOs with sufficient ADA found for {wallet.name}")
    
    inputs = [contribution_utxo, fee_utxos[0]]
    
    # Create a new output with the review added to the contribution
    # In a real implementation, we would need to modify the datum to include
    # the review information
    output = TransactionOutput(
        address=Address.from_primitive(self.validator_address),
        amount=contribution_utxo.output.amount
    )
    
    signed_tx = self.tx_handler.build_transaction(
        inputs=inputs,
        outputs=[output],
        signing_keys=[str(wallet.skey_path)],
        auxiliary_data=AuxiliaryData(data=redeemer)
    )
    
    tx_hash = self.tx_handler.submit_transaction(signed_tx)
    
    return {
        "transaction_hash": tx_hash,
        "contribution_id": contribution_id
    }