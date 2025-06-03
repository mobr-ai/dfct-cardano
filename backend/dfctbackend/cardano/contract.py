import time
from typing import Any
import logging

from pycardano import Address

from dfctbackend.config import settings
from dfctbackend.cardano.datum import DatumProcessor
from dfctbackend.cardano.transaction import CardanoTransaction, MAX_ATTEMPTS, WAIT_DATA_SYNC
from dfctbackend.cardano.utils import str_to_hex

logger = logging.getLogger(__name__)

class ContractError(Exception):
    """Base exception for contract interaction errors."""
    pass

class Contract:
    """Base class for Cardano smart contract interactions on the Preview Testnet."""

    def __init__(self, validator_address: str):
        """Initialize the contract with chain context and transaction handling."""
        try:
            self.tx = CardanoTransaction()
            self.dp = DatumProcessor()
            self.policy_id = settings.POLICY_ID
            self.validator_address = Address.from_primitive(validator_address)
            self.token_name = settings.TOKEN_NAME
            self.token_name_hex = str_to_hex(settings.TOKEN_NAME)
            self.docker_assets = settings.DOCKER_ASSETS_DIR

            logger.info(f"Initialized contract with policy_id: {self.policy_id}, validator_address: {self.validator_address}")

        except Exception as e:
            logger.error(f"Failed to initialize contract: {str(e)}")
            raise ContractError(f"Failed to initialize contract: {str(e)}")

    def get_contract_utxo_and_datum(self, str_id: str) -> tuple[Any, Any]:
        """Find UTxO and datum for a given ID."""
        attempt_nr = 0
        while attempt_nr < MAX_ATTEMPTS:
            utxos = self.tx.find_utxos_at_address(self.validator_address)
            utxo, datum = self.dp.find_utxo_with_datum_id(utxos, str_id)
            if utxo and datum:
                return utxo, datum
            time.sleep(WAIT_DATA_SYNC)
            attempt_nr += 1
        return None, None