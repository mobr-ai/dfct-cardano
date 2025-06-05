import time
from typing import Any, Tuple
import logging

from pycardano import Address

from dfctbackend.config import settings
from dfctbackend.cardano.utils import write_temp_json, str_to_hex
from dfctbackend.cardano.contract.datum import DatumProcessor
from dfctbackend.cardano.transaction import CardanoTransaction, MAX_ATTEMPTS, WAIT_DATA_SYNC

logger = logging.getLogger(__name__)

class ContractError(Exception):
    """Base exception for contract interaction errors."""
    pass

class Contract:
    """Base class for Cardano smart contract interactions on the Preview Testnet."""

    def __init__(self, validator_address: str):
        """Initialize the contract with chain context and transaction handling."""
        try:
            self.dp:DatumProcessor = None
            self.tx = CardanoTransaction()
            self.policy_id = settings.POLICY_ID
            self.validator_address = Address.from_primitive(validator_address)
            self.token_name = settings.TOKEN_NAME
            self.token_name_hex = str_to_hex(settings.TOKEN_NAME)
            self.docker_assets = settings.DOCKER_ASSETS_DIR

            logger.info(f"Initialized contract with policy_id: {self.policy_id}, validator_address: {self.validator_address}")

        except Exception as e:
            logger.error(f"Failed to initialize contract: {str(e)}")
            raise ContractError(f"Failed to initialize contract: {str(e)}")

    def _get_contract_utxo_and_datum(self, str_id: str, x: int = 0, y: int = 0, z: int = -1) -> tuple[Any, Any]:
        """Find UTxO and datum for a given ID."""
        attempt_nr = 0
        while attempt_nr < MAX_ATTEMPTS:
            utxos = self.tx.find_utxos_at_address(self.validator_address)
            utxo, datum = self.dp.find_utxo_with_datum_id(utxos, str_id, x, y, z)
            if utxo and datum:
                return utxo, datum
            time.sleep(WAIT_DATA_SYNC)
            attempt_nr += 1
        return None, None

    def _write_temp_files(self, datum: Any, redeemer: Any, file_prefix: str, id_str: str) -> Tuple[str, str]:
        """Write datum and redeemer to temporary JSON files."""
        datum_file = f"{file_prefix}-datum-{id_str}.json"
        redeemer_file = f"{file_prefix}-redeemer-{id_str}.json"
        write_temp_json(datum, datum_file, self.tx.assets_path)
        write_temp_json(redeemer, redeemer_file, self.tx.assets_path)
        return datum_file, redeemer_file

    def _prepare_validator_output(self, utxo: Any, validator_addr_str: str) -> str:
        """Prepare transaction output for validator UTxO."""
        lovelace = utxo.output.amount.coin
        reward_amount = self.tx.get_cnt_amount(utxo, self.policy_id)
        if reward_amount > 0:
            token_output = f'{reward_amount} {self.policy_id}.{self.token_name_hex}'
            validator_output = f"{validator_addr_str}+{lovelace}+\"{token_output}\""
        else:
            validator_output = f"{validator_addr_str}+{lovelace}"

        return validator_output

    def _add_change_output(self, build_cmd: list[str], wallet_addr: str, input_utxos: list, output_value: int, token_amount: int) -> list[str]:
        """Add a change output to conserve remaining assets."""

        change_lovelace = 0
        if output_value > 0:
            total_input_lovelace = sum(int(utxo.output.amount.coin) for utxo in input_utxos)
            output_lovelace = output_value
            change_lovelace = total_input_lovelace - output_lovelace - 2000000  # Subtract estimated fee

        change_tokens = 0
        if token_amount > 0:
            total_input_tokens = sum(self.tx.get_cnt_amount(utxo, self.policy_id) for utxo in input_utxos)
            output_tokens = token_amount
            change_tokens = total_input_tokens - output_tokens

        if change_lovelace > 0:
            if change_tokens > 0:
                build_cmd += ["--tx-out", f"{wallet_addr}+{change_lovelace}+\"{change_tokens} {self.policy_id}.{self.token_name_hex}\""]
            elif change_lovelace > 0:
                build_cmd += ["--tx-out", f"{wallet_addr}+{change_lovelace}"]

        return build_cmd
