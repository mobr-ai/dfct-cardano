from typing import Union
import logging
import uuid
from typing import Any, Tuple

from pycardano import (
    Address, ScriptHash, UTxO
)

from dfctbackend.config import settings
from dfctbackend.cardano.context import get_chain_context
from dfctbackend.cardano.utils import str_to_hex
from dfctbackend.cardano.wallet import CardanoWallet
from dfctbackend.cardano.cardano_cli import CardanoCli

logger = logging.getLogger(__name__)

LOVE_THRESHOLD = 1000000
FUND_FEE       = 2000000 + LOVE_THRESHOLD

MAX_ATTEMPTS   = 6
WAIT_DATA_SYNC = 30

class TransactionError(Exception):
    """Exception raised for transaction errors."""
    pass

class CardanoTransaction:
    """
    Class for interacting with Cardano transactions using PyCardano.
    """
    def __init__(self):
        """Initialize with chain context and load scripts."""
        self.context = get_chain_context()
        self.cli = CardanoCli()
        self.assets_path = settings.ASSETS_DIR
        self.docker_assets = settings.DOCKER_ASSETS_DIR
        self.policy_id = settings.POLICY_ID
        self.token_name = settings.TOKEN_NAME

    def create_utxo(self, wallet: CardanoWallet, src_utxo: UTxO, new_utxo_amount: int):
        if not src_utxo:
            raise TransactionError(f"Invalid src utxo for {wallet.name}")

        wallet_addr_str = str(wallet.address)
        tx_in = self.create_tx_in(src_utxo)

        # Build transaction to create utxo
        params = [
            "--change-address", wallet_addr_str,
            "--tx-in", tx_in,
            "--tx-out", f"\"{wallet_addr_str} + {new_utxo_amount} lovelace\"",
            "--out-file", f"{self.docker_assets}/new-utxo.body"
        ]
        self.cli.build_transaction(params)

        # Sign transaction to create utxo
        self.cli.sign_transaction(
            tx_body_file=f"{self.docker_assets}/new-utxo.body",
            signing_key_file=f"{self.docker_assets}/{wallet.name}.skey",
            out_file=f"{self.docker_assets}/new-utxo.signed"
        )

        # Submit signed transaction to create utxo
        self.cli.submit_transaction(f"{self.docker_assets}/new-utxo.signed")

    def find_largest_utxo(self, wallet: CardanoWallet) -> tuple[UTxO, int]:
        largest_value = 0
        largest_utxo = None
        utxos = self.find_utxos_at_address(wallet.address)
        for utxo in utxos:
            if int(utxo.output.amount.coin) > largest_value:
                largest_value = int(utxo.output.amount.coin)
                largest_utxo = utxo

        return largest_utxo, largest_value

    def find_utxo_and_create_tx_in(self, wallet: CardanoWallet, min_lovelace: int, exclude: list) -> tuple[str, UTxO]:
        utxos = self.find_utxos_at_address(
            address=wallet.address,
            min_lovelace=min_lovelace,
            exclude=exclude
        )

        if not utxos:
            raise TransactionError(f"No suitable UTxO found for {wallet.name}")

        utxo = utxos[0]
        return self.create_tx_in(utxo), utxo

    def create_tx_in(self, utxo: UTxO) -> str:
        tx_hash = utxo.input.transaction_id
        tx_ix = utxo.input.index
        return f"{tx_hash}#{tx_ix}"

    def find_utxos_at_address(
            self,
            address: Union[Address, str],
            min_lovelace: int = 0,
            exclude: list = []) -> list[UTxO]:

        """
        Find UTxOs at an address with a minimum amount of lovelace.
        
        Args:
            address: The address to search
            min_lovelace: Minimum amount of lovelace required (default: 0)
            
        Returns:
            List of UTxOs at the address with at least min_lovelace
        """
        if isinstance(address, str):
            address = Address.from_primitive(address)

        utxos = self.context.utxos(str(address))
        utxos = [utxo for utxo in utxos if utxo.output.amount.coin >= min_lovelace and utxo not in exclude]

        return utxos

    def find_token_utxo(
            self,
            address: Union[Address, str],
            policy_id: str,
            token_name: str,
            love_amount: int = 1,
            dfc_amount: int = 1) -> UTxO:

        """
        Find UTxO containing a specific token with minimum amounts.

        Args:
            address: The address to search
            policy_id: The policy ID of the token
            token_name: The name of the token
            love_amount: Minimum amount of lovelace required
            dfc_amount: Minimum amount of tokens required

        Returns:
            UTxO containing the specified tokens
        """
        if isinstance(address, str):
            address = Address.from_primitive(address)

        utxos = self.context.utxos(str(address))
        selected = None
        selected_value = 0
        for utxo in utxos:
            # Skip if not enough lovelace
            if int(utxo.output.amount.coin) < love_amount:
                continue

            dfc_amount = self.get_cnt_amount(utxo, policy_id=policy_id)
            if dfc_amount > selected_value:
                selected_value = dfc_amount
                selected = utxo

        return selected

    def get_cnt_amount(self, utxo, policy_id):
        if utxo:
            policy = ScriptHash(bytes.fromhex(policy_id))
            token_amount = 0
            if policy and policy in utxo.output.amount.multi_asset:
                token_amount = next(iter(utxo.output.amount.multi_asset[policy].values()))
            return int(token_amount)

        return 0

    def get_transaction_hash(self, file_name: str) -> str:
        return self.cli.get_transaction_hash(file_name)

    def get_current_slot_and_validity(self, validity_window: int = 1000) -> tuple[int, int, int]:
        return self.cli.get_current_slot_and_validity(validity_window)

    def prepare_tx_inputs(self, wallet: CardanoWallet, exclude_utxos: list[Any], min_collateral: int = 5000000) -> Tuple[str, str]:
        """Prepare transaction inputs for fee and collateral UTxOs."""
        txin_collateral, collateral_utxo = self.find_utxo_and_create_tx_in(
            wallet=wallet,
            min_lovelace=min_collateral,
            exclude=exclude_utxos
        )
        if not collateral_utxo:
            raise TransactionError(f"{wallet.name} does not have a collateral UTxO")

        txin_fee, fee_utxo = self.find_utxo_and_create_tx_in(
            wallet=wallet,
            min_lovelace=FUND_FEE,
            exclude=exclude_utxos + [collateral_utxo]
        )
        if not fee_utxo:
            raise TransactionError(f"{wallet.name} does not have a UTxO to fund fee")

        return txin_fee, txin_collateral

    def build_and_submit_tx(
            self,
            build_cmd: list[str],
            wallet: CardanoWallet,
            out_file: str,
            tx_name: str
    ) -> str:

        """Build, sign, and submit a transaction, returning the transaction hash."""
        raw_file = f"{out_file}.raw"
        signed_file = f"{out_file}.signed"

        logger.info(f"Parameters for {tx_name}:\n{build_cmd}")
        self.cli.build_transaction(build_cmd + ["--out-file", raw_file])

        self.cli.sign_transaction(
            tx_body_file=raw_file,
            signing_key_file=f"{self.docker_assets}/{wallet.name}.skey",
            out_file=signed_file
        )

        self.cli.submit_transaction(signed_file)
        transaction_hash = self.get_transaction_hash(signed_file)
        #TODO: delete files
        return transaction_hash
