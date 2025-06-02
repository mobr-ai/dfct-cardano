from typing import Union
import logging

from pycardano import (
    Address, ScriptHash, UTxO
)

from dfctbackend.config import settings
from dfctbackend.cardano.context import get_chain_context
from dfctbackend.cardano.utils import load_plutus_script, str_to_hex, load_raw_file
from dfctbackend.cardano.wallet import CardanoWallet
from dfctbackend.cardano.cardano_cli import CardanoCli

logger = logging.getLogger(__name__)

LOVE_THRESHOLD = 1000000
FUND_FEE       = 2000000 + LOVE_THRESHOLD

MAX_ATTEMPTS   = 6
WAIT_DATA_SYNC = 15

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
        self.provenance_address = Address.from_primitive(settings.PROVENANCE_ADDRESS)
        self.policy_id = settings.POLICY_ID
        self.token_name = settings.TOKEN_NAME

    def create_utxo(self, wallet: CardanoWallet, src_utxo: UTxO, new_utxo_amount: int):
        if not src_utxo:
            raise TransactionError(f"Invalid src utxo for {wallet.name}")

        wallet_addr_str = str(wallet.address)
        tx_in = self.create_tx_in(src_utxo)

        # Build transaction to create utxo
        params = [
            "--testnet-magic", str(self.cli.testnet_magic),
            "--socket-path", self.cli.socket_path,
            "--change-address", wallet_addr_str,
            "--tx-in", tx_in,
            "--tx-out", f"\"{wallet_addr_str} + {new_utxo_amount} lovelace\"",
            "--out-file", f"{self.cli.docker_assets}/new-utxo.body"
        ]
        self.cli.build_transaction(params)

        # Sign transaction to create utxo
        self.cli.sign_transaction(
            tx_body_file=f"{self.cli.docker_assets}/new-utxo.body",
            signing_key_file=f"{self.cli.docker_assets}/{wallet.name}.skey",
            out_file=f"{self.cli.docker_assets}/new-utxo.signed"
        )

        # Submit signed transaction to create utxo
        self.cli.submit_transaction(f"{self.cli.docker_assets}/new-utxo.signed")

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
        token_name_hex = str_to_hex(token_name)

        selected = None
        selected_value = 0
        for utxo in utxos:
            # Skip if not enough lovelace
            if int(utxo.output.amount.coin) < love_amount:
                continue

            # Check for the specific token
            ma = utxo.output.amount.multi_asset
            policy = ScriptHash(bytes.fromhex(policy_id))

            if policy in ma:
                for k, v in ma[policy].items():
                    if str(k) == str(token_name_hex) and int(v) >= int(dfc_amount):
                        if int(v) > selected_value:
                            selected_value = int(v)
                            selected = utxo

        return selected

    def get_cnt_amount(self, utxo, policy_id):
        if utxo:
            policy = ScriptHash(bytes.fromhex(policy_id))
            token_amount = next(iter(utxo.output.amount.multi_asset[policy].values()))
            return int(token_amount)

        return 0

    def get_transaction_hash(self, file_name: str) -> str:
        return self.cli.get_transaction_hash(file_name)

    def write_temp_json(self, data: dict, filename: str) -> str:
        return self.cli.write_temp_json(data, filename, self.assets_path)

    def get_current_slot_and_validity(self, validity_window: int = 1000) -> tuple[int, int, int]:
        return self.cli.get_current_slot_and_validity(validity_window)

    def prepare_script_transaction(
        self,
        action_name: str,
        id_str: str,
        wallet: CardanoWallet,
        redeemer_data: dict,
        output_datum: dict = None,
        min_lovelace: int = 3000000,
        token_output: str = None
    ) -> tuple[str, str]:
        """
        Prepare a script transaction for interacting with the validator.
        Returns the signed transaction file path and the id string.
        """
        # Find the UTxO containing the ID
        utxos = self.find_utxos_at_address(self.provenance_address)
        utxo, _ = self.context.find_utxo_with_datum_id(utxos, id_str)
        
        # Find collateral UTxO from the wallet
        collateral_utxos = [
            utxo for utxo in self.find_utxos_at_address(wallet.address)
            if not utxo.output.amount.multi_asset
            and utxo.output.amount.coin >= 5_000_000
        ]

        if not collateral_utxos:
            raise TransactionError(f"No suitable collateral UTxO found for {wallet.name}")
        
        collateral_utxo = collateral_utxos[0]
        col_tx_hash = collateral_utxo.input.transaction_id
        col_tx_ix = collateral_utxo.input.index
        txin_collateral = f"{col_tx_hash}#{col_tx_ix}"
        
        # Get transaction input for the UTxO we want to spend
        tx_hash = utxo.input.transaction_id
        tx_ix = utxo.input.index
        txin = f"{tx_hash}#{tx_ix}"
        
        # Get current slot and validity range
        _, invalid_before, invalid_hereafter = self.get_current_slot_and_validity()
        
        # Write redeemer to temporary file
        redeemer_file = f"{action_name}-redeemer-{id_str}.json"
        self.write_temp_json(redeemer_data, redeemer_file)
        
        # Build the base command
        wallet_addr_str = str(wallet.address)
        validator_addr_str = str(self.provenance_address)
        
        build_cmd = [
            "--testnet-magic", str(self.cli.testnet_magic),
            "--socket-path", self.cli.socket_path,
            "--change-address", wallet_addr_str,
            "--tx-in", txin,
            "--tx-in-script-file", f"{self.cli.docker_assets}/dfct-provenance.plutus",
            "--tx-in-inline-datum-present",
            "--tx-in-redeemer-file", f"{self.cli.docker_assets}/{redeemer_file}",
            "--tx-in-collateral", txin_collateral,
            "--required-signer", f"{self.cli.docker_assets}/{wallet.name}.skey",
            "--invalid-before", str(invalid_before),
            "--invalid-hereafter", str(invalid_hereafter)
        ]
        
        # Add output transaction with datum if provided
        if output_datum:
            output_datum_file = f"{action_name}-output-datum-{id_str}.json"
            self.write_temp_json(output_datum, output_datum_file)
            
            if token_output:
                tx_out = f"{validator_addr_str}+{min_lovelace}+\"{token_output}\""
            else:
                tx_out = f"{validator_addr_str}+{min_lovelace}"
                
            build_cmd.extend([
                "--tx-out", tx_out,
                "--tx-out-inline-datum-file", f"{self.cli.docker_assets}/{output_datum_file}"
            ])
        
        # Add output file
        raw_tx_file = f"{action_name}-{id_str}.raw"
        build_cmd.extend(["--out-file", f"{self.cli.docker_assets}/{raw_tx_file}"])
        
        # Build the transaction
        self.cli.build_transaction(build_cmd)
        
        # Sign the transaction
        signed_tx_file = f"{action_name}-{id_str}.signed"
        self.cli.sign_transaction(
            tx_body_file=f"{self.cli.docker_assets}/{raw_tx_file}",
            signing_key_file=f"{self.cli.docker_assets}/{wallet.name}.skey",
            out_file=f"{self.cli.docker_assets}/{signed_tx_file}"
        )

        return signed_tx_file, id_str