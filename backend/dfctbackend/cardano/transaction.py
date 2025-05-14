from typing import Union
import logging
import time
import json
import subprocess
import os

from pycardano import (
    Address, ScriptHash, UTxO
)

from dfctbackend.config import settings
from dfctbackend.cardano.context import get_chain_context
from dfctbackend.cardano.utils import load_plutus_script, str_to_hex, load_raw_file
from dfctbackend.cardano.wallet import CardanoWallet

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
    Class for interacting with Cardano transactions using PyCardano and cardano-cli.
    """
    def __init__(self):
        """Initialize with chain context and load scripts."""
        self.context = get_chain_context()
        self.testnet_magic = 2
        self.socket_path = "/data/node.socket"
        self.assets_path = settings.ASSETS_DIR
        self.docker_container = "cardano-node-preview"
        self.docker_assets = settings.DOCKER_ASSETS_DIR
        self.validator_path = settings.ASSETS_DIR / "dfct-provenance.plutus"
        self.validator_address_path = settings.ASSETS_DIR / "dfct-provenance.addr"
        self.policy_path = settings.ASSETS_DIR / "dfct-minting-policy.plutus"
        self.policy_id_path = settings.ASSETS_DIR / "dfct-minting-policy.id"
        self.validator_address = Address.from_primitive(settings.VALIDATOR_ADDRESS)
        self.policy_id = settings.POLICY_ID
        self.token_name = settings.TOKEN_NAME

    def create_utxo(self, wallet: CardanoWallet, src_utxo: UTxO, new_utxo_amount: int):
        if not src_utxo:
            raise TransactionError(f"Invalid src utxo for {wallet.name}")

        wallet_addr_str = str(wallet.address)
        tx_in = self.create_tx_in(src_utxo)

        # Build transaction to create utxo
        params = [
            "latest", "transaction", "build",
            "--testnet-magic", str(self.testnet_magic),
            "--socket-path", self.socket_path,
            "--change-address", wallet_addr_str,
            "--tx-in", tx_in,
            "--tx-out", f"\"{wallet_addr_str} + {new_utxo_amount} lovelace\"",
            "--out-file", f"{self.docker_assets}/new-utxo.body"
        ]
        self.run_cardano_cli(params)

        # Sign transaction to create utxo
        params = [
            "latest", "transaction", "sign",
            "--tx-body-file", f"{self.docker_assets}/new-utxo.body",
            "--signing-key-file", f"{self.docker_assets}/{wallet.name}.skey",
            "--out-file", f"{self.docker_assets}/new-utxo.signed",
            "--testnet-magic", str(self.testnet_magic)
        ]
        self.run_cardano_cli(params)

        # Submit signed transaction to create utxo
        submit_cmd = [
            "latest", "transaction", "submit",
            "--tx-file", f"{self.docker_assets}/new-utxo.signed",
            "--testnet-magic", str(self.testnet_magic),
            "--socket-path", self.socket_path
        ]
        self.run_cardano_cli(submit_cmd)

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
        Find UTXOs at an address with a minimum amount of lovelace.
        
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

    def get_transaction_hash(self, file_name:str) -> str:
        cmd = ["latest", "transaction", "txid", "--tx-file", file_name]
        logger.info(f"Waiting for transaction hash")
        attempt_nr = 0

        while attempt_nr < MAX_ATTEMPTS:
            try:
                result = self.run_cardano_cli(cmd)
                if result and result.stdout != "" and result.stderr == "":
                    return result.stdout.removesuffix("\n")

                time.sleep(WAIT_DATA_SYNC)
                attempt_nr += 1

            except TransactionError as te:
                return ""

        return ""

    def run_cardano_cli(self, cmd: list[str], check: bool = True) -> subprocess.CompletedProcess:
        """Run a cardano-cli command in the docker container."""
        docker_cmd = ["docker", "exec", self.docker_container, "cardano-cli"] + cmd
        cmd_str = ' '.join(docker_cmd)

        try:
            result = subprocess.run(cmd_str, shell=True, capture_output=True, text=True, check=check)
            if result.stderr and ("unexpected" in result.stderr or "Usage" in result.stderr or "Invalid" in result.stderr):
                raise TransactionError(
                    f"cardano-cli command failed!"
                    f"Command executed: {cmd_str}\nStdout:\n{result.stdout}\nStderr:\n{result.stderr}"
                )

            return result

        except subprocess.CalledProcessError as e:
            raise TransactionError(
                f"cardano-cli command failed: Exit code {e.returncode}, "
                f"Stdout: {e.stdout}, Stderr: {e.stderr}"
            )

    def write_temp_json(self, data: dict, filename: str) -> str:
        """Write a dictionary to a temporary JSON file in assets directory."""
        file_path = os.path.join(self.assets_path, filename)
        with open(file_path, 'w') as f:
            json.dump(data, f, indent=2)
        return file_path

    def get_current_slot_and_validity(self, validity_window: int = 1000) -> tuple[int, int, int]:
        """Get current slot and calculate validity range."""
        tip_cmd = [
            "query", "tip",
            "--testnet-magic", str(self.testnet_magic),
            "--socket-path", self.socket_path
        ]
        tip_result = self.run_cardano_cli(tip_cmd)
        current_slot = json.loads(tip_result.stdout).get("slot", 0)
        invalid_before = current_slot
        invalid_hereafter = current_slot + validity_window
        return current_slot, invalid_before, invalid_hereafter

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
        utxo = self.find_utxos_with_id(id_str)
        
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
        validator_addr_str = str(self.validator_address)
        
        build_cmd = [
            "latest", "transaction", "build",
            "--testnet-magic", str(self.testnet_magic),
            "--socket-path", self.socket_path,
            "--change-address", wallet_addr_str,
            "--tx-in", txin,
            "--tx-in-script-file", f"{self.docker_assets}/dfct-provenance.plutus",
            "--tx-in-inline-datum-present",
            "--tx-in-redeemer-file", f"{self.docker_assets}/{redeemer_file}",
            "--tx-in-collateral", txin_collateral,
            "--required-signer", f"{self.docker_assets}/{wallet.name}.skey",
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
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{output_datum_file}"
            ])
        
        # Add output file
        raw_tx_file = f"{action_name}-{id_str}.raw"
        build_cmd.extend(["--out-file", f"{self.docker_assets}/{raw_tx_file}"])
        
        # Build the transaction
        self.run_cardano_cli(build_cmd)
        
        # Sign the transaction
        signed_tx_file = f"{action_name}-{id_str}.signed"
        sign_cmd = [
            "latest", "transaction", "sign",
            "--tx-body-file", f"{self.docker_assets}/{raw_tx_file}",
            "--signing-key-file", f"{self.docker_assets}/{wallet.name}.skey",
            "--out-file", f"{self.docker_assets}/{signed_tx_file}",
            "--testnet-magic", str(self.testnet_magic)
        ]
        self.run_cardano_cli(sign_cmd)
        
        return signed_tx_file, id_str
