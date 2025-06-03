import subprocess
import json
import time
import logging

logger = logging.getLogger(__name__)

class CardanoCliError(Exception):
    """Exception raised for transaction errors."""
    pass

class CardanoCli:
    """Class to handle cardano-cli commands execution in a Docker container."""
    
    def __init__(self, testnet_magic: int = 2, socket_path: str = "/data/node.socket", 
                 docker_container: str = "cardano-node-preview"):
        self.testnet_magic = testnet_magic
        self.socket_path = socket_path
        self.docker_container = docker_container

    def run_cardano_cli(self, cmd: list[str], check: bool = True) -> subprocess.CompletedProcess:
        """Run a cardano-cli command in the docker container."""
        docker_cmd = ["docker", "exec", self.docker_container, "cardano-cli"] + cmd
        cmd_str = ' '.join(docker_cmd)

        try:
            result = subprocess.run(cmd_str, shell=True, capture_output=True, text=True, check=check)
            if result.stderr and ("unexpected" in result.stderr or "Usage" in result.stderr or "Invalid" in result.stderr):
                raise CardanoCliError(
                    f"cardano-cli command failed!"
                    f"Command executed: {cmd_str}\nStdout:\n{result.stdout}\nStderr:\n{result.stderr}"
                )
            return result

        except subprocess.CalledProcessError as e:
            raise CardanoCliError(
                f"cardano-cli command failed: Exit code {e.returncode}, \n"
                f"Command: {cmd_str}, \n"
                f"Stdout: {e.stdout}, Stderr: {e.stderr}  \n"
            )

    def get_transaction_hash(self, file_name: str, max_attempts: int = 6, wait_data_sync: int = 15) -> str:
        """Get the transaction hash from a signed transaction file."""
        cmd = ["latest", "transaction", "txid", "--tx-file", file_name]
        logger.info(f"Waiting for transaction hash")
        attempt_nr = 0

        while attempt_nr < max_attempts:
            try:
                result = self.run_cardano_cli(cmd)
                if result and result.stdout != "" and result.stderr == "":
                    return result.stdout.removesuffix("\n")
                time.sleep(wait_data_sync)
                attempt_nr += 1

            except CardanoCliError:
                return ""
        return ""

    def build_transaction(self, params: list[str]) -> subprocess.CompletedProcess:
        """Build a transaction using cardano-cli."""
        cmd = [
            "latest", "transaction", "build",
            "--testnet-magic", str(self.testnet_magic),
            "--socket-path", self.socket_path
        ] + params
        return self.run_cardano_cli(cmd)

    def sign_transaction(self, tx_body_file: str, signing_key_file: str, out_file: str) -> subprocess.CompletedProcess:
        """Sign a transaction using cardano-cli."""
        cmd = [
            "latest", "transaction", "sign",
            "--tx-body-file", tx_body_file,
            "--signing-key-file", signing_key_file,
            "--out-file", out_file,
            "--testnet-magic", str(self.testnet_magic)
        ]
        return self.run_cardano_cli(cmd)

    def submit_transaction(self, tx_file: str) -> subprocess.CompletedProcess:
        """Submit a signed transaction using cardano-cli."""
        cmd = [
            "latest", "transaction", "submit",
            "--tx-file", tx_file,
            "--testnet-magic", str(self.testnet_magic),
            "--socket-path", self.socket_path
        ]
        return self.run_cardano_cli(cmd)

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