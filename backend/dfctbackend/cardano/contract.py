import time
from typing import Any, Optional
import logging

from pycardano import (
    Address
)

from dfctbackend.config import settings
from dfctbackend.cardano.datum import TopicStatus, ContributionStatus, DatumProcessor
from dfctbackend.cardano.wallet import CardanoWallet, local_wallets
from dfctbackend.cardano.transaction import (
    CardanoTransaction,
    MAX_ATTEMPTS, WAIT_DATA_SYNC, FUND_FEE
)
from dfctbackend.cardano.utils import (
    generate_contribution_id, str_to_hex, hex_to_str
)
from dfctbackend.cardano.context import get_chain_context

logger = logging.getLogger(__name__)

class ContractError(Exception):
    """Exception raised for contract interaction errors."""
    pass

class ProvenanceContract:
    """
    Class for interacting with the DFCT Provenance Contract on Cardano Preview Testnet.
    Uses pycardano for UTxO queries and cardano-cli for transaction building and submission.
    """
    def __init__(self):
        """Initialize the contract with chain context and transaction handling."""
        try:
            self.context = get_chain_context()
            self.transactions = CardanoTransaction()
            self.dp = DatumProcessor()
            self.policy_id = settings.POLICY_ID
            self.validator_address = Address.from_primitive(settings.VALIDATOR_ADDRESS)
            self.token_name = settings.TOKEN_NAME
            self.token_name_hex = str_to_hex(settings.TOKEN_NAME)
            self.docker_assets = settings.DOCKER_ASSETS_DIR
            self.testnet_magic = self.transactions.testnet_magic
            self.socket_path = self.transactions.socket_path

            logger.info(f"Initialized ProvenanceContract with policy_id: {self.policy_id}, validator_address: {self.validator_address}")

        except Exception as e:
            logger.error(f"Failed to initialize ProvenanceContract: {str(e)}")
            raise ContractError(f"Failed to initialize contract: {str(e)}")

    def submit_topic(self, title: str, description: str, proposer: CardanoWallet, lovelace_amount: int, reward_amount: int = 1000) -> dict[str, str]:
        """
        Submit a new topic to the provenance validator using cardano-cli, following a two-step process:
        1. Send tokens and datum to validator address
        2. Consume the UTxO to submit the topic
        """
        try:
            # Step 1: Prepare topic datum and send tokens to validator
            topic_datum, redeemer, topic_id = self.dp.prepare_topic_datum_redeemer(
                title=title,
                description=description,
                proposer_pkh=proposer.public_key_hash,
                reward_amount=reward_amount
            )

            new_topic_file = f"new-topic-datum-{topic_id}.json"
            redeemer_file = f"new-topic-redeemer-{topic_id}.json"

            # Write datum and redeemer to temporary files
            self.transactions.write_temp_json(topic_datum, new_topic_file)
            self.transactions.write_temp_json(redeemer, redeemer_file)

            # Find UTxO with DFC tokens using pycardano
            token_utxo = self.transactions.find_token_utxo(
                proposer.address,
                self.policy_id,
                self.token_name,
                love_amount=lovelace_amount,
                dfc_amount=reward_amount
            )

            if not token_utxo:
                raise ContractError(f"No UTxO with {self.token_name} tokens found for {proposer.name}")

            txin = self.transactions.create_tx_in(token_utxo)

            # txin to fund tx fee
            txin_fee, fee_utxo = self.transactions.find_utxo_and_create_tx_in(
                wallet=proposer,
                min_lovelace=FUND_FEE,
                exclude=[token_utxo]
            )
            if not fee_utxo:
                raise ContractError(f"{proposer.name} does not have a UTxO to fund fee to submit_topic {topic_id}")

            # Build transaction to send tokens and datum to validator
            validator_addr_str = str(self.validator_address)
            token_output = f'{reward_amount} {self.policy_id}.{self.token_name_hex}'
            tx_out = f"{validator_addr_str}+{lovelace_amount}+\"{token_output}\""
            proposer_addr_str = str(proposer.address)

            build_cmd = [
                "latest", "transaction", "build",
                "--testnet-magic", str(self.testnet_magic),
                "--socket-path", self.socket_path,
                "--change-address", proposer_addr_str,
                "--tx-in", txin_fee,
                "--tx-in", txin,
                "--tx-out", tx_out,
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{new_topic_file}",
                "--out-file", f"{self.docker_assets}/send-dfc-new-topic-to-provenance-{topic_id}.raw"
            ]
            logger.info(f"Parameters for creating topic_id {topic_id}:\n{build_cmd}")
            result = self.transactions.run_cardano_cli(build_cmd)
            logger.info(f"Transaction built. topic_id: {topic_id} output: {result.stdout}")

            # Sign transaction
            sign_cmd = [
                "latest", "transaction", "sign",
                "--tx-body-file", f"{self.docker_assets}/send-dfc-new-topic-to-provenance-{topic_id}.raw",
                "--signing-key-file", f"{self.docker_assets}/{proposer.name}.skey",
                "--out-file", f"{self.docker_assets}/send-dfc-new-topic-to-provenance-{topic_id}.signed",
                "--testnet-magic", str(self.testnet_magic)
            ]
            self.transactions.run_cardano_cli(sign_cmd)
            logger.info(f"Transaction signed to submit topic {topic_id}")

            # Submit transaction
            submit_cmd = [
                "latest", "transaction", "submit",
                "--tx-file", f"{self.docker_assets}/send-dfc-new-topic-to-provenance-{topic_id}.signed",
                "--testnet-magic", str(self.testnet_magic),
                "--socket-path", self.socket_path
            ]
            self.transactions.run_cardano_cli(submit_cmd)
            logger.info(f"Transaction to send tokens and datum to validator for topic {topic_id} was submitted")

            # Step 2: Consume the UTxO to submit the topic
            # Wait for confirmation
            topic_utxo, _ = self.get_utxo_and_datum(topic_id)
            if not topic_utxo:
                raise ContractError(f"No UTxO found for id {topic_id}")

            # Find collateral UTxO
            txin_collateral, collateral_utxo = self.transactions.find_utxo_and_create_tx_in(
                wallet=proposer,
                min_lovelace=5000000,
                exclude=[token_utxo]
            )
            if not collateral_utxo:
                raise ContractError(f"{proposer.name} does not have a collateral UTxO to submit_topic {topic_id}")

            # txin to fund tx fee
            txin_fee, fee_utxo = self.transactions.find_utxo_and_create_tx_in(
                wallet=proposer,
                min_lovelace=FUND_FEE,
                exclude=[topic_utxo, collateral_utxo]
            )
            if not fee_utxo:
                raise ContractError(f"{proposer.name} does not have a UTxO to fund fee to submit_topic {topic_id}")

            # Get transaction input for topic UTxO
            txin_with_tokens = self.transactions.create_tx_in(topic_utxo)

            # Get current slot and validity range
            _, invalid_before, invalid_hereafter = self.transactions.get_current_slot_and_validity()

            # Prepare output
            topic_lovelace = topic_utxo.output.amount.coin
            tx_out_submit = f"{validator_addr_str}+{topic_lovelace}+\"{token_output}\""

            # Build transaction to submit topic
            build_submit_cmd = [
                "latest", "transaction", "build",
                "--testnet-magic", str(self.testnet_magic),
                "--socket-path", self.socket_path,
                "--change-address", proposer_addr_str,
                "--tx-in", txin_fee,
                "--tx-in", txin_with_tokens,
                "--tx-in-script-file", f"{self.docker_assets}/dfct-provenance.plutus",
                "--tx-in-inline-datum-present",
                "--tx-in-redeemer-file", f"{self.docker_assets}/{redeemer_file}",
                "--tx-in-collateral", txin_collateral,
                "--required-signer", f"{self.docker_assets}/{proposer.name}.skey",
                "--invalid-before", str(invalid_before),
                "--invalid-hereafter", str(invalid_hereafter),
                "--tx-out", tx_out_submit,
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{new_topic_file}",
                "--out-file", f"{self.docker_assets}/new-topic-{topic_id}.raw"
            ]
            logger.info(f"Parameters for submiting topic_id {topic_id}:\n{build_cmd}")
            self.transactions.run_cardano_cli(build_submit_cmd)

            # Sign transaction
            sign_submit_cmd = [
                "latest", "transaction", "sign",
                "--tx-body-file", f"{self.docker_assets}/new-topic-{topic_id}.raw",
                "--signing-key-file", f"{self.docker_assets}/{proposer.name}.skey",
                "--out-file", f"{self.docker_assets}/new-topic-{topic_id}.signed",
                "--testnet-magic", str(self.testnet_magic)
            ]
            self.transactions.run_cardano_cli(sign_submit_cmd)

            # Submit transaction
            submit_submit_cmd = [
                "latest", "transaction", "submit",
                "--tx-file", f"{self.docker_assets}/new-topic-{topic_id}.signed",
                "--testnet-magic", str(self.testnet_magic),
                "--socket-path", self.socket_path
            ]
            self.transactions.run_cardano_cli(submit_submit_cmd)
            transaction_hash = self.transactions.get_transaction_hash(
                f"{self.docker_assets}/new-topic-{topic_id}.signed"
            )

            logger.info(f"Topic submitted. Topic ID: {topic_id}")
            return {
                "transaction_hash": f"{transaction_hash}",  # Note: We don't extract tx_id
                "topic_id": topic_id
            }

        except Exception as e:
            logger.error(f"Failed to submit topic: {str(e)}")
            raise ContractError(f"Failed to submit topic: {str(e)}")

    def get_topic(self, topic_id: str) -> Optional[dict[str, Any]]:
        try:
            _, datum = self.get_utxo_and_datum(topic_id)
            if datum:
                return self.dp.extract_topic_from_datum(datum)

            return None

        except Exception as e:
            logger.error(f"Failed to get topic: {str(e)}")
            return None

    def get_topics(self) -> list[dict[str, Any]]:
        try:
            all_topics = []

            utxos = self.transactions.find_utxos_at_address(self.validator_address)
            for utxo in utxos:
                topic_datum = self.dp.decode_utxo_datum(utxo)
                if topic_datum:
                    topic = self.dp.extract_topic_from_datum(topic_datum)
                    if topic:
                        all_topics.append(topic)

            return all_topics

        except Exception as e:
            logger.error(f"Failed to get topics: {str(e)}")
            return []

    def review_topic(self, topic_id: str, approved: bool, reviewer: CardanoWallet) -> dict[str, str]:
        """
        Review a proposed topic on the blockchain.
        """
        try:
            # Check if the topic exists and is in proposed state
            topic_utxo, datum = self.get_utxo_and_datum(topic_id)
            if not topic_utxo:
                raise ContractError(f"Utxos for topic_id {topic_id} not found")

            topic = self.dp.extract_topic_from_datum(datum)
            topic_status = topic["status"]
            if topic_status != TopicStatus.PROPOSED:
                raise ContractError(f"Topic with ID {topic_id} is not in proposed state. Current status is {topic_status}")

            # Prepare the review topic redeemer
            redeemer = self.dp.prepare_review_topic_redeemer(
                topic_id=topic_id,
                approved=approved
            )

            # Write redeemer to temporary file
            redeemer_file = f"review-topic-redeemer-{topic_id}.json"
            self.transactions.write_temp_json(redeemer, redeemer_file)

            # Find collateral UTxO
            txin_collateral, collateral_utxo = self.transactions.find_utxo_and_create_tx_in(
                wallet=reviewer,
                min_lovelace=5000000,
                exclude=[topic_utxo]
            )
            if not collateral_utxo:
                raise ContractError(f"{reviewer.name} does not have a collateral UTxO to submit_topic {topic_id}")

            # txin to fund tx fee
            txin_fee, fee_utxo = self.transactions.find_utxo_and_create_tx_in(
                wallet=reviewer,
                min_lovelace=FUND_FEE,
                exclude=[topic_utxo, collateral_utxo]
            )
            if not fee_utxo:
                raise ContractError(f"{reviewer.name} does not have a UTxO to fund fee to submit_topic {topic_id}")

            # Get transaction input for topic UTxO
            txin_topic = self.transactions.create_tx_in(topic_utxo)

            # Get current slot and validity range
            _, invalid_before, invalid_hereafter = self.transactions.get_current_slot_and_validity()

            updated_datum, _ = self.dp.prepare_topic_json(datum, TopicStatus.REVIEWED.value)
            datum_file = f"reviewed-topic-datum-{topic_id}.json"
            self.transactions.write_temp_json(updated_datum, datum_file)

            # Set up transaction outputs
            validator_addr_str = str(self.validator_address)
            reviewer_addr_str = str(reviewer.address)

            reward_amount = self.transactions.get_cnt_amount(
                topic_utxo,
                self.policy_id
            )

            token_output = f'{reward_amount} {self.policy_id}.{self.token_name_hex}'
            topic_lovelace = topic_utxo.output.amount.coin
            tx_out = f"{validator_addr_str}+{topic_lovelace}+\"{token_output}\""

            # Build transaction to review topic
            build_cmd = [
                "latest", "transaction", "build",
                "--testnet-magic", str(self.testnet_magic),
                "--socket-path", self.socket_path,
                "--change-address", reviewer_addr_str,
                "--tx-in", txin_fee,
                "--tx-in", txin_topic,
                "--tx-in-script-file", f"{self.docker_assets}/dfct-provenance.plutus",
                "--tx-in-inline-datum-present",
                "--tx-in-redeemer-file", f"{self.docker_assets}/{redeemer_file}",
                "--tx-in-collateral", txin_collateral,
                "--required-signer", f"{self.docker_assets}/{reviewer.name}.skey",
                "--invalid-before", str(invalid_before),
                "--invalid-hereafter", str(invalid_hereafter),
                "--tx-out", tx_out,
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{datum_file}",
                "--out-file", f"{self.docker_assets}/review-topic-{topic_id}.raw"
            ]
            logger.info(f"Parameters for revewing topic_id {topic_id}:\n{build_cmd}")
            self.transactions.run_cardano_cli(build_cmd)

            # Sign transaction
            sign_cmd = [
                "latest", "transaction", "sign",
                "--tx-body-file", f"{self.docker_assets}/review-topic-{topic_id}.raw",
                "--signing-key-file", f"{self.docker_assets}/{reviewer.name}.skey",
                "--out-file", f"{self.docker_assets}/review-topic-{topic_id}.signed",
                "--testnet-magic", str(self.testnet_magic)
            ]
            self.transactions.run_cardano_cli(sign_cmd)

            # Submit transaction
            submit_cmd = [
                "latest", "transaction", "submit",
                "--tx-file", f"{self.docker_assets}/review-topic-{topic_id}.signed",
                "--testnet-magic", str(self.testnet_magic),
                "--socket-path", self.socket_path
            ]
            self.transactions.run_cardano_cli(submit_cmd)
            transaction_hash = self.transactions.get_transaction_hash(
                f"{self.docker_assets}/review-topic-{topic_id}.signed"
            )

            logger.info(f"Topic {topic_id} review sutmitted")
            return {
                "transaction_hash": f"{transaction_hash}",
                "topic_id": topic_id,
                "approved": approved
            }

        except Exception as e:
            logger.error(f"Failed to review topic: {str(e)}")
            raise ContractError(f"Failed to review topic: {str(e)}")

    def activate_topic(self, topic_id: str, wallet: CardanoWallet) -> dict[str, str]:
        """
        Activate a reviewed topic on the blockchain.
        """
        try:
            # Check if the topic exists and is in reviewed state
            topic_utxo, datum = self.get_utxo_and_datum(topic_id)
            if not topic_utxo:
                raise ContractError(f"Topic with ID {topic_id} not found")

            topic = self.dp.extract_topic_from_datum(datum)
            topic_status = topic["status"]
            if topic_status != TopicStatus.REVIEWED:
                raise ContractError(f"Topic with ID {topic_id} is not in reviewed state. Current status is {topic_status}")

            # Prepare the activate topic redeemer
            redeemer = self.dp.prepare_topic_action_redeemer(
                topic_id=topic_id,
                action_constructor=TopicStatus.ACTIVATED.value
            )

            # Write redeemer to temporary file
            redeemer_file = f"activate-topic-redeemer-{topic_id}.json"
            self.transactions.write_temp_json(redeemer, redeemer_file)

            # Find collateral UTxO
            txin_collateral, collateral_utxo = self.transactions.find_utxo_and_create_tx_in(
                wallet=wallet,
                min_lovelace=5000000,
                exclude=[topic_utxo]
            )
            if not collateral_utxo:
                raise ContractError(f"{wallet.name} does not have a collateral UTxO to submit_topic {topic_id}")

            # txin to fund tx fee
            txin_fee, fee_utxo = self.transactions.find_utxo_and_create_tx_in(
                wallet=wallet,
                min_lovelace=FUND_FEE,
                exclude=[topic_utxo, collateral_utxo]
            )
            if not fee_utxo:
                raise ContractError(f"{wallet.name} does not have a UTxO to fund fee to submit_topic {topic_id}")

            # Get transaction input for topic UTxO
            txin_topic = self.transactions.create_tx_in(topic_utxo)

            # Get current slot and validity range
            _, invalid_before, invalid_hereafter = self.transactions.get_current_slot_and_validity()

            # Prepare updated datum in Plutus-compatible format (TopicDatum)
            updated_datum, _ = self.dp.prepare_topic_json(datum, TopicStatus.ACTIVATED.value)
            datum_file = f"activated-topic-datum-{topic_id}.json"
            self.transactions.write_temp_json(updated_datum, datum_file)

            # Set up transaction outputs
            validator_addr_str = str(self.validator_address)
            wallet_addr_str = str(wallet.address)

            reward_amount = self.transactions.get_cnt_amount(
                topic_utxo,
                self.policy_id
            )
            token_output = f'{reward_amount} {self.policy_id}.{self.token_name_hex}'
            topic_lovelace = topic_utxo.output.amount.coin
            tx_out = f"{validator_addr_str}+{topic_lovelace}+\"{token_output}\""

            # Build transaction to activate topic
            build_cmd = [
                "latest", "transaction", "build",
                "--testnet-magic", str(self.testnet_magic),
                "--socket-path", self.socket_path,
                "--change-address", wallet_addr_str,
                "--tx-in", txin_fee,
                "--tx-in", txin_topic,
                "--tx-in-script-file", f"{self.docker_assets}/dfct-provenance.plutus",
                "--tx-in-inline-datum-present",
                "--tx-in-redeemer-file", f"{self.docker_assets}/{redeemer_file}",
                "--tx-in-collateral", txin_collateral,
                "--required-signer", f"{self.docker_assets}/{wallet.name}.skey",
                "--invalid-before", str(invalid_before),
                "--invalid-hereafter", str(invalid_hereafter),
                "--tx-out", tx_out,
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{datum_file}",
                "--out-file", f"{self.docker_assets}/activate-topic-{topic_id}.raw"
            ]
            logger.info(f"Parameters for activating topic_id {topic_id}:\n{build_cmd}")
            self.transactions.run_cardano_cli(build_cmd)

            # Sign transaction
            sign_cmd = [
                "latest", "transaction", "sign",
                "--tx-body-file", f"{self.docker_assets}/activate-topic-{topic_id}.raw",
                "--signing-key-file", f"{self.docker_assets}/{wallet.name}.skey",
                "--out-file", f"{self.docker_assets}/activate-topic-{topic_id}.signed",
                "--testnet-magic", str(self.testnet_magic)
            ]
            self.transactions.run_cardano_cli(sign_cmd)

            # Submit transaction
            submit_cmd = [
                "latest", "transaction", "submit",
                "--tx-file", f"{self.docker_assets}/activate-topic-{topic_id}.signed",
                "--testnet-magic", str(self.testnet_magic),
                "--socket-path", self.socket_path
            ]
            self.transactions.run_cardano_cli(submit_cmd)
            transaction_hash = self.transactions.get_transaction_hash(
                f"{self.docker_assets}/activate-topic-{topic_id}.signed"
            )

            logger.info(f"Topic {topic_id} activation submitted")
            return {
                "transaction_hash": f"{transaction_hash}",
                "topic_id": topic_id
            }

        except Exception as e:
            logger.error(f"Failed to activate topic: {str(e)}")
            raise ContractError(f"Failed to activate topic: {str(e)}")

    def close_topic(self, topic_id: str, wallet: CardanoWallet) -> dict[str, str]:
        """
        Close an activated topic on the blockchain.
        """
        try:
            # Check if the topic exists and is in activated state
            topic_utxo, datum = self.get_utxo_and_datum(topic_id)
            if not topic_utxo:
                raise ContractError(f"Topic with ID {topic_id} not found")

            topic = self.dp.extract_topic_from_datum(datum)
            topic_status = topic["status"]
            if topic_status != TopicStatus.ACTIVATED:
                raise ContractError(f"Topic with ID {topic_id} is not in activated state. Current state is {topic_status}")

            # Prepare the close topic redeemer
            redeemer = self.dp.prepare_topic_action_redeemer(
                topic_id=topic_id,
                action_constructor=3  # CloseTopic
            )

            # Write redeemer to temporary file
            redeemer_file = f"close-topic-redeemer-{topic_id}.json"
            self.transactions.write_temp_json(redeemer, redeemer_file)

            # Find collateral UTxO
            txin_collateral, collateral_utxo = self.transactions.find_utxo_and_create_tx_in(
                wallet=wallet,
                min_lovelace=5000000,
                exclude=[topic_utxo]
            )
            if not collateral_utxo:
                raise ContractError(f"{wallet.name} does not have a collateral UTxO to submit_topic {topic_id}")

            # txin to fund tx fee
            txin_fee, fee_utxo = self.transactions.find_utxo_and_create_tx_in(
                wallet=wallet,
                min_lovelace=FUND_FEE,
                exclude=[topic_utxo, collateral_utxo]
            )
            if not fee_utxo:
                raise ContractError(f"{wallet.name} does not have a UTxO to fund fee to submit_topic {topic_id}")

            # Get transaction input for topic UTxO
            txin_topic = self.transactions.create_tx_in(topic_utxo)

            # Get current slot and validity range
            _, invalid_before, invalid_hereafter = self.transactions.get_current_slot_and_validity()

            # Prepare updated datum in Plutus-compatible format (TopicDatum)
            updated_datum, _ = self.dp.prepare_topic_json(datum, TopicStatus.CLOSED.value)
            datum_file = f"closed-topic-datum-{topic_id}.json"
            self.transactions.write_temp_json(updated_datum, datum_file)

            # Set up transaction outputs
            validator_addr_str = str(self.validator_address)
            wallet_addr_str = str(wallet.address)

            reward_amount = self.transactions.get_cnt_amount(
                topic_utxo,
                self.policy_id
            )
            token_output = f'{reward_amount} {self.policy_id}.{self.token_name_hex}'
            topic_lovelace = topic_utxo.output.amount.coin
            tx_out = f"{validator_addr_str}+{topic_lovelace}+\"{token_output}\""

            # Build transaction to close topic
            build_cmd = [
                "latest", "transaction", "build",
                "--testnet-magic", str(self.testnet_magic),
                "--socket-path", self.socket_path,
                "--change-address", wallet_addr_str,
                "--tx-in", txin_fee,
                "--tx-in", txin_topic,
                "--tx-in-script-file", f"{self.docker_assets}/dfct-provenance.plutus",
                "--tx-in-inline-datum-present",
                "--tx-in-redeemer-file", f"{self.docker_assets}/{redeemer_file}",
                "--tx-in-collateral", txin_collateral,
                "--required-signer", f"{self.docker_assets}/{wallet.name}.skey",
                "--invalid-before", str(invalid_before),
                "--invalid-hereafter", str(invalid_hereafter),
                "--tx-out", tx_out,
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{datum_file}",
                "--out-file", f"{self.docker_assets}/close-topic-{topic_id}.raw"
            ]
            logger.info(f"Parameters for closing topic_id {topic_id}:\n{build_cmd}")
            self.transactions.run_cardano_cli(build_cmd)

            # Sign transaction
            sign_cmd = [
                "latest", "transaction", "sign",
                "--tx-body-file", f"{self.docker_assets}/close-topic-{topic_id}.raw",
                "--signing-key-file", f"{self.docker_assets}/{wallet.name}.skey",
                "--out-file", f"{self.docker_assets}/close-topic-{topic_id}.signed",
                "--testnet-magic", str(self.testnet_magic)
            ]
            self.transactions.run_cardano_cli(sign_cmd)

            # Submit transaction
            submit_cmd = [
                "latest", "transaction", "submit",
                "--tx-file", f"{self.docker_assets}/close-topic-{topic_id}.signed",
                "--testnet-magic", str(self.testnet_magic),
                "--socket-path", self.socket_path
            ]
            self.transactions.run_cardano_cli(submit_cmd)
            transaction_hash = self.transactions.get_transaction_hash(
                f"{self.docker_assets}/close-topic-{topic_id}.signed"
            )

            logger.info(f"Topic {topic_id} close request submitted")
            return {
                "transaction_hash": f"{transaction_hash}",
                "topic_id": topic_id
            }

        except Exception as e:
            logger.error(f"Failed to close topic: {str(e)}")
            raise ContractError(f"Failed to close topic: {str(e)}")


    def get_contribution(self, contribution_id: str) -> Optional[dict[str, Any]]:
        """
        Retrieve a contribution by its ID.
        """
        try:
            contribution_utxo, datum = self.get_utxo_and_datum(contribution_id)
            if contribution_utxo and datum:
                return self.dp.extract_contribution_from_datum(datum)

            return None

        except Exception as e:
            logger.error(f"Failed to get contribution: {str(e)}")
            return None

    def get_contributions_for_topic(self, topic_id: str) -> list[dict[str, Any]]:
        """
        Get all contributions for a specific topic.
        """
        try:
            validator_utxos = self.transactions.find_utxos_at_address(self.validator_address)
            contributions = []
            for utxo in validator_utxos:
                datum = self.dp.decode_utxo_datum(utxo)
                if not datum:
                    continue

                # Check if this might be a contribution by examining structure
                if len(datum) > 4 and isinstance(datum[0], list) and len(datum[0]) >= 7:
                    contribution = self.dp.extract_contribution_from_datum(datum)
                    if not contribution:
                        continue

                    # Compare both as hex and as string to be flexible
                    if contribution["topic_id"] == topic_id:
                        contributions.append(contribution)

            return contributions

        except Exception as e:
            logger.error(f"Failed to get contributions for topic: {str(e)}")
            return []

    def submit_contribution(self, topic_id: str, content: str, contributor: CardanoWallet) -> dict[str, str]:
        """
        Submit a contribution to an activated topic.
        """
        try:
            # Check if the topic exists and is in proposed state
            topic_utxo, datum = self.get_utxo_and_datum(topic_id)
            if not topic_utxo:
                raise ContractError(f"Utxos for topic_id {topic_id} not found")

            topic = self.dp.extract_topic_from_datum(datum)
            topic_status = topic["status"]
            if topic_status != TopicStatus.ACTIVATED:
                raise ContractError(f"Topic with ID {topic_id} is not in activated state. Current state is {topic_status}")

            # Prepare datum and redeemer of the new contribution
            contrib_datum, contrib_redeemer, contribution_id = self.dp.prepare_contribution_datum_redeemer(
                contribution_id=generate_contribution_id(),
                topic_id=topic_id,
                contribution_type="evidence",
                content=content,
                contributor_pkh=contributor.public_key_hash,
                contribution_status=ContributionStatus.EVIDENCE_PROPOSED.value,
                timestamp=int(time.time() * 1000)
            )

            # Write datum and redeemer to temporary files
            contrib_datum_file = f"contribution-datum-{contribution_id}.json"
            contrib_redeemer_file = f"contribution-redeemer-{contribution_id}.json"
            self.transactions.write_temp_json(contrib_datum, contrib_datum_file)
            self.transactions.write_temp_json(contrib_redeemer, contrib_redeemer_file)

            # Find collateral UTxO
            txin_collateral, collateral_utxo = self.transactions.find_utxo_and_create_tx_in(
                wallet=contributor,
                min_lovelace=5000000,
                exclude=[topic_utxo]
            )
            if not collateral_utxo:
                raise ContractError(f"{contributor.name} does not have a collateral UTxO to submit_topic {topic_id}")

            # txin to fund tx fee
            txin_fee, fee_utxo = self.transactions.find_utxo_and_create_tx_in(
                wallet=contributor,
                min_lovelace=FUND_FEE,
                exclude=[topic_utxo, collateral_utxo]
            )
            if not fee_utxo:
                raise ContractError(f"{contributor.name} does not have a UTxO to fund fee to submit_topic {topic_id}")

            # Get transaction input for topic UTxO
            txin_topic = self.transactions.create_tx_in(topic_utxo)

            # Get current slot and validity range
            _, invalid_before, invalid_hereafter = self.transactions.get_current_slot_and_validity()

            # Set up transaction outputs
            validator_addr_str = str(self.validator_address)
            contributor_addr_str = str(contributor.address)

            # Output for contribution
            tx_out = f"{validator_addr_str}+{FUND_FEE}"

            # Output for topic UTxO (reusing the same datum and token amount)
            reward_amount = self.transactions.get_cnt_amount(
                topic_utxo,
                self.policy_id
            )
            token_output = f'{reward_amount} {self.policy_id}.{self.token_name_hex}'
            topic_lovelace = topic_utxo.output.amount.coin
            tx_out_topic = f"{validator_addr_str}+{topic_lovelace}+\"{token_output}\""

            # Write topic datum to temporary file (reusing the existing topic datum)
            plutus_datum, _ = self.dp.prepare_topic_json(datum, topic["status"].value)
            topic_datum_file = f"topic-datum-{topic_id}.json"
            self.transactions.write_temp_json(plutus_datum, topic_datum_file)

            # Build transaction to review topic
            build_cmd = [
                "latest", "transaction", "build",
                "--testnet-magic", str(self.testnet_magic),
                "--socket-path", self.socket_path,
                "--change-address", contributor_addr_str,
                "--tx-in", txin_fee,
                "--tx-in", txin_topic,
                "--tx-in-script-file", f"{self.docker_assets}/dfct-provenance.plutus",
                "--tx-in-inline-datum-present",
                "--tx-in-redeemer-file", f"{self.docker_assets}/{contrib_redeemer_file}",
                "--tx-in-collateral", txin_collateral,
                "--required-signer", f"{self.docker_assets}/{contributor.name}.skey",
                "--invalid-before", str(invalid_before),
                "--invalid-hereafter", str(invalid_hereafter),
                "--tx-out", tx_out,
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{contrib_datum_file}",
                "--tx-out", tx_out_topic,
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{topic_datum_file}",
                "--out-file", f"{self.docker_assets}/submit-contribution-{topic_id}.raw"
            ]
            logger.info(f"Parameters for submiting a contribution to topic_id {topic_id}:\n{build_cmd}")
            self.transactions.run_cardano_cli(build_cmd)

            # Sign transaction
            sign_cmd = [
                "latest", "transaction", "sign",
                "--tx-body-file", f"{self.docker_assets}/submit-contribution-{topic_id}.raw",
                "--signing-key-file", f"{self.docker_assets}/{contributor.name}.skey",
                "--out-file", f"{self.docker_assets}/submit-contribution-{topic_id}.signed",
                "--testnet-magic", str(self.testnet_magic)
            ]
            self.transactions.run_cardano_cli(sign_cmd)

            # Submit transaction
            submit_cmd = [
                "latest", "transaction", "submit",
                "--tx-file", f"{self.docker_assets}/submit-contribution-{topic_id}.signed",
                "--testnet-magic", str(self.testnet_magic),
                "--socket-path", self.socket_path
            ]
            self.transactions.run_cardano_cli(submit_cmd)
            transaction_hash = self.transactions.get_transaction_hash(
                f"{self.docker_assets}/submit-contribution-{topic_id}.signed"
            )

            logger.info(f"Contribution {contribution_id} for topic {topic_id} submitted")
            return {
                "transaction_hash": f"{transaction_hash}",
                "topic_id": topic_id,
                "contribution_id": contribution_id
            }

        except Exception as e:
            logger.error(f"Failed to submit contribution: {str(e)}")
            raise ContractError(f"Failed to submit contribution: {str(e)}")

    def review_contribution(
        self,
        contribution_id: str,
        reviewer: CardanoWallet,
        relevance: int,
        accuracy: int,
        completeness: int,
        review_content: str
    ) -> dict[str, str]:
        """
        Review a contribution with scores and feedback.
        """
        try:
            # First, find the contribution and validate it's in proper state
            contrib_utxo, datum = self.get_utxo_and_datum(contribution_id)
            if not contrib_utxo:
                raise ContractError(f"Contribution with ID {contribution_id} not found")

            contribution = self.dp.extract_contribution_from_datum(datum)
            contrib_status = contribution["status"]
            if contrib_status != ContributionStatus.EVIDENCE_PROPOSED:
                raise ContractError(f"Contribution with ID {contribution_id} is not in proposed state. Current state is {contrib_status}")

            # Get topic data
            topic_id = contribution["topic_id"]
            topic_utxo, _ = self.get_utxo_and_datum(topic_id)
            if not topic_utxo:
                raise ContractError(f"Topic UTxO for topic {topic_id} for contribution with ID {contribution_id} not found")

            # Prepare the review contribution redeemer
            redeemer = self.dp.prepare_review_contribution_redeemer(
                contribution_id=contribution_id,
                reviewer_pkh=reviewer.public_key_hash,
                relevance=relevance,
                accuracy=accuracy,
                completeness=completeness,
                review_content=review_content
            )

            # Write redeemer to temporary file
            redeemer_file = f"review-contribution-redeemer-{contribution_id}.json"
            self.transactions.write_temp_json(redeemer, redeemer_file)

            # Find collateral UTxO
            txin_collateral, collateral_utxo = self.transactions.find_utxo_and_create_tx_in(
                wallet=reviewer,
                min_lovelace=5000000,
                exclude=[topic_utxo, contrib_utxo]
            )
            if not collateral_utxo:
                raise ContractError(f"{reviewer.name} does not have a collateral UTxO to review contribution {contribution_id}")

            # txin to fund tx fee
            txin_fee, fee_utxo = self.transactions.find_utxo_and_create_tx_in(
                wallet=reviewer,
                min_lovelace=FUND_FEE,
                exclude=[topic_utxo, collateral_utxo, contrib_utxo]
            )
            if not fee_utxo:
                raise ContractError(f"{reviewer.name} does not have a UTxO to fund fee to review contribution {contribution_id}")

            # Get transaction inputs
            txin_contrib = self.transactions.create_tx_in(contrib_utxo)
            txin_topic = self.transactions.create_tx_in(topic_utxo)

            # Get current slot and validity range
            _, invalid_before, invalid_hereafter = self.transactions.get_current_slot_and_validity()

            # Create review content structure
            current_time = int(time.time() * 1000)
            review_content_obj = {
                "constructor": 0,
                "fields": [
                    {"bytes": reviewer.public_key_hash},
                    {"bytes": str_to_hex(contribution_id)},
                    {"bytes": str_to_hex("Relevance: " + review_content)},
                    {"bytes": str_to_hex("Accuracy: " + review_content)},
                    {"bytes": str_to_hex("Completeness: " + review_content)},
                    {"int": current_time}
                ]
            }

            dispute_reason_obj = {
                "constructor": 0,
                "fields": [
                    {"bytes": ""},  # Empty initiator
                    {"bytes": ""},  # Empty reason
                    {"int": 0}      # Zero timestamp
                ]
            }

            dispute_reason = contribution.get("dispute_reason")
            if dispute_reason:
                dispute_reason_obj = {
                    "constructor": 0,
                    "fields": [
                        {"bytes": dispute_reason.get("initiator", "")},
                        {"bytes": str_to_hex(dispute_reason.get("reason", ""))},
                        {"int": dispute_reason.get("timestamp", 0)}
                    ]
                }

            updated_contrib_datum = {
                "constructor": 0,
                "fields": [
                    # Contribution details (index 0)
                    {
                        "constructor": 0,
                        "fields": [
                            {"bytes": str_to_hex(contribution_id)},
                            {"bytes": str_to_hex(topic_id)},
                            {"bytes": str_to_hex(contribution["type"])},
                            {"bytes": str_to_hex(contribution["content"])},
                            {"bytes": contribution["creator"]},
                            {"int": contribution["timestamp"]},
                            {"int": contribution["version"]},
                            {"bytes": str_to_hex(contribution.get("previous_version_id", ""))}
                        ]
                    },
                    # Status (index 1)
                    {
                        "constructor": ContributionStatus.CONTRIBUTION_REVIEWED.value,
                        "fields": []
                    },
                    # Scores
                    {"int": relevance},   # relevance (index 2)
                    {"int": accuracy},    # accuracy (index 3)
                    {"int": completeness}, # completeness (index 4)
                    review_content_obj, # reviewContent (index 5)
                    dispute_reason_obj, # disputeReason (index 6)
                    {"int": contribution.get("timeliness_score", 0)}  # Preserve existing timelinessScore
                ]
            }

            datum_file = f"reviewed-contribution-datum-{contribution_id}.json"
            self.transactions.write_temp_json(updated_contrib_datum, datum_file)

            # Set up transaction outputs
            validator_addr_str = str(self.validator_address)
            reviewer_addr_str = str(reviewer.address)

            # Output for contribution with updated scores
            contrib_lovelace = contrib_utxo.output.amount.coin
            tx_out_contrib = f"{validator_addr_str}+{contrib_lovelace}"

            # Build transaction to review contribution
            build_cmd = [
                "latest", "transaction", "build",
                "--testnet-magic", str(self.testnet_magic),
                "--socket-path", self.socket_path,
                "--change-address", reviewer_addr_str,
                "--tx-in", txin_fee,
                "--tx-in", txin_contrib,
                "--tx-in-script-file", f"{self.docker_assets}/dfct-provenance.plutus",
                "--tx-in-inline-datum-present",
                "--tx-in-redeemer-file", f"{self.docker_assets}/{redeemer_file}",
                "--read-only-tx-in-reference", txin_topic,
                "--tx-in-collateral", txin_collateral,
                "--required-signer", f"{self.docker_assets}/{reviewer.name}.skey",
                "--invalid-before", str(invalid_before),
                "--invalid-hereafter", str(invalid_hereafter),
                "--tx-out", tx_out_contrib,
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{datum_file}",
                "--out-file", f"{self.docker_assets}/review-contribution-{contribution_id}.raw"
            ]
            logger.info(f"Parameters for reviewing contribution {contribution_id} for topic {topic_id}:\n{build_cmd}")
            self.transactions.run_cardano_cli(build_cmd)

            # Sign transaction
            sign_cmd = [
                "latest", "transaction", "sign",
                "--tx-body-file", f"{self.docker_assets}/review-contribution-{contribution_id}.raw",
                "--signing-key-file", f"{self.docker_assets}/{reviewer.name}.skey",
                "--out-file", f"{self.docker_assets}/review-contribution-{contribution_id}.signed",
                "--testnet-magic", str(self.testnet_magic)
            ]
            self.transactions.run_cardano_cli(sign_cmd)

            # Submit transaction
            submit_cmd = [
                "latest", "transaction", "submit",
                "--tx-file", f"{self.docker_assets}/review-contribution-{contribution_id}.signed",
                "--testnet-magic", str(self.testnet_magic),
                "--socket-path", self.socket_path
            ]
            self.transactions.run_cardano_cli(submit_cmd)
            transaction_hash = self.transactions.get_transaction_hash(
                f"{self.docker_assets}/review-contribution-{contribution_id}.signed"
            )

            logger.info(f"Contribution {contribution_id} reviewed with scores - Relevance: {relevance}, Accuracy: {accuracy}, Completeness: {completeness}")
            return {
                "transaction_hash": f"{transaction_hash}",
                "contribution_id": contribution_id,
                "relevance": relevance,
                "accuracy": accuracy,
                "completeness": completeness
            }

        except Exception as e:
            logger.error(f"Failed to review contribution: {str(e)}")
            raise ContractError(f"Failed to review contribution: {str(e)}")

    def dispute_contribution(
        self, 
        contribution_id: str,
        dispute_reason: str,
        contributor: CardanoWallet
    ) -> dict[str, str]:
        """
        Dispute a reviewed contribution with a reason.
        """
        try:
            # First, find the contribution and validate it's in the proper state
            contrib_utxo, datum = self.get_utxo_and_datum(contribution_id)
            if not contrib_utxo:
                raise ContractError(f"Contribution with ID {contribution_id} not found")

            contribution = self.dp.extract_contribution_from_datum(datum)
            contrib_status = contribution["status"]
            if contrib_status != ContributionStatus.CONTRIBUTION_REVIEWED:
                raise ContractError(f"Contribution with ID {contribution_id} is not in reviewed state. Current status is {contrib_status}")

            # Get topic data
            topic_id = contribution["topic_id"]
            topic_utxo, _ = self.get_utxo_and_datum(topic_id)
            if not topic_utxo:
                raise ContractError(f"Topic UTxO for topic {topic_id} for contribution with ID {contribution_id} not found")

            # Prepare the dispute contribution redeemer
            redeemer = self.dp.prepare_dispute_contribution_redeemer(
                contribution_id=contribution_id,
                contributor_pkh=contributor.public_key_hash,
                dispute_reason=dispute_reason
            )

            # Write redeemer to temporary file
            redeemer_file = f"dispute-contribution-redeemer-{contribution_id}.json"
            self.transactions.write_temp_json(redeemer, redeemer_file)

            # Find collateral UTxO
            txin_collateral, collateral_utxo = self.transactions.find_utxo_and_create_tx_in(
                wallet=contributor,
                min_lovelace=5000000,
                exclude=[topic_utxo, contrib_utxo]
            )
            if not collateral_utxo:
                raise ContractError(f"{contributor.name} does not have a collateral UTxO to dispute contribution {contribution_id}")

            # txin to fund tx fee
            txin_fee, fee_utxo = self.transactions.find_utxo_and_create_tx_in(
                wallet=contributor,
                min_lovelace=FUND_FEE,
                exclude=[topic_utxo, collateral_utxo, contrib_utxo]
            )
            if not fee_utxo:
                raise ContractError(f"{contributor.name} does not have a UTxO to fund fee to dispute contribution {contribution_id}")

            # Get transaction input for contribution UTxO
            txin_contrib = self.transactions.create_tx_in(contrib_utxo)

            # Get current slot and validity range
            _, invalid_before, invalid_hereafter = self.transactions.get_current_slot_and_validity()

            # Create dispute reason structure
            current_time = int(time.time() * 1000)
            dispute_reason_obj = {
                "constructor": 0,
                "fields": [
                    {"bytes": contributor.public_key_hash},
                    {"bytes": str_to_hex(dispute_reason)},
                    {"int": current_time}
                ]
            }

            review_content_obj = {
                "constructor": 0,
                "fields": [
                    {"bytes": ""},       # reviewerPkh
                    {"bytes": ""},       # refCntribId 
                    {"bytes": ""},       # relevanceReason
                    {"bytes": ""},       # accuracyReason
                    {"bytes": ""},       # completenessReason
                    {"int": 0}           # reviewTimestamp
                ]
            }

            if contribution["review_content"]:
                review_content = contribution["review_content"]
                review_content_obj = {
                    "constructor": 0,
                    "fields": [
                        {"bytes": review_content["reviewer"]},
                        {"bytes": str_to_hex(review_content["contribution_id"])},
                        {"bytes": str_to_hex(review_content["relevance_reason"])},
                        {"bytes": str_to_hex(review_content["accuracy_reason"])},
                        {"bytes": str_to_hex(review_content["completeness_reason"])},
                        {"int": review_content["timestamp"]}
                    ]
                }

            # Create updated datum directly similar to review_contribution style
            updated_contrib_datum = {
                "constructor": 0,
                "fields": [
                    # Contribution details (index 0) - reuse from original datum
                    {
                        "constructor": 0,
                        "fields": [
                            {"bytes": str_to_hex(contribution_id)},
                            {"bytes": str_to_hex(topic_id)},
                            {"bytes": str_to_hex(contribution["type"])},
                            {"bytes": str_to_hex(contribution["content"])},
                            {"bytes": contribution["creator"]},
                            {"int": contribution["timestamp"]},
                            {"int": contribution["version"]},
                            {"bytes": str_to_hex(contribution.get("previous_version_id", ""))}
                        ]
                    },
                    # Status (index 1) - update to disputed
                    {
                        "constructor": ContributionStatus.CONTRIBUTION_DISPUTED.value,
                        "fields": []
                    },
                    # Keep existing scores
                    {"int": contribution["relevance"]},       # relevance (index 2)
                    {"int": contribution["accuracy"]},        # accuracy (index 3)
                    {"int": contribution["completeness"]},    # completeness (index 4)
                    review_content_obj,
                    dispute_reason_obj,
                    {"int": contribution.get("timeliness_score", 0)}
                ]
            }

            datum_file = f"disputed-contribution-datum-{contribution_id}.json"
            self.transactions.write_temp_json(updated_contrib_datum, datum_file)

            # Set up transaction outputs
            validator_addr_str = str(self.validator_address)
            contributor_addr_str = str(contributor.address)
            
            # Output for contribution with updated dispute status
            contrib_lovelace = contrib_utxo.output.amount.coin
            tx_out_contrib = f"{validator_addr_str}+{contrib_lovelace}"

            # Build transaction to dispute contribution
            build_cmd = [
                "latest", "transaction", "build",
                "--testnet-magic", str(self.testnet_magic),
                "--socket-path", self.socket_path,
                "--change-address", contributor_addr_str,
                "--tx-in", txin_fee,
                "--tx-in", txin_contrib,
                "--tx-in-script-file", f"{self.docker_assets}/dfct-provenance.plutus",
                "--tx-in-inline-datum-present",
                "--tx-in-redeemer-file", f"{self.docker_assets}/{redeemer_file}",
                "--read-only-tx-in-reference", self.transactions.create_tx_in(topic_utxo),
                "--tx-in-collateral", txin_collateral,
                "--required-signer", f"{self.docker_assets}/{contributor.name}.skey",
                "--invalid-before", str(invalid_before),
                "--invalid-hereafter", str(invalid_hereafter),
                "--tx-out", tx_out_contrib,
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{datum_file}",
                "--out-file", f"{self.docker_assets}/dispute-contribution-{contribution_id}.raw"
            ]
            logger.info(f"Parameters for disputing contribution {contribution_id} for topic {topic_id}:\n{build_cmd}")
            self.transactions.run_cardano_cli(build_cmd)

            # Sign transaction
            sign_cmd = [
                "latest", "transaction", "sign",
                "--tx-body-file", f"{self.docker_assets}/dispute-contribution-{contribution_id}.raw",
                "--signing-key-file", f"{self.docker_assets}/{contributor.name}.skey",
                "--out-file", f"{self.docker_assets}/dispute-contribution-{contribution_id}.signed",
                "--testnet-magic", str(self.testnet_magic)
            ]
            self.transactions.run_cardano_cli(sign_cmd)

            # Submit transaction
            submit_cmd = [
                "latest", "transaction", "submit",
                "--tx-file", f"{self.docker_assets}/dispute-contribution-{contribution_id}.signed",
                "--testnet-magic", str(self.testnet_magic),
                "--socket-path", self.socket_path
            ]
            self.transactions.run_cardano_cli(submit_cmd)
            transaction_hash = self.transactions.get_transaction_hash(
                f"{self.docker_assets}/dispute-contribution-{contribution_id}.signed"
            )

            logger.info(f"Contribution {contribution_id} dispute request submitted")
            return {
                "transaction_hash": f"{transaction_hash}",
                "contribution_id": contribution_id
            }

        except Exception as e:
            logger.error(f"Failed to dispute contribution: {str(e)}")
            raise ContractError(f"Failed to dispute contribution: {str(e)}")

    def _calculate_timeliness_score(self, creation_time: int, current_time: int) -> int:
        if current_time < creation_time:
            logger.warning("Current time is before creation time. Using minimum score.")
            return 1

        time_difference = current_time - creation_time
        hour_in_millis = 60 * 60 * 1000
        two_hours = 2 * hour_in_millis
        twelve_hours = 12 * hour_in_millis
    
        if time_difference <= two_hours:
            return 10
        elif time_difference <= twelve_hours:
            return 5
        else:
            return 1

    def get_utxo_and_datum(self, str_id):
        attempt_nr = 0

        while attempt_nr < MAX_ATTEMPTS:
            utxos = self.transactions.find_utxos_at_address(self.validator_address)
            utxo, datum = self.dp.find_utxo_with_datum_id(utxos, str_id)
            if utxo and datum:
                return utxo, datum

            time.sleep(WAIT_DATA_SYNC)
            attempt_nr += 1

        return None, None
