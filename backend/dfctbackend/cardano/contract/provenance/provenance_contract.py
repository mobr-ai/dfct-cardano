import time
from typing import Any, Optional
import logging

from dfctbackend.config import settings
from dfctbackend.cardano.contract.provenance.provenance_datum import (
    ProvenanceDatumProcessor, TopicStatus, ContributionStatus, ContributionType
)
from dfctbackend.cardano.wallet import CardanoWallet
from dfctbackend.cardano.contract.contract import Contract, ContractError
from dfctbackend.cardano.utils import write_temp_json
from dfctbackend.cardano.transaction import MAX_ATTEMPTS, WAIT_DATA_SYNC

logger = logging.getLogger(__name__)

class ProvenanceContract(Contract):
    """Class for interacting with the DFCT Provenance Contract on Cardano Preview Testnet."""
    
    def __init__(self):
        super().__init__(settings.PROVENANCE_ADDRESS)
        self.dp:ProvenanceDatumProcessor = ProvenanceDatumProcessor()

    def submit_topic(self, topic_id: str, proposer: CardanoWallet, lovelace_amount: int, reward_amount: int = 1000) -> dict[str, str]:
        """
        Submit a new topic to the provenance validator using cardano-cli, following a two-step process:
        1. Send tokens and datum to validator address
        2. Consume the UTxO to submit the topic
        """
        try:
            # Step 1: Prepare topic datum and send tokens to validator
            topic_datum, redeemer = self.dp.prepare_topic_datum_redeemer(
                topic_id=topic_id,
                proposer_pkh=proposer.pub_key_hash,
                reward_amount=reward_amount
            )

            new_topic_file, redeemer_file = self._write_temp_files(topic_datum, redeemer, "new-topic", topic_id)

            token_utxo = self.tx.find_token_utxo(
                proposer.address,
                self.policy_id,
                self.token_name,
                love_amount=lovelace_amount,
                dfc_amount=reward_amount
            )
            if not token_utxo:
                raise ContractError(f"No UTxO with {self.token_name} tokens found for {proposer.name}")

            txin = self.tx.create_tx_in(token_utxo)
            txin_fee, _ = self.tx.prepare_tx_inputs(proposer, [token_utxo])

            validator_addr_str = str(self.validator_address)
            token_output = f'{reward_amount} {self.policy_id}.{self.token_name_hex}'
            tx_out = f"{validator_addr_str}+{lovelace_amount}+\"{token_output}\""
            proposer_addr_str = str(proposer.address)

            build_cmd = [
                "--change-address", proposer_addr_str,
                "--tx-in", txin_fee,
                "--tx-in", txin,
                "--tx-out", tx_out,
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{new_topic_file}"
            ]
            build_cmd = self._add_change_output(build_cmd, proposer_addr_str, [token_utxo], lovelace_amount, reward_amount)
            out_file = f"{self.docker_assets}/send-dfc-new-topic-to-provenance-{topic_id}"
            self.tx.build_and_submit_tx(build_cmd, proposer, out_file, f"send-dfc-new-topic-to-provenance")

            # Step 2: Consume the UTxO to submit the topic
            topic_utxo, _ = self._get_contract_utxo_and_datum(topic_id)
            if not topic_utxo:
                raise ContractError(f"No UTxO found for id {topic_id}")

            txin_fee, txin_collateral = self.tx.prepare_tx_inputs(proposer, [topic_utxo])
            txin_with_tokens = self.tx.create_tx_in(topic_utxo)
            _, invalid_before, invalid_hereafter = self.tx.get_current_slot_and_validity()
            tx_out_submit = self._prepare_validator_output(topic_utxo, validator_addr_str)

            build_submit_cmd = [
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
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{new_topic_file}"
            ]
            build_submit_cmd = self._add_change_output(build_submit_cmd, proposer_addr_str, [topic_utxo], topic_utxo.output.amount.coin, self.tx.get_cnt_amount(topic_utxo, self.policy_id))
            out_file = f"{self.docker_assets}/new-topic-{topic_id}"
            transaction_hash = self.tx.build_and_submit_tx(build_submit_cmd, proposer, out_file, f"new-topic")

            logger.info(f"Topic submitted. Topic ID: {topic_id}")
            return {
                "transaction_hash": f"{transaction_hash}",
                "topic_id": topic_id
            }

        except Exception as e:
            logger.error(f"Failed to submit topic: {str(e)}")
            raise ContractError(f"Failed to submit topic: {str(e)}")

    def get_topic(self, topic_id: str) -> Optional[dict[str, Any]]:
        try:
            _, datum = self._get_contract_utxo_and_datum(topic_id)
            if datum:
                return self.dp.extract_topic_from_datum(datum)
            return None
        except Exception as e:
            logger.error(f"Failed to get topic: {str(e)}")
            return None

    def get_topics(self) -> list[dict[str, Any]]:
        try:
            all_topics = []
            utxos = self.tx.find_utxos_at_address(self.validator_address)
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
            topic_utxo, datum = self._get_contract_utxo_and_datum(topic_id)
            if not topic_utxo:
                raise ContractError(f"Utxos for topic_id {topic_id} not found")

            topic = self.dp.extract_topic_from_datum(datum)
            topic_status = topic["status"]
            if topic_status != TopicStatus.PROPOSED:
                raise ContractError(f"Topic with ID {topic_id} is not in proposed state. Current status is {topic_status}")

            redeemer = self.dp.prepare_review_topic_redeemer(topic_id=topic_id, approved=approved)
            datum_file, redeemer_file = self._write_temp_files(
                self.dp.prepare_topic_json(datum, TopicStatus.REVIEWED.value)[0], 
                redeemer, 
                "reviewed-topic", 
                topic_id
            )

            txin_fee, txin_collateral = self.tx.prepare_tx_inputs(reviewer, [topic_utxo])
            txin_topic = self.tx.create_tx_in(topic_utxo)
            _, invalid_before, invalid_hereafter = self.tx.get_current_slot_and_validity()

            validator_addr_str = str(self.validator_address)
            reviewer_addr_str = str(reviewer.address)
            tx_out = self._prepare_validator_output(topic_utxo, validator_addr_str)

            build_cmd = [
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
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{datum_file}"
            ]
            build_cmd = self._add_change_output(build_cmd, reviewer_addr_str, [topic_utxo], topic_utxo.output.amount.coin, self.tx.get_cnt_amount(topic_utxo, self.policy_id))

            out_file = f"{self.docker_assets}/review-topic-{topic_id}"
            transaction_hash = self.tx.build_and_submit_tx(build_cmd, reviewer, out_file, f"review-topic")

            logger.info(f"Topic {topic_id} review submitted")
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
            topic_utxo, datum = self._get_contract_utxo_and_datum(topic_id)
            if not topic_utxo:
                raise ContractError(f"Topic with ID {topic_id} not found")

            topic = self.dp.extract_topic_from_datum(datum)
            topic_status = topic["status"]
            if topic_status != TopicStatus.REVIEWED:
                raise ContractError(f"Topic with ID {topic_id} is not in reviewed state. Current status is {topic_status}")

            redeemer = self.dp.prepare_topic_action_redeemer(topic_id=topic_id, action_constructor=TopicStatus.ACTIVATED.value)
            datum_file, redeemer_file = self._write_temp_files(
                self.dp.prepare_topic_json(datum, TopicStatus.ACTIVATED.value)[0], 
                redeemer, 
                "activated-topic", 
                topic_id
            )

            txin_fee, txin_collateral = self.tx.prepare_tx_inputs(wallet, [topic_utxo])
            txin_topic = self.tx.create_tx_in(topic_utxo)
            _, invalid_before, invalid_hereafter = self.tx.get_current_slot_and_validity()

            validator_addr_str = str(self.validator_address)
            wallet_addr_str = str(wallet.address)
            tx_out = self._prepare_validator_output(topic_utxo, validator_addr_str)

            build_cmd = [
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
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{datum_file}"
            ]
            build_cmd = self._add_change_output(build_cmd, wallet_addr_str, [topic_utxo], topic_utxo.output.amount.coin, self.tx.get_cnt_amount(topic_utxo, self.policy_id))

            out_file = f"{self.docker_assets}/activate-topic-{topic_id}"
            transaction_hash = self.tx.build_and_submit_tx(build_cmd, wallet, out_file, f"activate-topic")

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
            topic_utxo, datum = self._get_contract_utxo_and_datum(topic_id)
            if not topic_utxo:
                raise ContractError(f"Topic with ID {topic_id} not found")

            topic = self.dp.extract_topic_from_datum(datum)
            topic_status = topic["status"]
            if topic_status != TopicStatus.ACTIVATED:
                raise ContractError(f"Topic with ID {topic_id} is not in activated state. Current state is {topic_status}")

            redeemer = self.dp.prepare_topic_action_redeemer(topic_id=topic_id, action_constructor=3)
            datum_file, redeemer_file = self._write_temp_files(
                self.dp.prepare_topic_json(datum, TopicStatus.CLOSED.value)[0], 
                redeemer, 
                "closed-topic", 
                topic_id
            )

            txin_fee, txin_collateral = self.tx.prepare_tx_inputs(wallet, [topic_utxo])
            txin_topic = self.tx.create_tx_in(topic_utxo)
            _, invalid_before, invalid_hereafter = self.tx.get_current_slot_and_validity()

            validator_addr_str = str(self.validator_address)
            wallet_addr_str = str(wallet.address)
            tx_out = self._prepare_validator_output(topic_utxo, validator_addr_str)

            build_cmd = [
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
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{datum_file}"
            ]
            build_cmd = self._add_change_output(build_cmd, wallet_addr_str, [topic_utxo], topic_utxo.output.amount.coin, self.tx.get_cnt_amount(topic_utxo, self.policy_id))

            out_file = f"{self.docker_assets}/close-topic-{topic_id}"
            transaction_hash = self.tx.build_and_submit_tx(build_cmd, wallet, out_file, f"close-topic")

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
            contribution_utxo, datum = self._get_contract_utxo_and_datum(contribution_id)
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
            validator_utxos = self.tx.find_utxos_at_address(self.validator_address)
            contributions = []
            for utxo in validator_utxos:
                datum = self.dp.decode_utxo_datum(utxo)
                if not datum:
                    continue
                if len(datum) > 4 and isinstance(datum[0], list) and len(datum[0]) >= 7:
                    contribution = self.dp.extract_contribution_from_datum(datum)
                    if contribution and contribution["topic_id"] == topic_id:
                        contributions.append(contribution)
            return contributions
        except Exception as e:
            logger.error(f"Failed to get contributions for topic: {str(e)}")
            return []

    def submit_contribution(
        self, topic_id: str,
        contribution_id: str,
        contribution_type: ContributionType,
        lovelace_amount: int,
        contributor: CardanoWallet
    ) -> dict[str, str]:
        """
        Submit a contribution to an activated topic.
        """
        try:
            topic_utxo, datum = self._get_contract_utxo_and_datum(topic_id)
            if not topic_utxo:
                raise ContractError(f"Utxos for topic_id {topic_id} not found")

            topic = self.dp.extract_topic_from_datum(datum)
            topic_status = topic["status"]
            if topic_status != TopicStatus.ACTIVATED:
                raise ContractError(f"Topic with ID {topic_id} is not in activated state. Current state is {topic_status}")

            contrib_datum, contrib_redeemer = self.dp.prepare_contribution_datum_redeemer(
                contribution_id=contribution_id,
                topic_id=topic_id,
                contribution_type=contribution_type,
                contributor_pkh=contributor.pub_key_hash,
                contribution_status=ContributionStatus.PROPOSED,
                timestamp=int(time.time() * 1000)
            )

            contrib_datum_file, contrib_redeemer_file = self._write_temp_files(contrib_datum, contrib_redeemer, "contribution", contribution_id)
            txin_fee, txin_collateral = self.tx.prepare_tx_inputs(contributor, [topic_utxo])
            txin_topic = self.tx.create_tx_in(topic_utxo)
            _, invalid_before, invalid_hereafter = self.tx.get_current_slot_and_validity()

            validator_addr_str = str(self.validator_address)
            contributor_addr_str = str(contributor.address)
            tx_out = f"{validator_addr_str}+{lovelace_amount}"
            tx_out_topic = self._prepare_validator_output(topic_utxo, validator_addr_str)

            plutus_datum, _ = self.dp.prepare_topic_json(datum, topic["status"].value)
            topic_datum_file = f"topic-datum-{topic_id}.json"
            write_temp_json(plutus_datum, topic_datum_file, self.tx.assets_path)

            build_cmd = [
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
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{topic_datum_file}"
            ]
            build_cmd = self._add_change_output(build_cmd, contributor_addr_str, [topic_utxo], lovelace_amount + topic_utxo.output.amount.coin, self.tx.get_cnt_amount(topic_utxo, self.policy_id))

            out_file = f"{self.docker_assets}/submit-contribution-{topic_id}"
            transaction_hash = self.tx.build_and_submit_tx(build_cmd, contributor, out_file, f"submit-contribution")

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
        completeness: int
    ) -> dict[str, str]:
        """
        Review a contribution with scores and feedback.
        """
        try:
            contrib_utxo, datum = self._get_contract_utxo_and_datum(contribution_id)
            if not contrib_utxo:
                raise ContractError(f"Contribution with ID {contribution_id} not found")

            contribution = self.dp.extract_contribution_from_datum(datum)
            contrib_status = contribution["status"]
            if contrib_status != ContributionStatus.PROPOSED:
                raise ContractError(f"Contribution with ID {contribution_id} is not in proposed state. Current state is {contrib_status}")

            topic_id = contribution["topic_id"]
            topic_utxo, _ = self._get_contract_utxo_and_datum(topic_id)
            if not topic_utxo:
                raise ContractError(f"Topic UTxO for topic {topic_id} for contribution with ID {contribution_id} not found")

            redeemer = self.dp.prepare_review_contribution_redeemer(
                contribution_id=contribution_id,
                reviewer_pkh=reviewer.pub_key_hash,
                relevance=relevance,
                accuracy=accuracy,
                completeness=completeness
            )
            updated_datum = self.dp.prepare_contribution_datum(
                contribution_id=contribution_id,
                topic_id=topic_id,
                contribution_type=contribution["type"],
                contribution_status=ContributionStatus.REVIEWED,
                contribution_timestamp=contribution["timestamp"],
                relevance=relevance,
                accuracy=accuracy,
                completeness=completeness,
                contributor_pkh=contribution["creator"],
                reviewer_pkh=reviewer.pub_key_hash,
                review_timestamp=contribution["review_content"]["review_timestamp"],
                initiator_pkh=contribution["dispute_content"]["initiator"],
                dispute_timestamp=contribution["dispute_content"]["dispute_timestamp"]
            )
            datum_file, redeemer_file = self._write_temp_files(updated_datum, redeemer, "reviewed-contribution", contribution_id)

            txin_fee, txin_collateral = self.tx.prepare_tx_inputs(reviewer, [topic_utxo, contrib_utxo])
            txin_contrib = self.tx.create_tx_in(contrib_utxo)
            txin_topic = self.tx.create_tx_in(topic_utxo)
            _, invalid_before, invalid_hereafter = self.tx.get_current_slot_and_validity()

            validator_addr_str = str(self.validator_address)
            reviewer_addr_str = str(reviewer.address)
            contrib_lovelace = contrib_utxo.output.amount.coin
            tx_out_contrib = f"{validator_addr_str}+{contrib_lovelace}"

            build_cmd = [
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
            ]

            out_file = f"{self.docker_assets}/review-contribution-{contribution_id}"
            transaction_hash = self.tx.build_and_submit_tx(build_cmd, reviewer, out_file, f"review-contribution")

            logger.info(f"Contribution {contribution_id} reviewed with scores - Relevance: {relevance}, Accuracy: {accuracy}, Completeness: {completeness}")
            return {
                "transaction_hash": f"{transaction_hash}",
                "contribution_id": contribution_id,
                "relevance": str(relevance),
                "accuracy": str(accuracy),
                "completeness": str(completeness)
            }

        except Exception as e:
            logger.error(f"Failed to review contribution: {str(e)}")
            raise ContractError(f"Failed to review contribution: {str(e)}")

    def dispute_contribution(
        self, 
        contribution_id: str,
        contributor: CardanoWallet
    ) -> dict[str, str]:
        """
        Dispute a reviewed contribution with a reason.
        """
        try:
            contrib_utxo, datum = self._get_contract_utxo_and_datum(contribution_id)
            if not contrib_utxo:
                raise ContractError(f"Contribution with ID {contribution_id} not found")

            contribution = self.dp.extract_contribution_from_datum(datum)
            contrib_status = contribution["status"]
            if contrib_status != ContributionStatus.REVIEWED:
                raise ContractError(f"Contribution with ID {contribution_id} is not in reviewed state. Current status is {contrib_status}")

            topic_id = contribution["topic_id"]
            topic_utxo, _ = self._get_contract_utxo_and_datum(topic_id)
            if not topic_utxo:
                raise ContractError(f"Topic UTxO for topic {topic_id} for contribution with ID {contribution_id} not found")

            redeemer = self.dp.prepare_dispute_contribution_redeemer(
                contribution_id=contribution_id,
                contributor_pkh=contributor.pub_key_hash
            )
            updated_datum = self.dp.prepare_contribution_datum(
                contribution_id=contribution_id,
                topic_id=topic_id,
                contribution_type=contribution["type"],
                contribution_status=ContributionStatus.DISPUTED,
                contribution_timestamp=contribution["timestamp"],
                relevance=contribution["relevance"],
                accuracy=contribution["accuracy"],
                completeness=contribution["completeness"],
                contributor_pkh=contribution["creator"],
                reviewer_pkh=contribution["review_content"]["reviewer"],
                review_timestamp=contribution["review_content"]["review_timestamp"],
                initiator_pkh=contributor.pub_key_hash,
                dispute_timestamp=int(time.time() * 1000)
            )
            datum_file, redeemer_file = self._write_temp_files(updated_datum, redeemer, "disputed-contribution", contribution_id)

            txin_fee, txin_collateral = self.tx.prepare_tx_inputs(contributor, [topic_utxo, contrib_utxo])
            txin_contrib = self.tx.create_tx_in(contrib_utxo)
            _, invalid_before, invalid_hereafter = self.tx.get_current_slot_and_validity()

            validator_addr_str = str(self.validator_address)
            contributor_addr_str = str(contributor.address)
            contrib_lovelace = contrib_utxo.output.amount.coin
            tx_out_contrib = f"{validator_addr_str}+{contrib_lovelace}"

            build_cmd = [
                "--change-address", contributor_addr_str,
                "--tx-in", txin_fee,
                "--tx-in", txin_contrib,
                "--tx-in-script-file", f"{self.docker_assets}/dfct-provenance.plutus",
                "--tx-in-inline-datum-present",
                "--tx-in-redeemer-file", f"{self.docker_assets}/{redeemer_file}",
                "--read-only-tx-in-reference", self.tx.create_tx_in(topic_utxo),
                "--tx-in-collateral", txin_collateral,
                "--required-signer", f"{self.docker_assets}/{contributor.name}.skey",
                "--invalid-before", str(invalid_before),
                "--invalid-hereafter", str(invalid_hereafter),
                "--tx-out", tx_out_contrib,
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{datum_file}"
            ]

            out_file = f"{self.docker_assets}/dispute-contribution-{contribution_id}"
            transaction_hash = self.tx.build_and_submit_tx(build_cmd, contributor, out_file, f"dispute-contribution")

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