from typing import Any, Optional
import logging

from dfctbackend.config import settings
from dfctbackend.cardano.datum import ProposalStatus
from dfctbackend.cardano.wallet import CardanoWallet
from dfctbackend.cardano.transaction import FUND_FEE
from dfctbackend.cardano.utils import write_temp_json
from dfctbackend.cardano.contract import Contract, ContractError

logger = logging.getLogger(__name__)

class GovernanceContract(Contract):
    """
    Class for interacting with the DFCT Governance Contract on Cardano Preview Testnet.
    """

    def __init__(self):
        super().__init__(settings.GOVERNANCE_ADDRESS)

    def submit_proposal(self, proposal_id: str, proposer: CardanoWallet, lovelace_amount: int, reward_amount: int = 1000) -> dict[str, str]:
        """
        Submit a new governance proposal to the governance validator.
        """
        try:
            # Prepare proposal datum and redeemer
            proposal_datum, redeemer, proposal_id = self.dp.prepare_proposal_datum_redeemer(
                proposal_id=proposal_id,
                proposer_pkh=proposer.pub_key_hash,
                reward_amount=reward_amount
            )

            new_proposal_file = f"new-proposal-datum-{proposal_id}.json"
            redeemer_file = f"new-proposal-redeemer-{proposal_id}.json"

            # Write datum and redeemer to temporary files
            write_temp_json(proposal_datum, new_proposal_file)
            write_temp_json(redeemer, redeemer_file)

            # Find UTxO with DFC tokens
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

            # txin to fund tx fee
            txin_fee, fee_utxo = self.tx.find_utxo_and_create_tx_in(
                wallet=proposer,
                min_lovelace=FUND_FEE,
                exclude=[token_utxo]
            )
            if not fee_utxo:
                raise ContractError(f"{proposer.name} does not have a UTxO to fund fee to submit_proposal {proposal_id}")

            # Build transaction to send tokens and datum to validator
            validator_addr_str = str(self.validator_address)
            token_output = f'{reward_amount} {self.policy_id}.{self.token_name_hex}'
            tx_out = f"{validator_addr_str}+{lovelace_amount}+\"{token_output}\""
            proposer_addr_str = str(proposer.address)

            build_cmd = [
                "--change-address", proposer_addr_str,
                "--tx-in", txin_fee,
                "--tx-in", txin,
                "--tx-out", tx_out,
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{new_proposal_file}",
                "--out-file", f"{self.docker_assets}/send-dfc-new-proposal-to-governance-{proposal_id}.raw"
            ]
            logger.info(f"Parameters for creating proposal_id {proposal_id}:\n{build_cmd}")
            self.tx.cli.build_transaction(build_cmd)

            # Sign transaction
            self.tx.cli.sign_transaction(
                tx_body_file=f"{self.docker_assets}/send-dfc-new-proposal-to-governance-{proposal_id}.raw",
                signing_key_file=f"{self.docker_assets}/{proposer.name}.skey",
                out_file=f"{self.docker_assets}/send-dfc-new-proposal-to-governance-{proposal_id}.signed"
            )

            # Submit transaction
            self.tx.cli.submit_transaction(
                f"{self.docker_assets}/send-dfc-new-proposal-to-governance-{proposal_id}.signed"
            )

            # Consume the UTxO to submit the proposal
            proposal_utxo, _ = self.get_contract_utxo_and_datum(proposal_id)
            if not proposal_utxo:
                raise ContractError(f"No UTxO found for id {proposal_id}")

            txin_collateral, collateral_utxo = self.tx.find_utxo_and_create_tx_in(
                wallet=proposer,
                min_lovelace=5000000,
                exclude=[token_utxo]
            )
            if not collateral_utxo:
                raise ContractError(f"{proposer.name} does not have a collateral UTxO to submit_proposal {proposal_id}")

            txin_fee, fee_utxo = self.tx.find_utxo_and_create_tx_in(
                wallet=proposer,
                min_lovelace=FUND_FEE,
                exclude=[proposal_utxo, collateral_utxo]
            )
            if not fee_utxo:
                raise ContractError(f"{proposer.name} does not have a UTxO to fund fee to submit_proposal {proposal_id}")

            txin_with_tokens = self.tx.create_tx_in(proposal_utxo)
            _, invalid_before, invalid_hereafter = self.tx.get_current_slot_and_validity()
            tx_out_submit = f"{validator_addr_str}+{lovelace_amount}+\"{token_output}\""

            build_submit_cmd = [
                "--change-address", proposer_addr_str,
                "--tx-in", txin_fee,
                "--tx-in", txin_with_tokens,
                "--tx-in-script-file", f"{self.docker_assets}/dfct-governance.plutus",
                "--tx-in-inline-datum-present",
                "--tx-in-redeemer-file", f"{self.docker_assets}/{redeemer_file}",
                "--tx-in-collateral", txin_collateral,
                "--required-signer", f"{self.docker_assets}/{proposer.name}.skey",
                "--invalid-before", str(invalid_before),
                "--invalid-hereafter", str(invalid_hereafter),
                "--tx-out", tx_out_submit,
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{new_proposal_file}",
                "--out-file", f"{self.docker_assets}/new-proposal-{proposal_id}.raw"
            ]
            logger.info(f"Parameters for submitting proposal_id {proposal_id}:\n{build_submit_cmd}")
            self.tx.cli.build_transaction(build_submit_cmd)

            self.tx.cli.sign_transaction(
                tx_body_file=f"{self.docker_assets}/new-proposal-{proposal_id}.raw",
                signing_key_file=f"{self.docker_assets}/{proposer.name}.skey",
                out_file=f"{self.docker_assets}/new-proposal-{proposal_id}.signed"
            )

            self.tx.cli.submit_transaction(
                f"{self.docker_assets}/new-proposal-{proposal_id}.signed"
            )
            transaction_hash = self.tx.get_transaction_hash(
                f"{self.docker_assets}/new-proposal-{proposal_id}.signed"
            )

            logger.info(f"Proposal submitted. Proposal ID: {proposal_id}")
            return {
                "transaction_hash": f"{transaction_hash}",
                "proposal_id": proposal_id
            }

        except Exception as e:
            logger.error(f"Failed to submit proposal: {str(e)}")
            raise ContractError(f"Failed to submit proposal: {str(e)}")

    def vote_on_proposal(self, proposal_id: str, voter: CardanoWallet, vote: bool, dfc_amount: int) -> dict[str, str]:
        """
        Cast a vote on a governance proposal.
        """
        try:
            proposal_utxo, datum = self.get_contract_utxo_and_datum(proposal_id)
            if not proposal_utxo:
                raise ContractError(f"Proposal with ID {proposal_id} not found")

            proposal = self.dp.extract_proposal_from_datum(datum)
            if proposal["status"] != ProposalStatus.VOTING:
                raise ContractError(f"Proposal with ID {proposal_id} is not in voting state. Current state is {proposal['status']}")

            redeemer = self.dp.prepare_vote_redeemer(
                proposal_id=proposal_id,
                voter_pkh=voter.pub_key_hash,
                vote=vote,
                dfc_amount=dfc_amount
            )

            redeemer_file = f"vote-proposal-redeemer-{proposal_id}.json"
            write_temp_json(redeemer, redeemer_file)

            token_utxo = self.tx.find_token_utxo(
                voter.address,
                self.policy_id,
                self.token_name,
                love_amount=FUND_FEE,
                dfc_amount=dfc_amount
            )
            if not token_utxo:
                raise ContractError(f"No UTxO with {dfc_amount} {self.token_name} tokens found for {voter.name}")

            txin_collateral, collateral_utxo = self.tx.find_utxo_and_create_tx_in(
                wallet=voter,
                min_lovelace=5000000,
                exclude=[token_utxo, proposal_utxo]
            )
            if not collateral_utxo:
                raise ContractError(f"{voter.name} does not have a collateral UTxO to vote on proposal {proposal_id}")

            txin_fee, fee_utxo = self.tx.find_utxo_and_create_tx_in(
                wallet=voter,
                min_lovelace=FUND_FEE,
                exclude=[token_utxo, collateral_utxo, proposal_utxo]
            )
            if not fee_utxo:
                raise ContractError(f"{voter.name} does not have a UTxO to fund fee to vote on proposal {proposal_id}")

            txin_proposal = self.tx.create_tx_in(proposal_utxo)
            txin_tokens = self.tx.create_tx_in(token_utxo)
            _, invalid_before, invalid_hereafter = self.tx.get_current_slot_and_validity()

            updated_datum = self.dp.prepare_updated_proposal_datum(datum, vote, voter.pub_key_hash, dfc_amount)
            datum_file = f"voted-proposal-datum-{proposal_id}.json"
            write_temp_json(updated_datum, datum_file)

            validator_addr_str = str(self.validator_address)
            voter_addr_str = str(voter.address)
            proposal_lovelace = proposal_utxo.output.amount.coin
            reward_amount = self.tx.get_cnt_amount(proposal_utxo, self.policy_id)
            token_output = f'{reward_amount + dfc_amount} {self.policy_id}.{self.token_name_hex}'
            tx_out = f"{validator_addr_str}+{proposal_lovelace}+\"{token_output}\""

            build_cmd = [
                "--change-address", voter_addr_str,
                "--tx-in", txin_fee,
                "--tx-in", txin_proposal,
                "--tx-in-script-file", f"{self.docker_assets}/dfct-governance.plutus",
                "--tx-in-inline-datum-present",
                "--tx-in-redeemer-file", f"{self.docker_assets}/{redeemer_file}",
                "--tx-in", txin_tokens,
                "--tx-in-collateral", txin_collateral,
                "--required-signer", f"{self.docker_assets}/{voter.name}.skey",
                "--invalid-before", str(invalid_before),
                "--invalid-hereafter", str(invalid_hereafter),
                "--tx-out", tx_out,
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{datum_file}",
                "--out-file", f"{self.docker_assets}/vote-proposal-{proposal_id}.raw"
            ]
            logger.info(f"Parameters for voting on proposal_id {proposal_id}:\n{build_cmd}")
            self.tx.cli.build_transaction(build_cmd)

            self.tx.cli.sign_transaction(
                tx_body_file=f"{self.docker_assets}/vote-proposal-{proposal_id}.raw",
                signing_key_file=f"{self.docker_assets}/{voter.name}.skey",
                out_file=f"{self.docker_assets}/vote-proposal-{proposal_id}.signed"
            )

            self.tx.cli.submit_transaction(
                f"{self.docker_assets}/vote-proposal-{proposal_id}.signed"
            )
            transaction_hash = self.tx.get_transaction_hash(
                f"{self.docker_assets}/vote-proposal-{proposal_id}.signed"
            )

            logger.info(f"Vote submitted for proposal {proposal_id}")
            return {
                "transaction_hash": f"{transaction_hash}",
                "proposal_id": proposal_id
            }

        except Exception as e:
            logger.error(f"Failed to vote on proposal: {str(e)}")
            raise ContractError(f"Failed to vote on proposal: {str(e)}")

    def finalize_proposal(self, proposal_id: str, wallet: CardanoWallet) -> dict[str, str]:
        """
        Finalize voting on a governance proposal and update its status.
        """
        try:
            proposal_utxo, datum = self.get_contract_utxo_and_datum(proposal_id)
            if not proposal_utxo:
                raise ContractError(f"Proposal with ID {proposal_id} not found")

            proposal = self.dp.extract_proposal_from_datum(datum)
            if proposal["status"] != ProposalStatus.VOTING:
                raise ContractError(f"Proposal with ID {proposal_id} is not in voting state. Current state is {proposal['status']}")

            # Calculate final status based on votes
            total_yes = sum(proposal["yes_votes"].values())
            total_no = sum(proposal["no_votes"].values())
            new_status = ProposalStatus.APPROVED if total_yes > total_no else ProposalStatus.REJECTED

            redeemer = self.dp.prepare_finalize_redeemer(proposal_id=proposal_id)

            redeemer_file = f"finalize-proposal-redeemer-{proposal_id}.json"
            write_temp_json(redeemer, redeemer_file)

            txin_collateral, collateral_utxo = self.tx.find_utxo_and_create_tx_in(
                wallet=wallet,
                min_lovelace=5000000,
                exclude=[proposal_utxo]
            )
            if not collateral_utxo:
                raise ContractError(f"{wallet.name} does not have a collateral UTxO to finalize proposal {proposal_id}")

            txin_fee, fee_utxo = self.tx.find_utxo_and_create_tx_in(
                wallet=wallet,
                min_lovelace=FUND_FEE,
                exclude=[proposal_utxo, collateral_utxo]
            )
            if not fee_utxo:
                raise ContractError(f"{wallet.name} does not have a UTxO to fund fee to finalize proposal {proposal_id}")

            txin_proposal = self.tx.create_tx_in(proposal_utxo)
            _, invalid_before, invalid_hereafter = self.tx.get_current_slot_and_validity()

            updated_datum = self.dp.prepare_updated_proposal_datum(datum, None, None, 0, new_status)
            datum_file = f"finalized-proposal-datum-{proposal_id}.json"
            write_temp_json(updated_datum, datum_file)

            validator_addr_str = str(self.validator_address)
            wallet_addr_str = str(wallet.address)
            proposal_lovelace = proposal_utxo.output.amount.coin
            reward_amount = self.tx.get_cnt_amount(proposal_utxo, self.policy_id)
            token_output = f'{reward_amount} {self.policy_id}.{self.token_name_hex}'
            tx_out = f"{validator_addr_str}+{proposal_lovelace}+\"{token_output}\""

            build_cmd = [
                "--change-address", wallet_addr_str,
                "--tx-in", txin_fee,
                "--tx-in", txin_proposal,
                "--tx-in-script-file", f"{self.docker_assets}/dfct-governance.plutus",
                "--tx-in-inline-datum-present",
                "--tx-in-redeemer-file", f"{self.docker_assets}/{redeemer_file}",
                "--tx-in-collateral", txin_collateral,
                "--required-signer", f"{self.docker_assets}/{wallet.name}.skey",
                "--invalid-before", str(invalid_before),
                "--invalid-hereafter", str(invalid_hereafter),
                "--tx-out", tx_out,
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{datum_file}",
                "--out-file", f"{self.docker_assets}/finalize-proposal-{proposal_id}.raw"
            ]
            logger.info(f"Parameters for finalizing proposal_id {proposal_id}:\n{build_cmd}")
            self.tx.cli.build_transaction(build_cmd)

            self.tx.cli.sign_transaction(
                tx_body_file=f"{self.docker_assets}/finalize-proposal-{proposal_id}.raw",
                signing_key_file=f"{self.docker_assets}/{wallet.name}.skey",
                out_file=f"{self.docker_assets}/finalize-proposal-{proposal_id}.signed"
            )

            self.tx.cli.submit_transaction(
                f"{self.docker_assets}/finalize-proposal-{proposal_id}.signed"
            )
            transaction_hash = self.tx.get_transaction_hash(
                f"{self.docker_assets}/finalize-proposal-{proposal_id}.signed"
            )

            logger.info(f"Proposal {proposal_id} finalized with status {new_status}")
            return {
                "transaction_hash": f"{transaction_hash}",
                "proposal_id": proposal_id,
                "status": new_status.value
            }

        except Exception as e:
            logger.error(f"Failed to finalize proposal: {str(e)}")
            raise ContractError(f"Failed to finalize proposal: {str(e)}")

    def get_proposal(self, proposal_id: str) -> Optional[dict[str, Any]]:
        """
        Retrieve a proposal by its ID.
        """
        try:
            _, datum = self.get_contract_utxo_and_datum(proposal_id)
            if datum:
                return self.dp.extract_proposal_from_datum(datum)
            return None

        except Exception as e:
            logger.error(f"Failed to get proposal: {str(e)}")
            return None

    def get_proposals(self) -> list[dict[str, Any]]:
        """
        Get all governance proposals.
        """
        try:
            all_proposals = []
            utxos = self.tx.find_utxos_at_address(self.validator_address)
            for utxo in utxos:
                proposal_datum = self.dp.decode_utxo_datum(utxo)
                if proposal_datum:
                    proposal = self.dp.extract_proposal_from_datum(proposal_datum)
                    if proposal:
                        all_proposals.append(proposal)
            return all_proposals

        except Exception as e:
            logger.error(f"Failed to get proposals: {str(e)}")
            return []