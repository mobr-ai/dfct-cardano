from typing import Any, Optional
import logging

from dfctbackend.config import settings
from dfctbackend.cardano.datum import ProposalStatus
from dfctbackend.cardano.wallet import CardanoWallet
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
            write_temp_json(proposal_datum, new_proposal_file, self.tx.assets_path)
            write_temp_json(redeemer, redeemer_file, self.tx.assets_path)

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
                min_lovelace=lovelace_amount,
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
                min_lovelace=lovelace_amount,
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
        pass

    def finalize_proposal(self, proposal_id: str, wallet: CardanoWallet) -> dict[str, str]:
        """
        Finalize voting on a governance proposal and update its status.
        """
        pass

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