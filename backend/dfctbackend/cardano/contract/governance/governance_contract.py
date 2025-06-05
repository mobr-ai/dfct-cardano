from typing import Any, Optional
import logging

from dfctbackend.config import settings
from dfctbackend.cardano.utils import str_to_hex
from dfctbackend.cardano.contract.governance.governance_datum import ProposalStatus, GovernanceDatumProcessor
from dfctbackend.cardano.wallet import CardanoWallet
from dfctbackend.cardano.contract.contract import Contract, ContractError

logger = logging.getLogger(__name__)

class GovernanceContract(Contract):
    """Class for interacting with the DFCT Governance Contract on Cardano Preview Testnet."""

    def __init__(self):
        super().__init__(settings.GOVERNANCE_ADDRESS)
        self.dp: GovernanceDatumProcessor = GovernanceDatumProcessor()

    def submit_proposal(
            self,
            proposal_id: str,
            proposer: CardanoWallet,
            lovelace_amount: int,
            owner_pkh: str,
            min_voting_tokens: int,
            voting_start: int,
            voting_end: int,
            authorized_pkhs: dict
    ) -> dict[str, str]:
        """
        Submit a new governance proposal using a two-step process.
        """
        try:
            proposal_datum, redeemer, proposal_id = self.dp.prepare_proposal_datum_redeemer(
                proposal_id=proposal_id,
                proposer_pkh=proposer.pub_key_hash,
                owner_pkh=owner_pkh,
                min_voting_tokens=min_voting_tokens,
                voting_start=voting_start,
                voting_end=voting_end,
                authorized_pkhs=authorized_pkhs
            )

            new_proposal_file, redeemer_file = self._write_temp_files(proposal_datum, redeemer, "new-proposal", proposal_id)

            token_utxo = self.tx.find_token_utxo(
                proposer.address,
                self.policy_id,
                self.token_name,
                love_amount=lovelace_amount,
                dfc_amount=0
            )
            if not token_utxo:
                raise ContractError(f"No UTxO with {self.token_name} tokens found for {proposer.name}")

            txin = self.tx.create_tx_in(token_utxo)
            txin_fee, _ = self.tx.prepare_tx_inputs(proposer, [token_utxo])

            validator_addr_str = str(self.validator_address)
            tx_out = f"{validator_addr_str}+{lovelace_amount}"
            proposer_addr_str = str(proposer.address)

            build_cmd = [
                "--change-address", proposer_addr_str,
                "--tx-in", txin_fee,
                "--tx-in", txin,
                "--tx-out", tx_out,
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{new_proposal_file}"
            ]
            build_cmd = self._add_change_output(build_cmd, proposer_addr_str, [token_utxo], lovelace_amount, 0)
            out_file = f"{self.docker_assets}/send-dfc-new-proposal-to-governance-{proposal_id}"
            self.tx.build_and_submit_tx(build_cmd, proposer, out_file, f"send-dfc-new-proposal-to-governance")

            proposal_utxo, _ = self._get_contract_utxo_and_datum(proposal_id, 1, 0, 0)
            if not proposal_utxo:
                raise ContractError(f"No UTxO found for id {proposal_id}")

            txin_fee, txin_collateral = self.tx.prepare_tx_inputs(proposer, [proposal_utxo])
            txin_with_tokens = self.tx.create_tx_in(proposal_utxo)
            _, invalid_before, invalid_hereafter = self.tx.get_current_slot_and_validity()
            tx_out_submit = self._prepare_validator_output(proposal_utxo, validator_addr_str)

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
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{new_proposal_file}"
            ]
            build_submit_cmd = self._add_change_output(build_submit_cmd, proposer_addr_str, [proposal_utxo], proposal_utxo.output.amount.coin, self.tx.get_cnt_amount(proposal_utxo, self.policy_id))
            out_file = f"{self.docker_assets}/new-proposal-{proposal_id}"
            transaction_hash = self.tx.build_and_submit_tx(build_submit_cmd, proposer, out_file, f"new-proposal")

            logger.info(f"Proposal submitted. Proposal ID: {proposal_id}")
            return {
                "transaction_hash": f"{transaction_hash}",
                "proposal_id": proposal_id
            }

        except Exception as e:
            logger.error(f"Failed to submit proposal: {str(e)}")
            raise ContractError(f"Failed to submit proposal: {str(e)}")

    def vote_on_proposal(
            self,
            proposal_id: str,
            voter: CardanoWallet,
            vote: int,
            dfc_amount: int
    ) -> dict[str, str]:
        """
        Cast a vote on a governance proposal.
        """
        try:
            proposal_utxo, datum = self._get_contract_utxo_and_datum(proposal_id, 1, 0, 0)
            if not proposal_utxo:
                raise ContractError(f"Proposal with ID {proposal_id} not found")

            proposal = self.dp.extract_proposal_from_datum(datum)
            if not proposal:
                raise ContractError("Failed to extract proposal from datum")

            redeemer = self.dp.prepare_vote_redeemer(
                proposal_id=proposal_id,
                vote=vote,
                dfc_amount=dfc_amount
            )
            updated_datum = self.dp.prepare_updated_proposal_datum(
                proposal=proposal,
                vote=vote,
                voter_pkh=voter.pub_key_hash,
                dfc_amount=dfc_amount,
                owner_pkh=proposal["owner_pkh"],
                min_voting_tokens=proposal["min_voting_tokens"],
                authorized_pkhs=proposal["authorized_pkhs"],
                status=ProposalStatus.VOTING
            )
            datum_file, redeemer_file = self._write_temp_files(updated_datum, redeemer, "vote-proposal", proposal_id)

            # Prepare fee input and collateral from voter (not using proposal UTxO)
            fee_utxo = self.tx.find_utxos_at_address(
                voter.address, 
                min_lovelace=5000000,  # Enough for fees
                exclude=[]
            )
            if not fee_utxo:
                raise ContractError(f"No suitable UTxO found for fees for {voter.name}")
            
            txin_fee, txin_collateral = self.tx.prepare_tx_inputs(voter, fee_utxo[:1])
            txin_proposal = self.tx.create_tx_in(proposal_utxo)
            _, invalid_before, invalid_hereafter = self.tx.get_current_slot_and_validity()

            validator_addr_str = str(self.validator_address)
            voter_addr_str = str(voter.address)

            # Output: Send the proposal UTxO back to script with updated datum
            # Keep the same value as the input proposal UTxO
            proposal_lovelace = proposal_utxo.output.amount.coin
            proposal_dfc = self.tx.get_cnt_amount(proposal_utxo, self.policy_id)

            if proposal_dfc > 0:
                tx_out = f"{validator_addr_str}+{proposal_lovelace}+{proposal_dfc} {self.policy_id}.{self.token_name}"
            else:
                tx_out = f"{validator_addr_str}+{proposal_lovelace}"

            build_cmd = [
                "--change-address", voter_addr_str,
                "--tx-in", txin_fee,
                "--tx-in", txin_proposal,
                "--tx-in-script-file", f"{self.docker_assets}/dfct-governance.plutus",
                "--tx-in-inline-datum-present",
                "--tx-in-redeemer-file", f"{self.docker_assets}/{redeemer_file}",
                "--tx-in-collateral", txin_collateral,
                "--required-signer", f"{self.docker_assets}/{voter.name}.skey",
                "--invalid-before", str(invalid_before),
                "--invalid-hereafter", str(invalid_hereafter),
                "--tx-out", tx_out,
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{datum_file}"
            ]

            out_file = f"{self.docker_assets}/vote-proposal-{proposal_id}"
            transaction_hash = self.tx.build_and_submit_tx(build_cmd, voter, out_file, f"vote-proposal")

            logger.info(f"Vote submitted for proposal {proposal_id}")
            return {
                "transaction_hash": f"{transaction_hash}",
                "proposal_id": proposal_id,
                "vote": str(vote),
                "dfc_amount": str(dfc_amount)
            }

        except Exception as e:
            logger.error(f"Failed to vote on proposal: {str(e)}")
            raise ContractError(f"Failed to vote on proposal: {str(e)}")

    def finalize_proposal(
            self,
            proposal_id: str,
            outcome: int,
            wallet: CardanoWallet
    ) -> dict[str, str]:
        """
        Finalize voting on a governance proposal and update its status.
        """
        try:
            proposal_utxo, datum = self._get_contract_utxo_and_datum(proposal_id, 1, 0, 0)
            if not proposal_utxo:
                raise ContractError(f"Proposal with ID {proposal_id} not found")

            proposal = self.dp.extract_proposal_from_datum(datum)
            if not proposal:
                raise ContractError("Failed to extract proposal from datum")
            proposal_status = proposal["status"]
            if proposal_status != ProposalStatus.VOTING:
                raise ContractError(f"Proposal with ID {proposal_id} is not in voting state. Current status is {proposal_status}")

            new_status = ProposalStatus.APPROVED if outcome == 1 else ProposalStatus.REJECTED
            redeemer = self.dp.prepare_finalize_redeemer(proposal_id=proposal_id, outcome=outcome)
            updated_datum = self.dp.prepare_updated_proposal_datum(
                proposal=proposal,
                owner_pkh=proposal["owner_pkh"],
                min_voting_tokens=proposal["min_voting_tokens"],
                authorized_pkhs=proposal["authorized_pkhs"],
                status=new_status
            )
            datum_file, redeemer_file = self._write_temp_files(updated_datum, redeemer, "finalize-proposal", proposal_id)

            txin_fee, txin_collateral = self.tx.prepare_tx_inputs(wallet, [proposal_utxo])
            txin_proposal = self.tx.create_tx_in(proposal_utxo)
            _, invalid_before, invalid_hereafter = self.tx.get_current_slot_and_validity()

            validator_addr_str = str(self.validator_address)
            wallet_addr_str = str(wallet.address)
            tx_out = self._prepare_validator_output(proposal_utxo, validator_addr_str)

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
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{datum_file}"
            ]
            build_cmd = self._add_change_output(build_cmd, wallet_addr_str, [proposal_utxo], proposal_utxo.output.amount.coin, self.tx.get_cnt_amount(proposal_utxo, self.policy_id))

            out_file = f"{self.docker_assets}/finalize-proposal-{proposal_id}"
            transaction_hash = self.tx.build_and_submit_tx(build_cmd, wallet, out_file, f"finalize-proposal")

            logger.info(f"Proposal {proposal_id} finalized with outcome: {new_status}")
            return {
                "transaction_hash": f"{transaction_hash}",
                "proposal_id": proposal_id,
                "outcome": str(outcome)
            }

        except Exception as e:
            logger.error(f"Failed to finalize proposal: {str(e)}")
            raise ContractError(f"Failed to finalize proposal: {str(e)}")

    def update_authorized_pkhs(self, proposal_id: str, new_pkhs: dict[str, int], wallet: CardanoWallet) -> dict[str, str]:
        """
        Update the authorized PKHs map.
        """
        try:
            if not new_pkhs:
                raise ContractError("Authorized PKHs map cannot be empty")

            if any(weight <= 0 for weight in new_pkhs.values()):
                raise ContractError("All PKH weights must be positive")

            proposal_utxo, datum = self._get_contract_utxo_and_datum(proposal_id, 1, 0, 0)
            if not proposal_utxo:
                raise ContractError("No governance UTxO found")

            proposal = self.dp.extract_proposal_from_datum(datum)
            if not proposal:
                raise ContractError("Failed to extract proposal from datum")

            redeemer = {
                "constructor": 2,  # UpdateAuthorizedPKHs
                "fields": [
                    {"map": [{"k": {"bytes": pkh}, "v": {"int": weight}} for pkh, weight in new_pkhs.items()]}
                ]
            }
            updated_datum = self.dp.prepare_updated_proposal_datum(
                proposal=proposal,
                owner_pkh=proposal["owner_pkh"],
                min_voting_tokens=proposal["min_voting_tokens"],
                authorized_pkhs=new_pkhs
            )
            datum_file, redeemer_file = self._write_temp_files(updated_datum, redeemer, "update-pkhs", "global")

            txin_fee, txin_collateral = self.tx.prepare_tx_inputs(wallet, [proposal_utxo])
            txin_proposal = self.tx.create_tx_in(proposal_utxo)
            _, invalid_before, invalid_hereafter = self.tx.get_current_slot_and_validity()

            validator_addr_str = str(self.validator_address)
            wallet_addr_str = str(wallet.address)
            tx_out = self._prepare_validator_output(proposal_utxo, validator_addr_str)

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
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{datum_file}"
            ]
            build_cmd = self._add_change_output(build_cmd, wallet_addr_str, [proposal_utxo], proposal_utxo.output.amount.coin, self.tx.get_cnt_amount(proposal_utxo, self.policy_id))

            out_file = f"{self.docker_assets}/update-authorized-pkhs"
            transaction_hash = self.tx.build_and_submit_tx(build_cmd, wallet, out_file, f"update-authorized-pkhs")

            logger.info(f"Authorized PKHs updated")
            return {
                "transaction_hash": f"{transaction_hash}"
            }

        except Exception as e:
            logger.error(f"Failed to update authorized PKHs: {str(e)}")
            raise ContractError(f"Failed to update authorized PKHs: {str(e)}")

    def update_min_voting_tokens(self, proposal_id: str, min_tokens: int, wallet: CardanoWallet) -> dict[str, str]:
        """
        Update the minimum voting tokens requirement.
        """
        try:
            if min_tokens <= 0:
                raise ContractError("Minimum voting tokens must be positive")

            proposal_utxo, datum = self._get_contract_utxo_and_datum(proposal_id, 1, 0, 0)
            if not proposal_utxo:
                raise ContractError("No governance UTxO found")

            proposal = self.dp.extract_proposal_from_datum(datum)
            if not proposal:
                raise ContractError("Failed to extract proposal from datum")

            redeemer = {
                "constructor": 3,  # UpdateMinVotingTokens
                "fields": [{"int": min_tokens}]
            }
            updated_datum = self.dp.prepare_updated_proposal_datum(
                proposal=proposal,
                owner_pkh=proposal["owner_pkh"],
                min_voting_tokens=min_tokens,
                authorized_pkhs=proposal["authorized_pkhs"]
            )
            datum_file, redeemer_file = self._write_temp_files(updated_datum, redeemer, "update-min-tokens", "global")

            txin_fee, txin_collateral = self.tx.prepare_tx_inputs(wallet, [proposal_utxo])
            txin_proposal = self.tx.create_tx_in(proposal_utxo)
            _, invalid_before, invalid_hereafter = self.tx.get_current_slot_and_validity()

            validator_addr_str = str(self.validator_address)
            wallet_addr_str = str(wallet.address)
            tx_out = self._prepare_validator_output(proposal_utxo, validator_addr_str)

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
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{datum_file}"
            ]
            build_cmd = self._add_change_output(build_cmd, wallet_addr_str, [proposal_utxo], proposal_utxo.output.amount.coin, self.tx.get_cnt_amount(proposal_utxo, self.policy_id))

            out_file = f"{self.docker_assets}/update-min-voting-tokens"
            transaction_hash = self.tx.build_and_submit_tx(build_cmd, wallet, out_file, f"update-min-voting-tokens")

            logger.info(f"Minimum voting tokens updated to {min_tokens}")
            return {
                "transaction_hash": f"{transaction_hash}"
            }

        except Exception as e:
            logger.error(f"Failed to update minimum voting tokens: {str(e)}")
            raise ContractError(f"Failed to update minimum voting tokens: {str(e)}")

    def set_voting_period(self, proposal_id: str, voting_start: int, voting_end: int, wallet: CardanoWallet) -> dict[str, str]:
        """
        Set the voting period for a proposal.
        """
        try:
            if voting_start >= voting_end:
                raise ContractError("Voting start time must be before end time")

            proposal_utxo, datum = self._get_contract_utxo_and_datum(proposal_id, 1, 0, 0)
            if not proposal_utxo:
                raise ContractError(f"Proposal with ID {proposal_id} not found")

            proposal = self.dp.extract_proposal_from_datum(datum)
            if not proposal:
                raise ContractError("Failed to extract proposal from datum")
            if proposal["status"] != ProposalStatus.PROPOSED:
                raise ContractError(f"Proposal with ID {proposal_id} is not in proposed state. Current status is {proposal['status']}")

            redeemer = {
                "constructor": 4,  # SetVotingPeriod
                "fields": [
                    {"bytes": str_to_hex(proposal_id)},
                    {"int": voting_start},
                    {"int": voting_end}
                ]
            }

            # Keep status as PROPOSED - don't change it to VOTING
            updated_datum = self.dp.prepare_updated_proposal_datum(
                proposal=proposal,
                owner_pkh=proposal["owner_pkh"],
                min_voting_tokens=proposal["min_voting_tokens"],
                authorized_pkhs=proposal["authorized_pkhs"],
                status=ProposalStatus.PROPOSED,
                voting_start=voting_start,
                voting_end=voting_end
            )
            datum_file, redeemer_file = self._write_temp_files(updated_datum, redeemer, "set-voting-period", proposal_id)

            txin_fee, txin_collateral = self.tx.prepare_tx_inputs(wallet, [proposal_utxo])
            txin_proposal = self.tx.create_tx_in(proposal_utxo)
            _, invalid_before, invalid_hereafter = self.tx.get_current_slot_and_validity()

            validator_addr_str = str(self.validator_address)
            wallet_addr_str = str(wallet.address)
            tx_out = self._prepare_validator_output(proposal_utxo, validator_addr_str)

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
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{datum_file}"
            ]
            build_cmd = self._add_change_output(build_cmd, wallet_addr_str, [proposal_utxo], proposal_utxo.output.amount.coin, self.tx.get_cnt_amount(proposal_utxo, self.policy_id))

            out_file = f"{self.docker_assets}/set-voting-period-{proposal_id}"
            transaction_hash = self.tx.build_and_submit_tx(build_cmd, wallet, out_file, f"set-voting-period")

            logger.info(f"Voting period set for proposal {proposal_id}")
            return {
                "transaction_hash": f"{transaction_hash}",
                "proposal_id": proposal_id
            }

        except Exception as e:
            logger.error(f"Failed to set voting period: {str(e)}")
            raise ContractError(f"Failed to set voting period: {str(e)}")

    def execute_proposal(self, proposal_id: str, wallet: CardanoWallet) -> dict[str, str]:
        """
        Execute an approved governance proposal.
        """
        try:
            proposal_utxo, datum = self._get_contract_utxo_and_datum(proposal_id, 1, 0, 0)
            if not proposal_utxo:
                raise ContractError(f"Proposal with ID {proposal_id} not found")

            proposal = self.dp.extract_proposal_from_datum(datum)
            if not proposal:
                raise ContractError("Failed to extract proposal from datum")
            if proposal["status"] != ProposalStatus.APPROVED:
                raise ContractError(f"Proposal with ID {proposal_id} is not in approved state. Current status is {proposal['status']}")

            redeemer = {
                "constructor": 6,  # ExecuteProposal
                "fields": [{"bytes": str_to_hex(proposal_id)}]
            }
            updated_datum = self.dp.prepare_updated_proposal_datum(
                proposal=proposal,
                owner_pkh=proposal["owner_pkh"],
                min_voting_tokens=proposal["min_voting_tokens"],
                authorized_pkhs=proposal["authorized_pkhs"],
                status=ProposalStatus.EXECUTED
            )
            datum_file, redeemer_file = self._write_temp_files(updated_datum, redeemer, "execute-proposal", proposal_id)

            txin_fee, txin_collateral = self.tx.prepare_tx_inputs(wallet, [proposal_utxo])
            txin_proposal = self.tx.create_tx_in(proposal_utxo)
            _, invalid_before, invalid_hereafter = self.tx.get_current_slot_and_validity()

            validator_addr_str = str(self.validator_address)
            wallet_addr_str = str(wallet.address)
            tx_out = self._prepare_validator_output(proposal_utxo, validator_addr_str)

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
                "--tx-out-inline-datum-file", f"{self.docker_assets}/{datum_file}"
            ]
            build_cmd = self._add_change_output(build_cmd, wallet_addr_str, [proposal_utxo], proposal_utxo.output.amount.coin, self.tx.get_cnt_amount(proposal_utxo, self.policy_id))

            out_file = f"{self.docker_assets}/execute-proposal-{proposal_id}"
            transaction_hash = self.tx.build_and_submit_tx(build_cmd, wallet, out_file, f"execute-proposal")

            logger.info(f"Proposal {proposal_id} executed")
            return {
                "transaction_hash": f"{transaction_hash}",
                "proposal_id": proposal_id
            }

        except Exception as e:
            logger.error(f"Failed to execute proposal: {str(e)}")
            raise ContractError(f"Failed to execute proposal: {str(e)}")

    def get_proposal(self, proposal_id: str) -> Optional[dict[str, Any]]:
        """
        Retrieve a proposal by its ID.
        """
        try:
            _, datum = self._get_contract_utxo_and_datum(proposal_id, 1, 0, 0)
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