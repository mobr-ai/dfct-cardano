from typing import Any
import logging
from enum import Enum
from dfctbackend.cardano.utils import str_to_hex
from dfctbackend.cardano.contract.datum import DatumProcessor

logger = logging.getLogger(__name__)

class ProposalStatus(Enum):
    PROPOSED = 0
    VOTING = 1
    APPROVED = 2
    REJECTED = 3
    EXECUTED = 4

class GovernanceDatumProcessor(DatumProcessor):
    """
    Class for handling governance-related datum processing in DFCT contracts.
    """
    def prepare_proposal_datum_redeemer(
        self,
        proposal_id: str,
        owner_pkh: str,
        proposer_pkh: str,
        min_voting_tokens: int,
        voting_start: int,
        voting_end: int,
        authorized_pkhs: dict
    ) -> tuple[dict[str, Any], dict[str, Any], str]:
        """
        Prepare a governance proposal datum and redeemer, matching the Plutus DFCTProposal and DFCTGovernanceDatum.
        """
        plutus_proposal = {
            "constructor": 0,  # DFCTProposal constructor
            "fields": [
                {"bytes": str_to_hex(proposal_id)},  # proposalId
                {"bytes": proposer_pkh},  # proposer
                {"int": voting_start},  # votingStart
                {"int": voting_end},  # votingEnd
                {"map": []},  # voteTally (empty initially)
                {"int": 2}  # proposalOutcome (2 = ongoing, not Maybe type)
            ]
        }

        plutus_params = {
            "constructor": 0,  # DFCTGovernanceParams constructor
            "fields": [
                {"bytes": owner_pkh},  # gpOwner
                {"int": min_voting_tokens},  # gpMinVotingTokens
                {"map": [{"k": {"bytes": pkh}, "v": {"int": weight}} for pkh, weight in authorized_pkhs.items()]}  # gpAuthorizedPKHs
            ]
        }

        datum = {
            "constructor": 0,  # DFCTGovernanceDatum constructor
            "fields": [
                plutus_params,  # govParams
                {
                    "constructor": 0,  # Just constructor for Maybe DFCTProposal
                    "fields": [plutus_proposal]  # The proposal wrapped in Just
                },
                {"constructor": ProposalStatus.PROPOSED.value, "fields": []}  # proposalStatus
            ]
        }

        redeemer = {
            "constructor": 0,  # SubmitProposal
            "fields": [plutus_proposal]
        }

        return datum, redeemer, proposal_id

    def prepare_vote_redeemer(
        self,
        proposal_id: str,
        vote: int,
        dfc_amount: int
    ) -> dict[str, Any]:
        """
        Prepare a vote redeemer for a governance proposal.
        """
        proposal_id_hex = str_to_hex(proposal_id)
        redeemer = {
            "constructor": 1, #VoteOnProposal
            "fields": [
                {"bytes": proposal_id_hex}, # proposal ID
                {"int": vote},              # 1 for approve, 0 for reject
                {"int": dfc_amount}         # vote weight
            ]
        }
        return redeemer

    def prepare_finalize_redeemer(self, proposal_id: str, outcome: int) -> dict[str, Any]:
        """
        Prepare a finalize redeemer for a governance proposal.
        """
        proposal_id_hex = str_to_hex(proposal_id)
        redeemer = {
            "constructor": 5,  # FinalizeProposal
            "fields": [
                {"bytes": proposal_id_hex},
                {"int": outcome}
            ]
        }
        return redeemer

    def prepare_updated_proposal_datum(
        self,
        proposal: dict,
        owner_pkh: str,
        min_voting_tokens: int,
        authorized_pkhs: dict,
        vote: int = None,
        voter_pkh: str = None,
        dfc_amount: int = 0,
        status: ProposalStatus = None,
        voting_start: int = 0,
        voting_end: int = 0
    ) -> dict[str, Any]:
        """
        Prepare an updated proposal datum after voting, finalization, or period update.
        """
        try:
            # Extract fields from the proposal dictionary
            proposal_id:str = proposal.get("proposal_id", "")
            proposer_pkh:str = proposal.get("proposer_pkh", "")
            current_voting_start:int = proposal.get("voting_start", 0)
            current_voting_end:int = proposal.get("voting_end", 0)
            vote_tally:dict = proposal.get("vote_tally", {})
            current_status:ProposalStatus = proposal.get("status", ProposalStatus.PROPOSED)

            # Use provided voting times or fall back to current ones
            voting_start = voting_start if voting_start is not 0 else current_voting_start
            voting_end = voting_end if voting_end is not 0 else current_voting_end

            # Build vote tally map
            vote_tally_map = []
            for k, v in vote_tally.items():
                vote_tally_map.append({"k": {"bytes": k}, "v": {"int": v}})

            # Update voteTally if a vote is provided
            if vote is not None and voter_pkh and dfc_amount > 0:
                vote_tally_map.append({"k": {"bytes": voter_pkh}, "v": {"int": dfc_amount}})

            # Determine proposal outcome
            if status in [ProposalStatus.APPROVED, ProposalStatus.REJECTED]:
                proposal_outcome = 1 if status == ProposalStatus.APPROVED else 0
            else:
                proposal_outcome = 2  # ongoing

            # Construct the Plutus proposal structure
            plutus_proposal = {
                "constructor": 0,
                "fields": [
                    {"bytes": str_to_hex(proposal_id)},  # proposalId
                    {"bytes": proposer_pkh},  # proposer
                    {"int": voting_start},  # votingStart
                    {"int": voting_end},  # votingEnd
                    {"map": vote_tally_map},  # voteTally
                    {"int": proposal_outcome}  # proposalOutcome
                ]
            }

            # Construct governance parameters
            plutus_params = {
                "constructor": 0,
                "fields": [
                    {"bytes": owner_pkh},  # gpOwner
                    {"int": min_voting_tokens},  # gpMinVotingTokens
                    {"map": [{"k": {"bytes": pkh}, "v": {"int": weight}} for pkh, weight in authorized_pkhs.items()]}  # gpAuthorizedPKHs
                ]
            }

            # Construct the updated datum
            updated_datum = {
                "constructor": 0,  # DFCTGovernanceDatum
                "fields": [
                    plutus_params,  # govParams
                    {
                        "constructor": 0,  # Just constructor for Maybe DFCTProposal
                        "fields": [plutus_proposal]  # proposal
                    },
                    {"constructor": (status or current_status).value, "fields": []}  # proposalStatus
                ]
            }

            return updated_datum

        except Exception as e:
            logger.error(f"Failed to prepare updated proposal datum: {str(e)}")
            raise

    def extract_proposal_from_datum(self, proposal_datum: list) -> dict[str, Any]:
        """
        Extract governance proposal data from its datum structure.
        """
        try:
            # Extract govParams
            gov_params = proposal_datum[0]
            if len(gov_params) != 3:
                logger.error("Invalid govParams structure")
                return {}

            owner_pkh = gov_params[0]
            min_voting_tokens = gov_params[1]
            authorized_pkhs = gov_params[2]

            # Extract status
            status = self.extract_int_from_datum(proposal_datum, 2)

            # Extract proposal
            proposal = proposal_datum[1][0]
            if not proposal:  # No proposal (Nothing)
                result = {
                    "owner_pkh": owner_pkh,
                    "min_voting_tokens": min_voting_tokens,
                    "authorized_pkhs": authorized_pkhs,
                    "status": ProposalStatus(status)
                }

                logger.info(f"Empty proposal. result:{result}")
                return result

            # Extract proposal details
            proposal_id = proposal[0]
            proposer_pkh = proposal[1]
            voting_start = proposal[2]
            voting_end = proposal[3]
            vote_tally = proposal[4]
            outcome = proposal[5]

            result = {
                "proposal_id": proposal_id,
                "proposer_pkh": proposer_pkh,
                "voting_start": voting_start,
                "voting_end": voting_end,
                "vote_tally": vote_tally,
                "outcome": outcome,
                "status": ProposalStatus(status),
                "owner_pkh": owner_pkh,
                "min_voting_tokens": min_voting_tokens,
                "authorized_pkhs": authorized_pkhs
            }

            logger.info(f"datum: result:{result}")
            return result

        except Exception as e:
            logger.error(f"Failed to extract proposal data: {str(e)}")
            return {}