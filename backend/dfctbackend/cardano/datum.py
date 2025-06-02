import time
from typing import Any
import logging
import cbor2
from pycardano import UTxO
from enum import Enum

from dfctbackend.config import settings
from dfctbackend.cardano.wallet import local_wallets
from dfctbackend.cardano.utils import (
    generate_topic_id, str_to_hex
)

class TopicStatus(Enum):
    PROPOSED = 0
    REVIEWED = 1
    ACTIVATED = 2
    CLOSED = 3
    REJECTED = 4

class ContributionStatus(Enum):
    PROPOSED = 0
    REVIEWED = 1
    DISPUTED = 2
    UPDATED = 3
    REJECTED = 4
    VERIFIED = 5
    EVALUATED = 6
    REWARDS_DISTRIBUTED = 7
    POOL_EVALUATED = 8

class ContributionType(Enum):
    EVIDENCE = 0
    TAG_SELECTION = 1
    VOTE_CASTED = 2

class ProposalStatus(Enum):
    PROPOSED = 0
    VOTING = 1
    APPROVED = 2
    REJECTED = 3

logger = logging.getLogger(__name__)

class TopicTrack:
    def __init__(self, current_status, contrib_no=0, utxos_no=0):
        self.current_status:TopicStatus=current_status
        self.contrib_no:int=contrib_no
        self.utxos_no:int=utxos_no

class DatumProcessor:
    """
    Class for extracting, creating, encoding and decoding datum for DFCT contracts.
    Uses pycardano for UTxO structure.
    """
    def __init__(self):
        """Initialize the contract with chain context and transaction handling."""
        self.token_name = settings.TOKEN_NAME
        self.token_name_hex = str_to_hex(settings.TOKEN_NAME)

    def prepare_topic_json(self, datum:dict, status:int) -> dict:
        return self._prepare_topic_json(datum[0], status, datum[2], datum[3])

    def _prepare_topic_json(self, topic:list, status:int, reward_pool:list, reviewers:list) -> dict:
        reviewer_map = []
        if reviewers:
            reviewer_map = [
                {
                    "k": {"bytes": pkh},
                    "v": {"int": 1}
                }
                for pkh in reviewers
            ]

        plutus_topic = [
            {"bytes": str_to_hex(topic[0])},  # topicId
            {"bytes": str_to_hex(topic[1])},  # topicTitle
            {"bytes": str_to_hex(topic[2])},  # topicDescription
            {"bytes": topic[3]},  # topicProposer
            {"int":   topic[4]}   # topicTimestamp
        ]

        plutus_rpi = [
            {"int":   reward_pool[0]},             # totalAmount
            {"int":   reward_pool[1]},             # allocatedAmount
            {"bytes": str_to_hex(reward_pool[2])}, # tokenName
            {"int":   reward_pool[3]}              # creationTimestamp
        ]

        datum = {
            "constructor": 0,  # TopicDatum constructor
            "fields": [
                {
                    "constructor": 0,  # Topic constructor
                    "fields": plutus_topic
                },
                {
                    "constructor": status,  # TopicClosed status
                    "fields": []
                },
                {
                    "constructor": 0,  # RewardPoolInfo constructor
                    "fields": plutus_rpi
                },
                {"map": reviewer_map}  # authorizedReviewers (ReviewerMap)
            ]
        }

        redeemer = {
            "constructor": 0,
            "fields": [
                {
                "constructor": 0,
                "fields": [
                    {
                    "constructor": 0,
                    "fields": plutus_topic
                    },
                    {
                    "constructor": 0,
                    "fields": plutus_rpi
                    }
                ]
                }
            ]
        }

        return datum, redeemer

    def prepare_topic_datum_redeemer(
        self,
        title: str,
        description: str,
        proposer_pkh: str,
        reward_amount: int = 1000,
        reviewers: list[str] = None
    ) -> tuple[dict[str, Any], dict[str, Any], str]:
        """
        Prepare a topic datum matching the exact format in generate-deploy-files.sh.
        """
        if reviewers is None:
            reviewers = [
                local_wallets["reviewer1"].public_key_hash,
                local_wallets["reviewer2"].public_key_hash
            ]

        topic_id = generate_topic_id()
        current_time = int(time.time() * 1000)
        topic_list = [topic_id, title, description, proposer_pkh, current_time]
        rpi_list = [reward_amount, 0, self.token_name, current_time]
        datum, redeemer = self._prepare_topic_json(topic_list, TopicStatus.PROPOSED.value, rpi_list, reviewers)

        return datum, redeemer, topic_id

    def prepare_review_topic_redeemer(self, topic_id: str, approved: bool) -> dict[str, Any]:
        """
        Prepare a review topic redeemer for the ReviewTopic action.

        Args:
            topic_id: The ID of the topic being reviewed.
            approved: Whether the topic is approved (True) or rejected (False).

        Returns:
            A dictionary representing the Plutus redeemer.
        """
        topic_id_hex = str_to_hex(topic_id)
        redeemer = {
            "constructor": 0,  # TopicAction
            "fields": [
                {
                    "constructor": 1,  # ReviewTopic
                    "fields": [
                        {"bytes": topic_id_hex},  # Topic ID
                        {"int": 1 if approved else 0}  # Approved (1 for True, 0 for False)
                    ]
                }
            ]
        }
        return redeemer

    def prepare_contribution_datum_redeemer(
        self,
        contribution_id: str,
        topic_id: str,
        contribution_type: str,
        content: str,
        contributor_pkh: str,
        contribution_status: int,
        timestamp: int
    ) -> tuple[dict[str, Any], dict[str, Any], str]:
        """
        Prepare a contribution datum and redeemer.
        """
        contribution_id_hex = str_to_hex(contribution_id)
        topic_id_hex = str_to_hex(topic_id)
        content_hex = str_to_hex(content)
        contribution_type_hex = str_to_hex(contribution_type)  # Default type

        plutus_contribution = [
            {"bytes": contribution_id_hex},
            {"bytes": topic_id_hex},
            {"bytes": contribution_type_hex},
            {"bytes": content_hex},
            {"bytes": contributor_pkh},
            {"int": timestamp},
            {"int": 1},  # version
            {"bytes": ""}  # previous version ID (empty for first version)
        ]

        # Create empty ReviewContent
        empty_review_content = {
            "constructor": 0,
            "fields": [
                {"bytes": ""},  # reviewerPkh (empty for new contributions)
                {"bytes": ""},  # refCntribId (empty for new contributions)
                {"bytes": ""},  # relevanceReason (empty for new contributions)
                {"bytes": ""},  # accuracyReason (empty for new contributions)
                {"bytes": ""},  # completenessReason (empty for new contributions)
                {"int": 0}      # reviewTimestamp (0 for new contributions)
            ]
        }

        # Create empty DisputeReason
        empty_dispute_reason = {
            "constructor": 0,
            "fields": [
                {"bytes": ""},  # disputeInitiator (empty for new contributions)
                {"bytes": ""},  # disputeContent (empty for new contributions)
                {"int": 0}      # disputeTimestamp (0 for new contributions)
            ]
        }

        # Create the contribution datum
        datum = {
            "constructor": 0,
            "fields": [
                {
                    "constructor": 0,
                    "fields": plutus_contribution
                },
                {
                    "constructor": contribution_status,
                    "fields": []
                },
                {"int": 0},  # relevance
                {"int": 0},  # accuracy
                {"int": 0},  # completeness
                empty_review_content,
                empty_dispute_reason,
                {"int": 0}   # timelinessScore (will be calculated on-chain)
            ]
        }

        # Create the redeemer for SubmitEvidence
        redeemer = {
            "constructor": 1,  # ContributionAction
            "fields": [
                {
                    "constructor": 0,  # SubmitEvidence
                    "fields": [
                        {
                            "constructor": 0,
                            "fields": plutus_contribution
                        }
                    ]
                }
            ]
        }
    
        return datum, redeemer, contribution_id

    def prepare_review_contribution_redeemer(
        self,
        contribution_id: str,
        reviewer_pkh: str,
        relevance: int,
        accuracy: int,
        completeness: int,
        review_content: str
    ) -> dict[str, Any]:
        """
        Prepare a review contribution redeemer.
        """
        contribution_id_hex = str_to_hex(contribution_id)
        current_time = int(time.time() * 1000)

        # Create review content structure
        review_content_obj = {
            "constructor": 0,
            "fields": [
                {"bytes": reviewer_pkh},
                {"bytes": contribution_id_hex},
                {"bytes": str_to_hex("Relevance: " + review_content)},   # Relevance reason
                {"bytes": str_to_hex("Accuracy: " + review_content)},    # Accuracy reason
                {"bytes": str_to_hex("Completeness: " + review_content)},# Completeness reason
                {"int": current_time}
            ]
        }

        # Create the redeemer for ReviewContribution
        redeemer = {
            "constructor": 1,  # ContributionAction
            "fields": [
                {
                    "constructor": 3,  # ReviewContribution
                    "fields": [
                        {"bytes": contribution_id_hex},
                        {"int": relevance},
                        {"int": accuracy},
                        {"int": completeness},
                        review_content_obj
                    ]
                }
            ]
        }

        return redeemer

    def prepare_dispute_contribution_redeemer(
        self,
        contribution_id: str,
        contributor_pkh: str,
        dispute_reason: str
    ) -> dict[str, Any]:
        """
        Prepare a dispute contribution redeemer.
        """
        contribution_id_hex = str_to_hex(contribution_id)
        dispute_reason_hex = str_to_hex(dispute_reason)
        current_time = int(time.time() * 1000)

        # Create dispute reason structure
        dispute_reason_obj = {
            "constructor": 0,
            "fields": [
                {"bytes": contributor_pkh},
                {"bytes": dispute_reason_hex},
                {"int": current_time}
            ]
        }

        # Create the redeemer for DisputeContribution
        redeemer = {
            "constructor": 1,  # ContributionAction
            "fields": [
                {
                    "constructor": 5,  # DisputeContribution
                    "fields": [
                        {"bytes": contribution_id_hex},
                        dispute_reason_obj
                    ]
                }
            ]
        }

        return redeemer

    def prepare_topic_action_redeemer(
        self,
        topic_id: str,
        action_constructor: int
    ) -> dict[str, Any]:
        """
        Prepare a generic topic action redeemer.
        Action constructors:
        2 - ActivateTopic
        3 - CloseTopic
        4 - RejectTopic
        """
        topic_id_hex = str_to_hex(topic_id)

        redeemer = {
            "constructor": 0,  # TopicAction
            "fields": [
                {
                    "constructor": action_constructor,
                    "fields": [
                        {"bytes": topic_id_hex}
                    ]
                }
            ]
        }

        return redeemer

    def extract_status_from_datum(self, datum):
        status = datum[1]
        if not status:
            status = 0
        elif type(status) is list:
            status = status[0]
        elif type(status) is dict:
            status = status.keys()[0]

        return status

    def extract_topic_from_datum(self, topic_datum: list) -> dict[str, Any]:
        try:
            if len(topic_datum) > 4:
                return None

            topic_details = topic_datum[0]
            topic_id = topic_details[0]
            title = topic_details[1]
            description = topic_details[2]
            proposer = topic_details[3]
            timestamp = topic_details[4]
            status = self.extract_status_from_datum(topic_datum)

            # Get reward pool information - third element
            reward_info = topic_datum[2]
            total_amount = reward_info[0]
            allocated_amount = reward_info[1]
            token_name = reward_info[2]
            creation_timestamp = reward_info[3]

            # Get reviewers information - fourth element
            reviewers = []
            if len(topic_datum) > 3:
                reviewers = topic_datum[3]

            # Construct the result
            result = {
                "topic_id": topic_id,
                "title": title,
                "description": description,
                "proposer": proposer,
                "timestamp": timestamp,
                "status": TopicStatus(status),
                "reward_pool": {
                    "total_amount": total_amount,
                    "allocated_amount": allocated_amount,
                    "token_name": token_name,
                    "creation_timestamp": creation_timestamp
                },
                "reviewers": reviewers
            }

            return result

        except Exception as e:
            logger.error(f"Failed to extract topic data: {str(e)}")
            # Print more detailed error information for debugging
            import traceback
            logger.error(traceback.format_exc())
            return {}

    def extract_contribution_from_datum(self, datum: list[Any]) -> dict[str, Any]:
        """
        Extract contribution data from its datum structure.
        """
        try:
            if len(datum) < 5:
                return None

            # Extract contribution details
            contribution_details = datum[0]
            contribution_id = contribution_details[0]
            topic_id = contribution_details[1]
            contribution_type = contribution_details[2]
            content = contribution_details[3]
            creator = contribution_details[4]
            timestamp = contribution_details[5]
            version = contribution_details[6]
            previous_version_id = contribution_details[7]
            status = self.extract_status_from_datum(datum)

            # Get review scores
            relevance = datum[2]
            accuracy = datum[3]
            completeness = datum[4]

            # Get review content
            review_content = {}
            if len(datum) > 5:
                d_review_content = datum[5]
                if isinstance(d_review_content, list) and len(d_review_content) >= 6:
                    try:
                        reviewer = d_review_content[0]
                        ref_contribution_id = d_review_content[1]
                        relevance_reason = d_review_content[2]
                        accuracy_reason = d_review_content[3]
                        completeness_reason = d_review_content[4]
                        review_timestamp = d_review_content[5]

                        review_content = {
                            "reviewer": reviewer,
                            "contribution_id": ref_contribution_id,
                            "relevance_reason": relevance_reason,
                            "accuracy_reason": accuracy_reason,
                            "completeness_reason": completeness_reason,
                            "timestamp": review_timestamp
                        }

                    except Exception as e:
                        logger.warning(f"Error processing review content: {e}")

            # Get dispute reason
            dispute_reason = {}
            if len(datum) > 6:
                d_dispute_reason = datum[6]
                if isinstance(d_dispute_reason, list) and len(d_dispute_reason) >= 3:
                    try:
                        initiator = d_dispute_reason[0]
                        reason = d_dispute_reason[1]
                        dispute_timestamp = d_dispute_reason[2]

                        dispute_reason = {
                            "initiator": initiator,
                            "reason": reason,
                            "timestamp": dispute_timestamp
                        }

                    except Exception as e:
                        logger.warning(f"Error processing dispute reason: {e}")

            # Get timeliness score
            timeliness_score = datum[7] if len(datum) > 7 else 0
            timeliness_int = int(timeliness_score) if isinstance(timeliness_score, (int, str)) else 0

            # Calculate total score based on the 3 review scores + timeliness
            total_score = int(relevance) + int(accuracy) + int(completeness) + int(timeliness_int)

            # Construct the result
            result = {
                "contribution_id": contribution_id,
                "topic_id": topic_id,
                "type": contribution_type,
                "content": content,
                "creator": creator,
                "timestamp": timestamp,
                "version": version,
                "previous_version_id": previous_version_id,
                "status": ContributionStatus(status),
                "relevance": relevance,
                "accuracy": accuracy,
                "completeness": completeness,
                "timeliness_score": timeliness_score,
                "total_score": total_score,
                "review_content": review_content,
                "dispute_reason": dispute_reason
            }

            return result

        except Exception as e:
            logger.error(f"Failed to extract contribution data: {str(e)}")
            # Print more detailed error information for debugging
            import traceback
            logger.error(traceback.format_exc())
            return {}


    def prepare_proposal_datum_redeemer(
        self,
        title: str,
        description: str,
        proposer_pkh: str,
        reward_amount: int = 1000
    ) -> tuple[dict[str, Any], dict[str, Any], str]:
        """
        Prepare a governance proposal datum and redeemer.
        """
        proposal_id = generate_proposal_id()
        current_time = int(time.time() * 1000)
        proposal_list = [proposal_id, title, description, proposer_pkh, current_time]
        rpi_list = [reward_amount, 0, self.token_name, current_time]

        plutus_proposal = [
            {"bytes": str_to_hex(proposal_id)},
            {"bytes": str_to_hex(title)},
            {"bytes": str_to_hex(description)},
            {"bytes": proposer_pkh},
            {"int": current_time}
        ]

        plutus_rpi = [
            {"int": reward_amount},
            {"int": 0},
            {"bytes": self.token_name_hex},
            {"int": current_time}
        ]

        datum = {
            "constructor": 0,
            "fields": [
                {
                    "constructor": 0,
                    "fields": plutus_proposal
                },
                {
                    "constructor": ProposalStatus.PROPOSED.value,
                    "fields": []
                },
                {
                    "constructor": 0,
                    "fields": plutus_rpi
                },
                {"list": []}  # votes
            ]
        }

        redeemer = {
            "constructor": 0,
            "fields": [
                {
                    "constructor": 0,  # SubmitProposal
                    "fields": [
                        {
                            "constructor": 0,
                            "fields": plutus_proposal
                        },
                        {
                            "constructor": 0,
                            "fields": plutus_rpi
                        }
                    ]
                }
            ]
        }

        return datum, redeemer, proposal_id

    def prepare_vote_redeemer(
        self,
        proposal_id: str,
        voter_pkh: str,
        vote: bool,
        dfc_amount: int
    ) -> dict[str, Any]:
        """
        Prepare a vote redeemer for a governance proposal.
        """
        proposal_id_hex = str_to_hex(proposal_id)
        redeemer = {
            "constructor": 0,
            "fields": [
                {
                    "constructor": 1,  # Vote
                    "fields": [
                        {"bytes": proposal_id_hex},
                        {"bytes": voter_pkh},
                        {"int": 1 if vote else 0},
                        {"int": dfc_amount}
                    ]
                }
            ]
        }
        return redeemer

    def prepare_finalize_redeemer(self, proposal_id: str) -> dict[str, Any]:
        """
        Prepare a finalize redeemer for a governance proposal.
        """
        proposal_id_hex = str_to_hex(proposal_id)
        redeemer = {
            "constructor": 0,
            "fields": [
                {
                    "constructor": 2,  # FinalizeVote
                    "fields": [
                        {"bytes": proposal_id_hex}
                    ]
                }
            ]
        }
        return redeemer

    def prepare_updated_proposal_datum(
        self,
        datum: dict,
        vote: bool = None,
        voter_pkh: str = None,
        dfc_amount: int = 0,
        status: ProposalStatus = None
    ) -> dict[str, Any]:
        """
        Prepare an updated proposal datum after voting or finalization.
        """
        updated_datum = datum.copy()
        if vote is not None and voter_pkh and dfc_amount:
            vote_record = {
                "constructor": 0,
                "fields": [
                    {"bytes": voter_pkh},
                    {"int": 1 if vote else 0},
                    {"int": dfc_amount}
                ]
            }
            updated_datum["fields"][3]["list"].append(vote_record)
        if status:
            updated_datum["fields"][1] = {
                "constructor": status.value,
                "fields": []
            }
        return updated_datum

    def extract_proposal_from_datum(self, proposal_datum: dict) -> dict[str, Any]:
        """
        Extract governance proposal data from its datum structure.
        """
        try:
            if len(proposal_datum) != 4:
                return None

            # Extract proposal details
            proposal_details = proposal_datum[0]["fields"]
            proposal_id = proposal_details[0]["bytes"]
            title = proposal_details[1]["bytes"]
            description = proposal_details[2]["bytes"]
            proposer = proposal_details[3]["bytes"]
            timestamp = proposal_details[4]["int"]
            status = ProposalStatus(proposal_datum[1]["constructor"])

            # Extract reward pool information
            reward_info = proposal_datum[2]["fields"]
            total_amount = reward_info[0]["int"]
            allocated_amount = reward_info[1]["int"]
            token_name = reward_info[2]["bytes"]
            creation_timestamp = reward_info[3]["int"]

            # Extract votes
            votes = []
            for vote in proposal_datum[3]["list"]:
                vote_record = {
                    "voter_pkh": vote["fields"][0]["bytes"],
                    "vote": bool(vote["fields"][1]["int"]),
                    "dfc_amount": vote["fields"][2]["amount"]
                }
                votes.append(vote_record)
            result = {
                "proposal_id": proposal_id,
                "title": title,
                "description": description,
                "proposer_pkh": proposer,
                "timestamp": timestamp,
                "status": status,
                "reward_pool": {
                    "total_amount": total_amount,
                    "allocated_amount": allocated_amount,
                    "token_name": token_name,
                    "creation_timestamp": creation_timestamp
                },
                "votes": votes
            }

            return result

        except Exception as e:
            logger.error(f"Failed to extract proposal data: {str(e)}")
            # Print more detailed error information for debugging
            import traceback
            logger.error(traceback.format_exc())
            return {}

    def _decode_value(self, value:Any) -> Any:
        if isinstance(value, bytes):
            datum = value
            if "\\" in str(value):
                return value.hex()

            return bytes(datum).decode('utf-8')

        if type(value) is str and str(value).startswith("b'"):
            return self._decode_value(bytes(value))

        return value

    def _decode_datum_list(self, datum_list: list) -> list:
        """
        Decode a datum's list.
        """
        assert (type(datum_list) is list)

        decoded = []
        for item in datum_list:
            new_item = item
            if isinstance(item, cbor2.CBORTag):
                new_item = self._decode_cbor_tag(item)

            elif isinstance(item, dict):
                new_item = self._decode_datum_dict(item)
            elif isinstance(item, list):
                new_item = self._decode_datum_list(item)
            else:
                new_item = self._decode_value(item)

            decoded.append(new_item)

        return decoded

    def _decode_datum_dict(self, datum_dict: dict) -> dict:
        """
        Decode a datum's dict.
        """
        assert (type(datum_dict) is dict)

        decoded = {}
        for key, value in datum_dict.items():
            d_key = self._decode_value(key)

            new_value = value
            if isinstance(value, cbor2.CBORTag):
                new_value = self._decode_cbor_tag(value)
            elif isinstance(value, list):
                new_value = self._decode_datum_list(value)
            elif isinstance(value, dict):
                new_value = self._decode_datum_dict(value)
            else:
                new_value = self._decode_value(value)

            decoded[d_key] = new_value

        return decoded

    def _decode_cbor_tag(self, datum: cbor2.CBORTag):
        """
        Decode a CBORTag object.
        """
        assert (isinstance(datum, cbor2.CBORTag))

        if not datum.value:
            decoded = (datum.tag - 121)
        else:
            decoded = self._decode_datum_bytes(datum.value)

        return decoded

    def _decode_datum_bytes(self, datum: list | dict):
        """
        Decode a datum's bytes.
        """

        if isinstance(datum, cbor2.CBORTag):
            return self._decode_cbor_tag(datum)

        if type(datum) is list:
            return self._decode_datum_list(datum)

        if type(datum) is dict:
            return self._decode_datum_dict(datum)

        return self._decode_value(datum)

    def decode_utxo_datum(self, topic_utxo:UTxO) -> list[Any]:
        """
        Extract and decode datum from a UTXO.

        Args:
            topic_utxo: The UTxO containing the datum to decode

        Returns:
            The decoded datum as a Python dictionary, or None if no datum is present
        """
        decoded = []
        if topic_utxo and hasattr(topic_utxo.output, 'datum') and topic_utxo.output.datum:
            datum = topic_utxo.output.datum

            # If datum is a RawCBOR object, decode it
            if hasattr(datum, "cbor") and datum.cbor:
                datum = cbor2.loads(datum.cbor)

            if datum:
                decoded = self._decode_datum_bytes(datum)

        return decoded

    def find_utxo_with_datum_id(self, utxos: list[UTxO], id_str: str) -> UTxO:
        """
        Find the UTxO with datum containing a specific ID in the id datum field.

        Args:
            utxos: List of utxos to find the id
            id_str: The id to look for in the datums

        Returns:
            UTxO containing the id in its datum
        """
        for utxo in utxos:
            decoded = self.decode_utxo_datum(utxo)
            if decoded:
                datum_id = decoded[0][0]
                hex_str_value = str_to_hex(id_str)
                if (datum_id == id_str or 
                    datum_id == hex_str_value
                ):
                    return utxo, decoded

        return None, None
