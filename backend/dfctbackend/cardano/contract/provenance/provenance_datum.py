from typing import Any
import logging
import time
from enum import Enum
from dfctbackend.cardano.wallet import local_wallets
from dfctbackend.cardano.utils import str_to_hex
from dfctbackend.cardano.contract.datum import DatumProcessor

logger = logging.getLogger(__name__)

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

class TopicTrack:
    def __init__(self, current_status, contrib_no=0, utxos_no=0):
        self.current_status: TopicStatus = current_status
        self.contrib_no: int = contrib_no
        self.utxos_no: int = utxos_no

class ProvenanceDatumProcessor(DatumProcessor):
    """
    Class for handling provenance-related datum processing in DFCT contracts.
    """
    def prepare_topic_json(self, datum: dict, status: int) -> dict:
        return self._prepare_topic_json(datum[0], status, datum[2], datum[3])

    def _prepare_topic_json(self, topic: list, status: int, reward_pool: list, reviewers: list) -> dict:
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
            {"bytes": topic[1]},  # topicProposer
            {"int": topic[2]}  # topicTimestamp
        ]

        plutus_rpi = [
            {"int": reward_pool[0]},  # totalAmount
            {"int": reward_pool[1]},  # allocatedAmount
            {"bytes": str_to_hex(reward_pool[2])},  # tokenName
            {"int": reward_pool[3]}  # creationTimestamp
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
        topic_id: str,
        proposer_pkh: str,
        reward_amount: int = 1000,
        reviewers: list[str] = None
    ) -> tuple[dict[str, Any], dict[str, Any]]:
        """
        Prepare a topic datum matching the exact format in generate-deploy-files.sh.
        """
        if reviewers is None:
            reviewers = [
                local_wallets["reviewer1"].pub_key_hash,
                local_wallets["reviewer2"].pub_key_hash
            ]

        current_time = int(time.time() * 1000)
        topic_list = [topic_id, proposer_pkh, current_time]
        rpi_list = [reward_amount, 0, self.token_name, current_time]
        datum, redeemer = self._prepare_topic_json(topic_list, TopicStatus.PROPOSED.value, rpi_list, reviewers)
        return datum, redeemer

    def prepare_review_topic_redeemer(self, topic_id: str, approved: bool) -> dict[str, Any]:
        """
        Prepare a review topic redeemer for the ReviewTopic action.
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
        contribution_type: ContributionType,
        contributor_pkh: str,
        contribution_status: ContributionStatus,
        timestamp: int
    ) -> tuple[dict[str, Any], dict[str, Any]]:
        """
        Prepare a contribution datum and redeemer.
        """
        contribution_id_hex = str_to_hex(contribution_id)
        topic_id_hex = str_to_hex(topic_id)

        plutus_contribution = [
            {"bytes": contribution_id_hex},
            {"bytes": topic_id_hex},
            {
                "constructor": contribution_type.value,
                "fields": []
            },
            {"bytes": contributor_pkh},
            {"int": timestamp},
            {"int": 1},  # version
            {"bytes": ""}  # previous version ID (empty for first version)
        ]

        datum = self.prepare_contribution_datum(
            contribution_id=contribution_id,
            topic_id=topic_id,
            contribution_type=contribution_type,
            contribution_status=contribution_status,
            contribution_timestamp=timestamp,
            contributor_pkh=contributor_pkh
        )

        redeemer = {
            "constructor": 1,  # ContributionAction
            "fields": [
                {
                    "constructor": 0,  # SubmitContribution
                    "fields": [
                        {
                            "constructor": 0,
                            "fields": plutus_contribution
                        }
                    ]
                }
            ]
        }

        return datum, redeemer

    def prepare_contribution_datum(
        self,
        contribution_id: str,
        topic_id: str,
        contribution_type: ContributionType,
        contribution_status: ContributionStatus,
        contribution_timestamp: int,
        relevance: int = 0,
        accuracy: int = 0,
        completeness: int = 0,
        contributor_pkh: str = "",
        reviewer_pkh: str = "",
        review_timestamp: int = 0,
        initiator_pkh: str = "",
        dispute_timestamp: int = 0
    ) -> dict[str, Any]:
        """
        Prepare a contribution datum.
        """
        dispute_content = {
            "constructor": 0,
            "fields": [
                {"bytes": initiator_pkh},  # disputeInitiator
                {"int": dispute_timestamp}  # disputeTimestamp
            ]
        }

        review_content = {
            "constructor": 0,
            "fields": [
                {"bytes": reviewer_pkh},  # reviewerPkh
                {"int": review_timestamp}  # reviewTimestamp
            ]
        }

        plutus_contribution = [
            {"bytes": str_to_hex(contribution_id)},
            {"bytes": str_to_hex(topic_id)},
            {
                "constructor": contribution_type.value,
                "fields": []
            },
            {"bytes": contributor_pkh},
            {"int": contribution_timestamp},
            {"int": 1},  # version
            {"bytes": ""}  # previous version ID
        ]

        return {
            "constructor": 0,
            "fields": [
                {
                    "constructor": 0,
                    "fields": plutus_contribution
                },
                {
                    "constructor": contribution_type.value,
                    "fields": []
                },
                {
                    "constructor": contribution_status.value,
                    "fields": []
                },
                {"int": relevance},
                {"int": accuracy},
                {"int": completeness},
                review_content,
                dispute_content,
                {"int": 0}  # timelinessScore
            ]
        }

    def prepare_review_contribution_redeemer(
        self,
        contribution_id: str,
        reviewer_pkh: str,
        relevance: int,
        accuracy: int,
        completeness: int
    ) -> dict[str, Any]:
        """
        Prepare a review contribution redeemer.
        """
        contribution_id_hex = str_to_hex(contribution_id)
        current_time = int(time.time() * 1000)

        review_content_obj = {
            "constructor": 0,
            "fields": [
                {"bytes": reviewer_pkh},
                {"int": current_time}
            ]
        }

        redeemer = {
            "constructor": 1,  # ContributionAction
            "fields": [
                {
                    "constructor": 1,  # ReviewContribution
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
        contributor_pkh: str
    ) -> dict[str, Any]:
        """
        Prepare a dispute contribution redeemer.
        """
        contribution_id_hex = str_to_hex(contribution_id)
        current_time = int(time.time() * 1000)

        dispute_content_obj = {
            "constructor": 3,
            "fields": [
                {"bytes": contributor_pkh},
                {"int": current_time}
            ]
        }

        redeemer = {
            "constructor": 1,  # ContributionAction
            "fields": [
                {
                    "constructor": 3,  # DisputeContribution
                    "fields": [
                        {"bytes": contribution_id_hex},
                        dispute_content_obj
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

    def extract_topic_from_datum(self, topic_datum: list) -> dict[str, Any]:
        """
        Extract topic data from its datum structure.
        """
        try:
            if len(topic_datum) > 4:
                return None

            topic_details = topic_datum[0]
            topic_id = topic_details[0]
            proposer = topic_details[1]
            timestamp = topic_details[2]
            status = self.extract_int_from_datum(topic_datum, 1)

            reward_info = topic_datum[2]
            total_amount = reward_info[0]
            allocated_amount = reward_info[1]
            token_name = reward_info[2]
            creation_timestamp = reward_info[3]

            reviewers = []
            if len(topic_datum) > 3:
                reviewers = topic_datum[3]

            result = {
                "topic_id": topic_id,
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
            return {}

    def extract_contribution_from_datum(self, datum: list[Any]) -> dict[str, Any]:
        """
        Extract contribution data from its datum structure.
        """
        try:
            if len(datum) < 8:
                return None

            contribution_details = datum[0]
            contribution_id = contribution_details[0]
            topic_id = contribution_details[1]
            contribution_type = self.extract_int_from_datum(contribution_details, 2)
            creator = contribution_details[3]
            timestamp = contribution_details[4]
            version = contribution_details[5]
            previous_version_id = contribution_details[6]
            status = self.extract_int_from_datum(datum, 2)

            relevance = datum[3]
            accuracy = datum[4]
            completeness = datum[5]

            review_content = {}
            if len(datum) > 6:
                d_review_content = datum[6]
                if isinstance(d_review_content, list) and len(d_review_content) >= 2:
                    try:
                        reviewer = d_review_content[0]
                        review_timestamp = d_review_content[1]
                        review_content = {
                            "reviewer": reviewer,
                            "review_timestamp": review_timestamp
                        }
                    except Exception as e:
                        logger.warning(f"Error processing review content: {e}")

            dispute_content = {}
            if len(datum) > 7:
                d_dispute_content = datum[7]
                if isinstance(d_dispute_content, list) and len(d_dispute_content) >= 2:
                    try:
                        initiator = d_dispute_content[0]
                        dispute_timestamp = d_dispute_content[1]
                        dispute_content = {
                            "initiator": initiator,
                            "dispute_timestamp": dispute_timestamp
                        }
                    except Exception as e:
                        logger.warning(f"Error processing dispute reason: {e}")

            timeliness_score = datum[8] if len(datum) > 8 else 0
            timeliness_int = int(timeliness_score) if isinstance(timeliness_score, (int, str)) else 0
            total_score = int(relevance) + int(accuracy) + int(completeness) + int(timeliness_int)

            result = {
                "contribution_id": contribution_id,
                "topic_id": topic_id,
                "type": ContributionType(contribution_type),
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
                "dispute_content": dispute_content
            }

            return result

        except Exception as e:
            logger.error(f"Failed to extract contribution data: {str(e)}")
            logger.error(f"\ndatum: {datum}\n")
            return {}
