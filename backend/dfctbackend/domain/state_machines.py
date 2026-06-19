from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Mapping


class StateMachineError(ValueError):
    """Raised when an invalid d-FCT state transition is requested."""


class Machine(str, Enum):
    DSM_PROV_TOPIC = "dsm-prov/topic"
    DSM_PROV_CONTRIBUTION = "dsm-prov/contribution"
    DSM_PROV_REWARD = "dsm-prov/reward"
    DSM_GOV = "dsm-gov"


class TopicState(str, Enum):
    PROPOSED = "topic_proposed"
    REWARD_AVAILABLE = "reward_available"  # virtual/backend state, current Plutus datum keeps pool inside topic
    REVIEWED = "topic_reviewed"
    ACTIVE = "topic_active"
    REJECTED = "topic_rejected"
    CLOSED = "topic_closed"


class ContributionState(str, Enum):
    PROPOSED = "contrib_proposed"
    REVIEWED = "contrib_reviewed"
    UPDATED = "contrib_updated"
    DISPUTED = "contrib_disputed"
    REJECTED = "contrib_rejected"
    VERIFIED = "contrib_verified"
    EVALUATED = "contribution_evaluated"
    REWARDS_DISTRIBUTED = "rewards_distributed"


class RewardState(str, Enum):
    AVAILABLE = "reward_available"
    DISTRIBUTED = "rewards_distributed"
    DEPLETED = "rewards_depleted"


class ProposalState(str, Enum):
    PROPOSED = "proposed"
    VOTING = "voting"
    APPROVED = "approved"
    REJECTED = "rejected"
    EXECUTED = "executed"


class Action(str, Enum):
    # dSM-Prov topic actions
    SUBMIT_TOPIC = "submit_topic"
    CHECK_POOL = "check_pool"
    REVIEW_TOPIC = "review_topic"
    ACTIVATE_TOPIC = "activate_topic"
    REJECT_TOPIC = "reject_topic"
    CLOSE_TOPIC = "close_topic"

    # dSM-Prov contribution actions
    SUBMIT_CONTRIBUTION = "submit_contribution"
    REVIEW_CONTRIBUTION = "review_contribution"
    UPDATE_CONTRIBUTION = "update_contribution"
    DISPUTE_CONTRIBUTION = "dispute_contribution"
    REJECT_CONTRIBUTION = "reject_contribution"
    VERIFY_CONTRIBUTION = "verify_contribution"
    EVALUATE_CONTRIBUTION = "evaluate_contribution"
    DISTRIBUTE_REWARDS = "distribute_rewards"

    # dSM-Prov reward actions
    CHECK_POOL_AVAILABLE = "check_pool_available"
    CHECK_POOL_DEPLETED = "check_pool_depleted"

    # dSM-Gov actions
    SUBMIT_PROPOSAL = "submit_proposal"
    UPDATE_PROPOSAL = "update_proposal"
    ACTIVATE_PROPOSAL = "activate_proposal"
    VOTE_ON_PROPOSAL = "vote_on_proposal"
    FINALIZE_APPROVED = "finalize_approved"
    FINALIZE_REJECTED = "finalize_rejected"
    EXECUTE_PROPOSAL = "execute_proposal"


class IdentityAssurance(str, Enum):
    """
    Voluntary transparency / identity-assurance scale.

    This is intentionally not mandatory KYC. Beginners can use d-FCT without Web3.
    Higher assurance can increase scoring and reward eligibility, but should not be
    required for basic participation.
    """

    VISITOR = "visitor"
    PSEUDONYMOUS_ACCOUNT = "pseudonymous_account"
    EMAIL_VERIFIED = "email_verified"
    OAUTH_VERIFIED = "oauth_verified"
    WALLET_BOUND = "wallet_bound"
    REPUTATION_ESTABLISHED = "reputation_established"
    ORGANIZATION_VERIFIED = "organization_verified"


@dataclass(frozen=True)
class IdentityPolicy:
    reward_eligible: bool
    signal_weight: float
    score_multiplier: float


IDENTITY_POLICIES: dict[IdentityAssurance, IdentityPolicy] = {
    IdentityAssurance.VISITOR: IdentityPolicy(False, 0.10, 0.00),
    IdentityAssurance.PSEUDONYMOUS_ACCOUNT: IdentityPolicy(True, 0.50, 1.00),
    IdentityAssurance.EMAIL_VERIFIED: IdentityPolicy(True, 0.70, 1.05),
    IdentityAssurance.OAUTH_VERIFIED: IdentityPolicy(True, 0.80, 1.10),
    IdentityAssurance.WALLET_BOUND: IdentityPolicy(True, 0.90, 1.20),
    IdentityAssurance.REPUTATION_ESTABLISHED: IdentityPolicy(True, 1.00, 1.30),
    IdentityAssurance.ORGANIZATION_VERIFIED: IdentityPolicy(True, 1.00, 1.50),
}


@dataclass(frozen=True)
class TransitionRule:
    machine: Machine
    action: Action
    from_state: str | None
    to_state: str
    reward_eligible_action: bool = False
    anchor_recommended: bool = True
    wallet_required_for_anchor: bool = False


@dataclass(frozen=True)
class TransitionEvent:
    machine: Machine
    action: Action
    from_state: str | None
    to_state: str
    actor_id: str | None = None
    identity_assurance: IdentityAssurance = IdentityAssurance.VISITOR
    reward_eligible: bool = False
    score: float = 0.0
    anonymous_signal_weight: float = 0.0
    anchor_recommended: bool = True
    wallet_required_for_anchor: bool = False
    metadata: Mapping[str, Any] = field(default_factory=dict)


TRANSITIONS: tuple[TransitionRule, ...] = (
    # dSM-Prov topic lifecycle
    TransitionRule(Machine.DSM_PROV_TOPIC, Action.SUBMIT_TOPIC, None, TopicState.PROPOSED.value, True),
    TransitionRule(Machine.DSM_PROV_TOPIC, Action.CHECK_POOL, TopicState.PROPOSED.value, TopicState.REWARD_AVAILABLE.value, False, False),
    TransitionRule(Machine.DSM_PROV_TOPIC, Action.REVIEW_TOPIC, TopicState.REWARD_AVAILABLE.value, TopicState.REVIEWED.value, True),
    TransitionRule(Machine.DSM_PROV_TOPIC, Action.REVIEW_TOPIC, TopicState.PROPOSED.value, TopicState.REVIEWED.value, True),
    TransitionRule(Machine.DSM_PROV_TOPIC, Action.ACTIVATE_TOPIC, TopicState.REVIEWED.value, TopicState.ACTIVE.value),
    TransitionRule(Machine.DSM_PROV_TOPIC, Action.REJECT_TOPIC, TopicState.PROPOSED.value, TopicState.REJECTED.value),
    TransitionRule(Machine.DSM_PROV_TOPIC, Action.REJECT_TOPIC, TopicState.REVIEWED.value, TopicState.REJECTED.value),
    TransitionRule(Machine.DSM_PROV_TOPIC, Action.CLOSE_TOPIC, TopicState.ACTIVE.value, TopicState.CLOSED.value),

    # dSM-Prov contribution lifecycle
    TransitionRule(Machine.DSM_PROV_CONTRIBUTION, Action.SUBMIT_CONTRIBUTION, None, ContributionState.PROPOSED.value, True),
    TransitionRule(Machine.DSM_PROV_CONTRIBUTION, Action.REVIEW_CONTRIBUTION, ContributionState.PROPOSED.value, ContributionState.REVIEWED.value, True),
    TransitionRule(Machine.DSM_PROV_CONTRIBUTION, Action.REVIEW_CONTRIBUTION, ContributionState.UPDATED.value, ContributionState.REVIEWED.value, True),
    TransitionRule(Machine.DSM_PROV_CONTRIBUTION, Action.UPDATE_CONTRIBUTION, ContributionState.DISPUTED.value, ContributionState.UPDATED.value, True),
    TransitionRule(Machine.DSM_PROV_CONTRIBUTION, Action.DISPUTE_CONTRIBUTION, ContributionState.REVIEWED.value, ContributionState.DISPUTED.value, True),
    TransitionRule(Machine.DSM_PROV_CONTRIBUTION, Action.REJECT_CONTRIBUTION, ContributionState.REVIEWED.value, ContributionState.REJECTED.value),
    TransitionRule(Machine.DSM_PROV_CONTRIBUTION, Action.REJECT_CONTRIBUTION, ContributionState.DISPUTED.value, ContributionState.REJECTED.value),
    TransitionRule(Machine.DSM_PROV_CONTRIBUTION, Action.VERIFY_CONTRIBUTION, ContributionState.REVIEWED.value, ContributionState.VERIFIED.value, True),
    TransitionRule(Machine.DSM_PROV_CONTRIBUTION, Action.VERIFY_CONTRIBUTION, ContributionState.DISPUTED.value, ContributionState.VERIFIED.value, True),
    TransitionRule(Machine.DSM_PROV_CONTRIBUTION, Action.EVALUATE_CONTRIBUTION, ContributionState.VERIFIED.value, ContributionState.EVALUATED.value, True),
    TransitionRule(Machine.DSM_PROV_CONTRIBUTION, Action.DISTRIBUTE_REWARDS, ContributionState.EVALUATED.value, ContributionState.REWARDS_DISTRIBUTED.value),

    # dSM-Prov reward lifecycle
    TransitionRule(Machine.DSM_PROV_REWARD, Action.CHECK_POOL_AVAILABLE, None, RewardState.AVAILABLE.value, False, False),
    TransitionRule(Machine.DSM_PROV_REWARD, Action.DISTRIBUTE_REWARDS, RewardState.AVAILABLE.value, RewardState.DISTRIBUTED.value),
    TransitionRule(Machine.DSM_PROV_REWARD, Action.CHECK_POOL_AVAILABLE, RewardState.DISTRIBUTED.value, RewardState.AVAILABLE.value, False, False),
    TransitionRule(Machine.DSM_PROV_REWARD, Action.CHECK_POOL_DEPLETED, RewardState.AVAILABLE.value, RewardState.DEPLETED.value),
    TransitionRule(Machine.DSM_PROV_REWARD, Action.CHECK_POOL_DEPLETED, RewardState.DISTRIBUTED.value, RewardState.DEPLETED.value),

    # dSM-Gov lifecycle
    TransitionRule(Machine.DSM_GOV, Action.SUBMIT_PROPOSAL, None, ProposalState.PROPOSED.value, True),
    TransitionRule(Machine.DSM_GOV, Action.UPDATE_PROPOSAL, ProposalState.PROPOSED.value, ProposalState.PROPOSED.value),
    TransitionRule(Machine.DSM_GOV, Action.ACTIVATE_PROPOSAL, ProposalState.PROPOSED.value, ProposalState.VOTING.value),
    TransitionRule(Machine.DSM_GOV, Action.VOTE_ON_PROPOSAL, ProposalState.PROPOSED.value, ProposalState.VOTING.value, True),
    TransitionRule(Machine.DSM_GOV, Action.VOTE_ON_PROPOSAL, ProposalState.VOTING.value, ProposalState.VOTING.value, True),
    TransitionRule(Machine.DSM_GOV, Action.FINALIZE_APPROVED, ProposalState.VOTING.value, ProposalState.APPROVED.value),
    TransitionRule(Machine.DSM_GOV, Action.FINALIZE_REJECTED, ProposalState.VOTING.value, ProposalState.REJECTED.value),
    TransitionRule(Machine.DSM_GOV, Action.EXECUTE_PROPOSAL, ProposalState.APPROVED.value, ProposalState.EXECUTED.value),
)


TRANSITION_INDEX = {
    (rule.machine, rule.action, rule.from_state): rule
    for rule in TRANSITIONS
}


BASE_ACTION_POINTS: dict[Action, int] = {
    Action.SUBMIT_TOPIC: 10,
    Action.REVIEW_TOPIC: 25,
    Action.SUBMIT_CONTRIBUTION: 20,
    Action.REVIEW_CONTRIBUTION: 25,
    Action.UPDATE_CONTRIBUTION: 10,
    Action.DISPUTE_CONTRIBUTION: 20,
    Action.VERIFY_CONTRIBUTION: 20,
    Action.EVALUATE_CONTRIBUTION: 15,
    Action.SUBMIT_PROPOSAL: 10,
    Action.VOTE_ON_PROPOSAL: 5,
}


def get_identity_policy(identity_assurance: IdentityAssurance | str) -> IdentityPolicy:
    return IDENTITY_POLICIES[IdentityAssurance(identity_assurance)]


def calculate_score(
    action: Action | str,
    identity_assurance: IdentityAssurance | str,
    *,
    quality_score: float | None = None,
    timeliness_multiplier: float = 1.0,
    reputation_multiplier: float = 1.0,
    max_multiplier: float = 2.0,
) -> float:
    action_enum = Action(action)
    policy = get_identity_policy(identity_assurance)

    if not policy.reward_eligible:
        return 0.0

    base = BASE_ACTION_POINTS.get(action_enum, 0)
    if base <= 0:
        return 0.0

    quality_multiplier = 1.0
    if quality_score is not None:
        bounded_quality = max(0.0, min(float(quality_score), 10.0))
        quality_multiplier = bounded_quality / 10.0

    combined_multiplier = (
        policy.score_multiplier
        * max(0.0, timeliness_multiplier)
        * max(0.0, reputation_multiplier)
        * quality_multiplier
    )

    return round(base * min(combined_multiplier, max_multiplier), 4)


def signal_weight(identity_assurance: IdentityAssurance | str) -> float:
    return get_identity_policy(identity_assurance).signal_weight


def transition(
    machine: Machine | str,
    action: Action | str,
    current_state: str | None,
    *,
    actor_id: str | None = None,
    identity_assurance: IdentityAssurance | str = IdentityAssurance.VISITOR,
    metadata: Mapping[str, Any] | None = None,
    quality_score: float | None = None,
    timeliness_multiplier: float = 1.0,
    reputation_multiplier: float = 1.0,
) -> TransitionEvent:
    machine_enum = Machine(machine)
    action_enum = Action(action)
    identity_enum = IdentityAssurance(identity_assurance)

    rule = TRANSITION_INDEX.get((machine_enum, action_enum, current_state))
    if rule is None:
        allowed = [
            r.action.value
            for r in TRANSITIONS
            if r.machine == machine_enum and r.from_state == current_state
        ]
        allowed_text = ", ".join(sorted(allowed)) or "none"
        raise StateMachineError(
            f"Invalid transition: machine={machine_enum.value}, "
            f"state={current_state!r}, action={action_enum.value!r}. "
            f"Allowed actions from this state: {allowed_text}."
        )

    score = calculate_score(
        action_enum,
        identity_enum,
        quality_score=quality_score,
        timeliness_multiplier=timeliness_multiplier,
        reputation_multiplier=reputation_multiplier,
    )

    policy = get_identity_policy(identity_enum)

    return TransitionEvent(
        machine=rule.machine,
        action=rule.action,
        from_state=rule.from_state,
        to_state=rule.to_state,
        actor_id=actor_id,
        identity_assurance=identity_enum,
        reward_eligible=rule.reward_eligible_action and policy.reward_eligible and score > 0,
        score=score,
        anonymous_signal_weight=policy.signal_weight,
        anchor_recommended=rule.anchor_recommended,
        wallet_required_for_anchor=rule.wallet_required_for_anchor,
        metadata=metadata or {},
    )
