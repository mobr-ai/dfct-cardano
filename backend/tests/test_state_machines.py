import pytest

from dfctbackend.domain.state_machines import (
    Action,
    ContributionState,
    IdentityAssurance,
    Machine,
    ProposalState,
    StateMachineError,
    TopicState,
    calculate_score,
    signal_weight,
    transition,
)


def test_dsm_prov_topic_path_with_virtual_reward_available_state():
    event = transition(
        Machine.DSM_PROV_TOPIC,
        Action.SUBMIT_TOPIC,
        None,
        actor_id="user-1",
        identity_assurance=IdentityAssurance.EMAIL_VERIFIED,
    )
    assert event.to_state == TopicState.PROPOSED.value
    assert event.reward_eligible is True
    assert event.score > 10

    event = transition(Machine.DSM_PROV_TOPIC, Action.CHECK_POOL, event.to_state)
    assert event.to_state == TopicState.REWARD_AVAILABLE.value
    assert event.anchor_recommended is False

    event = transition(
        Machine.DSM_PROV_TOPIC,
        Action.REVIEW_TOPIC,
        event.to_state,
        identity_assurance=IdentityAssurance.REPUTATION_ESTABLISHED,
    )
    assert event.to_state == TopicState.REVIEWED.value

    event = transition(Machine.DSM_PROV_TOPIC, Action.ACTIVATE_TOPIC, event.to_state)
    assert event.to_state == TopicState.ACTIVE.value

    event = transition(Machine.DSM_PROV_TOPIC, Action.CLOSE_TOPIC, event.to_state)
    assert event.to_state == TopicState.CLOSED.value


def test_dsm_prov_topic_path_without_virtual_pool_state_for_current_cardano_adapter():
    event = transition(Machine.DSM_PROV_TOPIC, Action.SUBMIT_TOPIC, None)
    assert event.to_state == TopicState.PROPOSED.value

    event = transition(Machine.DSM_PROV_TOPIC, Action.REVIEW_TOPIC, event.to_state)
    assert event.to_state == TopicState.REVIEWED.value


def test_dsm_prov_contribution_dispute_update_verify_evaluate_reward_path():
    event = transition(
        Machine.DSM_PROV_CONTRIBUTION,
        Action.SUBMIT_CONTRIBUTION,
        None,
        identity_assurance=IdentityAssurance.WALLET_BOUND,
        quality_score=8,
    )
    assert event.to_state == ContributionState.PROPOSED.value
    assert event.reward_eligible is True

    event = transition(
        Machine.DSM_PROV_CONTRIBUTION,
        Action.REVIEW_CONTRIBUTION,
        event.to_state,
        identity_assurance=IdentityAssurance.REPUTATION_ESTABLISHED,
        quality_score=9,
    )
    assert event.to_state == ContributionState.REVIEWED.value

    event = transition(
        Machine.DSM_PROV_CONTRIBUTION,
        Action.DISPUTE_CONTRIBUTION,
        event.to_state,
        identity_assurance=IdentityAssurance.EMAIL_VERIFIED,
    )
    assert event.to_state == ContributionState.DISPUTED.value

    event = transition(
        Machine.DSM_PROV_CONTRIBUTION,
        Action.UPDATE_CONTRIBUTION,
        event.to_state,
        identity_assurance=IdentityAssurance.WALLET_BOUND,
    )
    assert event.to_state == ContributionState.UPDATED.value

    event = transition(
        Machine.DSM_PROV_CONTRIBUTION,
        Action.REVIEW_CONTRIBUTION,
        event.to_state,
        identity_assurance=IdentityAssurance.REPUTATION_ESTABLISHED,
    )
    assert event.to_state == ContributionState.REVIEWED.value

    event = transition(
        Machine.DSM_PROV_CONTRIBUTION,
        Action.VERIFY_CONTRIBUTION,
        event.to_state,
        identity_assurance=IdentityAssurance.REPUTATION_ESTABLISHED,
    )
    assert event.to_state == ContributionState.VERIFIED.value

    event = transition(
        Machine.DSM_PROV_CONTRIBUTION,
        Action.EVALUATE_CONTRIBUTION,
        event.to_state,
        identity_assurance=IdentityAssurance.REPUTATION_ESTABLISHED,
    )
    assert event.to_state == ContributionState.EVALUATED.value

    event = transition(
        Machine.DSM_PROV_CONTRIBUTION,
        Action.DISTRIBUTE_REWARDS,
        event.to_state,
    )
    assert event.to_state == ContributionState.REWARDS_DISTRIBUTED.value


def test_dsm_gov_happy_path():
    event = transition(
        Machine.DSM_GOV,
        Action.SUBMIT_PROPOSAL,
        None,
        identity_assurance=IdentityAssurance.WALLET_BOUND,
    )
    assert event.to_state == ProposalState.PROPOSED.value

    event = transition(Machine.DSM_GOV, Action.UPDATE_PROPOSAL, event.to_state)
    assert event.to_state == ProposalState.PROPOSED.value

    event = transition(Machine.DSM_GOV, Action.ACTIVATE_PROPOSAL, event.to_state)
    assert event.to_state == ProposalState.VOTING.value

    event = transition(
        Machine.DSM_GOV,
        Action.VOTE_ON_PROPOSAL,
        event.to_state,
        identity_assurance=IdentityAssurance.WALLET_BOUND,
    )
    assert event.to_state == ProposalState.VOTING.value

    event = transition(Machine.DSM_GOV, Action.FINALIZE_APPROVED, event.to_state)
    assert event.to_state == ProposalState.APPROVED.value

    event = transition(Machine.DSM_GOV, Action.EXECUTE_PROPOSAL, event.to_state)
    assert event.to_state == ProposalState.EXECUTED.value


def test_invalid_transition_is_rejected():
    with pytest.raises(StateMachineError):
        transition(Machine.DSM_GOV, Action.EXECUTE_PROPOSAL, ProposalState.PROPOSED.value)


def test_identity_assurance_affects_score_without_forcing_web3():
    visitor_score = calculate_score(Action.SUBMIT_CONTRIBUTION, IdentityAssurance.VISITOR)
    pseudo_score = calculate_score(Action.SUBMIT_CONTRIBUTION, IdentityAssurance.PSEUDONYMOUS_ACCOUNT)
    wallet_score = calculate_score(Action.SUBMIT_CONTRIBUTION, IdentityAssurance.WALLET_BOUND)

    assert visitor_score == 0
    assert pseudo_score > visitor_score
    assert wallet_score > pseudo_score


def test_anonymous_signal_is_countable_but_not_reward_eligible():
    event = transition(
        Machine.DSM_PROV_CONTRIBUTION,
        Action.SUBMIT_CONTRIBUTION,
        None,
        identity_assurance=IdentityAssurance.VISITOR,
    )

    assert event.reward_eligible is False
    assert event.score == 0
    assert event.anonymous_signal_weight == signal_weight(IdentityAssurance.VISITOR)
    assert event.anonymous_signal_weight > 0
