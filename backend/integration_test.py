import logging
import time
import sys

from dfctbackend.cardano.wallet import CardanoWallet, local_wallets
from dfctbackend.cardano.contract import ProvenanceContract, ContractError, TopicStatus, ContributionStatus
from dfctbackend.cardano.transaction import CardanoTransaction, MAX_ATTEMPTS, WAIT_DATA_SYNC

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(name)s:%(lineno)s(%(levelname)s): %(message)s'
)
logger = logging.getLogger("dfct-example")
ogmios_logger = logging.getLogger('ogmios')
ogmios_logger.setLevel(level=logging.CRITICAL)

FUND_SCRIPT_VALUE = 70000000
DFC_REWARD_VALUE  = 10000

contract = ProvenanceContract()

def create_utxos(wallet: CardanoWallet, utxo_values: list, dfc_reward_pool) -> bool:
    utxo_values.sort(reverse=True)

    largest, largest_value = contract.transactions.find_largest_utxo(wallet)
    if not largest or largest_value < sum(utxo_values):
        logger.info(f"{wallet.name} needs funding from faucet")
        return False

    dfc_utxo = contract.transactions.find_token_utxo(
        wallet.address,
        contract.transactions.policy_id,
        contract.transactions.token_name,
        1,
        dfc_reward_pool
    )

    exclude = [dfc_utxo]
    needed = []
    for value in utxo_values:
        utxos = contract.transactions.find_utxos_at_address(
            address=wallet.address,
            min_lovelace=value,
            exclude=exclude
        )

        if not utxos:
            needed.append(value)
        else:
            exclude.append(utxos[0])

    if needed:
        utxos = contract.transactions.find_utxos_at_address(
            address=wallet.address
        )
        len_utxos = len(utxos)

        for value in needed:
            logger.info(f"Creating utxo with {value} lovelave for {wallet.name}")
            contract.transactions.create_utxo(wallet, largest, value)
            nr_attempts = 0
            while nr_attempts < MAX_ATTEMPTS:
                time.sleep(WAIT_DATA_SYNC)
                nr_attempts += 1
                utxos = contract.transactions.find_utxos_at_address(
                    address=wallet.address
                )
                if len_utxos < len(utxos):
                    break

                len_utxos = len(utxos)

            #updating largest utxo info (hash and index)
            largest, largest_value = contract.transactions.find_largest_utxo(wallet)

    return True

def main():
    """
    End-to-end integration test of DFCT Provenance Contract usage on Cardano Testnet
    with assertions to verify each step works correctly
    """

    # Get wallet references (assuming local_wallets is properly set up)
    proposer = local_wallets.get("proposer")
    reviewer1 = local_wallets.get("reviewer1")
    reviewer2 = local_wallets.get("reviewer2")
    contributor = proposer
    
    if not all([proposer, reviewer1, reviewer2, contributor]):
        raise ContractError("Required wallets not found in local_wallets")

    logger.info(f"Using proposer wallet: {proposer.name} ({proposer.address})")
    logger.info(f"Using reviewer wallets: {reviewer1.name}, {reviewer2.name}")
    logger.info(f"Using contributor wallet: {contributor.name}")

    ###
    # Pre-req: check wallets and create collateral and fee fund UTxOs on wallets)
    logger.info(f"Checking wallets")
    wallets = [proposer, reviewer1, reviewer2]
    collateral_and_fund_fee_values = [5000000, 10000000]
    for wallet in wallets:
        if not create_utxos(wallet, collateral_and_fund_fee_values, DFC_REWARD_VALUE):
            logger.info(f"Use a faucet to fund {wallet.name}")
            exit(1)

    ###
    # Step 1: Submit a new topic
    logger.info("Submitting a new topic...")
    topic_result = contract.submit_topic(
        title="Climate",
        description="biodiversity",
        proposer=proposer,
        lovelace_amount=FUND_SCRIPT_VALUE,
        reward_amount=DFC_REWARD_VALUE
    )

    # Assert topic submission was successful
    assert "topic_id" in topic_result, "Topic submission failed: no topic_id in result"
    topic_id = topic_result["topic_id"]
    logger.info(f"Topic submitted. Topic ID: {topic_id}")

    ###
    # Step 2: Get topic details
    logger.info("Waiting for topic submission to be confirmed...")
    topic_data = None
    attempt_nr = 0
    while attempt_nr < MAX_ATTEMPTS:
        time.sleep(WAIT_DATA_SYNC)
        attempt_nr += 1

        topic_data = contract.get_topic(topic_id)
        if topic_data:
            break

    assert topic_data is not None, f"Topic with ID {topic_id} not found"
    assert topic_data["status"] == TopicStatus.PROPOSED, f"Unexpected topic status: {topic_data['status']}"

    logger.info(f"Topic {topic_id} has status: {topic_data['status']}")
    logger.info(f"Topic details: {topic_data['title']} - {topic_data['description']}")

    ###
    # Step 3: Review the topic (approve it)
    logger.info(f"Reviewing topic {topic_id} with reviewer1...")
    review_result = contract.review_topic(
        topic_id=topic_id,
        approved=True,
        reviewer=reviewer1
    )

    # Assert review transaction was successful
    assert review_result is not None, "Topic review failed"
    logger.info(f"Topic review submitted. Transaction hash: {review_result.get('transaction_hash', 'unknown')}")

    # Wait sync and then erify topic status after review
    topic_after_review = None
    attempt_nr = 0
    while attempt_nr < MAX_ATTEMPTS:
        time.sleep(WAIT_DATA_SYNC)
        attempt_nr += 1

        topic_after_review = contract.get_topic(topic_id)
        if topic_after_review:
            if topic_after_review["status"] == TopicStatus.REVIEWED:
                break

    assert topic_after_review is not None, "Topic not found after review"
    assert topic_after_review["status"] == TopicStatus.REVIEWED, f"Unexpected status after review: {topic_after_review['status']}"
    logger.info(f"Topic status after review: {topic_after_review['status']}")

    ###
    # Step 4: Activate the topic
    logger.info(f"Activating topic {topic_id}...")
    activate_result = contract.activate_topic(
        topic_id=topic_id,
        wallet=proposer  # Topic proposer activates the topic
    )

    # Assert activation was successful
    assert activate_result is not None, "Topic activation failed"
    logger.info(f"Topic activated. Transaction: {activate_result.get('transaction_hash', 'unknown')}")

    # Wait sync and then verify topic status after activation
    updated_topic = None
    attempt_nr = 0
    while attempt_nr < MAX_ATTEMPTS:
        time.sleep(WAIT_DATA_SYNC)
        attempt_nr += 1

        updated_topic = contract.get_topic(topic_id)
        if updated_topic:
            if updated_topic["status"] == TopicStatus.ACTIVATED:
                break

    assert updated_topic is not None, "Topic not found after activation"
    assert updated_topic["status"] == TopicStatus.ACTIVATED, f"Topic not properly activated, status: {updated_topic['status']}"
    logger.info(f"Topic status after activation: {updated_topic['status']}")

    ###
    # Step 5: Submit a contribution (evidence)
    if not create_utxos(proposer, collateral_and_fund_fee_values, DFC_REWARD_VALUE):
        logger.info(f"Use a faucet to fund {proposer.name}")
        exit(1)

    logger.info(f"Submitting contribution to topic {topic_id}...")
    contribution_result = contract.submit_contribution(
        topic_id=topic_id,
        content="decline",
        contributor=contributor
    )

    # Assert contribution was submitted
    assert "contribution_id" in contribution_result, "Contribution submission failed"
    contribution_id = contribution_result["contribution_id"]
    assert contribution_result["topic_id"] == topic_id, "Topic ID mismatch in contribution result"
    logger.info(f"Contribution submitted. Contribution ID: {contribution_id}")

    # Wait sync and then verify contribution exists
    contribution_data_initial = None
    attempt_nr = 0
    while attempt_nr < MAX_ATTEMPTS:
        time.sleep(WAIT_DATA_SYNC)
        attempt_nr += 1

        contribution_data_initial = contract.get_contribution(contribution_id)
        if contribution_data_initial:
            if contribution_data_initial["status"] == ContributionStatus.EVIDENCE_PROPOSED:
                break

    assert contribution_data_initial is not None, f"Contribution with ID {contribution_id} not found"
    assert contribution_data_initial["status"] == ContributionStatus.EVIDENCE_PROPOSED, f"Unexpected contribution status: {contribution_data_initial['status']}"
    logger.info(f"Contribution initial status: {contribution_data_initial['status']}")

    ###
    # Step 6: Review the contribution
    logger.info(f"Reviewing contribution {contribution_id}...")
    contrib_review_result = contract.review_contribution(
        contribution_id=contribution_id,
        reviewer=reviewer1,
        relevance=8,      # Score out of 10
        accuracy=7,       # Score out of 10
        completeness=6,   # Score out of 10
        review_content="agree"
    )

    # Assert review was successful
    assert contrib_review_result is not None, "Contribution review failed"
    logger.info(f"Contribution review submitted. Transaction: {contrib_review_result.get('transaction_hash', 'unknown')}")

    # Wait sync and then verify contribution status after review
    contribution_after_review = None
    attempt_nr = 0
    while attempt_nr < MAX_ATTEMPTS:
        time.sleep(WAIT_DATA_SYNC)
        attempt_nr += 1

        contribution_after_review = contract.get_contribution(contribution_id)
        if contribution_after_review:
            if contribution_after_review["status"] == ContributionStatus.CONTRIBUTION_REVIEWED:
                break

    contribution_after_review = contract.get_contribution(contribution_id)
    assert contribution_after_review is not None, "Contribution not found after review"
    assert contribution_after_review["status"] == ContributionStatus.CONTRIBUTION_REVIEWED, f"Unexpected status after review: {contribution_after_review['status']}"
    assert contribution_after_review["relevance"] == 8, f"Relevance score not updated correctly: {contribution_after_review['relevance']}"
    assert contribution_after_review["accuracy"] == 7, f"Accuracy score not updated correctly: {contribution_after_review['accuracy']}"
    assert contribution_after_review["completeness"] == 6, f"Completeness score not updated correctly: {contribution_after_review['completeness']}"
    assert len(contribution_after_review["review_content"]) > 0, "Review content not saved"
    logger.info(f"Contribution status after review: {contribution_after_review['status']}")

    ###
    # Step 8: Close the topic with all contributions are in reviewed state
    logger.info(f"Closing topic {topic_id}...")
    close_result = contract.close_topic(
        topic_id=topic_id,
        wallet=proposer  # Topic proposer closes the topic
    )

    # Assert topic closure was successful
    assert close_result is not None, "Topic closure failed"
    logger.info(f"Topic closed. Transaction: {close_result.get('transaction_hash', 'unknown')}")

    # Wait sync and then verify final topic status
    final_topic = None
    attempt_nr = 0
    while attempt_nr < MAX_ATTEMPTS:
        time.sleep(WAIT_DATA_SYNC)
        attempt_nr += 1

        final_topic = contract.get_topic(topic_id)
        if final_topic:
            if final_topic["status"] == TopicStatus.CLOSED:
                break

    assert final_topic is not None, "Topic not found after closure"
    assert final_topic["status"] == TopicStatus.CLOSED, f"Topic not properly closed, status: {final_topic['status']}"
    logger.info(f"Final topic status: {final_topic['status']}")

    # Step 10: Get all contributions for the topic
    all_contributions = contract.get_contributions_for_topic(topic_id)
    assert all_contributions is not None, "Failed to retrieve contributions for topic"
    assert len(all_contributions) > 0, "No contributions found for topic"

    logger.info(f"Total contributions for topic: {len(all_contributions)}")
    for i, contrib in enumerate(all_contributions):
        logger.info(f"Contribution #{i+1}: ID={contrib['contribution_id']}, Score={contrib['total_score']}")
        assert contrib["topic_id"] == topic_id, f"Contribution topic ID mismatch: {contrib['topic_id']} vs {topic_id}"

    logger.info("All assertions passed!")
    logger.info("Integration test completed successfully!")


if __name__ == "__main__":
    main()