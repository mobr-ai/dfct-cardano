import logging
import time
import uuid

from dfctbackend.cardano.wallet import CardanoWallet, local_wallets
from dfctbackend.cardano.contract.governance.governance_contract import (
    GovernanceContract, ContractError, ProposalStatus
)
from dfctbackend.cardano.transaction import MAX_ATTEMPTS, WAIT_DATA_SYNC

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(name)s:%(lineno)s(%(levelname)s): %(message)s'
)
logger = logging.getLogger("dfct-governance-test")
ogmios_logger = logging.getLogger('ogmios')
ogmios_logger.setLevel(level=logging.CRITICAL)

FUND_SCRIPT_VALUE = 10000000
DFC_AVAIL = 10000
MIN_VOTING_TOKENS = 5000

contract = GovernanceContract()

def create_utxos(wallet: CardanoWallet, utxo_values: list, dfc_amount: int) -> bool:
    utxo_values.sort(reverse=True)

    # Check for sufficient lovelace
    largest, largest_value = contract.tx.find_largest_utxo(wallet)
    if not largest or largest_value < sum(utxo_values) + 2000000:  # Add buffer for fees
        logger.info(f"{wallet.name} needs funding from faucet for lovelace")
        return False

    # Check for sufficient DFC tokens
    dfc_utxo = contract.tx.find_token_utxo(
        wallet.address,
        contract.policy_id,
        contract.token_name,
        FUND_SCRIPT_VALUE,
        dfc_amount
    )
    if not dfc_utxo:
        logger.info(f"No UTxO with sufficient {contract.token_name} tokens found for {wallet.name}")
        return False

    exclude = [dfc_utxo]
    needed = []
    for value in utxo_values:
        utxos = contract.tx.find_utxos_at_address(
            address=wallet.address,
            min_lovelace=value,
            exclude=exclude
        )
        if not utxos:
            needed.append(value)
        else:
            exclude.append(utxos[0])

    if needed:
        utxos = contract.tx.find_utxos_at_address(address=wallet.address)
        len_utxos = len(utxos)
        for value in needed:
            logger.info(f"Creating utxo with {value} lovelace for {wallet.name}")
            contract.tx.create_utxo(wallet, largest, value)
            nr_attempts = 0
            while nr_attempts < MAX_ATTEMPTS:
                time.sleep(WAIT_DATA_SYNC)
                nr_attempts += 1
                utxos = contract.tx.find_utxos_at_address(address=wallet.address)
                if len(utxos) > len_utxos:
                    break
                len_utxos = len(utxos)
            largest, largest_value = contract.tx.find_largest_utxo(wallet)

    return True

def check_wallets(wallets):
    collateral_and_fund_fee_values = [5000000, 10000000]
    for wallet in wallets:
        if not create_utxos(wallet, collateral_and_fund_fee_values, DFC_AVAIL):
            logger.info(f"Use a faucet to fund {wallet.name}")
            exit(1)

def main():
    """
    End-to-end integration test of DFCT Governance Contract usage on Cardano Testnet
    with assertions to verify each step works correctly
    """

    # Get wallet references
    proposer = local_wallets.get("proposer")
    voter = local_wallets.get("reviewer1")

    if not all([proposer, voter]):
        raise ContractError("Required wallets not found in local_wallets")

    logger.info(f"Using proposer wallet: {proposer.name} ({proposer.address})")
    logger.info(f"Using voter wallet: {voter.name}")

    # Pre-req: check wallets and create collateral and fee fund UTxOs
    logger.info("Checking wallets")
    wallets = [proposer, voter]
    check_wallets(wallets)

    # Pre-req: define proposal_id and governance parameters
    proposal_id = f"p{uuid.uuid4().hex[:8]}"
    authorized_pkhs = {
        voter.pub_key_hash: 10
    }
    voting_start = int(time.time()) + 60  # Start 1 minute from now
    voting_end = voting_start + 3600  # End 1 hour later

    # Step 1: Submit a new proposal
    logger.info("Submitting a new proposal...")
    proposal_result = contract.submit_proposal(
        proposal_id=proposal_id,
        proposer=proposer,
        lovelace_amount=FUND_SCRIPT_VALUE,
        owner_pkh=proposer.pub_key_hash,  # Set proposer as owner
        min_voting_tokens=MIN_VOTING_TOKENS,
        voting_start=voting_start,
        voting_end=voting_end,
        authorized_pkhs=authorized_pkhs
    )

    # Assert proposal submission was successful
    assert "proposal_id" in proposal_result, "Proposal submission failed: no proposal_id in result"
    assert proposal_result["proposal_id"] == proposal_id, "Proposal ID mismatch"
    assert "transaction_hash" in proposal_result, "Proposal submission failed: no transaction_hash in result"
    logger.info(f"Proposal submitted. Proposal ID: {proposal_id}, Transaction: {proposal_result.get('transaction_hash', 'unknown')}")

    # Wait for transaction to be confirmed and UTxO to be available
    logger.info("Waiting for proposal UTxO to be confirmed...")
    attempt_nr = 0
    proposal_utxo = None
    while attempt_nr < MAX_ATTEMPTS:
        time.sleep(WAIT_DATA_SYNC)
        attempt_nr += 1
        proposal_utxo, _ = contract._get_contract_utxo_and_datum(proposal_id, 1, 0, 0)
        if proposal_utxo:
            break
        logger.info(f"Attempt {attempt_nr}/{MAX_ATTEMPTS}: Waiting for UTxO for proposal {proposal_id}")
    assert proposal_utxo is not None, f"No UTxO found for proposal {proposal_id} after {MAX_ATTEMPTS} attempts"

    # Step 2: Get proposal details
    logger.info("Fetching proposal details...")
    proposal_data = None
    attempt_nr = 0
    while attempt_nr < MAX_ATTEMPTS:
        time.sleep(WAIT_DATA_SYNC)
        attempt_nr += 1
        proposal_data = contract.get_proposal(proposal_id)
        if proposal_data:
            break
        logger.info(f"Attempt {attempt_nr}/{MAX_ATTEMPTS}: Waiting for proposal data {proposal_id}")

    assert proposal_data is not None, f"Proposal with ID {proposal_id} not found"
    assert proposal_data["status"] == ProposalStatus.PROPOSED, f"Unexpected proposal status: {proposal_data['status']}"
    assert proposal_data["min_voting_tokens"] == MIN_VOTING_TOKENS, f"Unexpected min_voting_tokens: {proposal_data['min_voting_tokens']}"
    assert proposal_data["authorized_pkhs"] == authorized_pkhs, f"Unexpected authorized_pkhs: {proposal_data['authorized_pkhs']}"
    logger.info(f"Proposal {proposal_id} has status: {proposal_data['status']}")

    # Step 3: Set voting period
    logger.info(f"Setting voting period for proposal {proposal_id}...")
    period_result = contract.set_voting_period(
        proposal_id=proposal_id,
        voting_start=voting_start,
        voting_end=voting_end,
        wallet=proposer
    )

    # Assert voting period was set
    assert "proposal_id" in period_result, "Setting voting period failed"
    assert period_result["proposal_id"] == proposal_id, "Proposal ID mismatch in period result"
    logger.info(f"Voting period set. Transaction: {period_result.get('transaction_hash', 'unknown')}")

    # Wait sync and verify proposal status
    proposal_after_period = None
    attempt_nr = 0
    while attempt_nr < MAX_ATTEMPTS:
        time.sleep(WAIT_DATA_SYNC)
        attempt_nr += 1
        proposal_after_period = contract.get_proposal(proposal_id)
        if proposal_after_period and proposal_after_period["status"] == ProposalStatus.PROPOSED:
            break
        logger.info(f"Attempt {attempt_nr}/{MAX_ATTEMPTS}: Waiting for voting status for proposal {proposal_id}")

    assert proposal_after_period is not None, f"Proposal {proposal_id} not found after setting period"
    assert proposal_after_period["status"] == ProposalStatus.PROPOSED, f"Unexpected status after period: {proposal_after_period['status']}"
    assert proposal_after_period["voting_start"] == voting_start, f"Unexpected voting_start: {proposal_after_period['voting_start']}"
    assert proposal_after_period["voting_end"] == voting_end, f"Unexpected voting_end: {proposal_after_period['voting_end']}"
    logger.info(f"Proposal status after setting period: {proposal_after_period['status']}")

    # Step 4: Vote on proposal
    logger.info(f"Voting on proposal {proposal_id}... ")
    vote_result = contract.vote_on_proposal(
        proposal_id=proposal_id,
        voter=voter,
        vote=1,  # Approve
        dfc_amount=DFC_AVAIL
    )

    # Assert votes were successful
    assert "proposal_id" in vote_result, "Vote submission failed for voter1"
    assert vote_result["vote"] == "1", "Vote value mismatch for voter1"
    assert vote_result["dfc_amount"] == str(DFC_AVAIL), "DFC amount mismatch for voter1"
    logger.info(f"Votes submitted. Voter: {vote_result.get('transaction_hash', 'unknown')}")

    # Wait sync and verify votes
    proposal_after_votes = None
    attempt_nr = 0
    while attempt_nr < MAX_ATTEMPTS:
        time.sleep(WAIT_DATA_SYNC)
        attempt_nr += 1
        proposal_after_votes = contract.get_proposal(proposal_id)
        if proposal_after_votes and len(proposal_after_votes.get("vote_tally", [])) == 1:
            break
        logger.info(f"Attempt {attempt_nr}/{MAX_ATTEMPTS}: Waiting for vote tally for proposal {proposal_id}")
        logger.info(f"    current proposal_after_votes: {proposal_after_votes}")

    vote_tally = proposal_after_votes["vote_tally"]
    logger.info(f"Proposal has the following vote_tally: {vote_tally}")

    assert proposal_after_votes is not None, f"Proposal {proposal_id} not found after voting"
    assert len(proposal_after_votes["vote_tally"]) == 1, f"Expected 1 vote, found {len(proposal_after_votes.get('vote_tally', []))}"
    assert voter.pub_key_hash in vote_tally and vote_tally[voter.pub_key_hash] == DFC_AVAIL, "Voter vote not recorded correctly"
    assert proposal_after_votes['status'] == ProposalStatus.VOTING

    # Step 5: Finalize proposal
    logger.info(f"Finalizing proposal {proposal_id}...")
    # Calculate outcome based on weighted votes
    approve_weight = DFC_AVAIL  # voter's vote
    outcome = 1 if approve_weight > 0 else 0  # Approve if majority
    finalize_result = contract.finalize_proposal(
        proposal_id=proposal_id,
        outcome=outcome,
        wallet=proposer
    )

    # Assert finalization was successful
    assert "proposal_id" in finalize_result, "Proposal finalization failed"
    assert finalize_result["proposal_id"] == proposal_id, "Proposal ID mismatch in finalization"
    assert finalize_result["outcome"] == str(outcome), f"Unexpected outcome: {finalize_result['outcome']}"
    expected_status = ProposalStatus.APPROVED if outcome == 1 else ProposalStatus.REJECTED
    logger.info(f"Proposal finalized. Outcome: {finalize_result['outcome']}, Transaction: {finalize_result.get('transaction_hash', 'unknown')}")

    # Wait sync and verify status
    proposal_after_finalize = None
    attempt_nr = 0
    while attempt_nr < MAX_ATTEMPTS:
        time.sleep(WAIT_DATA_SYNC)
        attempt_nr += 1
        proposal_after_finalize = contract.get_proposal(proposal_id)
        if proposal_after_finalize and proposal_after_finalize["status"] == expected_status:
            break
        logger.info(f"Attempt {attempt_nr}/{MAX_ATTEMPTS}: Waiting for finalization status for proposal {proposal_id}")

    assert proposal_after_finalize is not None, f"Proposal {proposal_id} not found after finalization"
    assert proposal_after_finalize["status"] == expected_status, f"Unexpected status after finalization: {proposal_after_finalize['status']}"
    assert proposal_after_finalize["outcome"] == outcome, f"Unexpected outcome in proposal data: {proposal_after_finalize['outcome']}"
    logger.info(f"Proposal status after finalization: {proposal_after_finalize['status']}")

    # Step 6: Execute proposal (if approved)
    if expected_status == ProposalStatus.APPROVED:
        logger.info(f"Executing proposal {proposal_id}...")
        execute_result = contract.execute_proposal(
            proposal_id=proposal_id,
            wallet=proposer
        )

        # Assert execution was successful
        assert "proposal_id" in execute_result, "Proposal execution failed"
        assert execute_result["proposal_id"] == proposal_id, "Proposal ID mismatch in execution"
        logger.info(f"Proposal executed. Transaction: {execute_result.get('transaction_hash', 'unknown')}")

        # Wait sync and verify status
        proposal_after_execute = None
        attempt_nr = 0
        while attempt_nr < MAX_ATTEMPTS:
            time.sleep(WAIT_DATA_SYNC)
            attempt_nr += 1
            proposal_after_execute = contract.get_proposal(proposal_id)
            if proposal_after_execute and proposal_after_execute["status"] == ProposalStatus.EXECUTED:
                break
            logger.info(f"Attempt {attempt_nr}/{MAX_ATTEMPTS}: Waiting for execution status for proposal {proposal_id}")

        assert proposal_after_execute is not None, f"Proposal {proposal_id} not found after execution"
        assert proposal_after_execute["status"] == ProposalStatus.EXECUTED, f"Unexpected status after execution: {proposal_after_execute['status']}"
        logger.info(f"Proposal status after execution: {proposal_after_execute['status']}")

    # Step 7: Update authorized PKHs
    logger.info("Updating authorized PKHs...")
    new_pkhs = {
        voter.pub_key_hash: 17
    }
    pkh_result = contract.update_authorized_pkhs(
        proposal_id=proposal_id,
        new_pkhs=new_pkhs,
        wallet=proposer
    )

    # Assert PKH update was successful
    assert "transaction_hash" in pkh_result, "Authorized PKHs update failed"
    logger.info(f"Authorized PKHs updated. Transaction: {pkh_result.get('transaction_hash', 'unknown')}")

    # Verify updated PKHs
    attempt_nr = 0
    updated_proposal = None
    while attempt_nr < MAX_ATTEMPTS:
        time.sleep(WAIT_DATA_SYNC)
        attempt_nr += 1
        updated_proposal = contract.get_proposal(proposal_id)
        if updated_proposal and updated_proposal.get("authorized_pkhs"):
            break
        logger.info(f"Attempt {attempt_nr}/{MAX_ATTEMPTS}: Waiting for updated PKHs for proposal {proposal_id}")

    assert updated_proposal is not None, f"Proposal {proposal_id} not found after PKH update"
    assert updated_proposal.get("authorized_pkhs", {}) == new_pkhs, f"Authorized PKHs not updated correctly: {updated_proposal.get('authorized_pkhs')}"

    # Step 8: Update minimum voting tokens
    logger.info("Updating minimum voting tokens...")
    new_min_tokens = MIN_VOTING_TOKENS + 1000
    token_result = contract.update_min_voting_tokens(
        proposal_id=proposal_id,
        min_tokens=new_min_tokens,
        wallet=proposer
    )

    # Assert token update was successful
    assert "transaction_hash" in token_result, "Minimum voting tokens update failed"
    logger.info(f"Minimum voting tokens updated to {new_min_tokens}. Transaction: {token_result.get('transaction_hash', 'unknown')}")

    # Verify updated minimum voting tokens
    attempt_nr = 0
    updated_proposal = None
    while attempt_nr < MAX_ATTEMPTS:
        time.sleep(WAIT_DATA_SYNC)
        attempt_nr += 1
        updated_proposal = contract.get_proposal(proposal_id)
        if updated_proposal and updated_proposal.get("min_voting_tokens"):
            break
        logger.info(f"Attempt {attempt_nr}/{MAX_ATTEMPTS}: Waiting for updated min tokens for proposal {proposal_id}")

    assert updated_proposal is not None, f"Proposal {proposal_id} not found after min tokens update"
    assert updated_proposal.get("min_voting_tokens") == new_min_tokens, f"Minimum voting tokens not updated to {new_min_tokens}"

    # Step 9: Get all proposals
    all_proposals = contract.get_proposals()
    assert all_proposals is not None, "Failed to retrieve proposals"
    assert len(all_proposals) > 0, "No proposals found"
    logger.info(f"Total proposals: {len(all_proposals)}")
    for i, proposal in enumerate(all_proposals):
        logger.info(f"Proposal #{i+1}: ID={proposal['proposal_id']}, Status={proposal['status']}")
        if proposal["proposal_id"] == proposal_id:
            assert proposal["status"] == (ProposalStatus.EXECUTED if expected_status == ProposalStatus.APPROVED else ProposalStatus.REJECTED), f"Unexpected final status for {proposal_id}"

    logger.info("All assertions passed!")
    logger.info("Integration test completed successfully!")

if __name__ == "__main__":
    main()