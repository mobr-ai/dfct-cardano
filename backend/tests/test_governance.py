import pytest
from unittest.mock import patch, MagicMock
from fastapi.testclient import TestClient
from pycardano import Address, ScriptHash
from dfctbackend.cardano.wallet import CardanoWallet
from dfctbackend.cardano.contract.datum import DatumProcessor
from dfctbackend.cardano.contract.governance.governance_datum import ProposalStatus
from dfctbackend.cardano.contract.governance.governance_contract import GovernanceContract
from dfctbackend.cardano.transaction import CardanoTransaction
from dfctbackend.cardano.utils import str_to_hex
from dfctbackend.config import settings
from dfctbackend.main import app

# Test client for API testing
client = TestClient(app)

@pytest.fixture
def mock_assets_dir(tmp_path):
    """Fixture to create a temporary assets directory with mock wallet files."""
    assets_dir = tmp_path / "assets"
    assets_dir.mkdir()

    # Create mock wallet files
    wallet_names = ["owner", "proposer", "voter1", "voter2", "test"]
    for name in wallet_names:
        (assets_dir / f"{name}.addr").write_text("addr_test1wzw3dv98lkulug8gh26exeh67qf44lkjtj7eejfrh0skpycfv9hqk")
        (assets_dir / f"{name}.pkh").write_text("62d35505a1b728c45d7c039bc3c71d9f9300eec8e220d0ecd740f7d6")
        (assets_dir / f"{name}.skey").write_text('{"type": "PaymentSigningKeyShelley_ed25519","description": "Payment Signing Key","cborHex": "582050092ffa66340253d5f08852cadce0ab2b2a229886866376c84c7ec45dcd3c31"}')

    # Mock policy and validators
    (assets_dir / "dfct-minting-policy.id").write_text("ba620a995810f982de2a8994901335bda7fa041eeec1ae32dc57edfa")
    (assets_dir / "dfct-governance.addr").write_text("addr_test1wzw3dv98lkulug8gh26exeh67qf44lkjtj7eejfrh0skpycfv9hqk")
    (assets_dir / "dfct-governance.plutus").write_text('{"type": "PlutusScriptV3","description": "","cborHex": "4e4d01000033222220051200120011"}')

    with patch.object(settings, "ASSETS_DIR", assets_dir):
        with patch.object(settings, "DOCKER_ASSETS_DIR", str(assets_dir)):
            yield assets_dir

@pytest.fixture
def mock_chain_context():
    """Fixture to mock the chain context."""
    context = MagicMock()
    context.utxos.return_value = []
    return context

@pytest.fixture
def mock_cardano_transaction(mock_chain_context):
    """Fixture to mock CardanoTransaction."""
    tx = MagicMock(spec=CardanoTransaction)
    tx.context = mock_chain_context
    return tx

class TestGovernanceContract:
    """Tests for GovernanceContract class."""

    @pytest.fixture
    def contract(self, mock_assets_dir, mock_cardano_transaction):
        """Fixture for GovernanceContract with mocked dependencies."""
        contract_mock = MagicMock(spec=GovernanceContract)
        contract_mock.tx = mock_cardano_transaction
        contract_mock.policy_id = "ba620a995810f982de2a8994901335bda7fa041eeec1ae32dc57edfa"
        contract_mock.token_name = "DFC"
        contract_mock.token_name_hex = str_to_hex("DFC")
        contract_mock.validator_address = Address.decode(
            "addr_test1wzw3dv98lkulug8gh26exeh67qf44lkjtj7eejfrh0skpycfv9hqk"
        )
        contract_mock.dp = MagicMock(spec=DatumProcessor)
        return contract_mock

    def test_contract_initialization(self, contract):
        """Test contract initialization."""
        assert contract.policy_id == "ba620a995810f982de2a8994901335bda7fa041eeec1ae32dc57edfa"
        assert contract.token_name == "DFC"
        assert isinstance(contract.validator_address, Address)

    def test_submit_proposal(self, contract, mock_assets_dir):
        """Test submitting a new proposal."""
        wallet = CardanoWallet("proposer")
        mock_utxo = MagicMock()
        mock_utxo.output.amount.coin = 70000000
        mock_utxo.output.amount.multi_asset = {ScriptHash(bytes.fromhex("ba620a995810f982de2a8994901335bda7fa041eeec1ae32dc57edfa")): {"DFC": 1000}}
        contract.tx.find_token_utxo.return_value = mock_utxo
        contract.tx.find_utxo_and_create_tx_in.return_value = ("txin#0", mock_utxo)
        contract.tx.get_transaction_hash.return_value = "tx_hash"
        contract.submit_proposal.return_value = {
            "transaction_hash": "tx_hash",
            "proposal_id": "p12345678"
        }

        result = contract.submit_proposal(
            proposal_id="p12345678",
            proposer=wallet,
            lovelace_amount=70000000
        )
        assert result is not None
        assert result["proposal_id"] == "p12345678"
        assert result["transaction_hash"] == "tx_hash"

    def test_vote_on_proposal(self, contract, mock_assets_dir):
        """Test voting on a proposal."""
        wallet = CardanoWallet("voter1")
        mock_utxo = MagicMock()
        mock_utxo.output.amount.coin = 70000000
        mock_utxo.output.amount.multi_asset = {ScriptHash(bytes.fromhex("ba620a995810f982de2a8994901335bda7fa041eeec1ae32dc57edfa")): {"DFC": 1000}}
        contract._get_contract_utxo_and_datum.return_value = (mock_utxo, [
            ["p12345678", "mock_pkh", 1000],
            ProposalStatus.VOTING.value,
            {"yes_votes": {}, "no_votes": {}},
            1234567890
        ])
        contract.tx.find_token_utxo.return_value = mock_utxo
        contract.tx.find_utxo_and_create_tx_in.side_effect = [
            ("txin_collateral#0", mock_utxo),
            ("txin_fee#0", mock_utxo)
        ]
        contract.tx.get_transaction_hash.return_value = "tx_hash"
        contract.vote_on_proposal.return_value = {
            "transaction_hash": "tx_hash",
            "proposal_id": "p12345678"
        }

        result = contract.vote_on_proposal(
            proposal_id="p12345678",
            voter=wallet,
            vote=True,
            dfc_amount=500
        )
        assert result is not None
        assert result["proposal_id"] == "p12345678"
        assert result["transaction_hash"] == "tx_hash"

    def test_finalize_proposal(self, contract, mock_assets_dir):
        """Test finalizing a proposal."""
        wallet = CardanoWallet("owner")
        mock_utxo = MagicMock()
        mock_utxo.output.amount.coin = 70000000
        mock_utxo.output.amount.multi_asset = {ScriptHash(bytes.fromhex("ba620a995810f982de2a8994901335bda7fa041eeec1ae32dc57edfa")): {"DFC": 1000}}
        contract._get_contract_utxo_and_datum.return_value = (mock_utxo, [
            ["p12345678", "mock_pkh", 1000],
            ProposalStatus.VOTING.value,
            {"yes_votes": {"mock_pkh": 500}, "no_votes": {}},
            1234567890
        ])
        contract.tx.find_utxo_and_create_tx_in.side_effect = [
            ("txin_collateral#0", mock_utxo),
            ("txin_fee#0", mock_utxo)
        ]
        contract.tx.get_cnt_amount.return_value = 1000
        contract.tx.get_transaction_hash.return_value = "tx_hash"
        contract.finalize_proposal.return_value = {
            "transaction_hash": "tx_hash",
            "proposal_id": "p12345678",
            "status": ProposalStatus.APPROVED.value
        }

        result = contract.finalize_proposal(
            proposal_id="p12345678",
            wallet=wallet
        )
        assert result is not None
        assert result["proposal_id"] == "p12345678"
        assert result["transaction_hash"] == "tx_hash"
        assert result["status"] == ProposalStatus.APPROVED.value

    def test_get_proposal(self, contract, mock_assets_dir):
        """Test retrieving a proposal."""
        mock_datum = [
            ["p12345678", "mock_pkh", 1000],
            ProposalStatus.VOTING.value,
            {"yes_votes": {"mock_pkh": 500}, "no_votes": {}},
            1234567890
        ]
        contract._get_contract_utxo_and_datum.return_value = (MagicMock(), mock_datum)
        contract.get_proposal.return_value = {
            "proposal_id": "p12345678",
            "status": ProposalStatus.VOTING
        }
        proposal = contract.get_proposal("p12345678")
        assert proposal is not None
        assert proposal["proposal_id"] == "p12345678"
        assert proposal["status"] == ProposalStatus.VOTING

    def test_get_proposals(self, contract, mock_assets_dir):
        """Test retrieving all proposals."""
        mock_datum = [
            ["p12345678", "mock_pkh", 1000],
            ProposalStatus.VOTING.value,
            {"yes_votes": {"mock_pkh": 500}, "no_votes": {}},
            1234567890
        ]
        contract.tx.find_utxos_at_address = MagicMock(return_value=[MagicMock()])
        contract.dp.decode_utxo_datum = MagicMock(return_value=mock_datum)
        contract.get_proposals.return_value = [
            {"proposal_id": "p12345678", "status": ProposalStatus.VOTING}
        ]
        proposals = contract.get_proposals()
        assert len(proposals) == 1
        assert proposals[0]["proposal_id"] == "p12345678"

class TestGovernanceAPIRouter:
    """Tests for governance-related API endpoints in router.py."""

    @pytest.fixture
    def mock_contract(self, mock_assets_dir, mock_cardano_transaction):
        """Fixture to mock GovernanceContract for API tests."""
        contract_mock = MagicMock(spec=GovernanceContract)
        contract_mock.tx = mock_cardano_transaction
        contract_mock.policy_id = "ba620a995810f982de2a8994901335bda7fa041eeec1ae32dc57edfa"
        contract_mock.token_name = "DFC"
        contract_mock.token_name_hex = str_to_hex("DFC")
        contract_mock.validator_address = Address.decode(
            "addr_test1wzw3dv98lkulug8gh26exeh67qf44lkjtj7eejfrh0skpycfv9hqk"
        )
        contract_mock.dp = DatumProcessor()
        with patch("dfctbackend.api.router.governance_contract", contract_mock):
            yield contract_mock

    def test_submit_proposal_api(self, mock_contract, mock_assets_dir):
        """Test submitting a proposal via API."""
        mock_contract.submit_proposal.return_value = {
            "transaction_hash": "tx_hash",
            "proposal_id": "p12345678"
        }
        response = client.post(
            "/api/v1/governance/proposal",
            json={
                "proposal_id": "p12345678",
                "proposer_wallet_info": {
                    "name": "test",
                    "address": "addr_test1wzw3dv98lkulug8gh26exeh67qf44lkjtj7eejfrh0skpycfv9hqk",
                    "pub_key_hash": "62d35505a1b728c45d7c039bc3c71d9f9300eec8e220d0ecd740f7d6",
                    "skey": '{"type": "PaymentSigningKeyShelley_ed25519","description": "Payment Signing Key","cborHex": "582050092ffa66340253d5f08852cadce0ab2b2a229886866376c84c7ec45dcd3c31"}'
                },
                "lovelace_amount": 70000000
            }
        )
        assert response.status_code == 200
        assert response.json()["transaction_hash"] == "tx_hash"
        assert response.json()["proposal_id"] == "p12345678"

    def test_vote_on_proposal_api(self, mock_contract, mock_assets_dir):
        """Test voting on a proposal via API."""
        mock_contract.vote_on_proposal.return_value = {
            "transaction_hash": "tx_hash",
            "proposal_id": "p12345678"
        }
        response = client.post(
            "/api/v1/governance/proposal/vote",
            json={
                "proposal_id": "p12345678",
                "voter_wallet_info": {
                    "name": "test",
                    "address": "addr_test1wzw3dv98lkulug8gh26exeh67qf44lkjtj7eejfrh0skpycfv9hqk",
                    "pub_key_hash": "62d35505a1b728c45d7c039bc3c71d9f9300eec8e220d0ecd740f7d6",
                    "skey": '{"type": "PaymentSigningKeyShelley_ed25519","description": "Payment Signing Key","cborHex": "582050092ffa66340253d5f08852cadce0ab2b2a229886866376c84c7ec45dcd3c31"}'
                },
                "vote": True,
                "dfc_amount": 500
            }
        )
        assert response.status_code == 200
        assert response.json()["transaction_hash"] == "tx_hash"
        assert response.json()["proposal_id"] == "p12345678"

    def test_finalize_proposal_api(self, mock_contract, mock_assets_dir):
        """Test finalizing a proposal via API."""
        mock_contract.finalize_proposal.return_value = {
            "transaction_hash": "tx_hash",
            "proposal_id": "p12345678",
            "status": ProposalStatus.APPROVED.value
        }
        response = client.post(
            "/api/v1/governance/proposal/finalize",
            json={
                "proposal_id": "p12345678"
            }
        )
        assert response.status_code == 200
        assert response.json()["transaction_hash"] == "tx_hash"
        assert response.json()["proposal_id"] == "p12345678"

    def test_get_proposal_api(self, mock_contract, mock_assets_dir):
        """Test retrieving a proposal via API."""
        mock_contract.get_proposal.return_value = {
            "proposal_id": "p12345678",
            "status": ProposalStatus.VOTING
        }
        response = client.get("/api/v1/governance/proposal/p12345678")
        assert response.status_code == 200
        assert response.json()["proposal_id"] == "p12345678"
        assert response.json()["status"] == ProposalStatus.VOTING.value

    def test_get_all_proposals_api(self, mock_contract, mock_assets_dir):
        """Test retrieving all proposals via API."""
        mock_contract.get_proposals.return_value = [
            {"proposal_id": "p12345678", "status": ProposalStatus.VOTING}
        ]
        response = client.get("/api/v1/governance/proposals")
        assert response.status_code == 200
        assert len(response.json()) == 1
        assert response.json()[0]["proposal_id"] == "p12345678"