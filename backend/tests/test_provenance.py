import pytest
import time
from unittest.mock import patch, MagicMock
from fastapi.testclient import TestClient
from pycardano import Address, ScriptHash
from dfctbackend.cardano.wallet import CardanoWallet
from dfctbackend.cardano.datum import DatumProcessor, TopicStatus, ContributionStatus, ContributionType
from dfctbackend.cardano.provenance_contract import ProvenanceContract
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
    wallet_names = ["owner", "proposer", "reviewer1", "reviewer2", "test"]
    for name in wallet_names:
        (assets_dir / f"{name}.addr").write_text("addr_test1wzw3dv98lkulug8gh26exeh67qf44lkjtj7eejfrh0skpycfv9hqk")
        (assets_dir / f"{name}.pkh").write_text("62d35505a1b728c45d7c039bc3c71d9f9300eec8e220d0ecd740f7d6")
        (assets_dir / f"{name}.skey").write_text('{"type": "PaymentSigningKeyShelley_ed25519","description": "Payment Signing Key","cborHex": "582050092ffa66340253d5f08852cadce0ab2b2a229886866376c84c7ec45dcd3c31"}')

    # Mock policy and validators
    (assets_dir / "dfct-minting-policy.id").write_text("ba620a995810f982de2a8994901335bda7fa041eeec1ae32dc57edfa")

    (assets_dir / "dfct-provenance.addr").write_text("addr_test1wzw3dv98lkulug8gh26exeh67qf44lkjtj7eejfrh0skpycfv9hqk")
    (assets_dir / "dfct-governance.addr").write_text("addr_test1wzw3dv98lkulug8gh26exeh67qf44lkjtj7eejfrh0skpycfv9hqk")

    (assets_dir / "dfct-provenance.plutus").write_text('{"type": "PlutusScriptV3","description": "","cborHex": "4e4d01000033222220051200120011"}')
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

class TestCardanoWallet:
    """Tests for CardanoWallet class."""

    def test_wallet_initialization(self, mock_assets_dir):
        """Test wallet initialization with valid files."""
        wallet = CardanoWallet("owner")
        assert wallet.name == "owner"
        assert isinstance(wallet.address, Address)
        assert wallet.pub_key_hash == "62d35505a1b728c45d7c039bc3c71d9f9300eec8e220d0ecd740f7d6"

    def test_wallet_missing_files(self, mock_assets_dir):
        """Test wallet initialization with missing files."""
        with pytest.raises(ValueError, match="Failed to load wallet data"):
            CardanoWallet("nonexistent")

class TestDatumProcessor:
    """Tests for DatumProcessor class."""

    def test_prepare_topic_datum(self):
        """Test preparing topic datum and redeemer."""
        dp = DatumProcessor()
        datum, redeemer = dp.prepare_topic_datum_redeemer(
            topic_id="t12345678",
            proposer_pkh="mock_pub_key_hash"
        )
        assert datum["constructor"] == 0
        assert datum["fields"][0]["fields"][0]["bytes"] == str_to_hex("t12345678")
        assert datum["fields"][0]["fields"][1]["bytes"] == "mock_pub_key_hash"
        assert redeemer["constructor"] == 0

    def test_prepare_contribution_datum(self):
        """Test preparing contribution datum and redeemer."""
        dp = DatumProcessor()
        contribution_id = "c12345678"
        datum, redeemer = dp.prepare_contribution_datum_redeemer(
            contribution_id=contribution_id,
            topic_id="t12345678",
            contribution_type=ContributionType.EVIDENCE,
            contributor_pkh="mock_pub_key_hash",
            contribution_status=ContributionStatus.PROPOSED,
            timestamp=int(time.time() * 1000)
        )
        assert datum["constructor"] == 0
        assert redeemer["constructor"] == 1

    def test_prepare_review_topic_redeemer(self):
        """Test preparing review topic redeemer."""
        dp = DatumProcessor()
        redeemer = dp.prepare_review_topic_redeemer(topic_id="t12345678", approved=True)
        assert redeemer["constructor"] == 0
        assert redeemer["fields"][0]["constructor"] == 1
        assert redeemer["fields"][0]["fields"][1]["int"] == 1

    def test_prepare_review_contribution_redeemer(self):
        """Test preparing review contribution redeemer."""
        dp = DatumProcessor()
        redeemer = dp.prepare_review_contribution_redeemer(
            contribution_id="c12345678",
            reviewer_pkh="mock_pub_key_hash",
            relevance=5,
            accuracy=5,
            completeness=5
        )
        assert redeemer["constructor"] == 1
        assert redeemer["fields"][0]["constructor"] == 3
        assert redeemer["fields"][0]["fields"][1]["int"] == 5

    def test_prepare_dispute_contribution_redeemer(self):
        """Test preparing dispute contribution redeemer."""
        dp = DatumProcessor()
        contribution_id="c12345678"
        redeemer = dp.prepare_dispute_contribution_redeemer(
            contribution_id=contribution_id,
            contributor_pkh="mock_pub_key_hash"
        )
        assert redeemer["constructor"] == 1
        assert redeemer["fields"][0]["constructor"] == 5
        assert redeemer["fields"][0]["fields"][0]["bytes"] == str_to_hex(contribution_id)

    def test_extract_topic_from_datum(self):
        """Test extracting topic from datum."""
        dp = DatumProcessor()
        mock_datum = [
            ["t12345678", "mock_pkh", 1234567890],
            TopicStatus.PROPOSED.value,
            [1000, 0, "DFC", 1234567890],
            [{"mock_pkh": 1}]
        ]
        topic = dp.extract_topic_from_datum(mock_datum)
        assert topic["topic_id"] == "t12345678"
        assert topic["status"] == TopicStatus.PROPOSED

class TestProvenanceContract:
    """Tests for ProvenanceContract class."""

    @pytest.fixture
    def contract(self, mock_assets_dir, mock_cardano_transaction):
        """Fixture for ProvenanceContract with mocked dependencies."""
        contract_mock = MagicMock(spec=ProvenanceContract)
        contract_mock.tx = mock_cardano_transaction
        contract_mock.policy_id = "ba620a995810f982de2a8994901335bda7fa041eeec1ae32dc57edfa"
        contract_mock.token_name = "DFC"
        contract_mock.provenance_address = Address.decode(
            "addr_test1wzw3dv98lkulug8gh26exeh67qf44lkjtj7eejfrh0skpycfv9hqk"
        )
        return contract_mock

    def test_contract_initialization(self, contract):
        """Test contract initialization."""
        assert contract.policy_id == "ba620a995810f982de2a8994901335bda7fa041eeec1ae32dc57edfa"
        assert contract.token_name == "DFC"
        assert isinstance(contract.provenance_address, Address)

    def test_submit_topic(self, contract, mock_assets_dir):
        """Test submitting a new topic."""
        wallet = CardanoWallet("proposer")
        mock_utxo = MagicMock()
        mock_utxo.output.amount.coin = 70000000
        mock_utxo.output.amount.multi_asset = {ScriptHash(bytes.fromhex("ba620a995810f982de2a8994901335bda7fa041eeec1ae32dc57edfa")): {"DFC": 1000}}
        contract.tx.find_token_utxo.return_value = mock_utxo
        contract.tx.find_utxo_and_create_tx_in.return_value = ("txin#0", mock_utxo)
        contract.tx.get_transaction_hash.return_value = "tx_hash"

        result = contract.submit_topic(
            topic_id="t12345678",
            proposer=wallet,
            lovelace_amount=70000000,
            reward_amount=1000
        )
        assert result is not None
        assert result["topic_id"].startswith("t")

class TestAPIRouter:
    """Tests for API endpoints in router.py."""

    @pytest.fixture
    def mock_contract(self, mock_assets_dir, mock_cardano_transaction):
        """Fixture to mock ProvenanceContract for API tests."""
        contract_mock = MagicMock(spec=ProvenanceContract)
        contract_mock.tx = mock_cardano_transaction
        contract_mock.policy_id = "ba620a995810f982de2a8994901335bda7fa041eeec1ae32dc57edfa"
        contract_mock.token_name = "DFC"
        contract_mock.token_name_hex = str_to_hex(contract_mock.token_name)
        contract_mock.validator_address = Address.decode(
            "addr_test1wzw3dv98lkulug8gh26exeh67qf44lkjtj7eejfrh0skpycfv9hqk"
        )
        contract_mock.dp = DatumProcessor()

        with patch("dfctbackend.api.router.provenance_contract", contract_mock):
            yield contract_mock

    def test_submit_topic_api(self, mock_contract, mock_assets_dir):
        """Test submitting a topic via API."""
        mock_contract.submit_topic.return_value = {
            "transaction_hash": "tx_hash",
            "topic_id": "t12345678"
        }
        response = client.post(
            "/api/v1/topic",
            json={
                "topic_id": "t12345678",
                "proposer_wallet_info": {
                    "name": "test",
                    "address": "addr_test1wzw3dv98lkulug8gh26exeh67qf44lkjtj7eejfrh0skpycfv9hqk",
                    "pub_key_hash": "62d35505a1b728c45d7c039bc3c71d9f9300eec8e220d0ecd740f7d6",
                    "skey": '{"type": "PaymentSigningKeyShelley_ed25519","description": "Payment Signing Key","cborHex": "582050092ffa66340253d5f08852cadce0ab2b2a229886866376c84c7ec45dcd3c31"}'
                },
                "lovelace_amount": 70000000,
                "reward_amount": 1000
            }
        )
        assert response.status_code == 200

    def test_review_topic_api(self, mock_contract, mock_assets_dir):
        """Test reviewing a topic via API."""
        mock_contract.review_topic.return_value = {
            "transaction_hash": "tx_hash",
            "topic_id": "t12345678",
            "approved": True
        }
        response = client.post(
            "/api/v1/topic/review",
            json={
                "topic_id": "t12345678",
                "reviewer_wallet_info": {
                    "name": "test",
                    "address": "addr_test1wzw3dv98lkulug8gh26exeh67qf44lkjtj7eejfrh0skpycfv9hqk",
                    "pub_key_hash": "62d35505a1b728c45d7c039bc3c71d9f9300eec8e220d0ecd740f7d6",
                    "skey": '{"type": "PaymentSigningKeyShelley_ed25519","description": "Payment Signing Key","cborHex": "582050092ffa66340253d5f08852cadce0ab2b2a229886866376c84c7ec45dcd3c31"}'
                },
                "approved": True
            }
        )
        assert response.status_code == 200

    def test_activate_topic_api(self, mock_contract, mock_assets_dir):
        """Test activating a topic via API."""
        mock_contract.activate_topic.return_value = {
            "transaction_hash": "tx_hash",
            "topic_id": "t12345678"
        }
        response = client.post(
            "/api/v1/topic/activate",
            json={"topic_id": "t12345678"}
        )
        assert response.status_code == 200

    def test_close_topic_api(self, mock_contract, mock_assets_dir):
        """Test closing a topic via API."""
        mock_contract.close_topic.return_value = {
            "transaction_hash": "tx_hash",
            "topic_id": "t12345678"
        }
        response = client.post("/api/v1/topic/t12345678/close")
        assert response.status_code == 200
        assert response.json()["transaction_hash"] == "tx_hash"

    def test_submit_contribution_api(self, mock_contract, mock_assets_dir):
        """Test submitting a contribution via API."""
        mock_contract.submit_contribution.return_value = {
            "transaction_hash": "tx_hash",
            "topic_id": "t12345678",
            "contribution_id": "c12345678"
        }
        response = client.post(
            "/api/v1/contribution",
            json={
                "topic_id": "t12345678",
                "contribution_id": "c12345678",
                "contribution_type": ContributionType.EVIDENCE.value,
                "lovelace_amount": 0,
                "contributor_wallet_info": {
                    "name": "test",
                    "address": "addr_test1wzw3dv98lkulug8gh26exeh67qf44lkjtj7eejfrh0skpycfv9hqk",
                    "pub_key_hash": "62d35505a1b728c45d7c039bc3c71d9f9300eec8e220d0ecd740f7d6",
                    "skey": '{"type": "PaymentSigningKeyShelley_ed25519","description": "Payment Signing Key","cborHex": "582050092ffa66340253d5f08852cadce0ab2b2a229886866376c84c7ec45dcd3c31"}'
                }
            }
        )
        assert response.status_code == 200
        assert response.json()["transaction_hash"] == "tx_hash"
        assert response.json()["topic_id"] == "t12345678"

    def test_review_contribution_api(self, mock_contract, mock_assets_dir):
        """Test reviewing a contribution via API."""
        mock_contract.review_contribution.return_value = {
            "transaction_hash": "tx_hash",
            "contribution_id": "c12345678",
            "relevance": 5,
            "accuracy": 5,
            "completeness": 5
        }
        response = client.post(
            "/api/v1/contribution/review",
            json={
                "contribution_id": "c12345678",
                "approved": True,
                "relevance": 5,
                "accuracy": 5,
                "completeness": 5,
                "reviewer_wallet_info": {
                    "name": "test",
                    "address": "addr_test1wzw3dv98lkulug8gh26exeh67qf44lkjtj7eejfrh0skpycfv9hqk",
                    "pub_key_hash": "62d35505a1b728c45d7c039bc3c71d9f9300eec8e220d0ecd740f7d6",
                    "skey": '{"type": "PaymentSigningKeyShelley_ed25519","description": "Payment Signing Key","cborHex": "582050092ffa66340253d5f08852cadce0ab2b2a229886866376c84c7ec45dcd3c31"}'
                }
            }
        )
        assert response.status_code == 200
        assert response.json()["transaction_hash"] == "tx_hash"

    def test_get_topic_api(self, mock_contract, mock_assets_dir):
        """Test retrieving a topic via API."""
        mock_contract.get_topic.return_value = {
            "topic_id": "t12345678",
            "status": TopicStatus.PROPOSED
        }
        response = client.get("/api/v1/topic/t12345678")
        assert response.status_code == 200
        assert response.json()["topic_id"] == "t12345678"

    def test_get_contribution_api(self, mock_contract, mock_assets_dir):
        """Test retrieving a contribution via API."""
        mock_contract.get_contribution.return_value = {
            "contribution_id": "c12345678",
            "topic_id": "t12345678",
            "status": ContributionStatus.PROPOSED
        }
        response = client.get("/api/v1/contribution/c12345678")
        assert response.status_code == 200
        assert response.json()["contribution_id"] == "c12345678"

    def test_get_all_topics_api(self, mock_contract, mock_assets_dir):
        """Test retrieving all topics via API."""
        mock_contract.get_topics.return_value = [
            {"topic_id": "t12345678", "status": TopicStatus.PROPOSED}
        ]
        response = client.get("/api/v1/topics")
        assert response.status_code == 200
        assert len(response.json()) == 1
        assert response.json()[0]["topic_id"] == "t12345678"

    def test_get_topic_contributions_api(self, mock_contract, mock_assets_dir):
        """Test retrieving contributions for a topic via API."""
        mock_contract.get_contributions_for_topic.return_value = [
            {"contribution_id": "c12345678", "topic_id": "t12345678"}
        ]
        response = client.get("/api/v1/topic/t12345678/contributions")
        assert response.status_code == 200
        assert len(response.json()) == 1
        assert response.json()[0]["contribution_id"] == "c12345678"
