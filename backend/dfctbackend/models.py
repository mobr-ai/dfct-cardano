from typing import Optional
from pydantic import BaseModel, Field, field_validator, ConfigDict

from dfctbackend.cardano.datum import ContributionType

class WalletInfo(BaseModel):
    """Model for wallet information."""
    model_config = ConfigDict(
        title="Wallet Information",
        extra="forbid"
    )

    name: str = Field(description="Name of the wallet")
    address: str = Field(description="Wallet address")
    pub_key_hash: str = Field(description="PubKeyHash")
    skey: str = Field(description="Signature")

class TopicSubmitRequest(BaseModel):
    """Request model for submitting a new topic."""
    model_config = ConfigDict(
        title="Topic Submit Request",
        extra="forbid"
    )

    topic_id: str = Field(description="Id of the topic")
    proposer_wallet_info: WalletInfo = Field(description="Topic proposer wallet info")
    lovelace_amount: int = Field(default=70000000, description="Amount of lovelace to fund the topic")
    reward_amount: int = Field(default=1000, description="Amount of DFC tokens to be used as reward")

    @field_validator('reward_amount')
    @classmethod
    def reward_must_be_positive(cls, v: int) -> int:
        if v <= 0:
            raise ValueError('Reward amount must be positive')
        return v

class TopicReviewRequest(BaseModel):
    """Request model for reviewing a topic."""
    model_config = ConfigDict(
        title="Topic Review Request",
        extra="forbid"
    )

    topic_id: str = Field(description="ID of the topic being reviewed")
    reviewer_wallet_info: WalletInfo = Field(description="Topic reviewer wallet info")
    approved: bool = Field(description="Whether the topic is approved")

class TopicActivateRequest(BaseModel):
    """Request model for activating a topic."""
    model_config = ConfigDict(
        title="Topic Activate Request",
        extra="forbid"
    )

    topic_id: str = Field(description="ID of the topic to activate")

class ContributionSubmitRequest(BaseModel):
    """Request model for submitting a contribution to a topic."""
    model_config = ConfigDict(
        title="Contribution Submit Request",
        extra="forbid"
    )

    topic_id: str = Field(description="ID of the topic to contribute to")
    contribution_id: str = Field(description="ID of the contribution")
    contributor_wallet_info: WalletInfo = Field(description="Contributor wallet info")

class ContributionReviewRequest(BaseModel):
    """Request model for reviewing a contribution."""
    model_config = ConfigDict(
        title="Contribution Review Request",
        extra="forbid"
    )

    contribution_id: str = Field(description="ID of the contribution being reviewed")
    reviewer_wallet_info: WalletInfo = Field(description="Contribution reviewer wallet info")
    approved: bool = Field(description="Whether the contribution is approved")
    relevance: int = Field(default=5, ge=0, le=10, description="Relevance score (0-10)")
    accuracy: int = Field(default=5, ge=0, le=10, description="Accuracy score (0-10)")
    completeness: int = Field(default=5, ge=0, le=10, description="Completeness score (0-10)")

class ContributionDisputeRequest(BaseModel):
    """Request model for disputing a contribution."""
    model_config = ConfigDict(
        title="Contribution Dispute Request",
        extra="forbid"
    )

    contribution_id: str = Field(description="ID of the contribution being disputed")
    proposer_wallet_info: WalletInfo = Field(description="Dispute proposer wallet info")

class ProposalSubmitRequest(BaseModel):
    """Request model for submitting a new governance proposal."""
    model_config = ConfigDict(
        title="Proposal Submission Request",
        extra="forbid"
    )

    proposal_id: str = Field(description="Id of the proposal")
    proposer_wallet_info: WalletInfo = Field(description="Proposal author's wallet info")
    lovelace_amount: int = Field(default=10000000, description="Amount of lovelace to fund the proposal")

class VoteRequest(BaseModel):
    """Request model for voting on a governance proposal."""
    model_config = ConfigDict(
        title="Vote Request",
        extra="forbid"
    )

    proposal_id: str = Field(description="ID of the proposal to vote on")
    voter_wallet_info: WalletInfo = Field(description="Proposal voter's wallet info")
    vote: bool = Field(description="True for yes, False for no")
    dfc_amount: int = Field(ge=1, description="Amount of DFC tokens to vote with")

class FinalizeProposalRequest(BaseModel):
    """Request model for finalizing a governance proposal."""
    model_config = ConfigDict(
        title="Finalize Proposal Request. Currently only the owner can finalize a proposal",
        extra="forbid"
    )

    proposal_id: str = Field(description="ID of the proposal to finalize")

class TransactionResponse(BaseModel):
    """Response model for transactions."""
    model_config = ConfigDict(
        title="Transaction Response",
        extra="forbid"
    )
    
    transaction_hash: str = Field(description="Hash of the submitted transaction")
    topic_id: Optional[str] = Field(None, description="ID of the topic, if applicable")
    contribution_id: Optional[str] = Field(None, description="ID of the contribution, if applicable")
    proposal_id: Optional[str] = Field(None, description="ID of the governance proposal, if applicable")
