from typing import Optional
from pydantic import BaseModel, Field, field_validator, ConfigDict

class TopicSubmitRequest(BaseModel):
    """Request model for submitting a new topic."""
    model_config = ConfigDict(
        title="Topic Submit Request",
        extra="forbid"
    )
    
    title: str = Field(description="Title of the topic")
    description: str = Field(description="Description of the topic")
    reward_amount: int = Field(description="Amount of tokens to be used as reward")
    
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
    content: str = Field(description="Content of the contribution")
    evidence_links: list[str] = Field(default=[], description="Links to evidence supporting the contribution")

class ContributionReviewRequest(BaseModel):
    """Request model for reviewing a contribution."""
    model_config = ConfigDict(
        title="Contribution Review Request",
        extra="forbid"
    )
    
    contribution_id: str = Field(description="ID of the contribution being reviewed")
    approved: bool = Field(description="Whether the contribution is approved")
    comment: Optional[str] = Field(None, description="Optional comment about the review decision")

class TransactionResponse(BaseModel):
    """Response model for transactions."""
    model_config = ConfigDict(
        title="Transaction Response",
        extra="forbid"
    )
    
    transaction_hash: str = Field(description="Hash of the submitted transaction")
    topic_id: Optional[str] = Field(None, description="ID of the topic, if applicable")

class ErrorResponse(BaseModel):
    """Response model for errors."""
    model_config = ConfigDict(
        title="Error Response",
        extra="forbid"
    )
    
    status: str = Field("error", description="Error status")
    message: str = Field(description="Error message")
    timestamp: Optional[int] = Field(None, description="Timestamp of the error")

class WalletInfo(BaseModel):
    """Model for wallet information."""
    model_config = ConfigDict(
        title="Wallet Information",
        extra="forbid"
    )
    
    name: str = Field(description="Name of the wallet")
    address: str = Field(description="Wallet address")
    payment_key_hash: str = Field(description="Payment key hash")