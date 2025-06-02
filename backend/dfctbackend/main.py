import time
import uvicorn
from fastapi import FastAPI, Request
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from fastapi.openapi.utils import get_openapi

from dfctbackend.api.router import router
from dfctbackend.config import settings

# Create FastAPI app
app = FastAPI(
    title="DFCT Backend API",
    description="API for Decentralized Fact-Checking Toolkit",
    version="0.1.0",
)

# Custom OpenAPI schema
def custom_openapi():
    if app.openapi_schema:
        return app.openapi_schema
    
    openapi_schema = get_openapi(
        title="DFCT Backend API",
        version="0.1.0",
        description="API for Decentralized Fact-Checking Toolkit - Cardano Smart Contract Interface",
        routes=app.routes,
    )
    
    # Add additional info
    openapi_schema["info"]["x-logo"] = {
        "url": "https://cardano.org/static/logo-cardano.svg"
    }
    
    app.openapi_schema = openapi_schema
    return app.openapi_schema

app.openapi = custom_openapi

# Add CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # In production, replace with specific origins
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Add router
app.include_router(router)

@app.get("/health")
async def health_check():
    """Health check endpoint."""
    return {
        "status": "healthy",
        "timestamp": int(time.time())
    }

@app.get("/config")
async def get_config():
    """Get configuration information."""
    return {
        "policy_id": settings.POLICY_ID,
        "provenance_address": settings.PROVENANCE_ADDRESS,
        "token_name": settings.TOKEN_NAME,
    }

@app.exception_handler(Exception)
async def global_exception_handler(request: Request, exc: Exception):
    """Global exception handler."""
    return JSONResponse(
        status_code=500,
        content={"status": "error", "message": str(exc), "timestamp": int(time.time())},
    )

if __name__ == "__main__":
    uvicorn.run("dfctbackend.main:app", host="0.0.0.0", port=8000, reload=True)