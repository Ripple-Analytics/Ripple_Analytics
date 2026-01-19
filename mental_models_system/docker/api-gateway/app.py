"""
API Gateway Service

Central entry point for all microservices.
Routes requests to appropriate backend services.
"""

from fastapi import FastAPI, HTTPException, Request
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
import httpx
import os
import logging

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

app = FastAPI(
    title="Mental Models API Gateway",
    description="Central gateway for Mental Models microservices",
    version="1.0.0"
)

# CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Service URLs (configurable via environment variables)
ANALYSIS_SERVICE_URL = os.getenv("ANALYSIS_SERVICE_URL", "http://analysis-service:8001")
HARVESTER_SERVICE_URL = os.getenv("HARVESTER_SERVICE_URL", "http://harvester-service:8002")
STORAGE_SERVICE_URL = os.getenv("STORAGE_SERVICE_URL", "http://storage-service:8003")


class AnalysisRequest(BaseModel):
    text: str
    top_n: int = 5


class HealthResponse(BaseModel):
    status: str
    services: dict


@app.get("/")
async def root():
    return {
        "service": "Mental Models API Gateway",
        "version": "1.0.0",
        "endpoints": {
            "health": "/health",
            "analysis": "/api/analysis",
            "harvester": "/api/harvester",
            "storage": "/api/storage"
        }
    }


@app.get("/health", response_model=HealthResponse)
async def health_check():
    """Check health of all services"""
    services = {}
    
    async with httpx.AsyncClient(timeout=5.0) as client:
        # Check analysis service
        try:
            resp = await client.get(f"{ANALYSIS_SERVICE_URL}/health")
            services["analysis"] = "healthy" if resp.status_code == 200 else "unhealthy"
        except Exception:
            services["analysis"] = "unavailable"
        
        # Check harvester service
        try:
            resp = await client.get(f"{HARVESTER_SERVICE_URL}/health")
            services["harvester"] = "healthy" if resp.status_code == 200 else "unhealthy"
        except Exception:
            services["harvester"] = "unavailable"
        
        # Check storage service
        try:
            resp = await client.get(f"{STORAGE_SERVICE_URL}/health")
            services["storage"] = "healthy" if resp.status_code == 200 else "unhealthy"
        except Exception:
            services["storage"] = "unavailable"
    
    all_healthy = all(s == "healthy" for s in services.values())
    
    return {
        "status": "healthy" if all_healthy else "degraded",
        "services": services
    }


# Analysis Service Routes
@app.post("/api/analysis/analyze")
async def analyze_text(request: AnalysisRequest):
    """Analyze text for mental models"""
    async with httpx.AsyncClient(timeout=30.0) as client:
        try:
            resp = await client.post(
                f"{ANALYSIS_SERVICE_URL}/analyze",
                json={"text": request.text, "top_n": request.top_n}
            )
            return resp.json()
        except httpx.RequestError as e:
            raise HTTPException(status_code=503, detail=f"Analysis service unavailable: {str(e)}")


@app.get("/api/analysis/models")
async def get_models():
    """Get all mental models"""
    async with httpx.AsyncClient(timeout=10.0) as client:
        try:
            resp = await client.get(f"{ANALYSIS_SERVICE_URL}/models")
            return resp.json()
        except httpx.RequestError as e:
            raise HTTPException(status_code=503, detail=f"Analysis service unavailable: {str(e)}")


@app.get("/api/analysis/models/{model_id}")
async def get_model(model_id: int):
    """Get a specific mental model"""
    async with httpx.AsyncClient(timeout=10.0) as client:
        try:
            resp = await client.get(f"{ANALYSIS_SERVICE_URL}/models/{model_id}")
            return resp.json()
        except httpx.RequestError as e:
            raise HTTPException(status_code=503, detail=f"Analysis service unavailable: {str(e)}")


@app.get("/api/analysis/categories")
async def get_categories():
    """Get all model categories"""
    async with httpx.AsyncClient(timeout=10.0) as client:
        try:
            resp = await client.get(f"{ANALYSIS_SERVICE_URL}/categories")
            return resp.json()
        except httpx.RequestError as e:
            raise HTTPException(status_code=503, detail=f"Analysis service unavailable: {str(e)}")


# Harvester Service Routes
@app.post("/api/harvester/scrape")
async def scrape_url(request: Request):
    """Scrape a URL for content"""
    body = await request.json()
    async with httpx.AsyncClient(timeout=60.0) as client:
        try:
            resp = await client.post(f"{HARVESTER_SERVICE_URL}/scrape", json=body)
            return resp.json()
        except httpx.RequestError as e:
            raise HTTPException(status_code=503, detail=f"Harvester service unavailable: {str(e)}")


@app.get("/api/harvester/status")
async def harvester_status():
    """Get harvester status"""
    async with httpx.AsyncClient(timeout=10.0) as client:
        try:
            resp = await client.get(f"{HARVESTER_SERVICE_URL}/status")
            return resp.json()
        except httpx.RequestError as e:
            raise HTTPException(status_code=503, detail=f"Harvester service unavailable: {str(e)}")


# Storage Service Routes
@app.get("/api/storage/data")
async def get_stored_data():
    """Get stored data"""
    async with httpx.AsyncClient(timeout=10.0) as client:
        try:
            resp = await client.get(f"{STORAGE_SERVICE_URL}/data")
            return resp.json()
        except httpx.RequestError as e:
            raise HTTPException(status_code=503, detail=f"Storage service unavailable: {str(e)}")


@app.post("/api/storage/data")
async def store_data(request: Request):
    """Store data"""
    body = await request.json()
    async with httpx.AsyncClient(timeout=10.0) as client:
        try:
            resp = await client.post(f"{STORAGE_SERVICE_URL}/data", json=body)
            return resp.json()
        except httpx.RequestError as e:
            raise HTTPException(status_code=503, detail=f"Storage service unavailable: {str(e)}")


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
