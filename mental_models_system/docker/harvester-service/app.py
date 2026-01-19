"""
Harvester Service

Data harvesting microservice for web scraping and file processing.
"""

from fastapi import FastAPI, HTTPException, BackgroundTasks
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import List, Optional, Dict, Any
import os
import logging
import asyncio
from datetime import datetime
import hashlib

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

app = FastAPI(
    title="Mental Models Harvester Service",
    description="Data harvesting for web scraping and file processing",
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

# In-memory job storage (would use Redis/DB in production)
jobs: Dict[str, Dict[str, Any]] = {}
scraped_data: List[Dict[str, Any]] = []


class ScrapeRequest(BaseModel):
    url: str
    extract_text: bool = True
    follow_links: bool = False
    max_depth: int = 1


class ScrapeResponse(BaseModel):
    job_id: str
    status: str
    message: str


class JobStatus(BaseModel):
    job_id: str
    status: str
    progress: float
    result: Optional[Dict[str, Any]] = None
    error: Optional[str] = None
    created_at: str
    updated_at: str


@app.get("/")
async def root():
    return {
        "service": "Mental Models Harvester Service",
        "version": "1.0.0",
        "active_jobs": len([j for j in jobs.values() if j["status"] == "running"])
    }


@app.get("/health")
async def health_check():
    return {"status": "healthy", "service": "harvester"}


async def scrape_url_task(job_id: str, url: str, extract_text: bool):
    """Background task to scrape a URL"""
    try:
        jobs[job_id]["status"] = "running"
        jobs[job_id]["progress"] = 0.1
        jobs[job_id]["updated_at"] = datetime.utcnow().isoformat()
        
        # Simulate scraping (in production, use aiohttp/httpx + BeautifulSoup)
        await asyncio.sleep(1)
        jobs[job_id]["progress"] = 0.5
        
        # Generate mock content based on URL
        content_hash = hashlib.md5(url.encode()).hexdigest()[:8]
        
        result = {
            "url": url,
            "title": f"Page from {url.split('/')[2] if '/' in url else url}",
            "content": f"Sample extracted content from {url}. This would contain the actual page text in production.",
            "word_count": 150,
            "links_found": 5,
            "scraped_at": datetime.utcnow().isoformat(),
            "content_hash": content_hash
        }
        
        jobs[job_id]["progress"] = 1.0
        jobs[job_id]["status"] = "completed"
        jobs[job_id]["result"] = result
        jobs[job_id]["updated_at"] = datetime.utcnow().isoformat()
        
        # Store scraped data
        scraped_data.append(result)
        
        logger.info(f"Scrape job {job_id} completed for {url}")
        
    except Exception as e:
        jobs[job_id]["status"] = "failed"
        jobs[job_id]["error"] = str(e)
        jobs[job_id]["updated_at"] = datetime.utcnow().isoformat()
        logger.error(f"Scrape job {job_id} failed: {e}")


@app.post("/scrape", response_model=ScrapeResponse)
async def scrape_url(request: ScrapeRequest, background_tasks: BackgroundTasks):
    """Start a scraping job"""
    job_id = hashlib.md5(f"{request.url}{datetime.utcnow().isoformat()}".encode()).hexdigest()[:12]
    
    jobs[job_id] = {
        "job_id": job_id,
        "url": request.url,
        "status": "pending",
        "progress": 0.0,
        "result": None,
        "error": None,
        "created_at": datetime.utcnow().isoformat(),
        "updated_at": datetime.utcnow().isoformat()
    }
    
    background_tasks.add_task(scrape_url_task, job_id, request.url, request.extract_text)
    
    return ScrapeResponse(
        job_id=job_id,
        status="pending",
        message=f"Scraping job started for {request.url}"
    )


@app.get("/jobs/{job_id}", response_model=JobStatus)
async def get_job_status(job_id: str):
    """Get status of a scraping job"""
    if job_id not in jobs:
        raise HTTPException(status_code=404, detail="Job not found")
    
    job = jobs[job_id]
    return JobStatus(
        job_id=job["job_id"],
        status=job["status"],
        progress=job["progress"],
        result=job["result"],
        error=job["error"],
        created_at=job["created_at"],
        updated_at=job["updated_at"]
    )


@app.get("/jobs")
async def list_jobs():
    """List all jobs"""
    return {
        "jobs": list(jobs.values()),
        "total": len(jobs),
        "running": len([j for j in jobs.values() if j["status"] == "running"]),
        "completed": len([j for j in jobs.values() if j["status"] == "completed"]),
        "failed": len([j for j in jobs.values() if j["status"] == "failed"])
    }


@app.delete("/jobs/{job_id}")
async def delete_job(job_id: str):
    """Delete a job"""
    if job_id not in jobs:
        raise HTTPException(status_code=404, detail="Job not found")
    
    del jobs[job_id]
    return {"message": f"Job {job_id} deleted"}


@app.get("/status")
async def harvester_status():
    """Get harvester status"""
    return {
        "status": "running",
        "jobs_total": len(jobs),
        "jobs_running": len([j for j in jobs.values() if j["status"] == "running"]),
        "jobs_completed": len([j for j in jobs.values() if j["status"] == "completed"]),
        "jobs_failed": len([j for j in jobs.values() if j["status"] == "failed"]),
        "data_collected": len(scraped_data),
        "uptime": "healthy"
    }


@app.get("/data")
async def get_scraped_data():
    """Get all scraped data"""
    return {
        "data": scraped_data,
        "count": len(scraped_data)
    }


@app.delete("/data")
async def clear_scraped_data():
    """Clear all scraped data"""
    scraped_data.clear()
    return {"message": "All scraped data cleared"}


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8002)
