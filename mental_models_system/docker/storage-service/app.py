"""
Storage Service

Data persistence microservice for storing analysis results, settings, and user data.
"""

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import List, Optional, Dict, Any
import os
import logging
import json
from datetime import datetime
import hashlib

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

app = FastAPI(
    title="Mental Models Storage Service",
    description="Data persistence for analysis results and settings",
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

# In-memory storage (would use SQLite/PostgreSQL in production)
storage: Dict[str, Dict[str, Any]] = {
    "analyses": {},
    "settings": {},
    "decisions": {},
    "models": {}
}

# Data directory for persistence
DATA_DIR = os.getenv("DATA_DIR", "/data")


class DataItem(BaseModel):
    key: str
    value: Any
    collection: str = "default"
    metadata: Optional[Dict[str, Any]] = None


class DataResponse(BaseModel):
    success: bool
    key: str
    collection: str
    message: str


@app.get("/")
async def root():
    return {
        "service": "Mental Models Storage Service",
        "version": "1.0.0",
        "collections": list(storage.keys()),
        "total_items": sum(len(v) for v in storage.values())
    }


@app.get("/health")
async def health_check():
    return {"status": "healthy", "service": "storage"}


@app.post("/data", response_model=DataResponse)
async def store_data(item: DataItem):
    """Store data item"""
    if item.collection not in storage:
        storage[item.collection] = {}
    
    storage[item.collection][item.key] = {
        "value": item.value,
        "metadata": item.metadata or {},
        "created_at": datetime.utcnow().isoformat(),
        "updated_at": datetime.utcnow().isoformat()
    }
    
    logger.info(f"Stored item {item.key} in collection {item.collection}")
    
    return DataResponse(
        success=True,
        key=item.key,
        collection=item.collection,
        message=f"Data stored successfully"
    )


@app.get("/data")
async def get_all_data():
    """Get all stored data"""
    return {
        "collections": {
            name: {
                "items": list(items.keys()),
                "count": len(items)
            }
            for name, items in storage.items()
        },
        "total_items": sum(len(v) for v in storage.values())
    }


@app.get("/data/{collection}")
async def get_collection(collection: str):
    """Get all items in a collection"""
    if collection not in storage:
        raise HTTPException(status_code=404, detail="Collection not found")
    
    return {
        "collection": collection,
        "items": storage[collection],
        "count": len(storage[collection])
    }


@app.get("/data/{collection}/{key}")
async def get_item(collection: str, key: str):
    """Get a specific item"""
    if collection not in storage:
        raise HTTPException(status_code=404, detail="Collection not found")
    
    if key not in storage[collection]:
        raise HTTPException(status_code=404, detail="Item not found")
    
    return {
        "key": key,
        "collection": collection,
        **storage[collection][key]
    }


@app.put("/data/{collection}/{key}")
async def update_item(collection: str, key: str, item: DataItem):
    """Update a specific item"""
    if collection not in storage:
        storage[collection] = {}
    
    existing = storage[collection].get(key, {})
    
    storage[collection][key] = {
        "value": item.value,
        "metadata": item.metadata or existing.get("metadata", {}),
        "created_at": existing.get("created_at", datetime.utcnow().isoformat()),
        "updated_at": datetime.utcnow().isoformat()
    }
    
    return DataResponse(
        success=True,
        key=key,
        collection=collection,
        message="Data updated successfully"
    )


@app.delete("/data/{collection}/{key}")
async def delete_item(collection: str, key: str):
    """Delete a specific item"""
    if collection not in storage:
        raise HTTPException(status_code=404, detail="Collection not found")
    
    if key not in storage[collection]:
        raise HTTPException(status_code=404, detail="Item not found")
    
    del storage[collection][key]
    
    return DataResponse(
        success=True,
        key=key,
        collection=collection,
        message="Data deleted successfully"
    )


@app.delete("/data/{collection}")
async def delete_collection(collection: str):
    """Delete an entire collection"""
    if collection not in storage:
        raise HTTPException(status_code=404, detail="Collection not found")
    
    count = len(storage[collection])
    del storage[collection]
    
    return {
        "success": True,
        "collection": collection,
        "items_deleted": count,
        "message": f"Collection {collection} deleted"
    }


# Analysis-specific endpoints
@app.post("/analyses")
async def store_analysis(analysis: Dict[str, Any]):
    """Store an analysis result"""
    analysis_id = hashlib.md5(
        f"{analysis.get('text', '')[:50]}{datetime.utcnow().isoformat()}".encode()
    ).hexdigest()[:12]
    
    storage["analyses"][analysis_id] = {
        "value": analysis,
        "metadata": {"type": "analysis"},
        "created_at": datetime.utcnow().isoformat(),
        "updated_at": datetime.utcnow().isoformat()
    }
    
    return {
        "success": True,
        "analysis_id": analysis_id,
        "message": "Analysis stored successfully"
    }


@app.get("/analyses")
async def get_analyses():
    """Get all stored analyses"""
    return {
        "analyses": [
            {"id": k, **v}
            for k, v in storage["analyses"].items()
        ],
        "count": len(storage["analyses"])
    }


@app.get("/analyses/{analysis_id}")
async def get_analysis(analysis_id: str):
    """Get a specific analysis"""
    if analysis_id not in storage["analyses"]:
        raise HTTPException(status_code=404, detail="Analysis not found")
    
    return {
        "id": analysis_id,
        **storage["analyses"][analysis_id]
    }


# Settings endpoints
@app.get("/settings")
async def get_settings():
    """Get all settings"""
    return storage.get("settings", {})


@app.post("/settings")
async def update_settings(settings: Dict[str, Any]):
    """Update settings"""
    for key, value in settings.items():
        storage["settings"][key] = {
            "value": value,
            "updated_at": datetime.utcnow().isoformat()
        }
    
    return {
        "success": True,
        "message": "Settings updated",
        "updated_keys": list(settings.keys())
    }


@app.get("/settings/{key}")
async def get_setting(key: str):
    """Get a specific setting"""
    if key not in storage["settings"]:
        raise HTTPException(status_code=404, detail="Setting not found")
    
    return {
        "key": key,
        **storage["settings"][key]
    }


# Stats endpoint
@app.get("/stats")
async def get_stats():
    """Get storage statistics"""
    return {
        "collections": len(storage),
        "total_items": sum(len(v) for v in storage.values()),
        "analyses_count": len(storage.get("analyses", {})),
        "settings_count": len(storage.get("settings", {})),
        "decisions_count": len(storage.get("decisions", {})),
        "storage_healthy": True
    }


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8003)
