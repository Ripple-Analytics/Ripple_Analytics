"""
Mental Models API Server

FastAPI server for mental model document analysis.

Endpoints:
- POST /analyze - Analyze text through mental models
- POST /analyze/file - Analyze uploaded file
- GET /models - List all mental models
- GET /models/{model_id} - Get model details
- GET /search - Search knowledge graph
- GET /lollapalooza - Find Lollapalooza effects
- GET /stats - Get system statistics
"""

import os
import json
import asyncio
from typing import List, Optional, Dict, Any
from datetime import datetime
from pathlib import Path

from fastapi import FastAPI, HTTPException, UploadFile, File, Query, BackgroundTasks
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel, Field

# Import our modules
import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from src.analysis import (
    MentalModelAnalyzer,
    MentalModelLoader,
    KnowledgeGraph,
    build_graph_from_analyses
)
from src.llm.local_llm import create_llm_client
from src.api.failure_mode_routes import router as failure_mode_router
from src.api.scheduler_routes import router as scheduler_router, webhook_router


# =============================================================================
# PYDANTIC MODELS
# =============================================================================

class AnalyzeRequest(BaseModel):
    """Request to analyze text."""
    text: str = Field(..., description="Text to analyze")
    document_name: str = Field(default="Unknown", description="Name for the document")


class ModelMatchResponse(BaseModel):
    """A mental model match."""
    model_id: str
    model_name: str
    relevance_score: float
    confidence: float
    evidence: str
    insights: List[str]


class LollapaloozaResponse(BaseModel):
    """A Lollapalooza effect."""
    models: List[str]
    convergence_score: float
    description: str
    implications: List[str]


class AnalysisResponse(BaseModel):
    """Response from analysis."""
    document_name: str
    analyzed_at: str
    model_matches: List[ModelMatchResponse]
    lollapalooza_alerts: List[LollapaloozaResponse]
    primary_models: List[str]
    categories: List[str]
    tags: List[str]
    summary: str
    processing_time: float


class MentalModelResponse(BaseModel):
    """A mental model."""
    id: str
    name: str
    category: str
    description: str
    how_to_apply: str
    keywords: List[str]
    related_models: List[str]


class SearchResult(BaseModel):
    """A search result."""
    type: str
    name: str
    match_field: str
    properties: Dict[str, Any]


class StatsResponse(BaseModel):
    """System statistics."""
    total_models: int
    models_by_category: Dict[str, int]
    total_analyses: int
    total_documents_in_graph: int


# =============================================================================
# APPLICATION
# =============================================================================

def create_app(
    llm_backend: str = "ollama",
    llm_model: str = "llama3:70b",
    llm_url: str = None
) -> FastAPI:
    """Create the FastAPI application."""
    
    app = FastAPI(
        title="Mental Models Analysis API",
        description="Analyze documents through the lens of 129 mental models",
        version="1.0.0"
    )
    
    # CORS
    app.add_middleware(
        CORSMiddleware,
        allow_origins=["*"],
        allow_credentials=True,
        allow_methods=["*"],
        allow_headers=["*"],
    )
    
    # State
    app.state.model_loader = MentalModelLoader()
    app.state.llm_client = None
    app.state.analyzer = None
    app.state.knowledge_graph = KnowledgeGraph()
    app.state.analyses = []
    
    # Lazy LLM initialization
    def get_analyzer():
        if app.state.analyzer is None:
            app.state.llm_client = create_llm_client(
                backend=llm_backend,
                model=llm_model,
                base_url=llm_url
            )
            app.state.analyzer = MentalModelAnalyzer(app.state.llm_client)
        return app.state.analyzer
    
    # =========================================================================
    # ROUTES
    # =========================================================================
    
    @app.get("/")
    async def root():
        """API root."""
        return {
            "name": "Mental Models Analysis API",
            "version": "1.0.0",
            "endpoints": {
                "POST /analyze": "Analyze text through mental models",
                "POST /analyze/file": "Analyze uploaded file",
                "GET /models": "List all mental models",
                "GET /models/{model_id}": "Get model details",
                "GET /search": "Search knowledge graph",
                "GET /lollapalooza": "Find Lollapalooza effects",
                "GET /stats": "Get system statistics"
            }
        }
    
    @app.post("/analyze", response_model=AnalysisResponse)
    async def analyze_text(request: AnalyzeRequest):
        """Analyze text through mental models."""
        analyzer = get_analyzer()
        
        try:
            analysis = await analyzer.analyze_text(request.text, request.document_name)
            
            # Add to knowledge graph
            app.state.knowledge_graph.add_analysis(analysis)
            app.state.analyses.append(analysis)
            
            return AnalysisResponse(
                document_name=analysis.document_name,
                analyzed_at=analysis.analyzed_at.isoformat(),
                model_matches=[
                    ModelMatchResponse(
                        model_id=m.model_id,
                        model_name=m.model_name,
                        relevance_score=m.relevance_score,
                        confidence=m.confidence,
                        evidence=m.evidence,
                        insights=m.insights
                    )
                    for m in analysis.model_matches
                ],
                lollapalooza_alerts=[
                    LollapaloozaResponse(
                        models=l.models,
                        convergence_score=l.convergence_score,
                        description=l.description,
                        implications=l.implications
                    )
                    for l in analysis.lollapalooza_alerts
                ],
                primary_models=analysis.primary_models,
                categories=analysis.categories,
                tags=analysis.tags,
                summary=analysis.summary,
                processing_time=analysis.processing_time
            )
        except Exception as e:
            raise HTTPException(status_code=500, detail=str(e))
    
    @app.post("/analyze/file", response_model=AnalysisResponse)
    async def analyze_file(file: UploadFile = File(...)):
        """Analyze an uploaded file."""
        from src.pipeline.terabyte_processor import TextExtractor, SemanticChunker
        
        # Save uploaded file temporarily
        temp_path = f"/tmp/{file.filename}"
        with open(temp_path, "wb") as f:
            content = await file.read()
            f.write(content)
        
        try:
            # Extract text
            text, metadata = TextExtractor.extract(temp_path)
            if not text:
                raise HTTPException(status_code=400, detail="Could not extract text from file")
            
            analyzer = get_analyzer()
            
            # Chunk if needed
            if len(text) > 3000:
                chunker = SemanticChunker(chunk_size=2000, overlap=200)
                chunks = chunker.chunk(text)
                analysis = await analyzer.analyze_chunks(chunks, file.filename)
            else:
                analysis = await analyzer.analyze_text(text, file.filename)
            
            # Add to knowledge graph
            app.state.knowledge_graph.add_analysis(analysis)
            app.state.analyses.append(analysis)
            
            return AnalysisResponse(
                document_name=analysis.document_name,
                analyzed_at=analysis.analyzed_at.isoformat(),
                model_matches=[
                    ModelMatchResponse(
                        model_id=m.model_id,
                        model_name=m.model_name,
                        relevance_score=m.relevance_score,
                        confidence=m.confidence,
                        evidence=m.evidence,
                        insights=m.insights
                    )
                    for m in analysis.model_matches
                ],
                lollapalooza_alerts=[
                    LollapaloozaResponse(
                        models=l.models,
                        convergence_score=l.convergence_score,
                        description=l.description,
                        implications=l.implications
                    )
                    for l in analysis.lollapalooza_alerts
                ],
                primary_models=analysis.primary_models,
                categories=analysis.categories,
                tags=analysis.tags,
                summary=analysis.summary,
                processing_time=analysis.processing_time
            )
        finally:
            # Clean up
            if os.path.exists(temp_path):
                os.remove(temp_path)
    
    @app.get("/models", response_model=List[MentalModelResponse])
    async def list_models(category: Optional[str] = None):
        """List all mental models."""
        if category:
            models = app.state.model_loader.get_models_by_category(category)
        else:
            models = app.state.model_loader.get_all_models()
        
        return [
            MentalModelResponse(
                id=m.id,
                name=m.name,
                category=m.category,
                description=m.description,
                how_to_apply=m.how_to_apply,
                keywords=m.keywords,
                related_models=m.related_models
            )
            for m in models
        ]
    
    @app.get("/models/{model_id}", response_model=MentalModelResponse)
    async def get_model(model_id: str):
        """Get a specific mental model."""
        model = app.state.model_loader.get_model(model_id)
        if not model:
            raise HTTPException(status_code=404, detail="Model not found")
        
        return MentalModelResponse(
            id=model.id,
            name=model.name,
            category=model.category,
            description=model.description,
            how_to_apply=model.how_to_apply,
            keywords=model.keywords,
            related_models=model.related_models
        )
    
    @app.get("/models/categories")
    async def list_categories():
        """List all model categories."""
        return {
            "categories": list(app.state.model_loader.models_by_category.keys()),
            "counts": {
                cat: len(models)
                for cat, models in app.state.model_loader.models_by_category.items()
            }
        }
    
    @app.get("/search", response_model=List[SearchResult])
    async def search(
        query: str = Query(..., description="Search query"),
        types: Optional[str] = Query(None, description="Node types (comma-separated)")
    ):
        """Search the knowledge graph."""
        type_list = types.split(',') if types else None
        results = app.state.knowledge_graph.search(query, types=type_list)
        
        return [
            SearchResult(
                type=r["type"],
                name=r["name"],
                match_field=r.get("match_field", "name"),
                properties=r.get("properties", {})
            )
            for r in results[:50]
        ]
    
    @app.get("/documents/by-model")
    async def documents_by_model(model_name: str = Query(..., description="Mental model name")):
        """Find documents by mental model."""
        docs = app.state.knowledge_graph.find_documents_by_model(model_name)
        return {"model": model_name, "documents": docs}
    
    @app.get("/documents/similar")
    async def similar_documents(
        document_name: str = Query(..., description="Document name"),
        min_similarity: float = Query(0.3, description="Minimum similarity score")
    ):
        """Find similar documents."""
        docs = app.state.knowledge_graph.find_similar_documents(document_name, min_similarity)
        return {"document": document_name, "similar": docs}
    
    @app.get("/lollapalooza")
    async def find_lollapalooza(min_score: float = Query(0.7, description="Minimum convergence score")):
        """Find Lollapalooza effects."""
        alerts = app.state.knowledge_graph.find_lollapalooza_documents(min_score)
        return {"min_score": min_score, "alerts": alerts}
    
    @app.get("/stats", response_model=StatsResponse)
    async def get_stats():
        """Get system statistics."""
        return StatsResponse(
            total_models=len(app.state.model_loader.models),
            models_by_category={
                cat: len(models)
                for cat, models in app.state.model_loader.models_by_category.items()
            },
            total_analyses=len(app.state.analyses),
            total_documents_in_graph=len(app.state.knowledge_graph._document_nodes)
        )
    
    @app.get("/graph/export")
    async def export_graph(format: str = Query("json", description="Export format")):
        """Export the knowledge graph."""
        if format == "json":
            return app.state.knowledge_graph.get_summary()
        else:
            raise HTTPException(status_code=400, detail=f"Unknown format: {format}")
    
    @app.get("/health")
    async def health():
        """Health check."""
        return {
            "status": "healthy",
            "timestamp": datetime.now().isoformat(),
            "models_loaded": len(app.state.model_loader.models),
            "llm_initialized": app.state.llm_client is not None
        }
    
    # Include failure mode routes
    app.include_router(failure_mode_router)
    
    # Include scheduler and webhook routes
    app.include_router(scheduler_router)
    app.include_router(webhook_router)
    
    return app


# =============================================================================
# MAIN
# =============================================================================

if __name__ == "__main__":
    import uvicorn
    
    app = create_app()
    uvicorn.run(app, host="0.0.0.0", port=8000)
