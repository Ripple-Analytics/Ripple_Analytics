#!/usr/bin/env python3
"""
FastAPI Application
REST API for the Mental Models System.
"""

import os
import sys
from typing import List, Optional
from datetime import date

from fastapi import FastAPI, HTTPException, Query
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
import psycopg2
from psycopg2.extras import RealDictCursor

sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
from config.settings import settings


app = FastAPI(
    title="Mental Models System API",
    description="The Oligarch's Operating System - Query mental models, principles, and case studies",
    version="1.0.0"
)

app.add_middleware(
    CORSMiddleware,
    allow_origins=settings.api.cors_origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


def get_db_connection():
    """Get database connection."""
    return psycopg2.connect(
        dbname=settings.database.name,
        user=settings.database.user,
        host=settings.database.host,
        port=settings.database.port,
        password=settings.database.password or None,
        cursor_factory=RealDictCursor
    )


class Framework(BaseModel):
    id: int
    name: str
    description: Optional[str]
    source: Optional[str]


class MentalModel(BaseModel):
    id: int
    name: str
    category: Optional[str]
    description: Optional[str]
    originator: Optional[str]
    modern_synthesizer: Optional[str]
    lindy_age_years: Optional[int]


class Principle(BaseModel):
    id: int
    thinker: str
    principle_name: str
    principle_description: Optional[str]
    category: Optional[str]
    source: Optional[str]


class CaseStudySummary(BaseModel):
    id: int
    name: str
    date: Optional[date]
    category: Optional[str]
    region: Optional[str]
    severity: Optional[float]
    financial_impact: Optional[float]
    models_involved: Optional[int]
    lollapalooza_score: Optional[float]


class SystemStats(BaseModel):
    total_models: int
    total_principles: int
    total_cases: int
    total_frameworks: int
    model_categories: int


@app.get("/")
async def root():
    """API root endpoint."""
    return {
        "name": "Mental Models System API",
        "version": "1.0.0",
        "description": "The Oligarch's Operating System"
    }


@app.get("/stats", response_model=SystemStats)
async def get_stats():
    """Get system statistics."""
    conn = get_db_connection()
    cur = conn.cursor()
    
    try:
        cur.execute("SELECT COUNT(*) as count FROM mental_models")
        total_models = cur.fetchone()['count']
        
        cur.execute("SELECT COUNT(*) as count FROM thinker_principles")
        total_principles = cur.fetchone()['count']
        
        cur.execute("SELECT COUNT(*) as count FROM case_studies")
        total_cases = cur.fetchone()['count']
        
        cur.execute("SELECT COUNT(*) as count FROM frameworks")
        total_frameworks = cur.fetchone()['count']
        
        cur.execute("SELECT COUNT(DISTINCT category) as count FROM mental_models")
        model_categories = cur.fetchone()['count']
        
        return SystemStats(
            total_models=total_models,
            total_principles=total_principles,
            total_cases=total_cases,
            total_frameworks=total_frameworks,
            model_categories=model_categories
        )
    finally:
        cur.close()
        conn.close()


@app.get("/frameworks", response_model=List[Framework])
async def get_frameworks():
    """Get all frameworks (thinkers)."""
    conn = get_db_connection()
    cur = conn.cursor()
    
    try:
        cur.execute("SELECT id, name, description, source FROM frameworks ORDER BY name")
        return [Framework(**row) for row in cur.fetchall()]
    finally:
        cur.close()
        conn.close()


@app.get("/models", response_model=List[MentalModel])
async def get_mental_models(
    category: Optional[str] = Query(None, description="Filter by category"),
    search: Optional[str] = Query(None, description="Search in name or description"),
    limit: int = Query(100, ge=1, le=1000)
):
    """Get mental models with optional filtering."""
    conn = get_db_connection()
    cur = conn.cursor()
    
    try:
        query = """
            SELECT id, name, category, description, originator, modern_synthesizer, lindy_age_years
            FROM mental_models
            WHERE 1=1
        """
        params = []
        
        if category:
            query += " AND category = %s"
            params.append(category)
        
        if search:
            query += " AND (name ILIKE %s OR description ILIKE %s)"
            params.extend([f"%{search}%", f"%{search}%"])
        
        query += " ORDER BY category, name LIMIT %s"
        params.append(limit)
        
        cur.execute(query, params)
        return [MentalModel(**row) for row in cur.fetchall()]
    finally:
        cur.close()
        conn.close()


@app.get("/models/categories")
async def get_model_categories():
    """Get all model categories."""
    conn = get_db_connection()
    cur = conn.cursor()
    
    try:
        cur.execute("""
            SELECT category, COUNT(*) as count 
            FROM mental_models 
            GROUP BY category 
            ORDER BY count DESC
        """)
        return [{"category": row['category'], "count": row['count']} for row in cur.fetchall()]
    finally:
        cur.close()
        conn.close()


@app.get("/models/{model_id}", response_model=MentalModel)
async def get_mental_model(model_id: int):
    """Get a specific mental model by ID."""
    conn = get_db_connection()
    cur = conn.cursor()
    
    try:
        cur.execute("""
            SELECT id, name, category, description, originator, modern_synthesizer, lindy_age_years
            FROM mental_models WHERE id = %s
        """, (model_id,))
        row = cur.fetchone()
        if not row:
            raise HTTPException(status_code=404, detail="Mental model not found")
        return MentalModel(**row)
    finally:
        cur.close()
        conn.close()


@app.get("/principles", response_model=List[Principle])
async def get_principles(
    thinker: Optional[str] = Query(None, description="Filter by thinker"),
    category: Optional[str] = Query(None, description="Filter by category"),
    limit: int = Query(100, ge=1, le=1000)
):
    """Get thinker principles with optional filtering."""
    conn = get_db_connection()
    cur = conn.cursor()
    
    try:
        query = """
            SELECT id, thinker, principle_name, principle_description, category, source
            FROM thinker_principles
            WHERE 1=1
        """
        params = []
        
        if thinker:
            query += " AND thinker = %s"
            params.append(thinker)
        
        if category:
            query += " AND category = %s"
            params.append(category)
        
        query += " ORDER BY thinker, principle_name LIMIT %s"
        params.append(limit)
        
        cur.execute(query, params)
        return [Principle(**row) for row in cur.fetchall()]
    finally:
        cur.close()
        conn.close()


@app.get("/principles/thinkers")
async def get_thinkers():
    """Get all thinkers with principle counts."""
    conn = get_db_connection()
    cur = conn.cursor()
    
    try:
        cur.execute("""
            SELECT thinker, COUNT(*) as count 
            FROM thinker_principles 
            GROUP BY thinker 
            ORDER BY count DESC
        """)
        return [{"thinker": row['thinker'], "count": row['count']} for row in cur.fetchall()]
    finally:
        cur.close()
        conn.close()


@app.get("/cases", response_model=List[CaseStudySummary])
async def get_case_studies(
    category: Optional[str] = Query(None, description="Filter by category"),
    region: Optional[str] = Query(None, description="Filter by region"),
    min_severity: Optional[float] = Query(None, ge=0, le=1),
    min_models: Optional[int] = Query(None, ge=1),
    limit: int = Query(100, ge=1, le=1000)
):
    """Get case studies with optional filtering."""
    conn = get_db_connection()
    cur = conn.cursor()
    
    try:
        query = """
            SELECT id, name, date, category, region, severity, financial_impact, 
                   models_involved, lollapalooza_score
            FROM case_studies
            WHERE 1=1
        """
        params = []
        
        if category:
            query += " AND category = %s"
            params.append(category)
        
        if region:
            query += " AND region = %s"
            params.append(region)
        
        if min_severity:
            query += " AND severity >= %s"
            params.append(min_severity)
        
        if min_models:
            query += " AND models_involved >= %s"
            params.append(min_models)
        
        query += " ORDER BY severity DESC, date DESC LIMIT %s"
        params.append(limit)
        
        cur.execute(query, params)
        return [CaseStudySummary(**row) for row in cur.fetchall()]
    finally:
        cur.close()
        conn.close()


@app.get("/cases/categories")
async def get_case_categories():
    """Get all case study categories with counts."""
    conn = get_db_connection()
    cur = conn.cursor()
    
    try:
        cur.execute("""
            SELECT category, COUNT(*) as count, 
                   ROUND(AVG(severity)::numeric, 2) as avg_severity
            FROM case_studies 
            GROUP BY category 
            ORDER BY count DESC
        """)
        return [dict(row) for row in cur.fetchall()]
    finally:
        cur.close()
        conn.close()


@app.get("/cases/regions")
async def get_case_regions():
    """Get all regions with case counts."""
    conn = get_db_connection()
    cur = conn.cursor()
    
    try:
        cur.execute("""
            SELECT region, COUNT(*) as count,
                   ROUND(AVG(severity)::numeric, 2) as avg_severity
            FROM case_studies 
            GROUP BY region 
            ORDER BY count DESC
        """)
        return [dict(row) for row in cur.fetchall()]
    finally:
        cur.close()
        conn.close()


@app.get("/lollapalooza")
async def get_lollapalooza_cases(
    min_models: int = Query(5, ge=3, description="Minimum models involved"),
    limit: int = Query(50, ge=1, le=500)
):
    """Get cases with lollapalooza effects (multiple interacting models)."""
    conn = get_db_connection()
    cur = conn.cursor()
    
    try:
        cur.execute("""
            SELECT id, name, date, category, region, severity, 
                   financial_impact, models_involved, lollapalooza_score
            FROM case_studies
            WHERE models_involved >= %s
            ORDER BY lollapalooza_score DESC
            LIMIT %s
        """, (min_models, limit))
        return [dict(row) for row in cur.fetchall()]
    finally:
        cur.close()
        conn.close()


@app.get("/analysis/decade")
async def get_decade_analysis():
    """Get case analysis by decade."""
    conn = get_db_connection()
    cur = conn.cursor()
    
    try:
        cur.execute("""
            SELECT 
                (EXTRACT(YEAR FROM date)::int / 10 * 10) AS decade,
                COUNT(*) AS case_count,
                ROUND(AVG(severity)::numeric, 2) AS avg_severity,
                ROUND(SUM(financial_impact)::numeric, 0) AS total_impact
            FROM case_studies
            WHERE date IS NOT NULL
            GROUP BY (EXTRACT(YEAR FROM date)::int / 10 * 10)
            ORDER BY decade
        """)
        return [dict(row) for row in cur.fetchall()]
    finally:
        cur.close()
        conn.close()


@app.get("/analysis/model-frequency")
async def get_model_frequency(limit: int = Query(20, ge=1, le=100)):
    """Get model frequency analysis from Planck matrix."""
    conn = get_db_connection()
    cur = conn.cursor()
    
    try:
        cur.execute("""
            SELECT 
                model_name,
                COUNT(*) AS occurrence_count,
                ROUND(AVG(effect_size)::numeric, 3) AS avg_effect_size,
                ROUND(STDDEV(effect_size)::numeric, 3) AS std_effect_size
            FROM planck_matrix
            GROUP BY model_name
            ORDER BY COUNT(*) DESC
            LIMIT %s
        """, (limit,))
        return [dict(row) for row in cur.fetchall()]
    finally:
        cur.close()
        conn.close()


class AgentQuery(BaseModel):
    problem: str


class AgentSearchResult(BaseModel):
    name: str
    content: str
    similarity: float
    source_type: str
    metadata: dict


class AgentQueryResponse(BaseModel):
    answer: str
    models_used: List[str]
    principles_applied: List[str]


class MungerAnalysis(BaseModel):
    problem: str
    disciplines: dict
    key_models: List[str]
    warnings: List[str]


@app.get("/agent/search", response_model=List[AgentSearchResult])
async def agent_search(
    q: str = Query(..., description="Search query"),
    limit: int = Query(10, ge=1, le=50)
):
    """Semantic search over mental models and principles."""
    try:
        from src.llm.agent import MentalModelsAgent
        agent = MentalModelsAgent()
        results = agent.quick_search(q, limit=limit)
        agent.close()
        return [
            AgentSearchResult(
                name=r.name,
                content=r.content,
                similarity=r.similarity,
                source_type=r.source_type,
                metadata=r.metadata
            )
            for r in results
        ]
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Search failed: {str(e)}")


@app.get("/agent/query", response_model=AgentQueryResponse)
async def agent_query(
    q: str = Query(..., description="Natural language question")
):
    """Ask a natural language question about mental models."""
    try:
        from src.llm.agent import MentalModelsAgent
        agent = MentalModelsAgent()
        response = agent.query(q)
        agent.close()
        return AgentQueryResponse(
            answer=response.answer,
            models_used=response.models_used,
            principles_applied=response.principles_applied
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Query failed: {str(e)}")


@app.post("/agent/analyze", response_model=MungerAnalysis)
async def agent_analyze(query: AgentQuery):
    """Perform Munger-style multi-model analysis of a problem."""
    try:
        from src.llm.agent import MentalModelsAgent
        agent = MentalModelsAgent()
        analysis = agent.munger_analysis(query.problem)
        agent.close()
        return MungerAnalysis(
            problem=analysis["problem"],
            disciplines=analysis["disciplines"],
            key_models=analysis["key_models"],
            warnings=analysis["warnings"]
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Analysis failed: {str(e)}")


@app.get("/agent/models")
async def get_relevant_models(
    situation: str = Query(..., description="Describe your situation or problem")
):
    """Get mental models relevant to a specific situation."""
    try:
        from src.llm.agent import MentalModelsAgent
        agent = MentalModelsAgent()
        models = agent.get_relevant_models(situation)
        agent.close()
        return models
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to get models: {str(e)}")


class IngestRequest(BaseModel):
    paths: List[str]


class IngestResponse(BaseModel):
    documents_processed: int
    chunks_created: int
    status: str


class SearchRequest(BaseModel):
    query: str
    limit: int = 10
    min_similarity: float = 0.5


class SearchResult(BaseModel):
    id: str
    document_path: str
    content: str
    chunk_index: int
    similarity: float
    metadata: dict


class KnowledgeQueryRequest(BaseModel):
    query: str
    context: Optional[str] = None
    max_sources: int = 10


class KnowledgeQueryResponse(BaseModel):
    query: str
    answer: str
    sources: List[dict]
    confidence: float


class ImprovementRequest(BaseModel):
    type: str
    title: str
    description: str
    proposed_changes: dict
    source_insights: Optional[List[str]] = None


class ImprovementResponse(BaseModel):
    id: str
    type: str
    title: str
    status: str


@app.post("/ingest/files", response_model=IngestResponse)
async def ingest_files(request: IngestRequest):
    """Ingest TXT files from specified paths."""
    try:
        from src.ingestion import DataIngestionPipeline
        from pathlib import Path
        
        pipeline = DataIngestionPipeline()
        pipeline.setup_database()
        
        paths = [Path(p) for p in request.paths]
        chunks = pipeline.process_files(paths)
        pipeline.close()
        
        return IngestResponse(
            documents_processed=len(request.paths),
            chunks_created=len(chunks),
            status="success"
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Ingestion failed: {str(e)}")


@app.post("/ingest/watch")
async def start_watching(paths: List[str]):
    """Start watching folders for new TXT files."""
    try:
        from src.ingestion import DataIngestionPipeline
        
        pipeline = DataIngestionPipeline(watch_paths=paths)
        pipeline.setup_database()
        pipeline.start_watching()
        
        return {
            "status": "watching",
            "paths": paths,
            "message": "Folder watcher started. New files will be automatically processed."
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to start watcher: {str(e)}")


@app.get("/ingest/stats")
async def get_ingestion_stats():
    """Get data ingestion statistics."""
    try:
        from src.ingestion import DataIngestionPipeline
        
        pipeline = DataIngestionPipeline()
        stats = pipeline.get_stats()
        pipeline.close()
        
        return stats
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to get stats: {str(e)}")


@app.post("/ingest/search", response_model=List[SearchResult])
async def search_documents(request: SearchRequest):
    """Semantic search over ingested documents."""
    try:
        from src.ingestion import DataIngestionPipeline
        
        pipeline = DataIngestionPipeline()
        results = pipeline.semantic_search(
            query=request.query,
            limit=request.limit,
            min_similarity=request.min_similarity
        )
        pipeline.close()
        
        return [
            SearchResult(
                id=r["id"],
                document_path=r["document_path"],
                content=r["content"],
                chunk_index=r["chunk_index"],
                similarity=r["similarity"],
                metadata=r.get("metadata", {})
            )
            for r in results
        ]
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Search failed: {str(e)}")


@app.post("/feedback/query", response_model=KnowledgeQueryResponse)
async def query_knowledge(request: KnowledgeQueryRequest):
    """Query the knowledge base with natural language and get synthesized answers."""
    try:
        from src.ingestion import FeedbackLoop
        
        feedback = FeedbackLoop()
        feedback.setup_database()
        result = feedback.query_knowledge(
            query=request.query,
            context=request.context,
            max_sources=request.max_sources
        )
        feedback.close()
        
        return KnowledgeQueryResponse(
            query=result["query"],
            answer=result["answer"],
            sources=result["sources"],
            confidence=result["confidence"]
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Query failed: {str(e)}")


@app.post("/feedback/improve", response_model=ImprovementResponse)
async def propose_improvement(request: ImprovementRequest):
    """Propose an improvement based on insights from the knowledge base."""
    try:
        from src.ingestion import FeedbackLoop, ImprovementType
        
        feedback = FeedbackLoop()
        feedback.setup_database()
        
        improvement_type = ImprovementType(request.type)
        improvement = feedback.propose_improvement(
            improvement_type=improvement_type,
            title=request.title,
            description=request.description,
            proposed_changes=request.proposed_changes,
            source_insights=request.source_insights
        )
        feedback.close()
        
        return ImprovementResponse(
            id=improvement.id,
            type=improvement.type.value,
            title=improvement.title,
            status=improvement.status.value
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to propose improvement: {str(e)}")


@app.get("/feedback/improvements")
async def get_pending_improvements():
    """Get all pending improvements."""
    try:
        from src.ingestion import FeedbackLoop
        
        feedback = FeedbackLoop()
        improvements = feedback.get_pending_improvements()
        feedback.close()
        
        return [
            {
                "id": imp.id,
                "type": imp.type.value,
                "title": imp.title,
                "description": imp.description,
                "status": imp.status.value,
                "created_at": imp.created_at.isoformat() if imp.created_at else None
            }
            for imp in improvements
        ]
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to get improvements: {str(e)}")


@app.post("/feedback/improvements/{improvement_id}/approve")
async def approve_improvement(improvement_id: str):
    """Approve a proposed improvement."""
    try:
        from src.ingestion import FeedbackLoop
        
        feedback = FeedbackLoop()
        success = feedback.approve_improvement(improvement_id)
        feedback.close()
        
        if success:
            return {"status": "approved", "improvement_id": improvement_id}
        else:
            raise HTTPException(status_code=404, detail="Improvement not found or already processed")
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to approve: {str(e)}")


@app.post("/feedback/improvements/{improvement_id}/reject")
async def reject_improvement(improvement_id: str, reason: Optional[str] = None):
    """Reject a proposed improvement."""
    try:
        from src.ingestion import FeedbackLoop
        
        feedback = FeedbackLoop()
        success = feedback.reject_improvement(improvement_id, reason)
        feedback.close()
        
        if success:
            return {"status": "rejected", "improvement_id": improvement_id}
        else:
            raise HTTPException(status_code=404, detail="Improvement not found or already processed")
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to reject: {str(e)}")


@app.post("/feedback/improvements/{improvement_id}/implemented")
async def mark_improvement_implemented(improvement_id: str):
    """Mark an approved improvement as implemented."""
    try:
        from src.ingestion import FeedbackLoop
        
        feedback = FeedbackLoop()
        success = feedback.mark_implemented(improvement_id)
        feedback.close()
        
        if success:
            return {"status": "implemented", "improvement_id": improvement_id}
        else:
            raise HTTPException(status_code=404, detail="Improvement not found or not approved")
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to mark implemented: {str(e)}")


@app.get("/feedback/analyze")
async def analyze_for_improvements(topic: Optional[str] = None):
    """Analyze the knowledge base for potential improvements."""
    try:
        from src.ingestion import FeedbackLoop
        
        feedback = FeedbackLoop()
        feedback.setup_database()
        suggestions = feedback.analyze_for_improvements(topic)
        feedback.close()
        
        return {"suggestions": suggestions}
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Analysis failed: {str(e)}")


@app.get("/feedback/stats")
async def get_feedback_stats():
    """Get feedback loop statistics."""
    try:
        from src.ingestion import FeedbackLoop
        
        feedback = FeedbackLoop()
        stats = feedback.get_stats()
        feedback.close()
        
        return stats
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to get stats: {str(e)}")


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host=settings.api.host, port=settings.api.port)
