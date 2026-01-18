#!/usr/bin/env python3
"""
FastAPI Application
REST API for the Mental Models System.
"""

import os
import sys
from typing import List, Optional
from datetime import date

from pathlib import Path
from fastapi import FastAPI, HTTPException, Query
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from fastapi.responses import HTMLResponse, FileResponse
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


class AnalyzeContentRequest(BaseModel):
    content: str
    document_id: Optional[str] = ""
    document_path: Optional[str] = ""


class ModelMatchResponse(BaseModel):
    model_name: str
    relevance_score: float
    explanation: str
    evidence: List[str]
    category: str


class BiasDetectionResponse(BaseModel):
    bias_name: str
    confidence: float
    evidence: str
    mitigation: str


class DocumentAnalysisResponse(BaseModel):
    document_id: str
    document_path: str
    content_preview: str
    applicable_models: List[ModelMatchResponse]
    detected_biases: List[BiasDetectionResponse]
    patterns: List[str]
    lollapalooza_score: float
    lollapalooza_models: List[str]
    key_insights: List[str]
    inverted_perspective: str


class ClassifyRequest(BaseModel):
    content: str
    target_models: Optional[List[str]] = None


class BatchAnalyzeRequest(BaseModel):
    documents: List[dict]
    analysis_type: str = "classify"


@app.post("/analyze/document", response_model=DocumentAnalysisResponse)
async def analyze_document_with_models(request: AnalyzeContentRequest):
    """Analyze a document using mental models via local LLM (LM Studio)."""
    try:
        from src.analysis.mental_model_analyzer import MentalModelAnalyzer
        
        analyzer = MentalModelAnalyzer()
        analysis = analyzer.analyze_document(
            content=request.content,
            document_id=request.document_id or "",
            document_path=request.document_path or "",
        )
        analyzer.close()
        
        return DocumentAnalysisResponse(
            document_id=analysis.document_id,
            document_path=analysis.document_path,
            content_preview=analysis.content_preview,
            applicable_models=[
                ModelMatchResponse(
                    model_name=m.model_name,
                    relevance_score=m.relevance_score,
                    explanation=m.explanation,
                    evidence=m.evidence,
                    category=m.category,
                )
                for m in analysis.applicable_models
            ],
            detected_biases=[
                BiasDetectionResponse(
                    bias_name=b.bias_name,
                    confidence=b.confidence,
                    evidence=b.evidence,
                    mitigation=b.mitigation,
                )
                for b in analysis.detected_biases
            ],
            patterns=analysis.patterns,
            lollapalooza_score=analysis.lollapalooza_score,
            lollapalooza_models=analysis.lollapalooza_models,
            key_insights=analysis.key_insights,
            inverted_perspective=analysis.inverted_perspective,
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Analysis failed: {str(e)}")


@app.post("/analyze/classify", response_model=List[ModelMatchResponse])
async def classify_content(request: ClassifyRequest):
    """Classify content by applicable mental models."""
    try:
        from src.analysis.mental_model_analyzer import MentalModelAnalyzer
        
        analyzer = MentalModelAnalyzer()
        matches = analyzer.classify_by_model(
            content=request.content,
            target_models=request.target_models,
        )
        analyzer.close()
        
        return [
            ModelMatchResponse(
                model_name=m.model_name,
                relevance_score=m.relevance_score,
                explanation=m.explanation,
                evidence=m.evidence,
                category=m.category,
            )
            for m in matches
        ]
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Classification failed: {str(e)}")


@app.post("/analyze/biases", response_model=List[BiasDetectionResponse])
async def detect_biases_in_content(request: AnalyzeContentRequest):
    """Detect cognitive biases in content using Munger's framework."""
    try:
        from src.analysis.mental_model_analyzer import MentalModelAnalyzer
        
        analyzer = MentalModelAnalyzer()
        biases = analyzer.detect_biases(request.content)
        analyzer.close()
        
        return [
            BiasDetectionResponse(
                bias_name=b.bias_name,
                confidence=b.confidence,
                evidence=b.evidence,
                mitigation=b.mitigation,
            )
            for b in biases
        ]
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Bias detection failed: {str(e)}")


@app.post("/analyze/lollapalooza")
async def find_lollapalooza_effects(request: AnalyzeContentRequest):
    """Find lollapalooza effects (multiple interacting mental models)."""
    try:
        from src.analysis.mental_model_analyzer import MentalModelAnalyzer
        
        analyzer = MentalModelAnalyzer()
        result = analyzer.find_lollapalooza(request.content)
        analyzer.close()
        
        return result
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Lollapalooza analysis failed: {str(e)}")


@app.post("/analyze/invert")
async def invert_analysis(request: AnalyzeContentRequest, question: Optional[str] = None):
    """Apply Munger's inversion technique to analyze what could go wrong."""
    try:
        from src.analysis.mental_model_analyzer import MentalModelAnalyzer
        
        analyzer = MentalModelAnalyzer()
        result = analyzer.invert_analysis(request.content, question or "")
        analyzer.close()
        
        return {"inverted_analysis": result}
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Inversion analysis failed: {str(e)}")


@app.post("/analyze/batch")
async def batch_analyze_documents(request: BatchAnalyzeRequest):
    """Batch analyze multiple documents with mental models."""
    try:
        from src.analysis.mental_model_analyzer import MentalModelAnalyzer, AnalysisType
        
        analyzer = MentalModelAnalyzer()
        
        analysis_type_map = {
            "classify": AnalysisType.CLASSIFY,
            "detect_bias": AnalysisType.DETECT_BIAS,
            "find_patterns": AnalysisType.FIND_PATTERNS,
            "lollapalooza": AnalysisType.LOLLAPALOOZA,
            "full": AnalysisType.FULL,
        }
        analysis_type = analysis_type_map.get(request.analysis_type, AnalysisType.CLASSIFY)
        
        results = analyzer.batch_analyze(request.documents, analysis_type)
        analyzer.close()
        
        return {
            "analyzed_count": len(results),
            "results": [
                {
                    "document_id": r.document_id,
                    "document_path": r.document_path,
                    "applicable_models": [
                        {"model_name": m.model_name, "relevance_score": m.relevance_score}
                        for m in r.applicable_models
                    ],
                    "detected_biases": [
                        {"bias_name": b.bias_name, "confidence": b.confidence}
                        for b in r.detected_biases
                    ],
                    "lollapalooza_score": r.lollapalooza_score,
                    "key_insights": r.key_insights,
                }
                for r in results
            ],
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Batch analysis failed: {str(e)}")


@app.get("/analyze/query-by-model")
async def query_documents_by_model(
    model_name: str = Query(..., description="Mental model name to search for"),
    min_relevance: float = Query(0.5, ge=0, le=1),
    limit: int = Query(50, ge=1, le=500),
):
    """Query analyzed documents by mental model."""
    try:
        from src.analysis.mental_model_analyzer import MentalModelAnalyzer
        
        analyzer = MentalModelAnalyzer()
        results = analyzer.query_by_model(model_name, min_relevance, limit)
        analyzer.close()
        
        return {"model_name": model_name, "results": results}
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Query failed: {str(e)}")


@app.get("/analyze/stats")
async def get_analysis_stats():
    """Get mental model analysis statistics."""
    try:
        from src.analysis.mental_model_analyzer import MentalModelAnalyzer
        
        analyzer = MentalModelAnalyzer()
        stats = analyzer.get_analysis_stats()
        analyzer.close()
        
        return stats
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to get stats: {str(e)}")


@app.post("/analyze/ingest-and-analyze")
async def ingest_and_analyze_files(paths: List[str], analysis_type: str = "classify"):
    """Ingest files and automatically analyze them with mental models."""
    try:
        from src.ingestion import DataIngestionPipeline
        from src.analysis.mental_model_analyzer import MentalModelAnalyzer, AnalysisType
        from pathlib import Path
        
        pipeline = DataIngestionPipeline()
        pipeline.setup_database()
        
        file_paths = [Path(p) for p in paths]
        chunks = pipeline.process_files(file_paths)
        pipeline.close()
        
        analyzer = MentalModelAnalyzer()
        
        analysis_type_map = {
            "classify": AnalysisType.CLASSIFY,
            "detect_bias": AnalysisType.DETECT_BIAS,
            "full": AnalysisType.FULL,
        }
        at = analysis_type_map.get(analysis_type, AnalysisType.CLASSIFY)
        
        documents = [
            {
                "id": chunk.document_id,
                "path": chunk.document_path,
                "chunk_id": chunk.id,
                "content": chunk.content,
            }
            for chunk in chunks
        ]
        
        analyses = analyzer.batch_analyze(documents, at)
        
        for analysis in analyses:
            analyzer.store_analysis(analysis)
        
        analyzer.close()
        
        return {
            "files_processed": len(paths),
            "chunks_created": len(chunks),
            "analyses_stored": len(analyses),
            "status": "success",
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Ingest and analyze failed: {str(e)}")


class FailureModeCheckRequest(BaseModel):
    content: str


class FailureModeResult(BaseModel):
    name: str
    severity: str
    description: str
    signals: List[str]
    prevention: List[str]


@app.post("/failure-modes/check")
async def check_failure_modes(request: FailureModeCheckRequest):
    """Check content for potential failure modes."""
    try:
        from src.failure_modes.registry import FailureModeRegistry
        from src.failure_modes.detector import FailureModeDetector
        
        registry = FailureModeRegistry()
        detector = FailureModeDetector()
        
        context = {"content": request.content, "source": "web"}
        detected_results = detector.detect_failures(context)
        
        detected_failures = []
        for result in detected_results[:10]:
            fm = result.failure_mode
            detected_failures.append({
                "name": fm.name,
                "severity": fm.severity.value if hasattr(fm.severity, 'value') else str(fm.severity),
                "description": fm.description if hasattr(fm, 'description') else "",
                "signals": result.signals_found[:3] if result.signals_found else [],
                "prevention": result.recommendations[:2] if result.recommendations else [],
            })
        
        stats = registry.get_stats()
        
        return {
            "detected_failures": detected_failures,
            "total_checked": stats.get("total_failure_modes", 645),
            "models_checked": stats.get("total_models_with_failure_modes", 129),
        }
    except Exception as e:
        return {
            "detected_failures": [],
            "error": str(e),
            "message": "Failure mode detection not fully configured. Consider common failure modes: confirmation bias, overconfidence, incomplete analysis.",
        }


@app.get("/failure-modes/stats")
async def get_failure_mode_stats():
    """Get failure mode statistics."""
    try:
        from src.failure_modes.registry import FailureModeRegistry
        
        registry = FailureModeRegistry()
        stats = registry.get_stats()
        
        return stats
    except Exception as e:
        return {
            "total_failure_modes": 645,
            "total_models_with_failure_modes": 129,
            "categories": ["data_bias", "reasoning_error", "incomplete_analysis", "overconfidence", "context_blindness"],
            "error": str(e),
        }


class HuggingfaceEmbeddingsRequest(BaseModel):
    texts: List[str]
    model: Optional[str] = None


class HuggingfaceClassifyRequest(BaseModel):
    text: str
    labels: List[str]
    multi_label: bool = False


class HuggingfaceSummarizeRequest(BaseModel):
    text: str
    max_length: int = 150
    min_length: int = 30


@app.post("/huggingface/embeddings")
async def get_huggingface_embeddings(request: HuggingfaceEmbeddingsRequest):
    """Generate embeddings using Huggingface models."""
    try:
        from src.connectors.huggingface_connector import create_huggingface_connector
        
        connector = create_huggingface_connector()
        await connector.connect()
        
        result = await connector.get_embeddings(request.texts, request.model)
        
        await connector.disconnect()
        return result
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.post("/huggingface/classify")
async def classify_with_huggingface(request: HuggingfaceClassifyRequest):
    """Classify text using Huggingface zero-shot classification."""
    try:
        from src.connectors.huggingface_connector import create_huggingface_connector
        
        connector = create_huggingface_connector()
        await connector.connect()
        
        result = await connector.classify_text(
            request.text, 
            request.labels, 
            multi_label=request.multi_label
        )
        
        await connector.disconnect()
        return result
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.post("/huggingface/summarize")
async def summarize_with_huggingface(request: HuggingfaceSummarizeRequest):
    """Summarize text using Huggingface models."""
    try:
        from src.connectors.huggingface_connector import create_huggingface_connector
        
        connector = create_huggingface_connector()
        await connector.connect()
        
        result = await connector.summarize_text(
            request.text,
            max_length=request.max_length,
            min_length=request.min_length
        )
        
        await connector.disconnect()
        return result
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.post("/huggingface/classify-mental-models")
async def classify_by_mental_models_hf(request: AnalyzeContentRequest):
    """Classify text by mental model categories using Huggingface."""
    try:
        from src.connectors.huggingface_connector import create_huggingface_connector
        
        connector = create_huggingface_connector()
        await connector.connect()
        
        result = await connector.classify_by_mental_models(request.content)
        
        await connector.disconnect()
        return result
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.post("/huggingface/detect-biases")
async def detect_biases_hf(request: AnalyzeContentRequest):
    """Detect cognitive biases in text using Huggingface."""
    try:
        from src.connectors.huggingface_connector import create_huggingface_connector
        
        connector = create_huggingface_connector()
        await connector.connect()
        
        result = await connector.detect_cognitive_biases(request.content)
        
        await connector.disconnect()
        return result
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.get("/huggingface/search-models")
async def search_huggingface_models(
    query: Optional[str] = None,
    task: Optional[str] = None,
    limit: int = 10
):
    """Search for models on Huggingface Hub."""
    try:
        from src.connectors.huggingface_connector import create_huggingface_connector
        
        connector = create_huggingface_connector()
        await connector.connect()
        
        result = await connector.search_models(query=query, task=task, limit=limit)
        
        await connector.disconnect()
        return result
    except Exception as e:
        return {"success": False, "error": str(e)}


class StatisticalAnalysisRequest(BaseModel):
    variables: dict
    dependent_variable: Optional[str] = None


class CorrelationRequest(BaseModel):
    variables: dict
    method: str = "pearson"


class RegressionRequest(BaseModel):
    dependent: str
    independents: List[str]
    data: dict


class CovariateRequest(BaseModel):
    target: str
    covariates: List[str]
    data: dict


@app.post("/statistics/synthesize")
async def synthesize_variables(request: StatisticalAnalysisRequest):
    """Synthesize multiple variables with full statistical analysis."""
    try:
        from src.analysis.statistical_engine import create_statistical_engine
        
        engine = create_statistical_engine()
        engine.add_variables_from_dict(request.variables)
        
        result = engine.synthesize_variables(
            var_names=list(request.variables.keys()),
            dependent=request.dependent_variable
        )
        
        return {
            "success": True,
            "synthesis": result.to_dict()
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.post("/statistics/correlations")
async def calculate_correlations(request: CorrelationRequest):
    """Calculate correlation matrix for multiple variables."""
    try:
        from src.analysis.statistical_engine import create_statistical_engine
        
        engine = create_statistical_engine()
        engine.add_variables_from_dict(request.variables)
        
        correlations = engine.correlation_matrix(
            var_names=list(request.variables.keys()),
            method=request.method
        )
        
        return {
            "success": True,
            "correlations": [c.to_dict() for c in correlations],
            "method": request.method
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.post("/statistics/regression")
async def run_regression(request: RegressionRequest):
    """Run multiple regression analysis."""
    try:
        from src.analysis.statistical_engine import create_statistical_engine
        
        engine = create_statistical_engine()
        engine.add_variables_from_dict(request.data)
        
        if len(request.independents) == 1:
            result = engine.simple_linear_regression(
                request.dependent, 
                request.independents[0]
            )
        else:
            result = engine.multiple_regression(
                request.dependent, 
                request.independents
            )
        
        return {
            "success": True,
            "regression": result.to_dict()
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.post("/statistics/covariates")
async def analyze_covariates(request: CovariateRequest):
    """Analyze covariate effects on target variable."""
    try:
        from src.analysis.statistical_engine import create_mental_model_statistics
        
        stats = create_mental_model_statistics()
        for var, values in request.data.items():
            stats.add_model_scores(var, values)
        
        result = stats.get_covariate_effects(request.target, request.covariates)
        
        return {
            "success": True,
            "covariate_analysis": result
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.post("/statistics/factor-analysis")
async def run_factor_analysis(request: StatisticalAnalysisRequest):
    """Run factor analysis on multiple variables."""
    try:
        from src.analysis.statistical_engine import create_statistical_engine
        
        engine = create_statistical_engine()
        engine.add_variables_from_dict(request.variables)
        
        result = engine.factor_analysis(var_names=list(request.variables.keys()))
        
        return {
            "success": True,
            "factor_analysis": result.to_dict()
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.post("/statistics/covariance-matrix")
async def calculate_covariance_matrix(request: StatisticalAnalysisRequest):
    """Calculate covariance matrix for multiple variables."""
    try:
        from src.analysis.statistical_engine import create_statistical_engine
        
        engine = create_statistical_engine()
        engine.add_variables_from_dict(request.variables)
        
        result = engine.covariance_matrix(var_names=list(request.variables.keys()))
        
        return {
            "success": True,
            "covariance_matrix": result.to_dict()
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.get("/statistics/descriptive/{variable_name}")
async def get_descriptive_statistics(variable_name: str, values: str):
    """Get descriptive statistics for a variable."""
    try:
        from src.analysis.statistical_engine import create_statistical_engine
        
        engine = create_statistical_engine()
        value_list = [float(v.strip()) for v in values.split(",")]
        engine.add_variable(variable_name, value_list)
        
        stats = engine.get_descriptive_stats(variable_name)
        
        return {
            "success": True,
            "variable": variable_name,
            "statistics": stats
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.post("/statistics/mental-model-correlations")
async def analyze_mental_model_correlations(request: StatisticalAnalysisRequest):
    """Analyze correlations between mental model scores across documents."""
    try:
        from src.analysis.statistical_engine import create_mental_model_statistics
        
        stats = create_mental_model_statistics()
        for model, scores in request.variables.items():
            stats.add_model_scores(model, scores)
        
        correlations = stats.analyze_model_correlations()
        clusters = stats.find_model_clusters()
        
        return {
            "success": True,
            "correlations": [c.to_dict() for c in correlations],
            "clusters": clusters
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.get("/metrics/code-quality")
async def get_code_quality_metrics(path: str = Query(None, description="Project path to analyze")):
    """Get comprehensive code quality metrics for the project."""
    try:
        from src.metrics import analyze_project, get_metric_summary
        
        project_path = path or str(Path(__file__).parent.parent.parent)
        report = analyze_project(project_path)
        
        return {
            "success": True,
            "summary": report.get("summary", {}),
            "project_metrics": report.get("project_metrics", {}),
            "metrics_by_category": report.get("metrics_by_category", {}),
            "total_metrics": len(report.get("all_metrics", [])),
            "total_issues": len(report.get("all_issues", []))
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.get("/metrics/code-quality/full")
async def get_full_code_quality_report(path: str = Query(None, description="Project path to analyze")):
    """Get full code quality report with all metrics and issues."""
    try:
        from src.metrics import analyze_project
        
        project_path = path or str(Path(__file__).parent.parent.parent)
        report = analyze_project(project_path)
        
        return {
            "success": True,
            **report
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.get("/metrics/code-quality/issues")
async def get_code_issues(
    path: str = Query(None, description="Project path to analyze"),
    severity: str = Query(None, description="Filter by severity: critical, high, medium, low, info"),
    category: str = Query(None, description="Filter by category")
):
    """Get code issues found during analysis."""
    try:
        from src.metrics import analyze_project
        
        project_path = path or str(Path(__file__).parent.parent.parent)
        report = analyze_project(project_path)
        
        issues = report.get("all_issues", [])
        
        if severity:
            issues = [i for i in issues if i.get("severity") == severity]
        if category:
            issues = [i for i in issues if i.get("category") == category]
        
        return {
            "success": True,
            "total_issues": len(issues),
            "issues": issues
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.get("/metrics/development")
async def get_development_metrics(path: str = Query(None, description="Repository path")):
    """Get git and development velocity metrics."""
    try:
        from src.metrics import collect_development_metrics
        
        repo_path = path or str(Path(__file__).parent.parent.parent.parent)
        metrics = collect_development_metrics(repo_path)
        
        return {
            "success": True,
            **metrics
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.get("/metrics/development/velocity")
async def get_velocity_metrics(
    path: str = Query(None, description="Repository path"),
    period: str = Query("weekly", description="Period: daily, weekly, monthly, quarterly, yearly")
):
    """Get development velocity metrics for a specific period."""
    try:
        from src.metrics import GitMetricsCollector, VelocityPeriod
        
        repo_path = path or str(Path(__file__).parent.parent.parent.parent)
        collector = GitMetricsCollector(repo_path)
        collector.collect_all()
        
        period_enum = VelocityPeriod(period)
        velocity = collector.get_velocity_metrics(period_enum)
        
        return {
            "success": True,
            "velocity": velocity
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.get("/metrics/development/hotspots")
async def get_code_hotspots(
    path: str = Query(None, description="Repository path"),
    limit: int = Query(20, ge=1, le=100)
):
    """Get code hotspots (files with high churn rate)."""
    try:
        from src.metrics import collect_development_metrics
        
        repo_path = path or str(Path(__file__).parent.parent.parent.parent)
        metrics = collect_development_metrics(repo_path)
        
        hotspots = metrics.get("git_metrics", {}).get("hotspots", [])[:limit]
        
        return {
            "success": True,
            "hotspots": hotspots
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.get("/metrics/development/contributors")
async def get_contributor_metrics(path: str = Query(None, description="Repository path")):
    """Get contributor metrics and statistics."""
    try:
        from src.metrics import collect_development_metrics
        
        repo_path = path or str(Path(__file__).parent.parent.parent.parent)
        metrics = collect_development_metrics(repo_path)
        
        return {
            "success": True,
            "top_contributors": metrics.get("git_metrics", {}).get("top_contributors", []),
            "all_authors": metrics.get("git_metrics", {}).get("all_authors", {})
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.get("/metrics/runtime")
async def get_runtime_metrics():
    """Get runtime metrics (counters, gauges, histograms)."""
    try:
        from src.metrics import get_metrics_report
        
        report = get_metrics_report()
        
        return {
            "success": True,
            **report
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.get("/metrics/runtime/prometheus")
async def get_prometheus_metrics():
    """Get metrics in Prometheus format."""
    try:
        from src.metrics import get_prometheus_metrics
        from fastapi.responses import PlainTextResponse
        
        metrics = get_prometheus_metrics()
        return PlainTextResponse(content=metrics, media_type="text/plain")
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.post("/metrics/runtime/start")
async def start_runtime_collection(interval: float = Query(5.0, ge=1.0, le=60.0)):
    """Start runtime metrics collection."""
    try:
        from src.metrics import start_metrics_collection
        
        collector = start_metrics_collection(interval)
        
        return {
            "success": True,
            "message": f"Runtime metrics collection started with {interval}s interval"
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.get("/metrics/summary")
async def get_metrics_summary(path: str = Query(None, description="Project/repo path")):
    """Get a summary of all metrics across all categories."""
    try:
        from src.metrics import (
            analyze_project, 
            collect_development_metrics, 
            get_metrics_report,
            list_all_metrics,
            get_total_metric_count
        )
        
        project_path = path or str(Path(__file__).parent.parent.parent)
        repo_path = path or str(Path(__file__).parent.parent.parent.parent)
        
        code_quality = analyze_project(project_path)
        dev_metrics = collect_development_metrics(repo_path)
        runtime = get_metrics_report()
        
        total_code_metrics = len(code_quality.get("all_metrics", []))
        total_dev_metrics = len(dev_metrics.get("git_metrics", {}).get("recent_commits", [])) + \
                          len(dev_metrics.get("git_metrics", {}).get("hotspots", [])) + \
                          len(dev_metrics.get("git_metrics", {}).get("top_contributors", []))
        total_runtime_metrics = len(runtime.get("counters", {})) + \
                               len(runtime.get("gauges", {})) + \
                               len(runtime.get("histograms", {}))
        
        return {
            "success": True,
            "total_metrics_tracked": total_code_metrics + total_dev_metrics + total_runtime_metrics,
            "available_metric_types": get_total_metric_count(),
            "code_quality": {
                "metrics_count": total_code_metrics,
                "issues_count": len(code_quality.get("all_issues", [])),
                "files_analyzed": code_quality.get("summary", {}).get("files_analyzed", 0),
                "total_lines": code_quality.get("project_metrics", {}).get("total_lines", 0)
            },
            "development": {
                "commits_analyzed": len(dev_metrics.get("git_metrics", {}).get("recent_commits", [])),
                "contributors": len(dev_metrics.get("git_metrics", {}).get("all_authors", {})),
                "iteration_speed": dev_metrics.get("iteration_speed", {})
            },
            "runtime": {
                "counters": len(runtime.get("counters", {})),
                "gauges": len(runtime.get("gauges", {})),
                "histograms": len(runtime.get("histograms", {})),
                "uptime_seconds": runtime.get("summary", {}).get("uptime_seconds", 0)
            }
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.get("/metrics/list")
async def list_available_metrics():
    """List all available metric types that can be tracked."""
    try:
        from src.metrics import list_all_metrics, METRIC_CATEGORIES
        
        return {
            "success": True,
            "categories": METRIC_CATEGORIES,
            "all_metrics": list_all_metrics(),
            "total_metric_types": len(list_all_metrics())
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


class LispAnalyzeRequest(BaseModel):
    situation: str
    context: Optional[dict] = None


class LispInvertRequest(BaseModel):
    problem: str


@app.get("/lisp/models")
async def get_lisp_models():
    """Get all mental models defined in the Lisp DSL."""
    try:
        from src.lisp import get_lisp_bridge, HY_AVAILABLE
        
        bridge = get_lisp_bridge()
        models = bridge.get_all_models()
        
        return {
            "success": True,
            "hy_available": HY_AVAILABLE,
            "models": models,
            "total_models": len(models)
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.get("/lisp/model-names")
async def get_lisp_model_names():
    """Get names of all Lisp-defined mental models."""
    try:
        from src.lisp import get_lisp_bridge
        
        bridge = get_lisp_bridge()
        names = bridge.get_model_names()
        
        return {
            "success": True,
            "model_names": names,
            "total": len(names)
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.post("/lisp/analyze")
async def lisp_analyze_context(request: LispAnalyzeRequest):
    """Analyze a situation using the Lisp mental models DSL.
    
    This uses Munger's latticework approach - applying multiple
    mental models to a single problem for comprehensive analysis.
    """
    try:
        from src.lisp import get_lisp_bridge
        
        bridge = get_lisp_bridge()
        context = request.context or {}
        context["situation"] = request.situation
        
        result = bridge.analyze_context(context)
        
        return {
            "success": True,
            "models_applied": result.models_applied,
            "individual_results": result.individual_results,
            "combined_confidence": result.combined_confidence,
            "lollapalooza_potential": result.lollapalooza_potential,
            "failure_analysis": result.failure_analysis,
            "two_track_analysis": result.two_track_analysis,
            "timestamp": result.timestamp
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.post("/lisp/invert")
async def lisp_invert_problem(request: LispInvertRequest):
    """Apply Munger's inversion technique to a problem.
    
    'Invert, always invert' - Carl Jacobi
    Think about what to avoid, not just what to do.
    """
    try:
        from src.lisp import get_lisp_bridge
        
        bridge = get_lisp_bridge()
        result = bridge.invert(request.problem)
        
        return {
            "success": True,
            **result
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.get("/lisp/two-track")
async def lisp_two_track_analysis(situation: str = Query(..., description="Situation to analyze")):
    """Perform Munger's two-track analysis.
    
    Track 1: Rational factors (economics, opportunity costs, etc.)
    Track 2: Psychological factors (biases, social proof, etc.)
    """
    try:
        from src.lisp import get_lisp_bridge
        
        bridge = get_lisp_bridge()
        result = bridge.two_track_analysis(situation)
        
        return {
            "success": True,
            **result
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.post("/lisp/lollapalooza")
async def lisp_detect_lollapalooza(request: LispAnalyzeRequest):
    """Detect lollapalooza effects - when multiple models reinforce each other.
    
    A lollapalooza occurs when 3+ mental models point in the same
    direction with high confidence, creating a powerful combined effect.
    """
    try:
        from src.lisp import get_lisp_bridge
        
        bridge = get_lisp_bridge()
        context = request.context or {}
        context["situation"] = request.situation
        
        result = bridge.detect_lollapalooza(context)
        
        return {
            "success": True,
            **result
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.get("/lisp/status")
async def get_lisp_status():
    """Get status of the Lisp (Hy) integration."""
    try:
        from src.lisp import get_lisp_bridge, HY_AVAILABLE
        
        bridge = get_lisp_bridge()
        
        return {
            "success": True,
            "hy_installed": HY_AVAILABLE,
            "dsl_loaded": bridge._hy_module is not None if HY_AVAILABLE else False,
            "fallback_active": not HY_AVAILABLE or bridge._hy_module is None,
            "registered_models": len(bridge.get_model_names()),
            "features": {
                "macros": HY_AVAILABLE,
                "repl": HY_AVAILABLE,
                "homoiconicity": HY_AVAILABLE,
                "python_interop": True
            },
            "benefits": [
                "Faster feature development via macros",
                "REPL-driven development for rapid iteration",
                "Code as data for self-modifying systems",
                "Minimal syntax reduces boilerplate"
            ]
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


TEMPLATES_DIR = Path(__file__).parent / "templates"


@app.get("/dashboard", response_class=HTMLResponse)
async def serve_dashboard():
    """Serve the web dashboard."""
    index_path = TEMPLATES_DIR / "index.html"
    if index_path.exists():
        return FileResponse(index_path, media_type="text/html")
    raise HTTPException(status_code=404, detail="Dashboard not found")


@app.get("/ui", response_class=HTMLResponse)
async def serve_ui():
    """Alias for dashboard."""
    return await serve_dashboard()


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host=settings.api.host, port=settings.api.port)
