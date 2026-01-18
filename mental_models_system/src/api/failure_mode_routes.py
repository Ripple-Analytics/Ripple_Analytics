"""
Failure Mode API Routes

REST API endpoints for failure mode search, risk assessment, and recommendations.
"""

from fastapi import APIRouter, Query, HTTPException
from pydantic import BaseModel
from typing import List, Optional, Dict
from ..safeguards.failure_search import (
    FailureModeSearchEngine,
    search_failure_modes,
    assess_decision_risk,
    get_safeguards
)

router = APIRouter(prefix="/failure-modes", tags=["Failure Modes"])

# Initialize search engine
engine = FailureModeSearchEngine()


# Request/Response Models
class SearchRequest(BaseModel):
    query: str
    limit: int = 10


class RiskAssessmentRequest(BaseModel):
    context: str
    models_in_use: Optional[List[str]] = None


class SafeguardRequest(BaseModel):
    models: List[str]


class FailureModeResponse(BaseModel):
    model_name: str
    mode_name: str
    description: str
    real_world_case: str
    warning_signs: List[str]
    safeguards: List[str]
    relevance_score: float
    match_reason: str


class RiskAssessmentResponse(BaseModel):
    overall_risk_score: float
    top_failure_modes: List[FailureModeResponse]
    risk_by_category: Dict[str, float]
    recommended_safeguards: List[str]
    warning_signs_to_watch: List[str]


class StatisticsResponse(BaseModel):
    total_models: int
    total_failure_modes: int
    average_modes_per_model: float
    modes_by_category: Dict[str, int]
    unique_words_indexed: int


# Routes
@router.get("/stats", response_model=StatisticsResponse)
async def get_statistics():
    """Get statistics about the failure modes database"""
    return engine.get_statistics()


@router.post("/search", response_model=List[FailureModeResponse])
async def search(request: SearchRequest):
    """
    Search for failure modes matching the query.
    
    Returns a list of failure modes ranked by relevance.
    """
    results = engine.search(request.query, request.limit)
    return [
        FailureModeResponse(
            model_name=r.model_name,
            mode_name=r.mode_name,
            description=r.description,
            real_world_case=r.real_world_case,
            warning_signs=r.warning_signs,
            safeguards=r.safeguards,
            relevance_score=r.relevance_score,
            match_reason=r.match_reason
        )
        for r in results
    ]


@router.get("/search")
async def search_get(
    q: str = Query(..., description="Search query"),
    limit: int = Query(10, description="Maximum results")
):
    """Search for failure modes (GET endpoint)"""
    results = engine.search(q, limit)
    return [
        {
            "model_name": r.model_name,
            "mode_name": r.mode_name,
            "description": r.description,
            "real_world_case": r.real_world_case,
            "warning_signs": r.warning_signs,
            "safeguards": r.safeguards,
            "relevance_score": r.relevance_score,
            "match_reason": r.match_reason
        }
        for r in results
    ]


@router.post("/assess-risk", response_model=RiskAssessmentResponse)
async def assess_risk(request: RiskAssessmentRequest):
    """
    Assess risk for a given context and set of mental models.
    
    Returns:
    - Overall risk score (0-1)
    - Top failure modes to watch
    - Risk breakdown by category
    - Recommended safeguards
    - Warning signs to monitor
    """
    assessment = engine.assess_risk(request.context, request.models_in_use)
    return RiskAssessmentResponse(
        overall_risk_score=assessment.overall_risk_score,
        top_failure_modes=[
            FailureModeResponse(
                model_name=m.model_name,
                mode_name=m.mode_name,
                description=m.description,
                real_world_case=m.real_world_case,
                warning_signs=m.warning_signs,
                safeguards=m.safeguards,
                relevance_score=m.relevance_score,
                match_reason=m.match_reason
            )
            for m in assessment.top_failure_modes
        ],
        risk_by_category=assessment.risk_by_category,
        recommended_safeguards=assessment.recommended_safeguards,
        warning_signs_to_watch=assessment.warning_signs_to_watch
    )


@router.get("/model/{model_name}")
async def get_failure_modes_for_model(model_name: str):
    """Get all failure modes for a specific mental model"""
    modes = engine.get_failure_modes_for_model(model_name)
    if not modes:
        raise HTTPException(status_code=404, detail=f"Model '{model_name}' not found")
    return {
        "model_name": model_name,
        "category": engine.model_to_category.get(model_name, "Unknown"),
        "failure_modes": modes
    }


@router.post("/safeguards")
async def get_safeguards_for_models(request: SafeguardRequest):
    """Get recommended safeguards for a list of mental models"""
    safeguards = engine.get_safeguards_for_decision("", request.models)
    return {
        "models": request.models,
        "safeguards_by_model": safeguards,
        "all_safeguards": list(set(
            s for safeguard_list in safeguards.values() 
            for s in safeguard_list
        ))
    }


@router.get("/similar-cases")
async def find_similar_cases(
    situation: str = Query(..., description="Description of current situation"),
    limit: int = Query(5, description="Maximum cases to return")
):
    """Find historical cases similar to the current situation"""
    cases = engine.find_similar_cases(situation, limit)
    return {
        "situation": situation,
        "similar_cases": cases
    }


@router.get("/categories")
async def get_categories():
    """Get all failure mode categories with counts"""
    stats = engine.get_statistics()
    return {
        "categories": stats['modes_by_category'],
        "total_models": stats['total_models'],
        "total_failure_modes": stats['total_failure_modes']
    }


@router.get("/models")
async def list_models():
    """List all models with failure modes"""
    return {
        "models": [
            {
                "name": name,
                "category": engine.model_to_category.get(name, "Unknown"),
                "failure_mode_count": len(modes)
            }
            for name, modes in engine.failure_modes.items()
        ]
    }
