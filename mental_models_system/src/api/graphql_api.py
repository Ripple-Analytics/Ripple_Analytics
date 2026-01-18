"""
GraphQL API Layer

Flexible query interface for the Mental Models System:
- Mental model queries
- Failure mode queries
- Analysis queries
- Decision queries
- Signal queries
- Aggregations and filters
"""

import asyncio
from dataclasses import dataclass
from datetime import datetime
from typing import Any, Dict, List, Optional

# Using Strawberry for GraphQL (lightweight, async-first)
try:
    import strawberry
    from strawberry.fastapi import GraphQLRouter
    HAS_STRAWBERRY = True
except ImportError:
    HAS_STRAWBERRY = False


# GraphQL Types (using dataclasses as fallback when Strawberry not available)
@dataclass
class MentalModelType:
    """GraphQL type for mental model."""
    id: str
    name: str
    category: str
    description: str
    key_insight: str
    application: str
    examples: List[str]
    related_models: List[str]
    thinker: str
    complexity: str


@dataclass
class FailureModeType:
    """GraphQL type for failure mode."""
    id: str
    model_name: str
    description: str
    warning_signs: List[str]
    safeguards: List[str]
    severity: str
    real_world_example: Optional[str]


@dataclass
class AnalysisResultType:
    """GraphQL type for analysis result."""
    id: str
    document_name: str
    timestamp: str
    models_applied: List[str]
    lollapalooza_score: float
    top_models: List[Dict[str, Any]]
    failure_modes_detected: List[str]


@dataclass
class DecisionType:
    """GraphQL type for decision."""
    id: str
    title: str
    description: str
    domain: str
    models_used: List[str]
    confidence: float
    outcome: Optional[str]
    created_at: str


@dataclass
class SignalType:
    """GraphQL type for signal."""
    id: str
    source: str
    content: str
    timestamp: str
    models_detected: List[str]
    lollapalooza_score: float
    priority: str


class GraphQLService:
    """
    GraphQL service for the Mental Models System.
    
    Provides flexible querying capabilities without requiring
    Strawberry/GraphQL dependencies.
    """
    
    def __init__(self):
        self._models_cache: Dict[str, MentalModelType] = {}
        self._failure_modes_cache: Dict[str, List[FailureModeType]] = {}
        self._analyses: List[AnalysisResultType] = []
        self._decisions: List[DecisionType] = []
        self._signals: List[SignalType] = []
        self._load_data()
    
    def _load_data(self):
        """Load data from the system."""
        try:
            from ..analysis.model_analyzer import MentalModelLoader
            loader = MentalModelLoader()
            
            for category, models in loader.models_by_category.items():
                for model in models:
                    # model is a MentalModel dataclass
                    name = model.name
                    self._models_cache[name] = MentalModelType(
                        id=str(model.id),
                        name=name,
                        category=model.category,
                        description=model.description,
                        key_insight=model.how_to_apply or "",
                        application=model.how_to_apply or "",
                        examples=[],
                        related_models=model.related_models or [],
                        thinker="Munger",
                        complexity="medium"
                    )
        except Exception as e:
            print(f"Error loading models: {e}")
        
        try:
            from ..safeguards.failure_modes_loader import FailureModesLoader
            loader = FailureModesLoader()
            
            # Use the loader's internal data structure - _data contains ModelFailureModes objects
            for model_id, model_failure_modes in loader._data.items():
                model_name = model_failure_modes.model_name
                self._failure_modes_cache[model_name] = [
                    FailureModeType(
                        id=fm.id,
                        model_name=model_name,
                        description=fm.description,
                        warning_signs=fm.behavioral_signals or [],
                        safeguards=[s.action for s in (fm.safeguards or [])],
                        severity="medium",
                        real_world_example=fm.case_studies[0].what_happened if fm.case_studies else None
                    )
                    for fm in model_failure_modes.failure_modes
                ]
        except Exception as e:
            print(f"Error loading failure modes: {e}")
    
    # Query methods
    def get_model(self, name: str) -> Optional[MentalModelType]:
        """Get a mental model by name."""
        return self._models_cache.get(name)
    
    def get_models(
        self,
        category: Optional[str] = None,
        thinker: Optional[str] = None,
        complexity: Optional[str] = None,
        search: Optional[str] = None,
        limit: int = 100,
        offset: int = 0
    ) -> List[MentalModelType]:
        """Get mental models with filters."""
        models = list(self._models_cache.values())
        
        if category:
            models = [m for m in models if m.category.lower() == category.lower()]
        
        if thinker:
            models = [m for m in models if thinker.lower() in m.thinker.lower()]
        
        if complexity:
            models = [m for m in models if m.complexity == complexity]
        
        if search:
            search_lower = search.lower()
            models = [
                m for m in models
                if search_lower in m.name.lower()
                or search_lower in m.description.lower()
                or search_lower in m.key_insight.lower()
            ]
        
        return models[offset:offset + limit]
    
    def get_failure_modes(
        self,
        model_name: Optional[str] = None,
        severity: Optional[str] = None,
        search: Optional[str] = None,
        limit: int = 100,
        offset: int = 0
    ) -> List[FailureModeType]:
        """Get failure modes with filters."""
        if model_name:
            modes = self._failure_modes_cache.get(model_name, [])
        else:
            modes = []
            for model_modes in self._failure_modes_cache.values():
                modes.extend(model_modes)
        
        if severity:
            modes = [m for m in modes if m.severity == severity]
        
        if search:
            search_lower = search.lower()
            modes = [
                m for m in modes
                if search_lower in m.description.lower()
                or any(search_lower in ws.lower() for ws in m.warning_signs)
            ]
        
        return modes[offset:offset + limit]
    
    def get_model_with_failure_modes(self, name: str) -> Dict[str, Any]:
        """Get a model with its failure modes."""
        model = self.get_model(name)
        if not model:
            return {}
        
        failure_modes = self._failure_modes_cache.get(name, [])
        
        return {
            "model": model,
            "failure_modes": failure_modes,
            "failure_mode_count": len(failure_modes)
        }
    
    def get_categories(self) -> List[Dict[str, Any]]:
        """Get all categories with counts."""
        categories: Dict[str, int] = {}
        for model in self._models_cache.values():
            cat = model.category
            categories[cat] = categories.get(cat, 0) + 1
        
        return [
            {"name": name, "count": count}
            for name, count in sorted(categories.items())
        ]
    
    def get_thinkers(self) -> List[Dict[str, Any]]:
        """Get all thinkers with counts."""
        thinkers: Dict[str, int] = {}
        for model in self._models_cache.values():
            thinker = model.thinker
            if thinker:
                thinkers[thinker] = thinkers.get(thinker, 0) + 1
        
        return [
            {"name": name, "count": count}
            for name, count in sorted(thinkers.items(), key=lambda x: -x[1])
        ]
    
    def get_related_models(self, name: str, depth: int = 1) -> List[MentalModelType]:
        """Get related models up to a certain depth."""
        model = self.get_model(name)
        if not model:
            return []
        
        related = set()
        to_process = [(name, 0)]
        processed = set()
        
        while to_process:
            current_name, current_depth = to_process.pop(0)
            if current_name in processed or current_depth > depth:
                continue
            
            processed.add(current_name)
            current_model = self.get_model(current_name)
            if current_model:
                for rel in current_model.related_models:
                    if rel != name:
                        related.add(rel)
                        if current_depth < depth:
                            to_process.append((rel, current_depth + 1))
        
        return [self.get_model(r) for r in related if self.get_model(r)]
    
    def search_all(self, query: str, limit: int = 50) -> Dict[str, Any]:
        """Search across all entities."""
        query_lower = query.lower()
        
        models = [
            m for m in self._models_cache.values()
            if query_lower in m.name.lower()
            or query_lower in m.description.lower()
        ][:limit // 2]
        
        failure_modes = []
        for model_modes in self._failure_modes_cache.values():
            for mode in model_modes:
                if query_lower in mode.description.lower():
                    failure_modes.append(mode)
                    if len(failure_modes) >= limit // 2:
                        break
            if len(failure_modes) >= limit // 2:
                break
        
        return {
            "query": query,
            "models": models,
            "failure_modes": failure_modes,
            "total_results": len(models) + len(failure_modes)
        }
    
    def get_stats(self) -> Dict[str, Any]:
        """Get system statistics."""
        total_failure_modes = sum(
            len(modes) for modes in self._failure_modes_cache.values()
        )
        
        return {
            "total_models": len(self._models_cache),
            "total_failure_modes": total_failure_modes,
            "categories": len(self.get_categories()),
            "thinkers": len(self.get_thinkers()),
            "analyses_count": len(self._analyses),
            "decisions_count": len(self._decisions),
            "signals_count": len(self._signals)
        }
    
    # Mutation methods (for recording data)
    def record_analysis(self, analysis: AnalysisResultType):
        """Record an analysis result."""
        self._analyses.append(analysis)
    
    def record_decision(self, decision: DecisionType):
        """Record a decision."""
        self._decisions.append(decision)
    
    def record_signal(self, signal: SignalType):
        """Record a signal."""
        self._signals.append(signal)
    
    def get_analyses(
        self,
        from_date: Optional[str] = None,
        to_date: Optional[str] = None,
        min_score: Optional[float] = None,
        limit: int = 100
    ) -> List[AnalysisResultType]:
        """Get analyses with filters."""
        analyses = self._analyses
        
        if from_date:
            analyses = [a for a in analyses if a.timestamp >= from_date]
        
        if to_date:
            analyses = [a for a in analyses if a.timestamp <= to_date]
        
        if min_score is not None:
            analyses = [a for a in analyses if a.lollapalooza_score >= min_score]
        
        return analyses[:limit]
    
    def get_decisions(
        self,
        domain: Optional[str] = None,
        has_outcome: Optional[bool] = None,
        limit: int = 100
    ) -> List[DecisionType]:
        """Get decisions with filters."""
        decisions = self._decisions
        
        if domain:
            decisions = [d for d in decisions if d.domain == domain]
        
        if has_outcome is not None:
            if has_outcome:
                decisions = [d for d in decisions if d.outcome is not None]
            else:
                decisions = [d for d in decisions if d.outcome is None]
        
        return decisions[:limit]
    
    def execute_query(self, query: str, variables: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """
        Execute a GraphQL-like query.
        
        Simplified query language:
        - models(category, thinker, search, limit)
        - model(name)
        - failureModes(model, severity, search, limit)
        - categories
        - thinkers
        - search(query)
        - stats
        """
        variables = variables or {}
        query = query.strip().lower()
        
        # Parse simple queries
        if query.startswith("models"):
            return {"data": {"models": [m.__dict__ for m in self.get_models(
                category=variables.get("category"),
                thinker=variables.get("thinker"),
                search=variables.get("search"),
                limit=variables.get("limit", 100)
            )]}}
        
        elif query.startswith("model("):
            name = variables.get("name", "")
            model = self.get_model(name)
            return {"data": {"model": model.__dict__ if model else None}}
        
        elif query.startswith("failuremodes"):
            return {"data": {"failureModes": [m.__dict__ for m in self.get_failure_modes(
                model_name=variables.get("model"),
                severity=variables.get("severity"),
                search=variables.get("search"),
                limit=variables.get("limit", 100)
            )]}}
        
        elif query == "categories":
            return {"data": {"categories": self.get_categories()}}
        
        elif query == "thinkers":
            return {"data": {"thinkers": self.get_thinkers()}}
        
        elif query.startswith("search"):
            search_query = variables.get("query", "")
            result = self.search_all(search_query)
            return {"data": {"search": {
                "query": result["query"],
                "models": [m.__dict__ for m in result["models"]],
                "failureModes": [m.__dict__ for m in result["failure_modes"]],
                "totalResults": result["total_results"]
            }}}
        
        elif query == "stats":
            return {"data": {"stats": self.get_stats()}}
        
        else:
            return {"errors": [{"message": f"Unknown query: {query}"}]}


# Create Strawberry schema if available
if HAS_STRAWBERRY:
    @strawberry.type
    class MentalModel:
        id: str
        name: str
        category: str
        description: str
        key_insight: str
        application: str
        examples: List[str]
        related_models: List[str]
        thinker: str
        complexity: str
    
    @strawberry.type
    class FailureMode:
        id: str
        model_name: str
        description: str
        warning_signs: List[str]
        safeguards: List[str]
        severity: str
        real_world_example: Optional[str]
    
    @strawberry.type
    class Category:
        name: str
        count: int
    
    @strawberry.type
    class Thinker:
        name: str
        count: int
    
    @strawberry.type
    class SearchResult:
        query: str
        models: List[MentalModel]
        failure_modes: List[FailureMode]
        total_results: int
    
    @strawberry.type
    class Stats:
        total_models: int
        total_failure_modes: int
        categories: int
        thinkers: int
    
    @strawberry.type
    class Query:
        @strawberry.field
        def models(
            self,
            category: Optional[str] = None,
            thinker: Optional[str] = None,
            complexity: Optional[str] = None,
            search: Optional[str] = None,
            limit: int = 100,
            offset: int = 0
        ) -> List[MentalModel]:
            service = GraphQLService()
            results = service.get_models(category, thinker, complexity, search, limit, offset)
            return [
                MentalModel(
                    id=r.id, name=r.name, category=r.category,
                    description=r.description, key_insight=r.key_insight,
                    application=r.application, examples=r.examples,
                    related_models=r.related_models, thinker=r.thinker,
                    complexity=r.complexity
                )
                for r in results
            ]
        
        @strawberry.field
        def model(self, name: str) -> Optional[MentalModel]:
            service = GraphQLService()
            r = service.get_model(name)
            if r:
                return MentalModel(
                    id=r.id, name=r.name, category=r.category,
                    description=r.description, key_insight=r.key_insight,
                    application=r.application, examples=r.examples,
                    related_models=r.related_models, thinker=r.thinker,
                    complexity=r.complexity
                )
            return None
        
        @strawberry.field
        def failure_modes(
            self,
            model_name: Optional[str] = None,
            severity: Optional[str] = None,
            search: Optional[str] = None,
            limit: int = 100
        ) -> List[FailureMode]:
            service = GraphQLService()
            results = service.get_failure_modes(model_name, severity, search, limit)
            return [
                FailureMode(
                    id=r.id, model_name=r.model_name, description=r.description,
                    warning_signs=r.warning_signs, safeguards=r.safeguards,
                    severity=r.severity, real_world_example=r.real_world_example
                )
                for r in results
            ]
        
        @strawberry.field
        def categories(self) -> List[Category]:
            service = GraphQLService()
            return [Category(**c) for c in service.get_categories()]
        
        @strawberry.field
        def thinkers(self) -> List[Thinker]:
            service = GraphQLService()
            return [Thinker(**t) for t in service.get_thinkers()]
        
        @strawberry.field
        def stats(self) -> Stats:
            service = GraphQLService()
            s = service.get_stats()
            return Stats(
                total_models=s["total_models"],
                total_failure_modes=s["total_failure_modes"],
                categories=s["categories"],
                thinkers=s["thinkers"]
            )
    
    # Create schema
    schema = strawberry.Schema(query=Query)
    
    def get_graphql_router() -> GraphQLRouter:
        """Get the GraphQL router for FastAPI."""
        return GraphQLRouter(schema)


# REST-like GraphQL endpoint for systems without Strawberry
def create_graphql_endpoint(service: Optional[GraphQLService] = None):
    """Create a REST endpoint that accepts GraphQL-like queries."""
    from fastapi import APIRouter, HTTPException
    from pydantic import BaseModel
    
    router = APIRouter(prefix="/graphql", tags=["graphql"])
    _service = service or GraphQLService()
    
    class GraphQLRequest(BaseModel):
        query: str
        variables: Optional[Dict[str, Any]] = None
    
    @router.post("")
    async def execute_graphql(request: GraphQLRequest):
        """Execute a GraphQL-like query."""
        try:
            result = _service.execute_query(request.query, request.variables)
            return result
        except Exception as e:
            raise HTTPException(status_code=400, detail=str(e))
    
    @router.get("/models")
    async def get_models(
        category: Optional[str] = None,
        thinker: Optional[str] = None,
        search: Optional[str] = None,
        limit: int = 100
    ):
        """Get mental models."""
        models = _service.get_models(category, thinker, None, search, limit)
        return [m.__dict__ for m in models]
    
    @router.get("/models/{name}")
    async def get_model(name: str):
        """Get a specific mental model."""
        model = _service.get_model(name)
        if not model:
            raise HTTPException(status_code=404, detail="Model not found")
        return model.__dict__
    
    @router.get("/failure-modes")
    async def get_failure_modes(
        model: Optional[str] = None,
        severity: Optional[str] = None,
        search: Optional[str] = None,
        limit: int = 100
    ):
        """Get failure modes."""
        modes = _service.get_failure_modes(model, severity, search, limit)
        return [m.__dict__ for m in modes]
    
    @router.get("/categories")
    async def get_categories():
        """Get all categories."""
        return _service.get_categories()
    
    @router.get("/thinkers")
    async def get_thinkers():
        """Get all thinkers."""
        return _service.get_thinkers()
    
    @router.get("/search")
    async def search(query: str, limit: int = 50):
        """Search across all entities."""
        result = _service.search_all(query, limit)
        return {
            "query": result["query"],
            "models": [m.__dict__ for m in result["models"]],
            "failure_modes": [m.__dict__ for m in result["failure_modes"]],
            "total_results": result["total_results"]
        }
    
    @router.get("/stats")
    async def get_stats():
        """Get system statistics."""
        return _service.get_stats()
    
    return router


if __name__ == "__main__":
    # Test the GraphQL service
    service = GraphQLService()
    
    print("=== GraphQL Service Test ===\n")
    
    # Test queries
    print("Stats:", service.get_stats())
    print("\nCategories:", service.get_categories()[:5])
    print("\nThinkers:", service.get_thinkers()[:5])
    
    # Test model query
    models = service.get_models(category="Psychology", limit=3)
    print(f"\nPsychology models ({len(models)}):")
    for m in models:
        print(f"  - {m.name}")
    
    # Test failure modes
    modes = service.get_failure_modes(severity="high", limit=3)
    print(f"\nHigh severity failure modes ({len(modes)}):")
    for m in modes:
        print(f"  - {m.model_name}: {m.description[:50]}...")
    
    # Test search
    result = service.search_all("network", limit=5)
    print(f"\nSearch 'network': {result['total_results']} results")
    
    # Test execute_query
    query_result = service.execute_query("stats")
    print(f"\nExecute query 'stats': {query_result}")
