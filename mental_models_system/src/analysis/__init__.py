"""
Mental Model Analysis Module

Analyze documents through the lens of 129 mental models and build knowledge graphs.
"""

from .model_analyzer import (
    # Data structures
    MentalModel,
    ModelMatch,
    LollapaloozaAlert,
    DocumentAnalysis,
    
    # Loaders
    MentalModelLoader,
    
    # Analyzers
    MentalModelAnalyzer,
    BatchModelAnalyzer,
    
    # Convenience
    create_analyzer,
    quick_analyze,
    load_models,
)

from .knowledge_graph import (
    # Graph structures
    Node,
    Edge,
    KnowledgeGraph,
    
    # Categorization
    DocumentCategorizer,
    
    # Convenience
    create_knowledge_graph,
    build_graph_from_analyses,
)

from .statistical_engine import (
    # Engine
    StatisticalEngine,
    MentalModelStatistics,
    
    # Data structures
    Variable,
    CorrelationResult,
    RegressionResult,
    CovarianceMatrix,
    FactorAnalysisResult,
    SynthesisResult,
    AnalysisType,
    
    # Convenience
    create_statistical_engine,
    create_mental_model_statistics,
)

__all__ = [
    # Model analyzer
    "MentalModel",
    "ModelMatch",
    "LollapaloozaAlert",
    "DocumentAnalysis",
    "MentalModelLoader",
    "MentalModelAnalyzer",
    "BatchModelAnalyzer",
    "create_analyzer",
    "quick_analyze",
    "load_models",
    
    # Knowledge graph
    "Node",
    "Edge",
    "KnowledgeGraph",
    "DocumentCategorizer",
    "create_knowledge_graph",
    "build_graph_from_analyses",
    
    # Statistical engine
    "StatisticalEngine",
    "MentalModelStatistics",
    "Variable",
    "CorrelationResult",
    "RegressionResult",
    "CovarianceMatrix",
    "FactorAnalysisResult",
    "SynthesisResult",
    "AnalysisType",
    "create_statistical_engine",
    "create_mental_model_statistics",
]
