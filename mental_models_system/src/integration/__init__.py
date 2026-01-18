"""
Manus Integration Module

Provides the bridge between local LLM processing and Manus for the
automated improvement cycle.
"""

from .manus_api import (
    # Data structures
    ImprovementType,
    ImprovementStatus,
    Priority,
    Improvement,
    ImportBatch,
    
    # Export/Import
    ManusExporter,
    ManusImporter,
    
    # Feedback
    FeedbackCollector,
    
    # Convenience
    create_exporter,
    create_importer,
    quick_export,
)

__all__ = [
    "ImprovementType",
    "ImprovementStatus",
    "Priority",
    "Improvement",
    "ImportBatch",
    "ManusExporter",
    "ManusImporter",
    "FeedbackCollector",
    "create_exporter",
    "create_importer",
    "quick_export",
]
