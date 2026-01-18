"""
Cross-Document Intelligence Network Module

Links concepts across documents, identifies contradictions, surfaces patterns.
"""

from .cross_document_network import (
    CrossDocumentNetwork,
    Entity,
    EntityType,
    Relationship,
    RelationType,
    DocumentNode,
    Contradiction,
    Pattern,
    EmbeddingEngine,
    EntityExtractor
)

__all__ = [
    "CrossDocumentNetwork",
    "Entity",
    "EntityType",
    "Relationship",
    "RelationType",
    "DocumentNode",
    "Contradiction",
    "Pattern",
    "EmbeddingEngine",
    "EntityExtractor"
]
