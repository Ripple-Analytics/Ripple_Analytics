"""
Cross-Document Intelligence Network

Links concepts across documents, identifies contradictions, surfaces patterns,
and enables semantic search across your entire document corpus.

Features:
1. Entity extraction and linking across documents
2. Contradiction detection when sources disagree
3. Pattern discovery across corpus
4. Semantic search with embeddings
5. Gap analysis for missing knowledge

Architecture:
┌─────────────────────────────────────────────────────────────────────────┐
│                    CROSS-DOCUMENT INTELLIGENCE NETWORK                   │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                      KNOWLEDGE GRAPH                             │   │
│  │                                                                   │   │
│  │     [Doc A]────Concept X────[Doc B]                              │   │
│  │          │                    │                                   │   │
│  │     [Doc C]────Concept Y────[Doc D]                              │   │
│  │                                                                   │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  CAPABILITIES:                                                           │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐        │
│  │ SEMANTIC SEARCH │  │ CONTRADICTION   │  │ PATTERN         │        │
│  │                 │  │ DETECTION       │  │ DISCOVERY       │        │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘        │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
"""

import json
import hashlib
import re
from dataclasses import dataclass, field, asdict
from datetime import datetime
from typing import Dict, List, Optional, Tuple, Any, Set
from enum import Enum
from pathlib import Path
from collections import defaultdict
import logging
import math

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =============================================================================
# DATA STRUCTURES
# =============================================================================

class EntityType(Enum):
    """Types of entities."""
    COMPANY = "company"
    PERSON = "person"
    CONCEPT = "concept"
    MENTAL_MODEL = "mental_model"
    EVENT = "event"
    METRIC = "metric"
    LOCATION = "location"
    DATE = "date"


class RelationType(Enum):
    """Types of relationships."""
    MENTIONS = "mentions"
    SUPPORTS = "supports"
    CONTRADICTS = "contradicts"
    APPLIES_TO = "applies_to"
    CAUSED_BY = "caused_by"
    SIMILAR_TO = "similar_to"
    EXAMPLE_OF = "example_of"
    PRECEDES = "precedes"


@dataclass
class Entity:
    """An entity extracted from documents."""
    id: str
    name: str
    entity_type: EntityType
    aliases: List[str] = field(default_factory=list)
    description: str = ""
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    # Document references
    document_ids: List[str] = field(default_factory=list)
    mention_count: int = 0
    
    # Embeddings for semantic search
    embedding: List[float] = field(default_factory=list)
    
    def to_dict(self) -> Dict:
        d = asdict(self)
        d['entity_type'] = self.entity_type.value
        return d


@dataclass
class Relationship:
    """A relationship between entities."""
    source_id: str
    target_id: str
    relation_type: RelationType
    weight: float = 1.0
    evidence: List[str] = field(default_factory=list)  # Document IDs
    context: str = ""
    
    def to_dict(self) -> Dict:
        d = asdict(self)
        d['relation_type'] = self.relation_type.value
        return d


@dataclass
class DocumentNode:
    """A document in the network."""
    id: str
    path: str
    title: str
    content_hash: str
    
    # Extracted data
    entities: List[str] = field(default_factory=list)  # Entity IDs
    mental_models: List[int] = field(default_factory=list)  # Model IDs
    key_claims: List[str] = field(default_factory=list)
    
    # Metadata
    created_at: datetime = None
    processed_at: datetime = None
    word_count: int = 0
    
    # Embeddings
    embedding: List[float] = field(default_factory=list)
    chunk_embeddings: List[Tuple[str, List[float]]] = field(default_factory=list)
    
    def to_dict(self) -> Dict:
        d = asdict(self)
        if self.created_at:
            d['created_at'] = self.created_at.isoformat()
        if self.processed_at:
            d['processed_at'] = self.processed_at.isoformat()
        return d


@dataclass
class Contradiction:
    """A detected contradiction between documents."""
    id: str
    claim_a: str
    claim_b: str
    document_a: str
    document_b: str
    entity_ids: List[str]
    confidence: float
    resolution: str = ""  # User-provided resolution
    resolved: bool = False
    
    def to_dict(self) -> Dict:
        return asdict(self)


@dataclass
class Pattern:
    """A discovered pattern across documents."""
    id: str
    name: str
    description: str
    entity_ids: List[str]
    document_ids: List[str]
    frequency: int
    correlation_score: float
    mental_models: List[int] = field(default_factory=list)
    
    def to_dict(self) -> Dict:
        return asdict(self)


# =============================================================================
# EMBEDDING UTILITIES
# =============================================================================

class EmbeddingEngine:
    """Generates embeddings for semantic search."""
    
    def __init__(self, model: str = "text-embedding-ada-002"):
        self.model = model
        self._cache: Dict[str, List[float]] = {}
    
    def embed(self, text: str) -> List[float]:
        """Generate embedding for text."""
        # Check cache
        cache_key = hashlib.md5(text[:1000].encode()).hexdigest()
        if cache_key in self._cache:
            return self._cache[cache_key]
        
        try:
            from openai import OpenAI
            client = OpenAI()
            
            response = client.embeddings.create(
                model=self.model,
                input=text[:8000]  # Limit input size
            )
            
            embedding = response.data[0].embedding
            self._cache[cache_key] = embedding
            return embedding
            
        except Exception as e:
            logger.warning(f"Embedding failed: {e}")
            # Return zero vector as fallback
            return [0.0] * 1536
    
    def embed_batch(self, texts: List[str]) -> List[List[float]]:
        """Generate embeddings for multiple texts."""
        return [self.embed(t) for t in texts]
    
    @staticmethod
    def cosine_similarity(a: List[float], b: List[float]) -> float:
        """Calculate cosine similarity between two vectors."""
        if not a or not b or len(a) != len(b):
            return 0.0
        
        dot_product = sum(x * y for x, y in zip(a, b))
        norm_a = math.sqrt(sum(x * x for x in a))
        norm_b = math.sqrt(sum(x * x for x in b))
        
        if norm_a == 0 or norm_b == 0:
            return 0.0
        
        return dot_product / (norm_a * norm_b)


# =============================================================================
# ENTITY EXTRACTOR
# =============================================================================

class EntityExtractor:
    """Extracts entities from text using LLM."""
    
    def __init__(self):
        self.models_data = self._load_models()
    
    def _load_models(self) -> Dict:
        """Load mental models for matching."""
        models_path = Path(__file__).parent.parent.parent / "data" / "raw" / "mental_models_complete.json"
        
        if models_path.exists():
            with open(models_path, 'r') as f:
                data = json.load(f)
            return {m["id"]: m for m in data.get("mental_models", [])}
        return {}
    
    def extract(self, text: str, document_id: str) -> Tuple[List[Entity], List[int]]:
        """Extract entities and mental models from text."""
        entities = []
        mental_models = []
        
        # Extract mental models by keyword matching
        text_lower = text.lower()
        for model_id, model in self.models_data.items():
            model_name = model["name"].lower()
            if model_name in text_lower or any(
                alias.lower() in text_lower 
                for alias in model.get("aliases", [])
            ):
                mental_models.append(model_id)
        
        # Use LLM for entity extraction
        try:
            from openai import OpenAI
            client = OpenAI()
            
            prompt = f"""Extract named entities from this text. Return JSON with:
{{
  "companies": ["company names"],
  "people": ["person names"],
  "concepts": ["key concepts/ideas"],
  "events": ["significant events"],
  "metrics": ["numbers/statistics mentioned"]
}}

TEXT:
{text[:3000]}

Return only valid JSON."""

            response = client.chat.completions.create(
                model="gpt-4.1-mini",
                messages=[{"role": "user", "content": prompt}],
                max_tokens=1000
            )
            
            result_text = response.choices[0].message.content
            
            # Parse JSON
            json_start = result_text.find("{")
            json_end = result_text.rfind("}") + 1
            if json_start != -1 and json_end > json_start:
                extracted = json.loads(result_text[json_start:json_end])
                
                # Create entities
                for company in extracted.get("companies", []):
                    entities.append(Entity(
                        id=hashlib.md5(f"company:{company}".encode()).hexdigest()[:12],
                        name=company,
                        entity_type=EntityType.COMPANY,
                        document_ids=[document_id],
                        mention_count=1
                    ))
                
                for person in extracted.get("people", []):
                    entities.append(Entity(
                        id=hashlib.md5(f"person:{person}".encode()).hexdigest()[:12],
                        name=person,
                        entity_type=EntityType.PERSON,
                        document_ids=[document_id],
                        mention_count=1
                    ))
                
                for concept in extracted.get("concepts", []):
                    entities.append(Entity(
                        id=hashlib.md5(f"concept:{concept}".encode()).hexdigest()[:12],
                        name=concept,
                        entity_type=EntityType.CONCEPT,
                        document_ids=[document_id],
                        mention_count=1
                    ))
                
        except Exception as e:
            logger.warning(f"Entity extraction failed: {e}")
        
        return entities, mental_models


# =============================================================================
# CROSS-DOCUMENT INTELLIGENCE NETWORK
# =============================================================================

class CrossDocumentNetwork:
    """
    Main intelligence network that links concepts across documents.
    """
    
    def __init__(self, storage_path: str = None):
        self.storage_path = Path(storage_path) if storage_path else None
        
        # Data stores
        self.documents: Dict[str, DocumentNode] = {}
        self.entities: Dict[str, Entity] = {}
        self.relationships: List[Relationship] = []
        self.contradictions: Dict[str, Contradiction] = {}
        self.patterns: Dict[str, Pattern] = {}
        
        # Utilities
        self.embedding_engine = EmbeddingEngine()
        self.entity_extractor = EntityExtractor()
        
        # Load existing data
        if self.storage_path and self.storage_path.exists():
            self._load()
    
    def _load(self):
        """Load network data from storage."""
        try:
            with open(self.storage_path, 'r') as f:
                data = json.load(f)
            
            # Load documents
            for d in data.get('documents', []):
                doc = DocumentNode(**d)
                self.documents[doc.id] = doc
            
            # Load entities
            for e in data.get('entities', []):
                e['entity_type'] = EntityType(e['entity_type'])
                entity = Entity(**e)
                self.entities[entity.id] = entity
            
            # Load relationships
            for r in data.get('relationships', []):
                r['relation_type'] = RelationType(r['relation_type'])
                self.relationships.append(Relationship(**r))
            
            # Load contradictions
            for c in data.get('contradictions', []):
                cont = Contradiction(**c)
                self.contradictions[cont.id] = cont
            
            # Load patterns
            for p in data.get('patterns', []):
                pattern = Pattern(**p)
                self.patterns[pattern.id] = pattern
            
            logger.info(f"Loaded network: {len(self.documents)} docs, {len(self.entities)} entities")
        except Exception as e:
            logger.error(f"Error loading network: {e}")
    
    def _save(self):
        """Save network data to storage."""
        if not self.storage_path:
            return
        
        try:
            self.storage_path.parent.mkdir(parents=True, exist_ok=True)
            
            data = {
                'documents': [d.to_dict() for d in self.documents.values()],
                'entities': [e.to_dict() for e in self.entities.values()],
                'relationships': [r.to_dict() for r in self.relationships],
                'contradictions': [c.to_dict() for c in self.contradictions.values()],
                'patterns': [p.to_dict() for p in self.patterns.values()],
                'metadata': {
                    'last_updated': datetime.now().isoformat(),
                    'total_documents': len(self.documents),
                    'total_entities': len(self.entities)
                }
            }
            
            with open(self.storage_path, 'w') as f:
                json.dump(data, f, indent=2)
            
            logger.info("Network data saved")
        except Exception as e:
            logger.error(f"Error saving network: {e}")
    
    # =========================================================================
    # DOCUMENT PROCESSING
    # =========================================================================
    
    def add_document(self, path: str, content: str, title: str = None) -> DocumentNode:
        """Add a document to the network."""
        doc_id = hashlib.md5(path.encode()).hexdigest()[:12]
        content_hash = hashlib.md5(content.encode()).hexdigest()
        
        # Check if already processed
        if doc_id in self.documents:
            existing = self.documents[doc_id]
            if existing.content_hash == content_hash:
                logger.info(f"Document {doc_id} unchanged, skipping")
                return existing
        
        # Extract entities and mental models
        entities, mental_models = self.entity_extractor.extract(content, doc_id)
        
        # Merge entities with existing
        entity_ids = []
        for entity in entities:
            if entity.id in self.entities:
                # Update existing entity
                existing = self.entities[entity.id]
                if doc_id not in existing.document_ids:
                    existing.document_ids.append(doc_id)
                    existing.mention_count += 1
            else:
                self.entities[entity.id] = entity
            entity_ids.append(entity.id)
        
        # Generate document embedding
        embedding = self.embedding_engine.embed(content[:5000])
        
        # Extract key claims
        key_claims = self._extract_claims(content)
        
        # Create document node
        doc = DocumentNode(
            id=doc_id,
            path=path,
            title=title or Path(path).stem,
            content_hash=content_hash,
            entities=entity_ids,
            mental_models=mental_models,
            key_claims=key_claims,
            processed_at=datetime.now(),
            word_count=len(content.split()),
            embedding=embedding
        )
        
        self.documents[doc_id] = doc
        
        # Create relationships
        self._create_relationships(doc)
        
        # Check for contradictions
        self._check_contradictions(doc)
        
        self._save()
        
        logger.info(f"Added document {doc_id}: {doc.title}")
        return doc
    
    def _extract_claims(self, text: str) -> List[str]:
        """Extract key claims from text."""
        try:
            from openai import OpenAI
            client = OpenAI()
            
            prompt = f"""Extract the 5 most important factual claims or assertions from this text.
Return as a JSON array of strings.

TEXT:
{text[:3000]}

Return only valid JSON array."""

            response = client.chat.completions.create(
                model="gpt-4.1-mini",
                messages=[{"role": "user", "content": prompt}],
                max_tokens=500
            )
            
            result = response.choices[0].message.content
            json_start = result.find("[")
            json_end = result.rfind("]") + 1
            if json_start != -1 and json_end > json_start:
                return json.loads(result[json_start:json_end])
        except Exception as e:
            logger.warning(f"Claim extraction failed: {e}")
        
        return []
    
    def _create_relationships(self, doc: DocumentNode):
        """Create relationships between entities in document."""
        # Link entities that appear in same document
        for i, entity_id_a in enumerate(doc.entities):
            for entity_id_b in doc.entities[i+1:]:
                # Check if relationship already exists
                exists = any(
                    r.source_id == entity_id_a and r.target_id == entity_id_b
                    for r in self.relationships
                )
                
                if not exists:
                    self.relationships.append(Relationship(
                        source_id=entity_id_a,
                        target_id=entity_id_b,
                        relation_type=RelationType.MENTIONS,
                        evidence=[doc.id]
                    ))
                else:
                    # Update evidence
                    for r in self.relationships:
                        if r.source_id == entity_id_a and r.target_id == entity_id_b:
                            if doc.id not in r.evidence:
                                r.evidence.append(doc.id)
                            r.weight += 0.1
    
    def _check_contradictions(self, doc: DocumentNode):
        """Check for contradictions with existing documents."""
        if not doc.key_claims:
            return
        
        # Compare claims with other documents
        for other_id, other_doc in self.documents.items():
            if other_id == doc.id or not other_doc.key_claims:
                continue
            
            # Use embeddings to find similar claims
            for claim_a in doc.key_claims:
                claim_a_emb = self.embedding_engine.embed(claim_a)
                
                for claim_b in other_doc.key_claims:
                    claim_b_emb = self.embedding_engine.embed(claim_b)
                    
                    similarity = EmbeddingEngine.cosine_similarity(claim_a_emb, claim_b_emb)
                    
                    # High similarity but potentially contradicting
                    if 0.7 < similarity < 0.95:
                        # Use LLM to check for contradiction
                        is_contradiction = self._check_claim_contradiction(claim_a, claim_b)
                        
                        if is_contradiction:
                            cont_id = hashlib.md5(f"{claim_a}{claim_b}".encode()).hexdigest()[:12]
                            
                            if cont_id not in self.contradictions:
                                self.contradictions[cont_id] = Contradiction(
                                    id=cont_id,
                                    claim_a=claim_a,
                                    claim_b=claim_b,
                                    document_a=doc.id,
                                    document_b=other_id,
                                    entity_ids=list(set(doc.entities) & set(other_doc.entities)),
                                    confidence=similarity
                                )
                                logger.info(f"Found contradiction: {cont_id}")
    
    def _check_claim_contradiction(self, claim_a: str, claim_b: str) -> bool:
        """Check if two claims contradict each other."""
        try:
            from openai import OpenAI
            client = OpenAI()
            
            prompt = f"""Do these two claims contradict each other?

Claim A: {claim_a}
Claim B: {claim_b}

Answer with just "yes" or "no"."""

            response = client.chat.completions.create(
                model="gpt-4.1-nano",
                messages=[{"role": "user", "content": prompt}],
                max_tokens=10
            )
            
            answer = response.choices[0].message.content.lower().strip()
            return "yes" in answer
            
        except Exception as e:
            logger.warning(f"Contradiction check failed: {e}")
            return False
    
    # =========================================================================
    # SEARCH
    # =========================================================================
    
    def semantic_search(self, query: str, limit: int = 10) -> List[Dict]:
        """Search documents by semantic similarity."""
        query_embedding = self.embedding_engine.embed(query)
        
        results = []
        for doc_id, doc in self.documents.items():
            if doc.embedding:
                similarity = EmbeddingEngine.cosine_similarity(query_embedding, doc.embedding)
                results.append({
                    "document_id": doc_id,
                    "title": doc.title,
                    "path": doc.path,
                    "similarity": similarity,
                    "mental_models": doc.mental_models,
                    "entities": [
                        self.entities[e].name 
                        for e in doc.entities[:5] 
                        if e in self.entities
                    ]
                })
        
        results.sort(key=lambda x: x["similarity"], reverse=True)
        return results[:limit]
    
    def search_by_entity(self, entity_name: str) -> List[Dict]:
        """Search documents containing an entity."""
        # Find matching entities
        matching_entities = [
            e for e in self.entities.values()
            if entity_name.lower() in e.name.lower() or
            any(entity_name.lower() in a.lower() for a in e.aliases)
        ]
        
        if not matching_entities:
            return []
        
        # Get documents
        doc_ids = set()
        for entity in matching_entities:
            doc_ids.update(entity.document_ids)
        
        results = []
        for doc_id in doc_ids:
            if doc_id in self.documents:
                doc = self.documents[doc_id]
                results.append({
                    "document_id": doc_id,
                    "title": doc.title,
                    "path": doc.path,
                    "matching_entities": [e.name for e in matching_entities if doc_id in e.document_ids]
                })
        
        return results
    
    def search_by_model(self, model_id: int) -> List[Dict]:
        """Search documents that apply a mental model."""
        results = []
        
        for doc_id, doc in self.documents.items():
            if model_id in doc.mental_models:
                results.append({
                    "document_id": doc_id,
                    "title": doc.title,
                    "path": doc.path,
                    "all_models": doc.mental_models
                })
        
        return results
    
    # =========================================================================
    # PATTERN DISCOVERY
    # =========================================================================
    
    def discover_patterns(self, min_frequency: int = 3) -> List[Pattern]:
        """Discover patterns across documents."""
        # Find frequently co-occurring entities
        entity_cooccurrence = defaultdict(int)
        entity_docs = defaultdict(set)
        
        for doc in self.documents.values():
            for i, entity_a in enumerate(doc.entities):
                for entity_b in doc.entities[i+1:]:
                    pair = tuple(sorted([entity_a, entity_b]))
                    entity_cooccurrence[pair] += 1
                    entity_docs[pair].add(doc.id)
        
        # Create patterns from frequent co-occurrences
        new_patterns = []
        for pair, frequency in entity_cooccurrence.items():
            if frequency >= min_frequency:
                pattern_id = hashlib.md5(str(pair).encode()).hexdigest()[:12]
                
                if pattern_id not in self.patterns:
                    entity_names = [
                        self.entities[e].name 
                        for e in pair 
                        if e in self.entities
                    ]
                    
                    pattern = Pattern(
                        id=pattern_id,
                        name=f"{entity_names[0]} + {entity_names[1]}",
                        description=f"Co-occurrence of {entity_names[0]} and {entity_names[1]}",
                        entity_ids=list(pair),
                        document_ids=list(entity_docs[pair]),
                        frequency=frequency,
                        correlation_score=frequency / len(self.documents) if self.documents else 0
                    )
                    
                    self.patterns[pattern_id] = pattern
                    new_patterns.append(pattern)
        
        if new_patterns:
            self._save()
            logger.info(f"Discovered {len(new_patterns)} new patterns")
        
        return new_patterns
    
    # =========================================================================
    # GAP ANALYSIS
    # =========================================================================
    
    def find_knowledge_gaps(self) -> Dict:
        """Identify gaps in the knowledge base."""
        gaps = {
            "underrepresented_models": [],
            "isolated_entities": [],
            "unresolved_contradictions": [],
            "missing_connections": []
        }
        
        # Find mental models with few examples
        model_counts = defaultdict(int)
        for doc in self.documents.values():
            for model_id in doc.mental_models:
                model_counts[model_id] += 1
        
        # Models with less than 3 examples
        for model_id, model in self.entity_extractor.models_data.items():
            if model_counts.get(model_id, 0) < 3:
                gaps["underrepresented_models"].append({
                    "model_id": model_id,
                    "model_name": model["name"],
                    "current_count": model_counts.get(model_id, 0)
                })
        
        # Find isolated entities (only in one document)
        for entity_id, entity in self.entities.items():
            if len(entity.document_ids) == 1:
                gaps["isolated_entities"].append({
                    "entity_id": entity_id,
                    "entity_name": entity.name,
                    "entity_type": entity.entity_type.value
                })
        
        # Unresolved contradictions
        for cont_id, cont in self.contradictions.items():
            if not cont.resolved:
                gaps["unresolved_contradictions"].append({
                    "contradiction_id": cont_id,
                    "claim_a": cont.claim_a[:100],
                    "claim_b": cont.claim_b[:100]
                })
        
        return gaps
    
    # =========================================================================
    # EXPORT
    # =========================================================================
    
    def export_graph(self, format: str = "json") -> str:
        """Export the knowledge graph."""
        if format == "json":
            return json.dumps({
                "nodes": [
                    {"id": e.id, "label": e.name, "type": e.entity_type.value}
                    for e in self.entities.values()
                ],
                "edges": [
                    {
                        "source": r.source_id,
                        "target": r.target_id,
                        "type": r.relation_type.value,
                        "weight": r.weight
                    }
                    for r in self.relationships
                ]
            }, indent=2)
        
        elif format == "graphml":
            # GraphML format for visualization tools
            lines = ['<?xml version="1.0" encoding="UTF-8"?>']
            lines.append('<graphml xmlns="http://graphml.graphdrawing.org/xmlns">')
            lines.append('  <graph id="G" edgedefault="directed">')
            
            for entity in self.entities.values():
                lines.append(f'    <node id="{entity.id}">')
                lines.append(f'      <data key="label">{entity.name}</data>')
                lines.append(f'      <data key="type">{entity.entity_type.value}</data>')
                lines.append('    </node>')
            
            for i, rel in enumerate(self.relationships):
                lines.append(f'    <edge id="e{i}" source="{rel.source_id}" target="{rel.target_id}">')
                lines.append(f'      <data key="type">{rel.relation_type.value}</data>')
                lines.append(f'      <data key="weight">{rel.weight}</data>')
                lines.append('    </edge>')
            
            lines.append('  </graph>')
            lines.append('</graphml>')
            return '\n'.join(lines)
        
        else:
            raise ValueError(f"Unknown format: {format}")
    
    def get_summary(self) -> Dict:
        """Get network summary."""
        return {
            "total_documents": len(self.documents),
            "total_entities": len(self.entities),
            "total_relationships": len(self.relationships),
            "total_contradictions": len(self.contradictions),
            "total_patterns": len(self.patterns),
            "entity_types": {
                t.value: len([e for e in self.entities.values() if e.entity_type == t])
                for t in EntityType
            },
            "unresolved_contradictions": len([c for c in self.contradictions.values() if not c.resolved])
        }


# =============================================================================
# CLI INTERFACE
# =============================================================================

def main():
    """CLI entry point."""
    import argparse
    
    parser = argparse.ArgumentParser(description="Cross-Document Intelligence Network")
    parser.add_argument("command", choices=["summary", "search", "gaps", "export", "patterns"])
    parser.add_argument("--storage", default="./data/network.json", help="Storage file path")
    parser.add_argument("--query", help="Search query")
    parser.add_argument("--format", default="json", help="Export format")
    
    args = parser.parse_args()
    
    network = CrossDocumentNetwork(args.storage)
    
    if args.command == "summary":
        print(json.dumps(network.get_summary(), indent=2))
    elif args.command == "search":
        if args.query:
            results = network.semantic_search(args.query)
            print(json.dumps(results, indent=2))
        else:
            print("Please provide --query")
    elif args.command == "gaps":
        print(json.dumps(network.find_knowledge_gaps(), indent=2))
    elif args.command == "export":
        print(network.export_graph(args.format))
    elif args.command == "patterns":
        patterns = network.discover_patterns()
        print(json.dumps([p.to_dict() for p in patterns], indent=2))


if __name__ == "__main__":
    main()
