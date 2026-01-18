"""
Knowledge Graph and Document Categorization System

Builds a searchable knowledge graph from mental model analysis results.

Features:
1. Document categorization by mental model
2. Tag-based organization
3. Relationship mapping between documents
4. Searchable index
5. Export to various formats (JSON, GraphML, Neo4j)

Architecture:
┌─────────────────────────────────────────────────────────────────────────────┐
│                         KNOWLEDGE GRAPH                                      │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌─────────────┐     ┌─────────────┐     ┌─────────────┐                   │
│  │  Documents  │────▶│   Models    │────▶│  Categories │                   │
│  └─────────────┘     └─────────────┘     └─────────────┘                   │
│         │                  │                    │                           │
│         │                  │                    │                           │
│         ▼                  ▼                    ▼                           │
│  ┌─────────────┐     ┌─────────────┐     ┌─────────────┐                   │
│  │    Tags     │◀───▶│  Insights   │◀───▶│ Lollapalooza│                   │
│  └─────────────┘     └─────────────┘     └─────────────┘                   │
│                                                                              │
│  Relationships:                                                              │
│  - Document APPLIES Model (with relevance score)                            │
│  - Document HAS Tag                                                         │
│  - Document BELONGS_TO Category                                             │
│  - Model RELATED_TO Model                                                   │
│  - Document SIMILAR_TO Document (via shared models)                         │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
"""

import os
import json
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Set, Tuple, Any
from datetime import datetime
from collections import defaultdict
from pathlib import Path
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =============================================================================
# DATA STRUCTURES
# =============================================================================

@dataclass
class Node:
    """A node in the knowledge graph."""
    id: str
    type: str  # "document", "model", "category", "tag", "insight"
    name: str
    properties: Dict = field(default_factory=dict)
    
    def to_dict(self) -> Dict:
        return {
            "id": self.id,
            "type": self.type,
            "name": self.name,
            "properties": self.properties
        }


@dataclass
class Edge:
    """An edge (relationship) in the knowledge graph."""
    source_id: str
    target_id: str
    type: str  # "APPLIES", "HAS_TAG", "BELONGS_TO", "RELATED_TO", "SIMILAR_TO"
    weight: float = 1.0
    properties: Dict = field(default_factory=dict)
    
    def to_dict(self) -> Dict:
        return {
            "source": self.source_id,
            "target": self.target_id,
            "type": self.type,
            "weight": self.weight,
            "properties": self.properties
        }


# =============================================================================
# KNOWLEDGE GRAPH
# =============================================================================

class KnowledgeGraph:
    """
    A knowledge graph built from mental model document analysis.
    
    Usage:
        graph = KnowledgeGraph()
        graph.add_analysis(analysis)  # Add DocumentAnalysis results
        
        # Query
        docs = graph.find_documents_by_model("Network Effects")
        similar = graph.find_similar_documents("doc_id")
        
        # Export
        graph.export_json("knowledge_graph.json")
    """
    
    def __init__(self):
        self.nodes: Dict[str, Node] = {}
        self.edges: List[Edge] = []
        
        # Indexes for fast lookup
        self._document_nodes: Dict[str, Node] = {}
        self._model_nodes: Dict[str, Node] = {}
        self._category_nodes: Dict[str, Node] = {}
        self._tag_nodes: Dict[str, Node] = {}
        
        # Adjacency lists
        self._outgoing: Dict[str, List[Edge]] = defaultdict(list)
        self._incoming: Dict[str, List[Edge]] = defaultdict(list)
        
        # Document-to-model mapping
        self._doc_models: Dict[str, Set[str]] = defaultdict(set)
        self._model_docs: Dict[str, Set[str]] = defaultdict(set)
    
    def _generate_id(self, type: str, name: str) -> str:
        """Generate a unique ID for a node."""
        import hashlib
        return f"{type}_{hashlib.md5(name.encode()).hexdigest()[:8]}"
    
    def add_node(self, type: str, name: str, properties: Dict = None) -> Node:
        """Add a node to the graph."""
        node_id = self._generate_id(type, name)
        
        if node_id in self.nodes:
            # Update properties if node exists
            if properties:
                self.nodes[node_id].properties.update(properties)
            return self.nodes[node_id]
        
        node = Node(
            id=node_id,
            type=type,
            name=name,
            properties=properties or {}
        )
        
        self.nodes[node_id] = node
        
        # Update type-specific indexes
        if type == "document":
            self._document_nodes[node_id] = node
        elif type == "model":
            self._model_nodes[node_id] = node
        elif type == "category":
            self._category_nodes[node_id] = node
        elif type == "tag":
            self._tag_nodes[node_id] = node
        
        return node
    
    def add_edge(self, source_id: str, target_id: str, type: str, 
                 weight: float = 1.0, properties: Dict = None) -> Edge:
        """Add an edge to the graph."""
        edge = Edge(
            source_id=source_id,
            target_id=target_id,
            type=type,
            weight=weight,
            properties=properties or {}
        )
        
        self.edges.append(edge)
        self._outgoing[source_id].append(edge)
        self._incoming[target_id].append(edge)
        
        return edge
    
    def add_analysis(self, analysis) -> None:
        """
        Add a DocumentAnalysis to the knowledge graph.
        
        Args:
            analysis: DocumentAnalysis from model_analyzer.py
        """
        # Add document node
        doc_node = self.add_node(
            type="document",
            name=analysis.document_name,
            properties={
                "path": analysis.document_path,
                "analyzed_at": analysis.analyzed_at.isoformat() if hasattr(analysis.analyzed_at, 'isoformat') else str(analysis.analyzed_at),
                "summary": analysis.summary,
                "num_chunks": analysis.num_chunks
            }
        )
        
        # Add model matches
        for match in analysis.model_matches:
            # Add model node
            model_node = self.add_node(
                type="model",
                name=match.model_name,
                properties={
                    "model_id": match.model_id
                }
            )
            
            # Add APPLIES edge
            self.add_edge(
                source_id=doc_node.id,
                target_id=model_node.id,
                type="APPLIES",
                weight=match.relevance_score,
                properties={
                    "confidence": match.confidence,
                    "evidence": match.evidence,
                    "insights": match.insights
                }
            )
            
            # Update indexes
            self._doc_models[doc_node.id].add(model_node.id)
            self._model_docs[model_node.id].add(doc_node.id)
        
        # Add categories
        for category in analysis.categories:
            if category:
                cat_node = self.add_node(type="category", name=category)
                self.add_edge(
                    source_id=doc_node.id,
                    target_id=cat_node.id,
                    type="BELONGS_TO"
                )
        
        # Add tags
        for tag in analysis.tags:
            if tag:
                tag_node = self.add_node(type="tag", name=tag)
                self.add_edge(
                    source_id=doc_node.id,
                    target_id=tag_node.id,
                    type="HAS_TAG"
                )
        
        # Add Lollapalooza insights
        for alert in analysis.lollapalooza_alerts:
            insight_node = self.add_node(
                type="insight",
                name=f"Lollapalooza: {', '.join(alert.models[:3])}",
                properties={
                    "models": alert.models,
                    "convergence_score": alert.convergence_score,
                    "description": alert.description,
                    "implications": alert.implications
                }
            )
            self.add_edge(
                source_id=doc_node.id,
                target_id=insight_node.id,
                type="HAS_INSIGHT",
                weight=alert.convergence_score
            )
    
    def compute_document_similarity(self) -> None:
        """Compute similarity between documents based on shared models."""
        doc_ids = list(self._document_nodes.keys())
        
        for i, doc1_id in enumerate(doc_ids):
            for doc2_id in doc_ids[i+1:]:
                models1 = self._doc_models[doc1_id]
                models2 = self._doc_models[doc2_id]
                
                if not models1 or not models2:
                    continue
                
                # Jaccard similarity
                intersection = len(models1 & models2)
                union = len(models1 | models2)
                
                if union > 0:
                    similarity = intersection / union
                    
                    if similarity >= 0.3:  # Only add significant similarities
                        self.add_edge(
                            source_id=doc1_id,
                            target_id=doc2_id,
                            type="SIMILAR_TO",
                            weight=similarity,
                            properties={
                                "shared_models": list(models1 & models2)
                            }
                        )
    
    # =========================================================================
    # QUERY METHODS
    # =========================================================================
    
    def find_documents_by_model(self, model_name: str) -> List[Dict]:
        """Find all documents that apply a specific model."""
        model_id = self._generate_id("model", model_name)
        
        if model_id not in self._model_docs:
            return []
        
        results = []
        for doc_id in self._model_docs[model_id]:
            doc_node = self._document_nodes.get(doc_id)
            if doc_node:
                # Find the edge to get relevance
                for edge in self._outgoing[doc_id]:
                    if edge.target_id == model_id:
                        results.append({
                            "document": doc_node.name,
                            "path": doc_node.properties.get("path", ""),
                            "relevance": edge.weight,
                            "evidence": edge.properties.get("evidence", ""),
                            "insights": edge.properties.get("insights", [])
                        })
                        break
        
        return sorted(results, key=lambda x: x["relevance"], reverse=True)
    
    def find_documents_by_tag(self, tag: str) -> List[Dict]:
        """Find all documents with a specific tag."""
        tag_id = self._generate_id("tag", tag)
        
        results = []
        for edge in self._incoming.get(tag_id, []):
            if edge.type == "HAS_TAG":
                doc_node = self._document_nodes.get(edge.source_id)
                if doc_node:
                    results.append({
                        "document": doc_node.name,
                        "path": doc_node.properties.get("path", ""),
                        "summary": doc_node.properties.get("summary", "")
                    })
        
        return results
    
    def find_documents_by_category(self, category: str) -> List[Dict]:
        """Find all documents in a category."""
        cat_id = self._generate_id("category", category)
        
        results = []
        for edge in self._incoming.get(cat_id, []):
            if edge.type == "BELONGS_TO":
                doc_node = self._document_nodes.get(edge.source_id)
                if doc_node:
                    results.append({
                        "document": doc_node.name,
                        "path": doc_node.properties.get("path", ""),
                        "summary": doc_node.properties.get("summary", "")
                    })
        
        return results
    
    def find_similar_documents(self, document_name: str, min_similarity: float = 0.3) -> List[Dict]:
        """Find documents similar to a given document."""
        # Try direct doc_id first (for test compatibility), then generate from name
        if document_name in self._document_nodes:
            doc_id = document_name
        else:
            doc_id = self._generate_id("document", document_name)
        
        results = []
        
        # Check outgoing SIMILAR_TO edges
        for edge in self._outgoing.get(doc_id, []):
            if edge.type == "SIMILAR_TO" and edge.weight >= min_similarity:
                other_doc = self._document_nodes.get(edge.target_id)
                if other_doc:
                    results.append({
                        "doc_id": edge.target_id,
                        "document": other_doc.name,
                        "similarity": edge.weight,
                        "shared_models": edge.properties.get("shared_models", [])
                    })
        
        # Check incoming SIMILAR_TO edges
        for edge in self._incoming.get(doc_id, []):
            if edge.type == "SIMILAR_TO" and edge.weight >= min_similarity:
                other_doc = self._document_nodes.get(edge.source_id)
                if other_doc:
                    results.append({
                        "doc_id": edge.source_id,
                        "document": other_doc.name,
                        "similarity": edge.weight,
                        "shared_models": edge.properties.get("shared_models", [])
                    })
        
        # If no SIMILAR_TO edges, compute similarity on the fly based on shared models
        if not results:
            my_models = self._doc_models.get(doc_id, set())
            if my_models:
                for other_doc_id, other_models in self._doc_models.items():
                    if other_doc_id != doc_id and other_models:
                        intersection = len(my_models & other_models)
                        union = len(my_models | other_models)
                        if union > 0:
                            similarity = intersection / union
                            if similarity >= min_similarity:
                                other_doc = self._document_nodes.get(other_doc_id)
                                if other_doc:
                                    results.append({
                                        "doc_id": other_doc_id,
                                        "document": other_doc.name,
                                        "similarity": similarity,
                                        "shared_models": list(my_models & other_models)
                                    })
        
        return sorted(results, key=lambda x: x["similarity"], reverse=True)
    
    def find_lollapalooza_documents(self, min_score: float = 0.7) -> List[Dict]:
        """Find documents with strong Lollapalooza effects."""
        results = []
        
        for edge in self.edges:
            if edge.type == "HAS_INSIGHT" and edge.weight >= min_score:
                doc_node = self._document_nodes.get(edge.source_id)
                insight_node = self.nodes.get(edge.target_id)
                
                if doc_node and insight_node:
                    results.append({
                        "document": doc_node.name,
                        "convergence_score": edge.weight,
                        "models": insight_node.properties.get("models", []),
                        "description": insight_node.properties.get("description", ""),
                        "implications": insight_node.properties.get("implications", [])
                    })
        
        return sorted(results, key=lambda x: x["convergence_score"], reverse=True)
    
    def get_model_statistics(self) -> List[Dict]:
        """Get statistics about model usage across documents."""
        stats = []
        
        for model_id, model_node in self._model_nodes.items():
            doc_count = len(self._model_docs.get(model_id, set()))
            
            # Calculate average relevance
            total_relevance = 0
            for doc_id in self._model_docs.get(model_id, set()):
                for edge in self._outgoing.get(doc_id, []):
                    if edge.target_id == model_id:
                        total_relevance += edge.weight
                        break
            
            avg_relevance = total_relevance / doc_count if doc_count > 0 else 0
            
            stats.append({
                "model": model_node.name,
                "document_count": doc_count,
                "avg_relevance": avg_relevance
            })
        
        return sorted(stats, key=lambda x: x["document_count"], reverse=True)
    
    def search(self, query: str, types: List[str] = None) -> List[Dict]:
        """
        Search the knowledge graph.
        
        Args:
            query: Search query (searches names and properties)
            types: Node types to search (default: all)
        
        Returns:
            List of matching nodes with context
        """
        query_lower = query.lower()
        types = types or ["document", "model", "category", "tag", "insight"]
        
        results = []
        
        for node in self.nodes.values():
            if node.type not in types:
                continue
            
            # Search in name
            if query_lower in node.name.lower():
                results.append({
                    "type": node.type,
                    "name": node.name,
                    "title": node.name,  # Alias for test compatibility
                    "match_field": "name",
                    "properties": node.properties
                })
                continue
            
            # Search in properties
            for key, value in node.properties.items():
                if isinstance(value, str) and query_lower in value.lower():
                    results.append({
                        "type": node.type,
                        "name": node.name,
                        "title": node.name,  # Alias for test compatibility
                        "match_field": key,
                        "match_value": value[:200],
                        "properties": node.properties
                    })
                    break
        
        return results
    
    # =========================================================================
    # EXPORT METHODS
    # =========================================================================
    
    def export_json(self, output_path: str) -> str:
        """Export graph to JSON format."""
        export_data = {
            "exported_at": datetime.now().isoformat(),
            "statistics": {
                "total_nodes": len(self.nodes),
                "total_edges": len(self.edges),
                "documents": len(self._document_nodes),
                "models": len(self._model_nodes),
                "categories": len(self._category_nodes),
                "tags": len(self._tag_nodes)
            },
            "nodes": [n.to_dict() for n in self.nodes.values()],
            "edges": [e.to_dict() for e in self.edges]
        }
        
        with open(output_path, 'w') as f:
            json.dump(export_data, f, indent=2, default=str)
        
        return output_path
    
    def export_graphml(self, output_path: str) -> str:
        """Export graph to GraphML format (for visualization tools)."""
        lines = [
            '<?xml version="1.0" encoding="UTF-8"?>',
            '<graphml xmlns="http://graphml.graphdrawing.org/xmlns">',
            '  <key id="type" for="node" attr.name="type" attr.type="string"/>',
            '  <key id="name" for="node" attr.name="name" attr.type="string"/>',
            '  <key id="weight" for="edge" attr.name="weight" attr.type="double"/>',
            '  <key id="edge_type" for="edge" attr.name="edge_type" attr.type="string"/>',
            '  <graph id="G" edgedefault="directed">'
        ]
        
        # Add nodes
        for node in self.nodes.values():
            lines.append(f'    <node id="{node.id}">')
            lines.append(f'      <data key="type">{node.type}</data>')
            lines.append(f'      <data key="name">{node.name}</data>')
            lines.append('    </node>')
        
        # Add edges
        for i, edge in enumerate(self.edges):
            lines.append(f'    <edge id="e{i}" source="{edge.source_id}" target="{edge.target_id}">')
            lines.append(f'      <data key="weight">{edge.weight}</data>')
            lines.append(f'      <data key="edge_type">{edge.type}</data>')
            lines.append('    </edge>')
        
        lines.append('  </graph>')
        lines.append('</graphml>')
        
        with open(output_path, 'w') as f:
            f.write('\n'.join(lines))
        
        return output_path
    
    def export_neo4j_cypher(self, output_path: str) -> str:
        """Export graph as Neo4j Cypher statements."""
        lines = [
            "// Mental Models Knowledge Graph",
            f"// Generated: {datetime.now().isoformat()}",
            "",
            "// Create nodes"
        ]
        
        for node in self.nodes.values():
            props = json.dumps(node.properties)
            lines.append(
                f'CREATE (n:{node.type.capitalize()} {{id: "{node.id}", name: "{node.name}", properties: {props}}});'
            )
        
        lines.append("")
        lines.append("// Create relationships")
        
        for edge in self.edges:
            lines.append(
                f'MATCH (a {{id: "{edge.source_id}"}}), (b {{id: "{edge.target_id}"}}) '
                f'CREATE (a)-[:{edge.type} {{weight: {edge.weight}}}]->(b);'
            )
        
        with open(output_path, 'w') as f:
            f.write('\n'.join(lines))
        
        return output_path
    
    def get_summary(self) -> Dict:
        """Get a summary of the knowledge graph."""
        return {
            "total_nodes": len(self.nodes),
            "total_edges": len(self.edges),
            "node_counts": {
                "documents": len(self._document_nodes),
                "models": len(self._model_nodes),
                "categories": len(self._category_nodes),
                "tags": len(self._tag_nodes)
            },
            "edge_counts": self._count_edges_by_type(),
            "top_models": self.get_model_statistics()[:10],
            "lollapalooza_count": len(self.find_lollapalooza_documents())
        }
    
    def _count_edges_by_type(self) -> Dict[str, int]:
        """Count edges by type."""
        counts = defaultdict(int)
        for edge in self.edges:
            counts[edge.type] += 1
        return dict(counts)
    
    # =========================================================================
    # CONVENIENCE METHODS (for test compatibility)
    # =========================================================================
    
    def node_count(self) -> int:
        """Return the number of nodes in the graph."""
        return len(self.nodes)
    
    def edge_count(self) -> int:
        """Return the number of edges in the graph."""
        return len(self.edges)
    
    def has_node(self, node_id: str) -> bool:
        """Check if a node exists in the graph."""
        return node_id in self.nodes
    
    def add_document(self, doc_id: str, title: str, path: str, metadata: Dict = None) -> Node:
        """Add a document node to the graph."""
        node = Node(
            id=doc_id,
            type="document",
            name=title,
            properties={"path": path, **(metadata or {})}
        )
        self.nodes[doc_id] = node
        self._document_nodes[doc_id] = node
        return node
    
    def add_model(self, model_id_or_obj=None, name: str = None, category: str = None, model_id: int = None) -> Node:
        """
        Add a model node to the graph.
        
        Args:
            model_id_or_obj: Either an integer model ID or a MentalModel object (positional)
            model_id: Model ID (keyword argument, for backward compatibility)
            name: Model name (required if using model_id keyword)
            category: Model category (required if using model_id keyword)
        
        Returns:
            Node object
        """
        # Handle keyword argument model_id for backward compatibility
        if model_id is not None:
            model_id_or_obj = model_id
        
        if model_id_or_obj is None:
            raise ValueError("Either model_id_or_obj or model_id must be provided")
        
        # Handle both signatures: add_model(model_obj) and add_model(id, name, category)
        if isinstance(model_id_or_obj, int):
            model_id_value = model_id_or_obj
            if name is None or category is None:
                raise ValueError("name and category are required when model_id_or_obj is an integer")
        else:
            # It's a model object
            model = model_id_or_obj
            model_id_value = int(model.id) if hasattr(model, 'id') else hash(model.name)
            name = model.name if hasattr(model, 'name') else str(model)
            category = model.category if hasattr(model, 'category') else "Unknown"
        
        node_id = f"model_{model_id_value}"
        node = Node(
            id=node_id,
            type="model",
            name=name,
            properties={"model_id": model_id_value, "category": category}
        )
        self.nodes[node_id] = node
        self._model_nodes[node_id] = node
        return node
    
    def link_document_to_model(self, doc_id: str, model_id: int, relevance: float = 1.0) -> Edge:
        """Link a document to a model with a relevance score."""
        model_node_id = f"model_{model_id}"
        edge = self.add_edge(
            source_id=doc_id,
            target_id=model_node_id,
            type="APPLIES",
            weight=relevance
        )
        self._doc_models[doc_id].add(model_node_id)
        self._model_docs[model_node_id].add(doc_id)
        return edge
    
    def get_documents_by_model(self, model_id: int) -> List[Dict]:
        """Get all documents that reference a model."""
        model_node_id = f"model_{model_id}"
        results = []
        for doc_id in self._model_docs.get(model_node_id, set()):
            doc_node = self._document_nodes.get(doc_id)
            if doc_node:
                relevance = 0.0
                for edge in self._outgoing.get(doc_id, []):
                    if edge.target_id == model_node_id:
                        relevance = edge.weight
                        break
                results.append({
                    "doc_id": doc_id,
                    "title": doc_node.name,
                    "path": doc_node.properties.get("path", ""),
                    "relevance": relevance
                })
        return sorted(results, key=lambda x: x["relevance"], reverse=True)
    
    def get_models_by_document(self, doc_id: str) -> List[Dict]:
        """Get all models referenced by a document."""
        results = []
        for model_node_id in self._doc_models.get(doc_id, set()):
            model_node = self._model_nodes.get(model_node_id)
            if model_node:
                relevance = 0.0
                for edge in self._outgoing.get(doc_id, []):
                    if edge.target_id == model_node_id:
                        relevance = edge.weight
                        break
                results.append({
                    "model_id": model_node.properties.get("model_id"),
                    "name": model_node.name,
                    "category": model_node.properties.get("category", ""),
                    "relevance": relevance
                })
        return sorted(results, key=lambda x: x["relevance"], reverse=True)
    
    def detect_lollapalooza_clusters(self, min_models: int = 3) -> List[Dict]:
        """Detect documents with multiple models (Lollapalooza effect)."""
        clusters = []
        for doc_id, model_ids in self._doc_models.items():
            if len(model_ids) >= min_models:
                doc_node = self._document_nodes.get(doc_id)
                if doc_node:
                    models = []
                    for model_id in model_ids:
                        model_node = self._model_nodes.get(model_id)
                        if model_node:
                            models.append(model_node.name)
                    clusters.append({
                        "document": doc_id,
                        "title": doc_node.name,
                        "model_count": len(model_ids),
                        "models": models
                    })
        return sorted(clusters, key=lambda x: x["model_count"], reverse=True)
    
    def get_statistics(self) -> Dict:
        """Get graph statistics."""
        return {
            "total_nodes": len(self.nodes),
            "total_edges": len(self.edges),
            "document_count": len(self._document_nodes),
            "model_count": len(self._model_nodes),
            "category_count": len(self._category_nodes),
            "tag_count": len(self._tag_nodes)
        }
    
    def save(self, path: str) -> None:
        """Save graph to JSON file."""
        self.export_json(path)
    
    def load(self, path: str) -> None:
        """Load graph from JSON file."""
        with open(path, 'r') as f:
            data = json.load(f)
        
        # Clear existing data
        self.nodes.clear()
        self.edges.clear()
        self._document_nodes.clear()
        self._model_nodes.clear()
        self._category_nodes.clear()
        self._tag_nodes.clear()
        self._outgoing.clear()
        self._incoming.clear()
        self._doc_models.clear()
        self._model_docs.clear()
        
        # Load nodes
        for node_data in data.get("nodes", []):
            node = Node(
                id=node_data["id"],
                type=node_data["type"],
                name=node_data["name"],
                properties=node_data.get("properties", {})
            )
            self.nodes[node.id] = node
            if node.type == "document":
                self._document_nodes[node.id] = node
            elif node.type == "model":
                self._model_nodes[node.id] = node
            elif node.type == "category":
                self._category_nodes[node.id] = node
            elif node.type == "tag":
                self._tag_nodes[node.id] = node
        
        # Load edges
        for edge_data in data.get("edges", []):
            edge = Edge(
                source_id=edge_data["source"],
                target_id=edge_data["target"],
                type=edge_data["type"],
                weight=edge_data.get("weight", 1.0),
                properties=edge_data.get("properties", {})
            )
            self.edges.append(edge)
            self._outgoing[edge.source_id].append(edge)
            self._incoming[edge.target_id].append(edge)
            
            # Rebuild doc-model mappings
            if edge.type == "APPLIES":
                self._doc_models[edge.source_id].add(edge.target_id)
                self._model_docs[edge.target_id].add(edge.source_id)
    
    def get_model_frequency(self) -> Dict[int, int]:
        """Get frequency of each model across documents."""
        freq = {}
        for model_node_id, doc_ids in self._model_docs.items():
            model_node = self._model_nodes.get(model_node_id)
            if model_node:
                model_id = model_node.properties.get("model_id")
                if model_id is not None:
                    freq[model_id] = len(doc_ids)
        return freq
    
    def get_category_distribution(self) -> Dict[str, int]:
        """Get distribution of models by category."""
        dist = defaultdict(int)
        for model_node in self._model_nodes.values():
            category = model_node.properties.get("category", "Unknown")
            dist[category] += 1
        return dict(dist)
    
    # =========================================================================
    # GRAPH TRAVERSAL METHODS
    # =========================================================================
    
    def find_related_models(self, model_name: str, max_distance: int = 2) -> List[Dict]:
        """
        Find models related to a given model through document co-occurrence.
        
        Args:
            model_name: Name of the model to find relations for
            max_distance: Maximum distance in the graph (1 = direct, 2 = through one hop)
        
        Returns:
            List of related models with relationship strength
        """
        # Find the model node by name (search through all model nodes)
        model_id = None
        for node_id, node in self._model_nodes.items():
            if node.name == model_name:
                model_id = node_id
                break
        
        if model_id is None:
            return []
        
        # Find documents that use this model
        docs_with_model = self._model_docs.get(model_id, set())
        
        # Find other models that appear in the same documents
        related_models = defaultdict(int)
        for doc_id in docs_with_model:
            other_models = self._doc_models.get(doc_id, set())
            for other_model_id in other_models:
                if other_model_id != model_id:
                    related_models[other_model_id] += 1
        
        # If no documents exist, find related models by category
        if not docs_with_model:
            model_node = self._model_nodes.get(model_id)
            if model_node:
                model_category = model_node.properties.get('category')
                # Find other models in the same category
                for other_model_id, other_node in self._model_nodes.items():
                    if other_model_id != model_id:
                        other_category = other_node.properties.get('category')
                        if other_category == model_category:
                            related_models[other_model_id] += 2
        
        # If max_distance > 1, also find models related to related models
        if max_distance > 1:
            second_degree = defaultdict(int)
            for related_model_id in related_models.keys():
                related_docs = self._model_docs.get(related_model_id, set())
                for doc_id in related_docs:
                    other_models = self._doc_models.get(doc_id, set())
                    for other_model_id in other_models:
                        if other_model_id != model_id and other_model_id not in related_models:
                            second_degree[other_model_id] += 1
            
            # Add second-degree relations with lower weight
            for model_id_2, count in second_degree.items():
                related_models[model_id_2] = count * 0.5
        
        # Convert to result format
        results = []
        for related_model_id, strength in related_models.items():
            model_node = self._model_nodes.get(related_model_id)
            if model_node:
                results.append({
                    "model_id": model_node.properties.get("model_id"),
                    "name": model_node.name,
                    "category": model_node.properties.get("category", ""),
                    "relationship_strength": strength,
                    "shared_documents": int(strength) if strength >= 1 else 1
                })
        
        return sorted(results, key=lambda x: x["relationship_strength"], reverse=True)
    
    def find_shortest_path(self, model_name_1: str, model_name_2: str) -> Optional[List[str]]:
        """
        Find the shortest path between two models through documents.
        
        Args:
            model_name_1: First model name
            model_name_2: Second model name
        
        Returns:
            List of model names representing the path, or None if no path exists
        """
        from collections import deque
        
        model_id_1 = self._generate_id("model", model_name_1)
        model_id_2 = self._generate_id("model", model_name_2)
        
        if model_id_1 not in self._model_nodes or model_id_2 not in self._model_nodes:
            return None
        
        if model_id_1 == model_id_2:
            return [model_name_1]
        
        # BFS to find shortest path
        queue = deque([(model_id_1, [model_id_1])])
        visited = {model_id_1}
        
        while queue:
            current_id, path = queue.popleft()
            
            # Find documents that use this model
            docs = self._model_docs.get(current_id, set())
            
            # Find other models in those documents
            for doc_id in docs:
                other_models = self._doc_models.get(doc_id, set())
                for other_model_id in other_models:
                    if other_model_id == model_id_2:
                        # Found the target
                        final_path = path + [other_model_id]
                        return [self._model_nodes[mid].name for mid in final_path]
                    
                    if other_model_id not in visited:
                        visited.add(other_model_id)
                        queue.append((other_model_id, path + [other_model_id]))
        
        return None
    
    def get_most_central_models(self, top_n: int = 10) -> List[Dict]:
        """
        Get the most central models in the graph based on document connections.
        
        Args:
            top_n: Number of top models to return
        
        Returns:
            List of models with centrality scores
        """
        # Calculate degree centrality (number of documents)
        centrality = []
        for model_id, model_node in self._model_nodes.items():
            doc_count = len(self._model_docs.get(model_id, set()))
            
            # Also count connections to other models
            connected_models = set()
            for doc_id in self._model_docs.get(model_id, set()):
                other_models = self._doc_models.get(doc_id, set())
                connected_models.update(other_models - {model_id})
            
            centrality.append({
                "model_id": model_node.properties.get("model_id"),
                "name": model_node.name,
                "category": model_node.properties.get("category", ""),
                "document_count": doc_count,
                "connected_models": len(connected_models),
                "centrality_score": doc_count + len(connected_models) * 0.5
            })
        
        return sorted(centrality, key=lambda x: x["centrality_score"], reverse=True)[:top_n]


# =============================================================================
# DOCUMENT CATEGORIZER
# =============================================================================

class DocumentCategorizer:
    """
    Categorize documents based on mental model analysis.
    
    Provides hierarchical categorization:
    - Primary category (dominant theme)
    - Secondary categories
    - Tags (specific topics)
    - Model-based groupings
    """
    
    def __init__(self, knowledge_graph: KnowledgeGraph = None):
        self.graph = knowledge_graph or KnowledgeGraph()
        
        # Category hierarchy
        self.category_hierarchy = {
            "Investment": ["Value Investing", "Growth Investing", "Risk Management", "Portfolio Theory"],
            "Business": ["Strategy", "Operations", "Marketing", "Finance", "Leadership"],
            "Psychology": ["Cognitive Biases", "Decision Making", "Behavioral Economics", "Social Psychology"],
            "Technology": ["Software", "Hardware", "AI/ML", "Networks", "Platforms"],
            "Economics": ["Macro", "Micro", "Markets", "Trade", "Policy"],
            "Science": ["Physics", "Biology", "Chemistry", "Mathematics"],
            "History": ["Business History", "Economic History", "Political History"],
            "Philosophy": ["Ethics", "Epistemology", "Logic", "Wisdom"]
        }
    
    def categorize_by_models(self, document_name: str) -> Dict:
        """
        Categorize a document based on its mental model matches.
        
        Returns category assignments with confidence scores.
        """
        doc_id = self.graph._generate_id("document", document_name)
        
        if doc_id not in self.graph._document_nodes:
            return {"error": "Document not found"}
        
        # Get model matches for this document
        model_categories = defaultdict(float)
        
        for edge in self.graph._outgoing.get(doc_id, []):
            if edge.type == "APPLIES":
                model_node = self.graph.nodes.get(edge.target_id)
                if model_node:
                    # Map model to category based on model properties
                    model_id = model_node.properties.get("model_id", "")
                    
                    # Determine category from model
                    category = self._model_to_category(model_node.name)
                    model_categories[category] += edge.weight
        
        # Sort by total relevance
        sorted_categories = sorted(model_categories.items(), key=lambda x: x[1], reverse=True)
        
        return {
            "document": document_name,
            "primary_category": sorted_categories[0][0] if sorted_categories else "Uncategorized",
            "primary_score": sorted_categories[0][1] if sorted_categories else 0,
            "secondary_categories": [
                {"category": cat, "score": score}
                for cat, score in sorted_categories[1:4]
            ],
            "all_categories": dict(sorted_categories)
        }
    
    def _model_to_category(self, model_name: str) -> str:
        """Map a mental model to a high-level category."""
        model_lower = model_name.lower()
        
        # Investment models
        if any(term in model_lower for term in ["moat", "margin of safety", "intrinsic value", "compound"]):
            return "Investment"
        
        # Psychology models
        if any(term in model_lower for term in ["bias", "incentive", "social proof", "authority", "reciprocity"]):
            return "Psychology"
        
        # Business models
        if any(term in model_lower for term in ["network effect", "scale", "brand", "switching cost"]):
            return "Business"
        
        # Economics models
        if any(term in model_lower for term in ["supply", "demand", "opportunity cost", "comparative advantage"]):
            return "Economics"
        
        # Science models
        if any(term in model_lower for term in ["entropy", "evolution", "feedback", "equilibrium"]):
            return "Science"
        
        # Thinking models
        if any(term in model_lower for term in ["inversion", "first principles", "second-order", "probabilistic"]):
            return "Philosophy"
        
        return "General"
    
    def get_category_summary(self) -> Dict:
        """Get summary of documents by category."""
        category_docs = defaultdict(list)
        
        for doc_id, doc_node in self.graph._document_nodes.items():
            categorization = self.categorize_by_models(doc_node.name)
            primary = categorization.get("primary_category", "Uncategorized")
            category_docs[primary].append({
                "document": doc_node.name,
                "score": categorization.get("primary_score", 0)
            })
        
        return {
            "categories": {
                cat: {
                    "count": len(docs),
                    "documents": sorted(docs, key=lambda x: x["score"], reverse=True)
                }
                for cat, docs in category_docs.items()
            },
            "total_documents": len(self.graph._document_nodes)
        }


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def create_knowledge_graph() -> KnowledgeGraph:
    """Create a new knowledge graph."""
    return KnowledgeGraph()


def build_graph_from_analyses(analyses: List) -> KnowledgeGraph:
    """Build a knowledge graph from a list of DocumentAnalysis objects."""
    graph = KnowledgeGraph()
    for analysis in analyses:
        graph.add_analysis(analysis)
    graph.compute_document_similarity()
    return graph


# =============================================================================
# CLI
# =============================================================================

if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description="Knowledge Graph Manager")
    parser.add_argument("--load", help="Load graph from JSON file")
    parser.add_argument("--search", help="Search the graph")
    parser.add_argument("--model", help="Find documents by model")
    parser.add_argument("--export", help="Export format (json, graphml, neo4j)")
    parser.add_argument("--output", help="Output file path")
    
    args = parser.parse_args()
    
    graph = KnowledgeGraph()
    
    if args.load:
        with open(args.load, 'r') as f:
            data = json.load(f)
        for node_data in data.get("nodes", []):
            graph.add_node(
                type=node_data["type"],
                name=node_data["name"],
                properties=node_data.get("properties", {})
            )
        for edge_data in data.get("edges", []):
            graph.add_edge(
                source_id=edge_data["source"],
                target_id=edge_data["target"],
                type=edge_data["type"],
                weight=edge_data.get("weight", 1.0),
                properties=edge_data.get("properties", {})
            )
        print(f"Loaded graph with {len(graph.nodes)} nodes and {len(graph.edges)} edges")
    
    if args.search:
        results = graph.search(args.search)
        print(f"\nSearch results for '{args.search}':")
        for r in results[:10]:
            print(f"  - [{r['type']}] {r['name']}")
    
    if args.model:
        docs = graph.find_documents_by_model(args.model)
        print(f"\nDocuments applying '{args.model}':")
        for d in docs[:10]:
            print(f"  - {d['document']} (relevance: {d['relevance']:.2f})")
    
    if args.export and args.output:
        if args.export == "json":
            graph.export_json(args.output)
        elif args.export == "graphml":
            graph.export_graphml(args.output)
        elif args.export == "neo4j":
            graph.export_neo4j_cypher(args.output)
        print(f"Exported to {args.output}")

    # =========================================================================
