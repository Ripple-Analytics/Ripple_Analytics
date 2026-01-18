"""
Unit tests for knowledge graph module.
"""

import pytest
from unittest.mock import MagicMock
import json
import tempfile
from pathlib import Path

import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))


class TestKnowledgeGraph:
    """Tests for KnowledgeGraph class."""
    
    def test_create_empty_graph(self):
        """Test creating an empty knowledge graph."""
        from analysis.knowledge_graph import KnowledgeGraph
        
        graph = KnowledgeGraph()
        
        assert graph.node_count() == 0
        assert graph.edge_count() == 0
    
    def test_add_document_node(self):
        """Test adding a document node."""
        from analysis.knowledge_graph import KnowledgeGraph
        
        graph = KnowledgeGraph()
        graph.add_document(
            doc_id="doc1",
            title="Test Document",
            path="/path/to/doc.pdf",
            metadata={"author": "Test Author"}
        )
        
        assert graph.node_count() == 1
        assert graph.has_node("doc1")
    
    def test_add_model_node(self):
        """Test adding a mental model node."""
        from analysis.knowledge_graph import KnowledgeGraph
        
        graph = KnowledgeGraph()
        graph.add_model(
            model_id=1,
            name="Incentive-Caused Bias",
            category="Psychology"
        )
        
        assert graph.has_node("model_1")
    
    def test_link_document_to_model(self):
        """Test linking document to model."""
        from analysis.knowledge_graph import KnowledgeGraph
        
        graph = KnowledgeGraph()
        
        # Add nodes
        graph.add_document("doc1", "Test Doc", "/path")
        graph.add_model(1, "Test Model", "Psychology")
        
        # Link them
        graph.link_document_to_model("doc1", 1, relevance=0.9)
        
        assert graph.edge_count() == 1
    
    def test_get_documents_by_model(self):
        """Test getting documents that reference a model."""
        from analysis.knowledge_graph import KnowledgeGraph
        
        graph = KnowledgeGraph()
        
        # Add documents and model
        graph.add_document("doc1", "Doc 1", "/path1")
        graph.add_document("doc2", "Doc 2", "/path2")
        graph.add_model(1, "Test Model", "Psychology")
        
        # Link documents to model
        graph.link_document_to_model("doc1", 1, relevance=0.9)
        graph.link_document_to_model("doc2", 1, relevance=0.7)
        
        docs = graph.get_documents_by_model(1)
        
        assert len(docs) == 2
    
    def test_get_models_by_document(self):
        """Test getting models referenced by a document."""
        from analysis.knowledge_graph import KnowledgeGraph
        
        graph = KnowledgeGraph()
        
        # Add document and models
        graph.add_document("doc1", "Test Doc", "/path")
        graph.add_model(1, "Model 1", "Psychology")
        graph.add_model(2, "Model 2", "Economics")
        
        # Link document to models
        graph.link_document_to_model("doc1", 1, relevance=0.9)
        graph.link_document_to_model("doc1", 2, relevance=0.8)
        
        models = graph.get_models_by_document("doc1")
        
        assert len(models) == 2
    
    def test_find_similar_documents(self):
        """Test finding similar documents based on shared models."""
        from analysis.knowledge_graph import KnowledgeGraph
        
        graph = KnowledgeGraph()
        
        # Add documents and models
        graph.add_document("doc1", "Doc 1", "/path1")
        graph.add_document("doc2", "Doc 2", "/path2")
        graph.add_document("doc3", "Doc 3", "/path3")
        graph.add_model(1, "Model 1", "Psychology")
        graph.add_model(2, "Model 2", "Economics")
        
        # doc1 and doc2 share model 1
        graph.link_document_to_model("doc1", 1, relevance=0.9)
        graph.link_document_to_model("doc2", 1, relevance=0.8)
        graph.link_document_to_model("doc3", 2, relevance=0.7)
        
        similar = graph.find_similar_documents("doc1")
        
        assert "doc2" in [d["doc_id"] for d in similar]
    
    def test_detect_lollapalooza_clusters(self):
        """Test detecting Lollapalooza clusters."""
        from analysis.knowledge_graph import KnowledgeGraph
        
        graph = KnowledgeGraph()
        
        # Add document and multiple models
        graph.add_document("doc1", "Complex Doc", "/path")
        for i in range(1, 6):
            graph.add_model(i, f"Model {i}", "Psychology")
            graph.link_document_to_model("doc1", i, relevance=0.8)
        
        clusters = graph.detect_lollapalooza_clusters(min_models=3)
        
        assert len(clusters) >= 1
        assert "doc1" in [c["document"] for c in clusters]
    
    def test_export_to_json(self):
        """Test exporting graph to JSON."""
        from analysis.knowledge_graph import KnowledgeGraph
        
        graph = KnowledgeGraph()
        graph.add_document("doc1", "Test Doc", "/path")
        graph.add_model(1, "Test Model", "Psychology")
        graph.link_document_to_model("doc1", 1, relevance=0.9)
        
        with tempfile.NamedTemporaryFile(suffix=".json", delete=False) as f:
            graph.export_json(f.name)
            
            # Read and verify
            with open(f.name) as rf:
                data = json.load(rf)
            
            assert "nodes" in data
            assert "edges" in data
    
    def test_export_to_graphml(self):
        """Test exporting graph to GraphML."""
        from analysis.knowledge_graph import KnowledgeGraph
        
        graph = KnowledgeGraph()
        graph.add_document("doc1", "Test Doc", "/path")
        graph.add_model(1, "Test Model", "Psychology")
        
        with tempfile.NamedTemporaryFile(suffix=".graphml", delete=False) as f:
            graph.export_graphml(f.name)
            
            # Verify file exists and has content
            content = Path(f.name).read_text()
            assert "graphml" in content.lower()
    
    def test_search_nodes(self):
        """Test searching nodes by text."""
        from analysis.knowledge_graph import KnowledgeGraph
        
        graph = KnowledgeGraph()
        graph.add_document("doc1", "Investment Strategy", "/path1")
        graph.add_document("doc2", "Marketing Plan", "/path2")
        graph.add_model(1, "Incentive-Caused Bias", "Psychology")
        
        results = graph.search("Investment")
        
        assert len(results) >= 1
        assert any("Investment" in r.get("title", "") for r in results)
    
    def test_get_statistics(self):
        """Test getting graph statistics."""
        from analysis.knowledge_graph import KnowledgeGraph
        
        graph = KnowledgeGraph()
        graph.add_document("doc1", "Doc 1", "/path1")
        graph.add_document("doc2", "Doc 2", "/path2")
        graph.add_model(1, "Model 1", "Psychology")
        graph.link_document_to_model("doc1", 1, relevance=0.9)
        
        stats = graph.get_statistics()
        
        assert stats["total_nodes"] == 3
        assert stats["total_edges"] == 1
        assert stats["document_count"] == 2
        assert stats["model_count"] == 1


class TestGraphPersistence:
    """Tests for graph persistence."""
    
    def test_save_and_load(self):
        """Test saving and loading graph."""
        from analysis.knowledge_graph import KnowledgeGraph
        
        # Create graph
        graph1 = KnowledgeGraph()
        graph1.add_document("doc1", "Test Doc", "/path")
        graph1.add_model(1, "Test Model", "Psychology")
        graph1.link_document_to_model("doc1", 1, relevance=0.9)
        
        with tempfile.NamedTemporaryFile(suffix=".json", delete=False) as f:
            # Save
            graph1.save(f.name)
            
            # Load into new graph
            graph2 = KnowledgeGraph()
            graph2.load(f.name)
            
            assert graph2.node_count() == graph1.node_count()
            assert graph2.edge_count() == graph1.edge_count()


class TestGraphQueries:
    """Tests for graph query operations."""
    
    def test_get_model_frequency(self):
        """Test getting model frequency across documents."""
        from analysis.knowledge_graph import KnowledgeGraph
        
        graph = KnowledgeGraph()
        
        # Add documents and models
        for i in range(1, 4):
            graph.add_document(f"doc{i}", f"Doc {i}", f"/path{i}")
        
        graph.add_model(1, "Popular Model", "Psychology")
        graph.add_model(2, "Less Popular", "Economics")
        
        # Model 1 appears in all docs, Model 2 in one
        for i in range(1, 4):
            graph.link_document_to_model(f"doc{i}", 1, relevance=0.8)
        graph.link_document_to_model("doc1", 2, relevance=0.7)
        
        freq = graph.get_model_frequency()
        
        assert freq[1] == 3
        assert freq[2] == 1
    
    def test_get_category_distribution(self):
        """Test getting category distribution."""
        from analysis.knowledge_graph import KnowledgeGraph
        
        graph = KnowledgeGraph()
        
        graph.add_model(1, "Model 1", "Psychology")
        graph.add_model(2, "Model 2", "Psychology")
        graph.add_model(3, "Model 3", "Economics")
        
        dist = graph.get_category_distribution()
        
        assert dist["Psychology"] == 2
        assert dist["Economics"] == 1
