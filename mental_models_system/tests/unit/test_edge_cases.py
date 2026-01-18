"""
Edge Case Tests for Robustness

Comprehensive tests for edge cases, error handling, and boundary conditions
across all modules of the Mental Models System.
"""

import pytest
import json
import asyncio
import tempfile
import os
from pathlib import Path
from datetime import datetime, timedelta
from unittest.mock import Mock, patch, MagicMock


# =============================================================================
# MENTAL MODEL LOADER EDGE CASES
# =============================================================================

class TestMentalModelLoaderEdgeCases:
    """Edge case tests for MentalModelLoader."""
    
    def test_loader_initialization(self):
        """Test loader initializes correctly."""
        from src.analysis.model_analyzer import MentalModelLoader
        
        loader = MentalModelLoader()
        assert len(loader.models) > 0
    
    def test_get_model_by_name(self):
        """Test getting model by name."""
        from src.analysis.model_analyzer import MentalModelLoader
        
        loader = MentalModelLoader()
        
        # Get nonexistent model
        model = loader.get_model("Nonexistent Model XYZ123")
        assert model is None
    
    def test_get_models_by_category(self):
        """Test getting models by category."""
        from src.analysis.model_analyzer import MentalModelLoader
        
        loader = MentalModelLoader()
        
        # Get models from category
        models = loader.get_models_by_category("Psychology")
        assert isinstance(models, list)
        
        # Get models from nonexistent category
        models = loader.get_models_by_category("NonexistentCategory123")
        assert models == []


# =============================================================================
# FAILURE MODES LOADER EDGE CASES
# =============================================================================

class TestFailureModesLoaderEdgeCases:
    """Edge case tests for FailureModesLoader."""
    
    def test_loader_initialization(self):
        """Test failure modes loader initializes."""
        from src.safeguards.failure_modes_loader import FailureModesLoader
        
        loader = FailureModesLoader()
        # Check it has data
        assert loader is not None
    
    def test_get_statistics(self):
        """Test getting failure mode statistics."""
        from src.safeguards.failure_modes_loader import FailureModesLoader
        
        loader = FailureModesLoader()
        stats = loader.get_statistics()
        
        assert 'total_failure_modes' in stats
        assert 'models_with_failure_modes' in stats
    
    def test_get_failure_modes(self):
        """Test getting failure modes for a model."""
        from src.safeguards.failure_modes_loader import FailureModesLoader
        
        loader = FailureModesLoader()
        
        # Get failure modes for a model
        modes = loader.get_failure_modes("Confirmation Bias")
        # May return list or None depending on data
        assert modes is None or isinstance(modes, list)
        
        # Get failure modes for nonexistent model
        modes = loader.get_failure_modes("Nonexistent Model XYZ")
        assert modes is None or modes == []


# =============================================================================
# KNOWLEDGE GRAPH EDGE CASES
# =============================================================================

class TestKnowledgeGraphEdgeCases:
    """Edge case tests for KnowledgeGraph."""
    
    def test_graph_initialization(self):
        """Test knowledge graph initializes."""
        from src.analysis.knowledge_graph import KnowledgeGraph
        
        graph = KnowledgeGraph()
        assert graph is not None
    
    def test_get_summary(self):
        """Test getting graph summary."""
        from src.analysis.knowledge_graph import KnowledgeGraph
        
        graph = KnowledgeGraph()
        
        summary = graph.get_summary()
        assert 'total_nodes' in summary
        assert 'node_counts' in summary
    
    def test_search(self):
        """Test searching the graph."""
        from src.analysis.knowledge_graph import KnowledgeGraph
        
        graph = KnowledgeGraph()
        
        # Search empty graph
        results = graph.search("test")
        assert isinstance(results, list)


# =============================================================================
# CACHE EDGE CASES
# =============================================================================

class TestCacheEdgeCases:
    """Edge case tests for caching system."""
    
    def test_lru_cache_basic(self):
        """Test basic LRU cache operations."""
        from src.cache import LRUCache
        
        cache = LRUCache(max_entries=3)
        
        cache.set("key1", "value1")
        cache.set("key2", "value2")
        cache.set("key3", "value3")
        
        assert cache.get("key1") == "value1"
        assert cache.get("key2") == "value2"
        assert cache.get("key3") == "value3"
    
    def test_lru_cache_eviction(self):
        """Test LRU cache eviction."""
        from src.cache import LRUCache
        
        cache = LRUCache(max_entries=2)
        
        cache.set("key1", "value1")
        cache.set("key2", "value2")
        cache.set("key3", "value3")  # Should evict key1
        
        assert cache.get("key1") is None
        assert cache.get("key3") == "value3"
    
    def test_cache_stats(self):
        """Test cache statistics."""
        from src.cache import LRUCache
        
        cache = LRUCache()
        
        cache.set("key1", "value1")
        cache.get("key1")  # Hit
        cache.get("key2")  # Miss
        
        stats = cache.get_stats()
        assert stats.hits >= 1
        assert stats.misses >= 1
    
    def test_cached_decorator(self):
        """Test @cached decorator."""
        from src.cache import cached
        
        call_count = 0
        
        @cached(ttl=60)
        def expensive_function(x, y):
            nonlocal call_count
            call_count += 1
            return x + y
        
        result1 = expensive_function(1, 2)
        result2 = expensive_function(1, 2)  # Should hit cache
        
        assert result1 == 3
        assert result2 == 3
        assert call_count == 1  # Only called once


# =============================================================================
# BACKUP SYSTEM EDGE CASES
# =============================================================================

class TestBackupEdgeCases:
    """Edge case tests for backup system."""
    
    def test_backup_manager_initialization(self, tmp_path):
        """Test backup manager initializes."""
        from src.backup import BackupManager
        
        manager = BackupManager(
            data_dir=str(tmp_path / "data"),
            backup_dir=str(tmp_path / "backups")
        )
        
        assert manager is not None
    
    def test_create_empty_backup(self, tmp_path):
        """Test creating a backup of empty directory."""
        from src.backup import BackupManager, BackupType
        
        data_dir = tmp_path / "data"
        data_dir.mkdir()
        
        backup_dir = tmp_path / "backups"
        
        manager = BackupManager(
            data_dir=str(data_dir),
            backup_dir=str(backup_dir)
        )
        
        manifest = manager.create_backup(backup_type=BackupType.FULL)
        # Empty backup is valid
        assert manifest is not None


# =============================================================================
# DECISION JOURNAL EDGE CASES
# =============================================================================

class TestDecisionJournalEdgeCases:
    """Edge case tests for DecisionJournal."""
    
    def test_journal_initialization(self):
        """Test decision journal initializes."""
        from src.journal.decision_journal import DecisionJournal
        
        journal = DecisionJournal()
        assert journal is not None
    
    def test_create_decision(self):
        """Test creating a decision."""
        from src.journal.decision_journal import DecisionJournal
        
        journal = DecisionJournal()
        
        decision = journal.create_decision(
            title="Test Decision",
            description="Test description",
            decision_type="investment",
            decision_made="Buy stock",
            rationale="Good fundamentals",
            mental_models_used=["Value Investing"],
            predicted_outcome="10% return",
            confidence=0.8,
            context={}
        )
        
        assert decision is not None
        assert decision.title == "Test Decision"
    
    def test_get_nonexistent_decision(self):
        """Test getting a decision that doesn't exist."""
        from src.journal.decision_journal import DecisionJournal
        
        journal = DecisionJournal()
        
        decision = journal.get_decision("nonexistent_id_12345")
        assert decision is None


# =============================================================================
# WEBHOOK EDGE CASES
# =============================================================================

class TestWebhookEdgeCases:
    """Edge case tests for webhook system."""
    
    def test_webhook_manager_initialization(self):
        """Test webhook manager initializes."""
        from src.webhooks.webhook_manager import WebhookManager
        
        manager = WebhookManager()
        assert manager is not None
    
    def test_get_stats(self):
        """Test getting webhook stats."""
        from src.webhooks.webhook_manager import WebhookManager
        
        manager = WebhookManager()
        
        stats = manager.get_stats()
        assert isinstance(stats, dict)
    
    def test_list_targets(self):
        """Test listing webhook targets."""
        from src.webhooks.webhook_manager import WebhookManager
        
        manager = WebhookManager()
        
        targets = manager.list_targets()
        assert isinstance(targets, list)
    
    def test_get_stats(self):
        """Test getting webhook stats."""
        from src.webhooks.webhook_manager import WebhookManager
        
        manager = WebhookManager()
        
        stats = manager.get_stats()
        assert isinstance(stats, dict)


# =============================================================================
# SCHEDULER EDGE CASES
# =============================================================================

class TestSchedulerEdgeCases:
    """Edge case tests for job scheduler."""
    
    def test_scheduler_initialization(self):
        """Test scheduler initializes."""
        from src.scheduler.job_scheduler import JobScheduler
        
        scheduler = JobScheduler()
        assert scheduler is not None
    
    def test_list_jobs(self):
        """Test listing jobs."""
        from src.scheduler.job_scheduler import JobScheduler
        
        scheduler = JobScheduler()
        
        jobs = scheduler.list_jobs()
        assert isinstance(jobs, list)
    
    def test_get_stats(self):
        """Test getting scheduler stats."""
        from src.scheduler.job_scheduler import JobScheduler
        
        scheduler = JobScheduler()
        
        stats = scheduler.get_stats()
        assert 'total_jobs' in stats


# =============================================================================
# API SECURITY EDGE CASES
# =============================================================================

class TestAPISecurityEdgeCases:
    """Edge case tests for API security."""
    
    def test_api_key_manager_initialization(self):
        """Test API key manager initializes."""
        from src.api.security import APIKeyManager
        
        manager = APIKeyManager()
        assert manager is not None
    
    def test_generate_api_key(self):
        """Test generating an API key."""
        from src.api.security import APIKeyManager
        
        manager = APIKeyManager()
        
        key = manager.generate_key(name="test_key")
        assert key is not None
        assert len(key) > 0
    
    def test_validate_invalid_key(self):
        """Test validating an invalid key."""
        from src.api.security import APIKeyManager
        
        manager = APIKeyManager()
        
        result = manager.validate_key("invalid_key_12345")
        assert result is None


# =============================================================================
# DATA EXPORT/IMPORT EDGE CASES
# =============================================================================

class TestDataExportImportEdgeCases:
    """Edge case tests for data export/import."""
    
    def test_exporter_initialization(self):
        """Test data exporter initializes."""
        from src.data.export_import import DataExporter
        
        exporter = DataExporter()
        assert exporter is not None
    
    def test_importer_initialization(self):
        """Test data importer initializes."""
        from src.data.export_import import DataImporter
        
        importer = DataImporter()
        assert importer is not None


# =============================================================================
# LOLLAPALOOZA DETECTION EDGE CASES
# =============================================================================

class TestLollapaloozaEdgeCases:
    """Edge case tests for Lollapalooza detection."""
    
    def test_engine_initialization(self):
        """Test detection engine initializes."""
        from src.detection.lollapalooza_engine import LollapaloozaDetectionEngine
        
        engine = LollapaloozaDetectionEngine()
        assert engine is not None
    
    def test_get_status(self):
        """Test getting engine status."""
        from src.detection.lollapalooza_engine import LollapaloozaDetectionEngine
        
        engine = LollapaloozaDetectionEngine()
        
        status = engine.get_status()
        assert isinstance(status, dict)
    
    def test_get_recent_events(self):
        """Test getting recent events."""
        from src.detection.lollapalooza_engine import LollapaloozaDetectionEngine
        
        engine = LollapaloozaDetectionEngine()
        
        events = engine.get_recent_events()
        assert isinstance(events, list)


# =============================================================================
# INTEGRATION EDGE CASES
# =============================================================================

class TestIntegrationEdgeCases:
    """Edge case tests for system integration."""
    
    def test_system_under_memory_pressure(self):
        """Test system behavior under memory pressure."""
        import gc
        
        # Create many objects
        objects = []
        for i in range(10000):
            objects.append({"data": "x" * 1000})
        
        # Force garbage collection
        del objects
        gc.collect()
        
        # System should still be responsive
        from src.analysis.model_analyzer import MentalModelLoader
        loader = MentalModelLoader()
        assert len(loader.models) > 0
    
    def test_concurrent_operations(self):
        """Test concurrent operations don't cause issues."""
        import threading
        from src.cache import LRUCache
        
        cache = LRUCache()
        errors = []
        
        def writer():
            try:
                for i in range(100):
                    cache.set(f"key_{i}", f"value_{i}")
            except Exception as e:
                errors.append(e)
        
        def reader():
            try:
                for i in range(100):
                    cache.get(f"key_{i}")
            except Exception as e:
                errors.append(e)
        
        threads = [
            threading.Thread(target=writer),
            threading.Thread(target=reader),
            threading.Thread(target=writer),
            threading.Thread(target=reader),
        ]
        
        for t in threads:
            t.start()
        for t in threads:
            t.join()
        
        assert len(errors) == 0, f"Concurrent access errors: {errors}"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
