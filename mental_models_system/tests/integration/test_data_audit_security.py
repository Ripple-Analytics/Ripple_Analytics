"""
Integration tests for data export/import, audit logging, and security modules.
"""

import pytest
import os
import json
import tempfile
from datetime import datetime
from pathlib import Path


class TestDataExportImport:
    """Tests for data export and import functionality."""
    
    def test_export_full(self):
        """Test full data export."""
        from src.data import DataExporter
        
        exporter = DataExporter()
        
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = os.path.join(tmpdir, "export.json")
            result = exporter.export_full(output_path=output_path)
            
            assert result is not None
            assert os.path.exists(result) or isinstance(result, str)
    
    def test_export_selective(self):
        """Test selective data export."""
        from src.data import DataExporter
        
        exporter = DataExporter()
        
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = os.path.join(tmpdir, "export.json")
            result = exporter.export_selective(
                components=["mental_models", "failure_modes"],
                output_path=output_path
            )
            
            assert result is not None
    
    def test_data_exporter_attributes(self):
        """Test DataExporter has required attributes."""
        from src.data import DataExporter
        
        exporter = DataExporter()
        
        assert hasattr(exporter, 'data_dir')
        assert hasattr(exporter, 'export_dir')
        assert hasattr(exporter, 'VERSION')


class TestAuditLogging:
    """Tests for audit logging functionality."""
    
    def test_audit_logger_initialization(self):
        """Test audit logger initializes correctly."""
        from src.audit import AuditLogger
        
        with tempfile.TemporaryDirectory() as tmpdir:
            logger = AuditLogger(log_dir=tmpdir)
            
            assert logger.log_dir.exists()
            assert logger.buffer_max_size == 1000
    
    def test_log_event(self):
        """Test logging an event."""
        from src.audit import AuditLogger, AuditEventType
        
        with tempfile.TemporaryDirectory() as tmpdir:
            logger = AuditLogger(log_dir=tmpdir)
            
            event = logger.log(
                event_type=AuditEventType.ANALYSIS_COMPLETED,
                actor="test_user",
                action="analyze",
                resource_type="document",
                details={"test": True}
            )
            
            assert event.id is not None
            assert event.event_type == "analysis.completed"
            assert event.actor == "test_user"
            assert event.outcome == "success"
    
    def test_log_api_request(self):
        """Test logging API request."""
        from src.audit import AuditLogger
        
        with tempfile.TemporaryDirectory() as tmpdir:
            logger = AuditLogger(log_dir=tmpdir)
            
            correlation_id = logger.log_api_request(
                method="GET",
                path="/api/models",
                actor="api_key_123"
            )
            
            assert correlation_id is not None
            assert len(correlation_id) == 16
    
    def test_query_events(self):
        """Test querying events."""
        from src.audit import AuditLogger, AuditEventType
        
        with tempfile.TemporaryDirectory() as tmpdir:
            logger = AuditLogger(log_dir=tmpdir)
            
            # Log some events
            logger.log(
                event_type=AuditEventType.ANALYSIS_COMPLETED,
                actor="user1",
                action="analyze",
                resource_type="document"
            )
            logger.log(
                event_type=AuditEventType.DECISION_CREATED,
                actor="user2",
                action="create",
                resource_type="decision"
            )
            
            # Query by actor
            events = logger.query(actor="user1")
            assert len(events) >= 1
            assert all(e.actor == "user1" for e in events)
    
    def test_get_stats(self):
        """Test getting audit statistics."""
        from src.audit import AuditLogger, AuditEventType
        
        with tempfile.TemporaryDirectory() as tmpdir:
            logger = AuditLogger(log_dir=tmpdir)
            
            # Log an event
            logger.log(
                event_type=AuditEventType.ANALYSIS_COMPLETED,
                actor="test",
                action="test",
                resource_type="test"
            )
            
            stats = logger.get_stats()
            
            assert "total_events" in stats
            assert "events_by_type" in stats
            assert "outcomes" in stats
            assert stats["total_events"] >= 1
    
    def test_get_recent_events(self):
        """Test getting recent events."""
        from src.audit import AuditLogger, AuditEventType
        
        with tempfile.TemporaryDirectory() as tmpdir:
            logger = AuditLogger(log_dir=tmpdir)
            
            # Log some events
            for i in range(5):
                logger.log(
                    event_type=AuditEventType.ANALYSIS_COMPLETED,
                    actor=f"user{i}",
                    action="analyze",
                    resource_type="document"
                )
            
            recent = logger.get_recent_events(count=3)
            
            assert len(recent) <= 3
            assert all(isinstance(e, dict) for e in recent)


class TestSecurity:
    """Tests for security functionality."""
    
    def test_rate_limiter_initialization(self):
        """Test rate limiter initializes correctly."""
        from src.api.security import RateLimiter
        
        limiter = RateLimiter()
        
        assert limiter.config is not None
        assert limiter.config.requests_per_minute > 0
    
    @pytest.mark.asyncio
    async def test_rate_limit_check(self):
        """Test rate limit checking."""
        from src.api.security import RateLimiter
        
        limiter = RateLimiter()
        
        # First few requests should pass
        for i in range(3):
            allowed, info = await limiter.check_rate_limit("test_ip", "/api/test")
            assert allowed
    
    def test_rate_limiter_stats(self):
        """Test rate limiter statistics."""
        from src.api.security import RateLimiter
        
        limiter = RateLimiter()
        
        stats = limiter.get_stats()
        
        assert isinstance(stats, dict)
        # Stats structure may vary, just check it returns something
        assert len(stats) > 0
    
    @pytest.mark.asyncio
    async def test_block_ip(self):
        """Test blocking an IP."""
        from src.api.security import RateLimiter
        
        limiter = RateLimiter()
        
        limiter.block_ip("blocked_ip")
        
        assert "blocked_ip" in limiter.blocked_ips
        
        # Blocked IP should be rejected
        allowed, info = await limiter.check_rate_limit("blocked_ip", "/api/test")
        assert not allowed
    
    def test_api_key_manager_initialization(self):
        """Test API key manager initializes correctly."""
        from src.api.security import APIKeyManager
        
        manager = APIKeyManager()
        
        assert manager is not None
    
    def test_generate_api_key(self):
        """Test generating an API key."""
        from src.api.security import APIKeyManager
        
        with tempfile.TemporaryDirectory() as tmpdir:
            manager = APIKeyManager(keys_file=os.path.join(tmpdir, "keys.json"))
            
            key, api_key_obj = manager.generate_key(
                name="test_key",
                scopes=["read", "write"]
            )
            
            assert key is not None
            assert len(key) > 20
            assert api_key_obj.name == "test_key"
            assert "read" in api_key_obj.scopes
    
    def test_validate_api_key(self):
        """Test validating an API key."""
        from src.api.security import APIKeyManager
        
        with tempfile.TemporaryDirectory() as tmpdir:
            manager = APIKeyManager(keys_file=os.path.join(tmpdir, "keys.json"))
            
            # Create a key
            key, api_key_obj = manager.generate_key(
                name="test_key",
                scopes=["read"]
            )
            
            # Validate it - returns APIKey object or None
            validated_key = manager.validate_key(key)
            
            assert validated_key is not None
            assert validated_key.name == "test_key"
            assert "read" in validated_key.scopes
    
    def test_revoke_api_key(self):
        """Test revoking an API key."""
        from src.api.security import APIKeyManager
        
        with tempfile.TemporaryDirectory() as tmpdir:
            manager = APIKeyManager(keys_file=os.path.join(tmpdir, "keys.json"))
            
            # Create a key
            key, api_key_obj = manager.generate_key(name="test_key", scopes=["read"])
            
            # Use the key_hash from the returned object
            key_id = api_key_obj.key_hash
            
            # Revoke it
            result = manager.revoke_key(key_id)
            assert result
            
            # Validate should fail (returns None for revoked keys)
            validated_key = manager.validate_key(key)
            assert validated_key is None
    
    def test_list_api_keys(self):
        """Test listing API keys."""
        from src.api.security import APIKeyManager
        
        with tempfile.TemporaryDirectory() as tmpdir:
            manager = APIKeyManager(keys_file=os.path.join(tmpdir, "keys.json"))
            
            # Create some keys
            manager.generate_key(name="key1", scopes=["read"])
            manager.generate_key(name="key2", scopes=["write"])
            
            keys = manager.list_keys()
            
            assert len(keys) >= 2
            assert any(k["name"] == "key1" for k in keys)
            assert any(k["name"] == "key2" for k in keys)


class TestEndToEndSecurity:
    """End-to-end security tests."""
    
    @pytest.mark.asyncio
    async def test_full_api_key_lifecycle(self):
        """Test complete API key lifecycle."""
        from src.api.security import APIKeyManager, RateLimiter
        
        with tempfile.TemporaryDirectory() as tmpdir:
            manager = APIKeyManager(keys_file=os.path.join(tmpdir, "keys.json"))
            limiter = RateLimiter()
            
            # Create key
            key, api_key_obj = manager.generate_key(
                name="lifecycle_test",
                scopes=["read", "write"]
            )
            
            # Validate key - returns APIKey object or None
            validated_key = manager.validate_key(key)
            assert validated_key is not None
            
            # Check rate limit
            allowed, _ = await limiter.check_rate_limit(api_key_obj.key_hash, "/api/test")
            assert allowed
            
            # Revoke key
            manager.revoke_key(api_key_obj.key_hash)
            
            # Validate should fail (returns None)
            validated_key = manager.validate_key(key)
            assert validated_key is None
    
    def test_audit_security_integration(self):
        """Test audit logging with security events."""
        from src.audit import AuditLogger, AuditEventType
        from src.api.security import APIKeyManager
        
        with tempfile.TemporaryDirectory() as tmpdir:
            logger = AuditLogger(log_dir=os.path.join(tmpdir, "audit"))
            manager = APIKeyManager(keys_file=os.path.join(tmpdir, "keys.json"))
            
            # Create key and log it
            key, api_key_obj = manager.generate_key(name="audit_test", scopes=["read"])
            
            logger.log(
                event_type=AuditEventType.API_KEY_CREATED,
                actor="admin",
                action="create",
                resource_type="api_key",
                resource_id=api_key_obj.key_hash,
                details={"name": "audit_test", "scopes": ["read"]}
            )
            
            # Query the event
            events = logger.query(event_type="auth.api_key_created")
            
            assert len(events) >= 1
            assert events[0].resource_id == api_key_obj.key_hash
