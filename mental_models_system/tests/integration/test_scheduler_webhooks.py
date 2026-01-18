"""
Integration tests for Job Scheduler and Webhook Manager.
"""

import pytest
import asyncio
from datetime import datetime, timedelta
from unittest.mock import Mock, patch, AsyncMock
import json
import os
import tempfile


class TestCronParser:
    """Tests for cron expression parsing."""
    
    def test_parse_every_minute(self):
        from src.scheduler import CronParser
        
        result = CronParser.parse("* * * * *")
        assert len(result["minute"]) == 60
        assert len(result["hour"]) == 24
    
    def test_parse_specific_time(self):
        from src.scheduler import CronParser
        
        result = CronParser.parse("30 6 * * *")
        assert result["minute"] == [30]
        assert result["hour"] == [6]
    
    def test_parse_range(self):
        from src.scheduler import CronParser
        
        result = CronParser.parse("0 9-17 * * 1-5")
        assert result["hour"] == list(range(9, 18))
        assert result["weekday"] == list(range(1, 6))
    
    def test_parse_step(self):
        from src.scheduler import CronParser
        
        result = CronParser.parse("*/15 * * * *")
        assert result["minute"] == [0, 15, 30, 45]
    
    def test_get_next_run(self):
        from src.scheduler import CronParser
        
        # Test 6 AM daily
        next_run = CronParser.get_next_run("0 6 * * *")
        assert next_run.hour == 6
        assert next_run.minute == 0
    
    def test_invalid_expression(self):
        from src.scheduler import CronParser
        
        with pytest.raises(ValueError):
            CronParser.parse("invalid")


class TestJobScheduler:
    """Tests for job scheduler."""
    
    @pytest.fixture
    def scheduler(self):
        from src.scheduler import JobScheduler
        
        with tempfile.TemporaryDirectory() as tmpdir:
            scheduler = JobScheduler(data_dir=tmpdir)
            yield scheduler
    
    def test_add_job(self, scheduler):
        from src.scheduler import ScheduledJob, JobPriority
        
        job = ScheduledJob(
            id="test_job",
            name="Test Job",
            description="A test job",
            handler="src.test.handler",
            schedule_type="interval",
            schedule_value="3600",
            priority=JobPriority.MEDIUM
        )
        
        result = scheduler.add_job(job)
        assert result is True
        assert "test_job" in scheduler.jobs
        assert scheduler.jobs["test_job"].next_run is not None
    
    def test_remove_job(self, scheduler):
        from src.scheduler import ScheduledJob
        
        job = ScheduledJob(
            id="test_job",
            name="Test Job",
            description="A test job",
            handler="src.test.handler",
            schedule_type="interval",
            schedule_value="3600"
        )
        
        scheduler.add_job(job)
        result = scheduler.remove_job("test_job")
        assert result is True
        assert "test_job" not in scheduler.jobs
    
    def test_enable_disable_job(self, scheduler):
        from src.scheduler import ScheduledJob
        
        job = ScheduledJob(
            id="test_job",
            name="Test Job",
            description="A test job",
            handler="src.test.handler",
            schedule_type="interval",
            schedule_value="3600"
        )
        
        scheduler.add_job(job)
        
        scheduler.disable_job("test_job")
        assert scheduler.jobs["test_job"].enabled is False
        assert scheduler.jobs["test_job"].next_run is None
        
        scheduler.enable_job("test_job")
        assert scheduler.jobs["test_job"].enabled is True
        assert scheduler.jobs["test_job"].next_run is not None
    
    def test_get_due_jobs(self, scheduler):
        from src.scheduler import ScheduledJob
        
        # Add a job that's already due
        job = ScheduledJob(
            id="due_job",
            name="Due Job",
            description="A due job",
            handler="src.test.handler",
            schedule_type="interval",
            schedule_value="1"  # 1 second
        )
        
        scheduler.add_job(job)
        
        # Wait a moment for it to become due
        import time
        time.sleep(1.1)
        
        due_jobs = scheduler.get_due_jobs()
        assert len(due_jobs) >= 1
        assert any(j.id == "due_job" for j in due_jobs)
    
    def test_cron_job_scheduling(self, scheduler):
        from src.scheduler import ScheduledJob
        
        job = ScheduledJob(
            id="cron_job",
            name="Cron Job",
            description="A cron job",
            handler="src.test.handler",
            schedule_type="cron",
            schedule_value="0 6 * * *"  # 6 AM daily
        )
        
        scheduler.add_job(job)
        
        assert scheduler.jobs["cron_job"].next_run is not None
        assert scheduler.jobs["cron_job"].next_run.hour == 6
    
    def test_job_persistence(self):
        from src.scheduler import JobScheduler, ScheduledJob
        
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create scheduler and add job
            scheduler1 = JobScheduler(data_dir=tmpdir)
            job = ScheduledJob(
                id="persist_job",
                name="Persist Job",
                description="A persistent job",
                handler="src.test.handler",
                schedule_type="interval",
                schedule_value="3600"
            )
            scheduler1.add_job(job)
            
            # Create new scheduler and verify job is loaded
            scheduler2 = JobScheduler(data_dir=tmpdir)
            assert "persist_job" in scheduler2.jobs
            assert scheduler2.jobs["persist_job"].name == "Persist Job"
    
    def test_get_stats(self, scheduler):
        from src.scheduler import ScheduledJob
        
        job = ScheduledJob(
            id="stats_job",
            name="Stats Job",
            description="A job for stats",
            handler="src.test.handler",
            schedule_type="interval",
            schedule_value="3600"
        )
        
        scheduler.add_job(job)
        stats = scheduler.get_stats()
        
        assert stats["total_jobs"] == 1
        assert stats["enabled_jobs"] == 1
        assert stats["total_runs"] == 0


class TestWebhookManager:
    """Tests for webhook manager."""
    
    @pytest.fixture
    def manager(self):
        from src.webhooks import WebhookManager
        
        with tempfile.TemporaryDirectory() as tmpdir:
            manager = WebhookManager(data_dir=tmpdir)
            yield manager
    
    def test_add_target(self, manager):
        from src.webhooks import WebhookTarget, WebhookType, EventType
        
        target = WebhookTarget(
            id="test_target",
            name="Test Target",
            type=WebhookType.SLACK,
            url="https://hooks.slack.com/test",
            events=[EventType.LOLLAPALOOZA_DETECTED]
        )
        
        result = manager.add_target(target)
        assert result is True
        assert "test_target" in manager.targets
    
    def test_remove_target(self, manager):
        from src.webhooks import WebhookTarget, WebhookType
        
        target = WebhookTarget(
            id="test_target",
            name="Test Target",
            type=WebhookType.SLACK,
            url="https://hooks.slack.com/test"
        )
        
        manager.add_target(target)
        result = manager.remove_target("test_target")
        assert result is True
        assert "test_target" not in manager.targets
    
    def test_enable_disable_target(self, manager):
        from src.webhooks import WebhookTarget, WebhookType
        
        target = WebhookTarget(
            id="test_target",
            name="Test Target",
            type=WebhookType.SLACK,
            url="https://hooks.slack.com/test"
        )
        
        manager.add_target(target)
        
        manager.disable_target("test_target")
        assert manager.targets["test_target"].enabled is False
        
        manager.enable_target("test_target")
        assert manager.targets["test_target"].enabled is True
    
    def test_event_filtering(self, manager):
        from src.webhooks import (
            WebhookTarget, WebhookEvent, WebhookType, 
            EventType, EventPriority
        )
        
        # Target only wants LOLLAPALOOZA_DETECTED events
        target = WebhookTarget(
            id="filtered_target",
            name="Filtered Target",
            type=WebhookType.SLACK,
            url="https://hooks.slack.com/test",
            events=[EventType.LOLLAPALOOZA_DETECTED]
        )
        
        manager.add_target(target)
        
        # Should match
        event1 = WebhookEvent(
            type=EventType.LOLLAPALOOZA_DETECTED,
            priority=EventPriority.HIGH,
            title="Test",
            message="Test message"
        )
        assert manager._should_send_to_target(target, event1) is True
        
        # Should not match
        event2 = WebhookEvent(
            type=EventType.SIGNAL_HARVESTED,
            priority=EventPriority.HIGH,
            title="Test",
            message="Test message"
        )
        assert manager._should_send_to_target(target, event2) is False
    
    def test_priority_filtering(self, manager):
        from src.webhooks import (
            WebhookTarget, WebhookEvent, WebhookType, 
            EventType, EventPriority
        )
        
        # Target only wants HIGH priority or above
        target = WebhookTarget(
            id="priority_target",
            name="Priority Target",
            type=WebhookType.SLACK,
            url="https://hooks.slack.com/test",
            min_priority=EventPriority.HIGH
        )
        
        manager.add_target(target)
        
        # Should match (HIGH)
        event1 = WebhookEvent(
            type=EventType.SYSTEM_ALERT,
            priority=EventPriority.HIGH,
            title="Test",
            message="Test message"
        )
        assert manager._should_send_to_target(target, event1) is True
        
        # Should not match (LOW)
        event2 = WebhookEvent(
            type=EventType.SYSTEM_ALERT,
            priority=EventPriority.LOW,
            title="Test",
            message="Test message"
        )
        assert manager._should_send_to_target(target, event2) is False
    
    def test_slack_payload_format(self, manager):
        from src.webhooks import WebhookEvent, EventType, EventPriority
        
        event = WebhookEvent(
            type=EventType.LOLLAPALOOZA_DETECTED,
            priority=EventPriority.HIGH,
            title="Test Alert",
            message="This is a test",
            data={"key": "value"}
        )
        
        payload = manager._format_slack_payload(event)
        
        assert "blocks" in payload
        assert len(payload["blocks"]) >= 3
        assert payload["blocks"][0]["type"] == "header"
    
    def test_discord_payload_format(self, manager):
        from src.webhooks import WebhookEvent, EventType, EventPriority
        
        event = WebhookEvent(
            type=EventType.LOLLAPALOOZA_DETECTED,
            priority=EventPriority.HIGH,
            title="Test Alert",
            message="This is a test",
            data={"key": "value"}
        )
        
        payload = manager._format_discord_payload(event)
        
        assert "embeds" in payload
        assert len(payload["embeds"]) == 1
        assert payload["embeds"][0]["title"] == "Test Alert"
    
    def test_teams_payload_format(self, manager):
        from src.webhooks import WebhookEvent, EventType, EventPriority
        
        event = WebhookEvent(
            type=EventType.SYSTEM_ALERT,
            priority=EventPriority.MEDIUM,
            title="Test Alert",
            message="This is a test"
        )
        
        payload = manager._format_teams_payload(event)
        
        assert payload["@type"] == "MessageCard"
        assert payload["summary"] == "Test Alert"
    
    def test_target_persistence(self):
        from src.webhooks import WebhookManager, WebhookTarget, WebhookType
        
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create manager and add target
            manager1 = WebhookManager(data_dir=tmpdir)
            target = WebhookTarget(
                id="persist_target",
                name="Persist Target",
                type=WebhookType.SLACK,
                url="https://hooks.slack.com/test"
            )
            manager1.add_target(target)
            
            # Create new manager and verify target is loaded
            manager2 = WebhookManager(data_dir=tmpdir)
            assert "persist_target" in manager2.targets
            assert manager2.targets["persist_target"].name == "Persist Target"
    
    def test_get_stats(self, manager):
        from src.webhooks import WebhookTarget, WebhookType
        
        target = WebhookTarget(
            id="stats_target",
            name="Stats Target",
            type=WebhookType.SLACK,
            url="https://hooks.slack.com/test"
        )
        
        manager.add_target(target)
        stats = manager.get_stats()
        
        assert stats["total_targets"] == 1
        assert stats["enabled_targets"] == 1
        assert stats["total_deliveries"] == 0


class TestSchedulerWebhookIntegration:
    """Integration tests for scheduler and webhooks working together."""
    
    @pytest.fixture
    def setup(self):
        from src.scheduler import JobScheduler
        from src.webhooks import WebhookManager
        
        with tempfile.TemporaryDirectory() as tmpdir:
            scheduler = JobScheduler(data_dir=os.path.join(tmpdir, "scheduler"))
            webhook_manager = WebhookManager(data_dir=os.path.join(tmpdir, "webhooks"))
            yield scheduler, webhook_manager
    
    def test_scheduler_webhook_notification(self, setup):
        """Test that scheduler can notify webhooks on job completion."""
        from src.scheduler import ScheduledJob
        from src.webhooks import WebhookTarget, WebhookType
        
        scheduler, webhook_manager = setup
        
        # Add webhook target
        target = WebhookTarget(
            id="job_notifications",
            name="Job Notifications",
            type=WebhookType.SLACK,
            url="https://hooks.slack.com/test"
        )
        webhook_manager.add_target(target)
        
        # Add scheduler webhook
        scheduler.add_webhook("https://hooks.slack.com/test")
        
        assert len(scheduler.webhooks) == 1
    
    def test_default_jobs_creation(self):
        from src.scheduler import setup_default_scheduler
        
        scheduler = setup_default_scheduler()
        
        assert len(scheduler.jobs) >= 4
        assert "daily_improvement_analysis" in scheduler.jobs
        assert "hourly_signal_harvest" in scheduler.jobs


class TestCacheIntegration:
    """Tests for cache integration."""
    
    def test_cache_basic_operations(self):
        from src.cache import get_cache
        
        cache = get_cache()
        
        # Test set and get
        cache.set("test_key", {"value": 42})
        result = cache.get("test_key")
        assert result == {"value": 42}
        
        # Test delete
        cache.delete("test_key")
        result = cache.get("test_key")
        assert result is None
    
    def test_cache_decorator(self):
        from src.cache import get_cache
        
        cache = get_cache()
        # Clear cache to ensure clean state for test
        cache.clear()
        call_count = 0
        
        @cache.cached(ttl=60, key_prefix="test:")
        def expensive_function(x):
            nonlocal call_count
            call_count += 1
            return x * 2
        
        # First call should execute function
        result1 = expensive_function(5)
        assert result1 == 10
        assert call_count == 1
        
        # Second call should use cache
        result2 = expensive_function(5)
        assert result2 == 10
        assert call_count == 1  # Not incremented
    
    def test_llm_cache(self):
        from src.cache import get_llm_cache
        
        llm_cache = get_llm_cache()
        
        # Set LLM response
        llm_cache.set_llm_response("llama3", "What is 2+2?", "4")
        
        # Get LLM response
        result = llm_cache.get_llm_response("llama3", "What is 2+2?")
        assert result == "4"
        
        # Different prompt should return None
        result = llm_cache.get_llm_response("llama3", "What is 3+3?")
        assert result is None


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
