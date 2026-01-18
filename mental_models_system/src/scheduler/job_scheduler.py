"""
Job Scheduler for Mental Models System.

Provides scheduled execution of:
- Periodic improvement analysis
- Signal harvesting
- Document re-indexing
- Model effectiveness updates
- Report generation

Features:
- Cron-style scheduling
- Interval-based scheduling
- Job persistence
- Failure recovery
- Webhook notifications
"""

import asyncio
import json
import os
import time
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import List, Dict, Optional, Callable, Any
from enum import Enum
import threading
import logging

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class JobStatus(Enum):
    """Job execution status."""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


class JobPriority(Enum):
    """Job priority levels."""
    LOW = 1
    MEDIUM = 2
    HIGH = 3
    CRITICAL = 4


@dataclass
class JobResult:
    """Result of a job execution."""
    job_id: str
    status: JobStatus
    started_at: datetime
    completed_at: Optional[datetime]
    duration_seconds: float
    result: Any = None
    error: Optional[str] = None
    
    def to_dict(self) -> Dict:
        return {
            "job_id": self.job_id,
            "status": self.status.value,
            "started_at": self.started_at.isoformat(),
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "duration_seconds": self.duration_seconds,
            "result": self.result,
            "error": self.error
        }


@dataclass
class ScheduledJob:
    """A scheduled job definition."""
    id: str
    name: str
    description: str
    handler: str  # Module path to handler function
    schedule_type: str  # "cron", "interval", "once"
    schedule_value: str  # Cron expression or interval in seconds
    priority: JobPriority = JobPriority.MEDIUM
    enabled: bool = True
    timeout_seconds: int = 3600  # 1 hour default
    retry_count: int = 3
    retry_delay_seconds: int = 60
    last_run: Optional[datetime] = None
    next_run: Optional[datetime] = None
    run_count: int = 0
    failure_count: int = 0
    metadata: Dict = field(default_factory=dict)
    
    def to_dict(self) -> Dict:
        return {
            "id": self.id,
            "name": self.name,
            "description": self.description,
            "handler": self.handler,
            "schedule_type": self.schedule_type,
            "schedule_value": self.schedule_value,
            "priority": self.priority.value,
            "enabled": self.enabled,
            "timeout_seconds": self.timeout_seconds,
            "retry_count": self.retry_count,
            "last_run": self.last_run.isoformat() if self.last_run else None,
            "next_run": self.next_run.isoformat() if self.next_run else None,
            "run_count": self.run_count,
            "failure_count": self.failure_count,
            "metadata": self.metadata
        }


class CronParser:
    """Simple cron expression parser."""
    
    @staticmethod
    def parse(expression: str) -> Dict[str, List[int]]:
        """
        Parse a cron expression.
        Format: minute hour day_of_month month day_of_week
        """
        parts = expression.split()
        if len(parts) != 5:
            raise ValueError(f"Invalid cron expression: {expression}")
        
        return {
            "minute": CronParser._parse_field(parts[0], 0, 59),
            "hour": CronParser._parse_field(parts[1], 0, 23),
            "day": CronParser._parse_field(parts[2], 1, 31),
            "month": CronParser._parse_field(parts[3], 1, 12),
            "weekday": CronParser._parse_field(parts[4], 0, 6)
        }
    
    @staticmethod
    def _parse_field(field: str, min_val: int, max_val: int) -> List[int]:
        """Parse a single cron field."""
        if field == "*":
            return list(range(min_val, max_val + 1))
        
        values = []
        for part in field.split(","):
            if "-" in part:
                start, end = part.split("-")
                values.extend(range(int(start), int(end) + 1))
            elif "/" in part:
                base, step = part.split("/")
                if base == "*":
                    base = min_val
                else:
                    base = int(base)
                values.extend(range(base, max_val + 1, int(step)))
            else:
                values.append(int(part))
        
        return sorted(set(v for v in values if min_val <= v <= max_val))
    
    @staticmethod
    def get_next_run(expression: str, from_time: datetime = None) -> datetime:
        """Get the next run time for a cron expression."""
        from_time = from_time or datetime.now()
        parsed = CronParser.parse(expression)
        
        # Start from next minute
        next_time = from_time.replace(second=0, microsecond=0) + timedelta(minutes=1)
        
        # Find next matching time (limit iterations to prevent infinite loop)
        for _ in range(525600):  # Max 1 year of minutes
            if (next_time.minute in parsed["minute"] and
                next_time.hour in parsed["hour"] and
                next_time.day in parsed["day"] and
                next_time.month in parsed["month"] and
                next_time.weekday() in parsed["weekday"]):
                return next_time
            next_time += timedelta(minutes=1)
        
        raise ValueError(f"Could not find next run time for: {expression}")


class JobScheduler:
    """
    Job scheduler for periodic tasks.
    
    Usage:
        scheduler = JobScheduler()
        
        # Add a job
        scheduler.add_job(ScheduledJob(
            id="improvement_analysis",
            name="Daily Improvement Analysis",
            description="Generate improvement suggestions",
            handler="src.improvement.ImprovementSuggestionEngine.generate_all",
            schedule_type="cron",
            schedule_value="0 6 * * *"  # 6 AM daily
        ))
        
        # Start the scheduler
        await scheduler.start()
    """
    
    def __init__(self, data_dir: str = None):
        self.data_dir = data_dir or os.path.join(
            os.path.dirname(__file__),
            "../../data/scheduler"
        )
        os.makedirs(self.data_dir, exist_ok=True)
        
        self.jobs: Dict[str, ScheduledJob] = {}
        self.job_history: List[JobResult] = []
        self.running = False
        self._lock = threading.Lock()
        
        # Webhook callbacks
        self.webhooks: List[str] = []
        
        # Load persisted jobs
        self._load_jobs()
    
    def _load_jobs(self):
        """Load jobs from persistence."""
        jobs_file = os.path.join(self.data_dir, "jobs.json")
        if os.path.exists(jobs_file):
            try:
                with open(jobs_file) as f:
                    data = json.load(f)
                    for job_data in data.get("jobs", []):
                        job = ScheduledJob(
                            id=job_data["id"],
                            name=job_data["name"],
                            description=job_data["description"],
                            handler=job_data["handler"],
                            schedule_type=job_data["schedule_type"],
                            schedule_value=job_data["schedule_value"],
                            priority=JobPriority(job_data.get("priority", 2)),
                            enabled=job_data.get("enabled", True),
                            timeout_seconds=job_data.get("timeout_seconds", 3600),
                            retry_count=job_data.get("retry_count", 3),
                            run_count=job_data.get("run_count", 0),
                            failure_count=job_data.get("failure_count", 0),
                            metadata=job_data.get("metadata", {})
                        )
                        if job_data.get("last_run"):
                            job.last_run = datetime.fromisoformat(job_data["last_run"])
                        self.jobs[job.id] = job
                        self._calculate_next_run(job)
            except Exception as e:
                logger.error(f"Error loading jobs: {e}")
    
    def _save_jobs(self):
        """Save jobs to persistence."""
        jobs_file = os.path.join(self.data_dir, "jobs.json")
        data = {
            "jobs": [job.to_dict() for job in self.jobs.values()],
            "updated_at": datetime.now().isoformat()
        }
        with open(jobs_file, 'w') as f:
            json.dump(data, f, indent=2)
    
    def _calculate_next_run(self, job: ScheduledJob):
        """Calculate the next run time for a job."""
        if not job.enabled:
            job.next_run = None
            return
        
        if job.schedule_type == "cron":
            job.next_run = CronParser.get_next_run(job.schedule_value)
        elif job.schedule_type == "interval":
            interval = int(job.schedule_value)
            if job.last_run:
                job.next_run = job.last_run + timedelta(seconds=interval)
            else:
                job.next_run = datetime.now() + timedelta(seconds=interval)
        elif job.schedule_type == "once":
            if not job.last_run:
                job.next_run = datetime.fromisoformat(job.schedule_value)
            else:
                job.next_run = None
    
    def add_job(self, job: ScheduledJob) -> bool:
        """Add a new scheduled job."""
        with self._lock:
            if job.id in self.jobs:
                logger.warning(f"Job {job.id} already exists, updating")
            
            self._calculate_next_run(job)
            self.jobs[job.id] = job
            self._save_jobs()
            logger.info(f"Added job: {job.name} (next run: {job.next_run})")
            return True
    
    def remove_job(self, job_id: str) -> bool:
        """Remove a scheduled job."""
        with self._lock:
            if job_id in self.jobs:
                del self.jobs[job_id]
                self._save_jobs()
                logger.info(f"Removed job: {job_id}")
                return True
            return False
    
    def enable_job(self, job_id: str) -> bool:
        """Enable a job."""
        with self._lock:
            if job_id in self.jobs:
                self.jobs[job_id].enabled = True
                self._calculate_next_run(self.jobs[job_id])
                self._save_jobs()
                return True
            return False
    
    def disable_job(self, job_id: str) -> bool:
        """Disable a job."""
        with self._lock:
            if job_id in self.jobs:
                self.jobs[job_id].enabled = False
                self.jobs[job_id].next_run = None
                self._save_jobs()
                return True
            return False
    
    def get_job(self, job_id: str) -> Optional[ScheduledJob]:
        """Get a job by ID."""
        return self.jobs.get(job_id)
    
    def list_jobs(self) -> List[ScheduledJob]:
        """List all jobs."""
        return list(self.jobs.values())
    
    def get_due_jobs(self) -> List[ScheduledJob]:
        """Get jobs that are due to run."""
        now = datetime.now()
        due = []
        for job in self.jobs.values():
            if job.enabled and job.next_run and job.next_run <= now:
                due.append(job)
        return sorted(due, key=lambda j: j.priority.value, reverse=True)
    
    async def run_job(self, job: ScheduledJob) -> JobResult:
        """Execute a job."""
        started_at = datetime.now()
        logger.info(f"Running job: {job.name}")
        
        try:
            # Import and execute handler
            module_path, func_name = job.handler.rsplit(".", 1)
            
            # Dynamic import
            import importlib
            module = importlib.import_module(module_path)
            handler = getattr(module, func_name)
            
            # Execute with timeout
            if asyncio.iscoroutinefunction(handler):
                result = await asyncio.wait_for(
                    handler(),
                    timeout=job.timeout_seconds
                )
            else:
                result = handler()
            
            completed_at = datetime.now()
            duration = (completed_at - started_at).total_seconds()
            
            job_result = JobResult(
                job_id=job.id,
                status=JobStatus.COMPLETED,
                started_at=started_at,
                completed_at=completed_at,
                duration_seconds=duration,
                result=result
            )
            
            # Update job stats
            job.last_run = completed_at
            job.run_count += 1
            self._calculate_next_run(job)
            
            logger.info(f"Job completed: {job.name} ({duration:.2f}s)")
            
        except asyncio.TimeoutError:
            completed_at = datetime.now()
            duration = (completed_at - started_at).total_seconds()
            
            job_result = JobResult(
                job_id=job.id,
                status=JobStatus.FAILED,
                started_at=started_at,
                completed_at=completed_at,
                duration_seconds=duration,
                error=f"Timeout after {job.timeout_seconds}s"
            )
            
            job.failure_count += 1
            job.last_run = completed_at
            self._calculate_next_run(job)
            
            logger.error(f"Job timed out: {job.name}")
            
        except Exception as e:
            completed_at = datetime.now()
            duration = (completed_at - started_at).total_seconds()
            
            job_result = JobResult(
                job_id=job.id,
                status=JobStatus.FAILED,
                started_at=started_at,
                completed_at=completed_at,
                duration_seconds=duration,
                error=str(e)
            )
            
            job.failure_count += 1
            job.last_run = completed_at
            self._calculate_next_run(job)
            
            logger.error(f"Job failed: {job.name} - {e}")
        
        # Save state and record history
        self._save_jobs()
        self.job_history.append(job_result)
        
        # Notify webhooks
        await self._notify_webhooks(job_result)
        
        return job_result
    
    async def _notify_webhooks(self, result: JobResult):
        """Send webhook notifications."""
        if not self.webhooks:
            return
        
        import aiohttp
        
        payload = {
            "event": "job_completed",
            "timestamp": datetime.now().isoformat(),
            "result": result.to_dict()
        }
        
        async with aiohttp.ClientSession() as session:
            for webhook_url in self.webhooks:
                try:
                    async with session.post(webhook_url, json=payload) as resp:
                        if resp.status != 200:
                            logger.warning(f"Webhook failed: {webhook_url} ({resp.status})")
                except Exception as e:
                    logger.error(f"Webhook error: {webhook_url} - {e}")
    
    def add_webhook(self, url: str):
        """Add a webhook URL for notifications."""
        if url not in self.webhooks:
            self.webhooks.append(url)
    
    def remove_webhook(self, url: str):
        """Remove a webhook URL."""
        if url in self.webhooks:
            self.webhooks.remove(url)
    
    async def start(self, check_interval: int = 60):
        """Start the scheduler loop."""
        self.running = True
        logger.info("Scheduler started")
        
        while self.running:
            try:
                # Get due jobs
                due_jobs = self.get_due_jobs()
                
                # Run due jobs
                for job in due_jobs:
                    await self.run_job(job)
                
                # Wait for next check
                await asyncio.sleep(check_interval)
                
            except Exception as e:
                logger.error(f"Scheduler error: {e}")
                await asyncio.sleep(check_interval)
    
    def stop(self):
        """Stop the scheduler."""
        self.running = False
        logger.info("Scheduler stopped")
    
    def get_stats(self) -> Dict:
        """Get scheduler statistics."""
        return {
            "total_jobs": len(self.jobs),
            "enabled_jobs": sum(1 for j in self.jobs.values() if j.enabled),
            "total_runs": sum(j.run_count for j in self.jobs.values()),
            "total_failures": sum(j.failure_count for j in self.jobs.values()),
            "recent_history": [r.to_dict() for r in self.job_history[-10:]],
            "webhooks": len(self.webhooks)
        }


# =============================================================================
# DEFAULT JOBS
# =============================================================================

def get_default_jobs() -> List[ScheduledJob]:
    """Get default scheduled jobs for the Mental Models System."""
    return [
        ScheduledJob(
            id="daily_improvement_analysis",
            name="Daily Improvement Analysis",
            description="Generate improvement suggestions based on system state",
            handler="src.improvement.suggestion_engine.generate_all_suggestions",
            schedule_type="cron",
            schedule_value="0 6 * * *",  # 6 AM daily
            priority=JobPriority.HIGH
        ),
        ScheduledJob(
            id="hourly_signal_harvest",
            name="Hourly Signal Harvest",
            description="Harvest signals from configured sources",
            handler="src.harvester.signal_harvester.harvest_all",
            schedule_type="cron",
            schedule_value="0 * * * *",  # Every hour
            priority=JobPriority.MEDIUM
        ),
        ScheduledJob(
            id="weekly_model_effectiveness",
            name="Weekly Model Effectiveness Update",
            description="Update model effectiveness scores based on outcomes",
            handler="src.tracker.effectiveness_tracker.update_all_scores",
            schedule_type="cron",
            schedule_value="0 0 * * 0",  # Sunday midnight
            priority=JobPriority.MEDIUM
        ),
        ScheduledJob(
            id="daily_cache_cleanup",
            name="Daily Cache Cleanup",
            description="Clean up expired cache entries",
            handler="src.cache.redis_cache.cleanup_expired",
            schedule_type="cron",
            schedule_value="0 3 * * *",  # 3 AM daily
            priority=JobPriority.LOW
        )
    ]


def setup_default_scheduler() -> JobScheduler:
    """Create a scheduler with default jobs."""
    scheduler = JobScheduler()
    
    for job in get_default_jobs():
        if job.id not in scheduler.jobs:
            scheduler.add_job(job)
    
    return scheduler


if __name__ == "__main__":
    # Test the scheduler
    scheduler = setup_default_scheduler()
    
    print("Scheduled Jobs:")
    for job in scheduler.list_jobs():
        print(f"  - {job.name}: next run at {job.next_run}")
    
    print(f"\nStats: {scheduler.get_stats()}")
