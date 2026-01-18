"""
API routes for scheduler and webhook management.
"""
from fastapi import APIRouter, HTTPException, BackgroundTasks
from pydantic import BaseModel
from typing import Optional, List
from datetime import datetime

router = APIRouter(prefix="/scheduler", tags=["scheduler"])
webhook_router = APIRouter(prefix="/webhooks", tags=["webhooks"])


# Pydantic models for request/response
class JobCreate(BaseModel):
    id: str
    name: str
    description: Optional[str] = None
    handler: str
    schedule_type: str  # "cron" or "interval"
    schedule_value: str
    priority: str = "medium"
    enabled: bool = True


class JobUpdate(BaseModel):
    name: Optional[str] = None
    description: Optional[str] = None
    schedule_value: Optional[str] = None
    priority: Optional[str] = None
    enabled: Optional[bool] = None


class JobResponse(BaseModel):
    id: str
    name: str
    description: str
    handler: str
    schedule_type: str
    schedule_value: str
    priority: str
    enabled: bool
    next_run: Optional[str] = None
    last_run: Optional[str] = None
    run_count: int
    failure_count: int


class WebhookCreate(BaseModel):
    id: str
    name: str
    type: str  # "slack", "discord", "teams", "generic"
    url: str
    events: Optional[List[str]] = None
    min_priority: str = "low"
    enabled: bool = True
    rate_limit_per_minute: int = 10
    max_retries: int = 3


class WebhookUpdate(BaseModel):
    name: Optional[str] = None
    url: Optional[str] = None
    events: Optional[List[str]] = None
    min_priority: Optional[str] = None
    enabled: Optional[bool] = None


class WebhookResponse(BaseModel):
    id: str
    name: str
    type: str
    url: str
    events: List[str]
    min_priority: str
    enabled: bool
    rate_limit_per_minute: int
    max_retries: int


# ==================== SCHEDULER ROUTES ====================

@router.get("/jobs", response_model=List[JobResponse])
async def list_jobs():
    """List all scheduled jobs."""
    from src.scheduler import JobScheduler
    
    scheduler = JobScheduler()
    jobs = scheduler.list_jobs()
    
    return [
        JobResponse(
            id=job.id,
            name=job.name,
            description=job.description,
            handler=job.handler,
            schedule_type=job.schedule_type,
            schedule_value=job.schedule_value,
            priority=job.priority.name.lower(),
            enabled=job.enabled,
            next_run=job.next_run.isoformat() if job.next_run else None,
            last_run=job.last_run.isoformat() if job.last_run else None,
            run_count=job.run_count,
            failure_count=job.failure_count
        )
        for job in jobs
    ]


@router.get("/jobs/{job_id}", response_model=JobResponse)
async def get_job(job_id: str):
    """Get a specific job by ID."""
    from src.scheduler import JobScheduler
    
    scheduler = JobScheduler()
    
    if job_id not in scheduler.jobs:
        raise HTTPException(status_code=404, detail=f"Job not found: {job_id}")
    
    job = scheduler.jobs[job_id]
    
    return JobResponse(
        id=job.id,
        name=job.name,
        description=job.description,
        handler=job.handler,
        schedule_type=job.schedule_type,
        schedule_value=job.schedule_value,
        priority=job.priority.name.lower(),
        enabled=job.enabled,
        next_run=job.next_run.isoformat() if job.next_run else None,
        last_run=job.last_run.isoformat() if job.last_run else None,
        run_count=job.run_count,
        failure_count=job.failure_count
    )


@router.post("/jobs", response_model=JobResponse)
async def create_job(job_data: JobCreate):
    """Create a new scheduled job."""
    from src.scheduler import JobScheduler, ScheduledJob, JobPriority
    
    scheduler = JobScheduler()
    
    if job_data.id in scheduler.jobs:
        raise HTTPException(status_code=400, detail=f"Job already exists: {job_data.id}")
    
    try:
        priority = JobPriority[job_data.priority.upper()]
    except KeyError:
        raise HTTPException(status_code=400, detail=f"Invalid priority: {job_data.priority}")
    
    job = ScheduledJob(
        id=job_data.id,
        name=job_data.name,
        description=job_data.description or f"Custom job: {job_data.name}",
        handler=job_data.handler,
        schedule_type=job_data.schedule_type,
        schedule_value=job_data.schedule_value,
        priority=priority,
        enabled=job_data.enabled
    )
    
    if not scheduler.add_job(job):
        raise HTTPException(status_code=500, detail="Failed to add job")
    
    return JobResponse(
        id=job.id,
        name=job.name,
        description=job.description,
        handler=job.handler,
        schedule_type=job.schedule_type,
        schedule_value=job.schedule_value,
        priority=job.priority.name.lower(),
        enabled=job.enabled,
        next_run=job.next_run.isoformat() if job.next_run else None,
        last_run=job.last_run.isoformat() if job.last_run else None,
        run_count=job.run_count,
        failure_count=job.failure_count
    )


@router.patch("/jobs/{job_id}", response_model=JobResponse)
async def update_job(job_id: str, job_data: JobUpdate):
    """Update an existing job."""
    from src.scheduler import JobScheduler, JobPriority
    
    scheduler = JobScheduler()
    
    if job_id not in scheduler.jobs:
        raise HTTPException(status_code=404, detail=f"Job not found: {job_id}")
    
    job = scheduler.jobs[job_id]
    
    if job_data.name is not None:
        job.name = job_data.name
    if job_data.description is not None:
        job.description = job_data.description
    if job_data.schedule_value is not None:
        job.schedule_value = job_data.schedule_value
    if job_data.priority is not None:
        try:
            job.priority = JobPriority[job_data.priority.upper()]
        except KeyError:
            raise HTTPException(status_code=400, detail=f"Invalid priority: {job_data.priority}")
    if job_data.enabled is not None:
        job.enabled = job_data.enabled
    
    return JobResponse(
        id=job.id,
        name=job.name,
        description=job.description,
        handler=job.handler,
        schedule_type=job.schedule_type,
        schedule_value=job.schedule_value,
        priority=job.priority.name.lower(),
        enabled=job.enabled,
        next_run=job.next_run.isoformat() if job.next_run else None,
        last_run=job.last_run.isoformat() if job.last_run else None,
        run_count=job.run_count,
        failure_count=job.failure_count
    )


@router.delete("/jobs/{job_id}")
async def delete_job(job_id: str):
    """Delete a scheduled job."""
    from src.scheduler import JobScheduler
    
    scheduler = JobScheduler()
    
    if not scheduler.remove_job(job_id):
        raise HTTPException(status_code=404, detail=f"Job not found: {job_id}")
    
    return {"message": f"Job deleted: {job_id}"}


@router.post("/jobs/{job_id}/enable")
async def enable_job(job_id: str):
    """Enable a scheduled job."""
    from src.scheduler import JobScheduler
    
    scheduler = JobScheduler()
    
    if not scheduler.enable_job(job_id):
        raise HTTPException(status_code=404, detail=f"Job not found: {job_id}")
    
    return {"message": f"Job enabled: {job_id}"}


@router.post("/jobs/{job_id}/disable")
async def disable_job(job_id: str):
    """Disable a scheduled job."""
    from src.scheduler import JobScheduler
    
    scheduler = JobScheduler()
    
    if not scheduler.disable_job(job_id):
        raise HTTPException(status_code=404, detail=f"Job not found: {job_id}")
    
    return {"message": f"Job disabled: {job_id}"}


@router.post("/jobs/{job_id}/run")
async def run_job(job_id: str, background_tasks: BackgroundTasks):
    """Run a job immediately in the background."""
    from src.scheduler import JobScheduler
    import importlib
    
    scheduler = JobScheduler()
    
    if job_id not in scheduler.jobs:
        raise HTTPException(status_code=404, detail=f"Job not found: {job_id}")
    
    job = scheduler.jobs[job_id]
    
    async def execute_job():
        try:
            module_path, func_name = job.handler.rsplit('.', 1)
            module = importlib.import_module(module_path)
            handler = getattr(module, func_name)
            
            import asyncio
            if asyncio.iscoroutinefunction(handler):
                await handler()
            else:
                handler()
        except Exception as e:
            print(f"Job {job_id} failed: {e}")
    
    background_tasks.add_task(execute_job)
    
    return {"message": f"Job started: {job_id}", "job_name": job.name}


@router.get("/stats")
async def get_scheduler_stats():
    """Get scheduler statistics."""
    from src.scheduler import JobScheduler
    
    scheduler = JobScheduler()
    stats = scheduler.get_stats()
    
    jobs = scheduler.list_jobs()
    by_type = {}
    by_priority = {}
    for job in jobs:
        by_type[job.schedule_type] = by_type.get(job.schedule_type, 0) + 1
        by_priority[job.priority.name] = by_priority.get(job.priority.name, 0) + 1
    
    success_rate = None
    if stats['total_runs'] > 0:
        success_rate = ((stats['total_runs'] - stats['total_failures']) / stats['total_runs']) * 100
    
    return {
        "total_jobs": stats['total_jobs'],
        "enabled_jobs": stats['enabled_jobs'],
        "total_runs": stats['total_runs'],
        "total_failures": stats['total_failures'],
        "success_rate": success_rate,
        "jobs_by_type": by_type,
        "jobs_by_priority": by_priority
    }


# ==================== WEBHOOK ROUTES ====================

@webhook_router.get("/targets", response_model=List[WebhookResponse])
async def list_webhook_targets():
    """List all webhook targets."""
    from src.webhooks import WebhookManager
    
    manager = WebhookManager()
    targets = manager.list_targets()
    
    return [
        WebhookResponse(
            id=target.id,
            name=target.name,
            type=target.type.value,
            url=target.url,
            events=[e.value for e in target.events] if target.events else [],
            min_priority=target.min_priority.name.lower(),
            enabled=target.enabled,
            rate_limit_per_minute=target.rate_limit_per_minute,
            max_retries=target.max_retries
        )
        for target in targets
    ]


@webhook_router.get("/targets/{target_id}", response_model=WebhookResponse)
async def get_webhook_target(target_id: str):
    """Get a specific webhook target by ID."""
    from src.webhooks import WebhookManager
    
    manager = WebhookManager()
    
    if target_id not in manager.targets:
        raise HTTPException(status_code=404, detail=f"Target not found: {target_id}")
    
    target = manager.targets[target_id]
    
    return WebhookResponse(
        id=target.id,
        name=target.name,
        type=target.type.value,
        url=target.url,
        events=[e.value for e in target.events] if target.events else [],
        min_priority=target.min_priority.name.lower(),
        enabled=target.enabled,
        rate_limit_per_minute=target.rate_limit_per_minute,
        max_retries=target.max_retries
    )


@webhook_router.post("/targets", response_model=WebhookResponse)
async def create_webhook_target(webhook_data: WebhookCreate):
    """Create a new webhook target."""
    from src.webhooks import WebhookManager, WebhookTarget, WebhookType, EventType, EventPriority
    
    manager = WebhookManager()
    
    if webhook_data.id in manager.targets:
        raise HTTPException(status_code=400, detail=f"Target already exists: {webhook_data.id}")
    
    try:
        webhook_type = WebhookType(webhook_data.type)
    except ValueError:
        raise HTTPException(status_code=400, detail=f"Invalid webhook type: {webhook_data.type}")
    
    try:
        min_priority = EventPriority[webhook_data.min_priority.upper()]
    except KeyError:
        raise HTTPException(status_code=400, detail=f"Invalid priority: {webhook_data.min_priority}")
    
    events = []
    if webhook_data.events:
        for event_name in webhook_data.events:
            try:
                events.append(EventType(event_name))
            except ValueError:
                raise HTTPException(status_code=400, detail=f"Invalid event type: {event_name}")
    
    target = WebhookTarget(
        id=webhook_data.id,
        name=webhook_data.name,
        type=webhook_type,
        url=webhook_data.url,
        events=events,
        min_priority=min_priority,
        enabled=webhook_data.enabled,
        rate_limit_per_minute=webhook_data.rate_limit_per_minute,
        max_retries=webhook_data.max_retries
    )
    
    if not manager.add_target(target):
        raise HTTPException(status_code=500, detail="Failed to add webhook target")
    
    return WebhookResponse(
        id=target.id,
        name=target.name,
        type=target.type.value,
        url=target.url,
        events=[e.value for e in target.events] if target.events else [],
        min_priority=target.min_priority.name.lower(),
        enabled=target.enabled,
        rate_limit_per_minute=target.rate_limit_per_minute,
        max_retries=target.max_retries
    )


@webhook_router.delete("/targets/{target_id}")
async def delete_webhook_target(target_id: str):
    """Delete a webhook target."""
    from src.webhooks import WebhookManager
    
    manager = WebhookManager()
    
    if not manager.remove_target(target_id):
        raise HTTPException(status_code=404, detail=f"Target not found: {target_id}")
    
    return {"message": f"Target deleted: {target_id}"}


@webhook_router.post("/targets/{target_id}/test")
async def test_webhook_target(target_id: str):
    """Send a test webhook to a specific target."""
    from src.webhooks import WebhookManager, WebhookEvent, EventType, EventPriority
    
    manager = WebhookManager()
    
    if target_id not in manager.targets:
        raise HTTPException(status_code=404, detail=f"Target not found: {target_id}")
    
    target = manager.targets[target_id]
    
    event = WebhookEvent(
        type=EventType.SYSTEM_ALERT,
        priority=EventPriority.LOW,
        title="Test Webhook",
        message="This is a test message from the Mental Models System API",
        data={"test": True, "timestamp": datetime.now().isoformat()}
    )
    
    success = await manager._send_to_target(target, event)
    
    if success:
        return {"message": f"Test webhook sent successfully to: {target.name}"}
    else:
        raise HTTPException(status_code=500, detail="Failed to send test webhook")


@webhook_router.get("/stats")
async def get_webhook_stats():
    """Get webhook statistics."""
    from src.webhooks import WebhookManager
    
    manager = WebhookManager()
    stats = manager.get_stats()
    
    return stats
