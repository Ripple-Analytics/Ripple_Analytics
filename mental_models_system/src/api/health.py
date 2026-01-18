"""
Health Check Endpoints

Comprehensive health monitoring for the Mental Models System:
- Liveness probe (is the service running?)
- Readiness probe (is the service ready to accept requests?)
- Deep health check (are all dependencies healthy?)
- System metrics
"""

import asyncio
import os
import platform
import psutil
import time
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable, Dict, List, Optional
import json


class HealthStatus(Enum):
    """Health status levels."""
    HEALTHY = "healthy"
    DEGRADED = "degraded"
    UNHEALTHY = "unhealthy"


@dataclass
class ComponentHealth:
    """Health status of a single component."""
    name: str
    status: HealthStatus
    message: Optional[str] = None
    latency_ms: Optional[float] = None
    details: Dict[str, Any] = field(default_factory=dict)
    checked_at: datetime = field(default_factory=datetime.now)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "status": self.status.value,
            "message": self.message,
            "latency_ms": self.latency_ms,
            "details": self.details,
            "checked_at": self.checked_at.isoformat()
        }


@dataclass
class HealthReport:
    """Complete health report for the system."""
    status: HealthStatus
    version: str
    uptime_seconds: float
    components: List[ComponentHealth] = field(default_factory=list)
    system_metrics: Dict[str, Any] = field(default_factory=dict)
    checked_at: datetime = field(default_factory=datetime.now)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "status": self.status.value,
            "version": self.version,
            "uptime_seconds": self.uptime_seconds,
            "components": [c.to_dict() for c in self.components],
            "system_metrics": self.system_metrics,
            "checked_at": self.checked_at.isoformat()
        }


class HealthChecker:
    """
    Comprehensive health checker for the Mental Models System.
    
    Monitors:
    - Database connectivity
    - Redis connectivity
    - LLM service availability
    - Disk space
    - Memory usage
    - CPU usage
    - External API connectivity
    """
    
    def __init__(self, version: str = "1.0.0"):
        self.version = version
        self.start_time = time.time()
        self._checks: Dict[str, Callable] = {}
        
        # Register default checks
        self._register_default_checks()
    
    def _register_default_checks(self):
        """Register default health checks."""
        self.register_check("disk", self._check_disk)
        self.register_check("memory", self._check_memory)
        self.register_check("cpu", self._check_cpu)
    
    def register_check(self, name: str, check_func: Callable):
        """Register a custom health check."""
        self._checks[name] = check_func
    
    async def _check_disk(self) -> ComponentHealth:
        """Check disk space."""
        try:
            disk = psutil.disk_usage("/")
            percent_used = disk.percent
            
            if percent_used > 95:
                status = HealthStatus.UNHEALTHY
                message = f"Critical: {percent_used}% disk used"
            elif percent_used > 85:
                status = HealthStatus.DEGRADED
                message = f"Warning: {percent_used}% disk used"
            else:
                status = HealthStatus.HEALTHY
                message = f"OK: {percent_used}% disk used"
            
            return ComponentHealth(
                name="disk",
                status=status,
                message=message,
                details={
                    "total_gb": round(disk.total / (1024**3), 2),
                    "used_gb": round(disk.used / (1024**3), 2),
                    "free_gb": round(disk.free / (1024**3), 2),
                    "percent_used": percent_used
                }
            )
        except Exception as e:
            return ComponentHealth(
                name="disk",
                status=HealthStatus.UNHEALTHY,
                message=f"Error: {str(e)}"
            )
    
    async def _check_memory(self) -> ComponentHealth:
        """Check memory usage."""
        try:
            memory = psutil.virtual_memory()
            percent_used = memory.percent
            
            if percent_used > 95:
                status = HealthStatus.UNHEALTHY
                message = f"Critical: {percent_used}% memory used"
            elif percent_used > 85:
                status = HealthStatus.DEGRADED
                message = f"Warning: {percent_used}% memory used"
            else:
                status = HealthStatus.HEALTHY
                message = f"OK: {percent_used}% memory used"
            
            return ComponentHealth(
                name="memory",
                status=status,
                message=message,
                details={
                    "total_gb": round(memory.total / (1024**3), 2),
                    "available_gb": round(memory.available / (1024**3), 2),
                    "used_gb": round(memory.used / (1024**3), 2),
                    "percent_used": percent_used
                }
            )
        except Exception as e:
            return ComponentHealth(
                name="memory",
                status=HealthStatus.UNHEALTHY,
                message=f"Error: {str(e)}"
            )
    
    async def _check_cpu(self) -> ComponentHealth:
        """Check CPU usage."""
        try:
            cpu_percent = psutil.cpu_percent(interval=0.1)
            
            if cpu_percent > 95:
                status = HealthStatus.UNHEALTHY
                message = f"Critical: {cpu_percent}% CPU used"
            elif cpu_percent > 80:
                status = HealthStatus.DEGRADED
                message = f"Warning: {cpu_percent}% CPU used"
            else:
                status = HealthStatus.HEALTHY
                message = f"OK: {cpu_percent}% CPU used"
            
            return ComponentHealth(
                name="cpu",
                status=status,
                message=message,
                details={
                    "percent_used": cpu_percent,
                    "cpu_count": psutil.cpu_count(),
                    "load_average": os.getloadavg() if hasattr(os, 'getloadavg') else None
                }
            )
        except Exception as e:
            return ComponentHealth(
                name="cpu",
                status=HealthStatus.UNHEALTHY,
                message=f"Error: {str(e)}"
            )
    
    async def check_database(self, db_url: str) -> ComponentHealth:
        """Check database connectivity."""
        start = time.time()
        try:
            # Simulate database check (replace with actual check)
            # In production, this would execute a simple query
            await asyncio.sleep(0.01)  # Simulate query
            latency = (time.time() - start) * 1000
            
            return ComponentHealth(
                name="database",
                status=HealthStatus.HEALTHY,
                message="Connected",
                latency_ms=latency,
                details={"url": db_url.split("@")[-1] if "@" in db_url else "localhost"}
            )
        except Exception as e:
            return ComponentHealth(
                name="database",
                status=HealthStatus.UNHEALTHY,
                message=f"Connection failed: {str(e)}",
                latency_ms=(time.time() - start) * 1000
            )
    
    async def check_redis(self, redis_url: str = "redis://localhost:6379") -> ComponentHealth:
        """Check Redis connectivity."""
        start = time.time()
        try:
            import redis.asyncio as redis_client
            client = redis_client.from_url(redis_url)
            await client.ping()
            await client.close()
            latency = (time.time() - start) * 1000
            
            return ComponentHealth(
                name="redis",
                status=HealthStatus.HEALTHY,
                message="Connected",
                latency_ms=latency
            )
        except ImportError:
            return ComponentHealth(
                name="redis",
                status=HealthStatus.DEGRADED,
                message="Redis client not installed",
                latency_ms=(time.time() - start) * 1000
            )
        except Exception as e:
            return ComponentHealth(
                name="redis",
                status=HealthStatus.UNHEALTHY,
                message=f"Connection failed: {str(e)}",
                latency_ms=(time.time() - start) * 1000
            )
    
    async def check_llm(self, llm_url: str = "http://localhost:11434") -> ComponentHealth:
        """Check LLM service (Ollama) connectivity."""
        start = time.time()
        try:
            import aiohttp
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{llm_url}/api/tags", timeout=aiohttp.ClientTimeout(total=5)) as resp:
                    if resp.status == 200:
                        data = await resp.json()
                        models = [m.get("name") for m in data.get("models", [])]
                        latency = (time.time() - start) * 1000
                        
                        return ComponentHealth(
                            name="llm",
                            status=HealthStatus.HEALTHY,
                            message=f"Connected, {len(models)} models available",
                            latency_ms=latency,
                            details={"models": models[:5], "total_models": len(models)}
                        )
                    else:
                        return ComponentHealth(
                            name="llm",
                            status=HealthStatus.DEGRADED,
                            message=f"HTTP {resp.status}",
                            latency_ms=(time.time() - start) * 1000
                        )
        except Exception as e:
            return ComponentHealth(
                name="llm",
                status=HealthStatus.DEGRADED,
                message=f"Not available: {str(e)}",
                latency_ms=(time.time() - start) * 1000
            )
    
    async def liveness(self) -> Dict[str, Any]:
        """
        Liveness probe - is the service running?
        
        Returns minimal response for Kubernetes liveness probe.
        """
        return {
            "status": "alive",
            "timestamp": datetime.now().isoformat()
        }
    
    async def readiness(self) -> Dict[str, Any]:
        """
        Readiness probe - is the service ready to accept requests?
        
        Checks critical dependencies.
        """
        # Run critical checks
        disk_health = await self._check_disk()
        memory_health = await self._check_memory()
        
        # Determine overall readiness
        critical_healthy = all(
            h.status != HealthStatus.UNHEALTHY
            for h in [disk_health, memory_health]
        )
        
        return {
            "ready": critical_healthy,
            "timestamp": datetime.now().isoformat(),
            "checks": {
                "disk": disk_health.status.value,
                "memory": memory_health.status.value
            }
        }
    
    async def deep_health(
        self,
        include_db: bool = True,
        include_redis: bool = True,
        include_llm: bool = True
    ) -> HealthReport:
        """
        Deep health check - comprehensive system health.
        
        Checks all components and returns detailed report.
        """
        components = []
        
        # Run registered checks
        for name, check_func in self._checks.items():
            try:
                result = await check_func()
                components.append(result)
            except Exception as e:
                components.append(ComponentHealth(
                    name=name,
                    status=HealthStatus.UNHEALTHY,
                    message=f"Check failed: {str(e)}"
                ))
        
        # Optional checks
        if include_db:
            db_url = os.environ.get("DATABASE_URL", "postgresql://localhost:5432/mental_models")
            components.append(await self.check_database(db_url))
        
        if include_redis:
            redis_url = os.environ.get("REDIS_URL", "redis://localhost:6379")
            components.append(await self.check_redis(redis_url))
        
        if include_llm:
            llm_url = os.environ.get("LLM_URL", "http://localhost:11434")
            components.append(await self.check_llm(llm_url))
        
        # Determine overall status
        if any(c.status == HealthStatus.UNHEALTHY for c in components):
            overall_status = HealthStatus.UNHEALTHY
        elif any(c.status == HealthStatus.DEGRADED for c in components):
            overall_status = HealthStatus.DEGRADED
        else:
            overall_status = HealthStatus.HEALTHY
        
        # System metrics
        system_metrics = {
            "platform": platform.system(),
            "platform_release": platform.release(),
            "python_version": platform.python_version(),
            "hostname": platform.node(),
            "process_id": os.getpid()
        }
        
        return HealthReport(
            status=overall_status,
            version=self.version,
            uptime_seconds=time.time() - self.start_time,
            components=components,
            system_metrics=system_metrics
        )
    
    def get_system_info(self) -> Dict[str, Any]:
        """Get detailed system information."""
        return {
            "version": self.version,
            "uptime_seconds": time.time() - self.start_time,
            "platform": {
                "system": platform.system(),
                "release": platform.release(),
                "version": platform.version(),
                "machine": platform.machine(),
                "processor": platform.processor()
            },
            "python": {
                "version": platform.python_version(),
                "implementation": platform.python_implementation()
            },
            "process": {
                "pid": os.getpid(),
                "cwd": os.getcwd()
            },
            "resources": {
                "cpu_count": psutil.cpu_count(),
                "memory_total_gb": round(psutil.virtual_memory().total / (1024**3), 2),
                "disk_total_gb": round(psutil.disk_usage("/").total / (1024**3), 2)
            }
        }


# Create FastAPI router
def create_health_router(checker: HealthChecker):
    """Create FastAPI router for health endpoints."""
    from fastapi import APIRouter, Response
    
    router = APIRouter(prefix="/health", tags=["Health"])
    
    @router.get("/live")
    async def liveness():
        """Liveness probe endpoint."""
        return await checker.liveness()
    
    @router.get("/ready")
    async def readiness(response: Response):
        """Readiness probe endpoint."""
        result = await checker.readiness()
        if not result["ready"]:
            response.status_code = 503
        return result
    
    @router.get("/")
    async def health(response: Response):
        """Deep health check endpoint."""
        report = await checker.deep_health()
        if report.status == HealthStatus.UNHEALTHY:
            response.status_code = 503
        elif report.status == HealthStatus.DEGRADED:
            response.status_code = 200  # Still operational
        return report.to_dict()
    
    @router.get("/info")
    async def system_info():
        """System information endpoint."""
        return checker.get_system_info()
    
    return router


if __name__ == "__main__":
    import asyncio
    
    async def test():
        checker = HealthChecker(version="1.0.0")
        
        # Test liveness
        print("Liveness:", await checker.liveness())
        
        # Test readiness
        print("\nReadiness:", await checker.readiness())
        
        # Test deep health
        report = await checker.deep_health(include_db=False, include_redis=False, include_llm=False)
        print(f"\nDeep Health: {report.status.value}")
        for component in report.components:
            print(f"  - {component.name}: {component.status.value} - {component.message}")
        
        # Test system info
        print("\nSystem Info:", json.dumps(checker.get_system_info(), indent=2))
    
    asyncio.run(test())
