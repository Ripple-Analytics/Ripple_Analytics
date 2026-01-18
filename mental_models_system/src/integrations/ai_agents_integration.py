import logging
import json
import asyncio
import aiohttp
from dataclasses import dataclass, field
from typing import Dict, Any, List, Optional, Callable
from datetime import datetime
from enum import Enum
from pathlib import Path

logger = logging.getLogger(__name__)


class AgentType(Enum):
    DEVIN = "devin"
    MANUS = "manus"
    CUSTOM = "custom"


class ImprovementType(Enum):
    CODE_REFACTOR = "code_refactor"
    BUG_FIX = "bug_fix"
    FEATURE_ADD = "feature_add"
    DOCUMENTATION = "documentation"
    PERFORMANCE = "performance"
    SECURITY = "security"
    TEST_COVERAGE = "test_coverage"
    MODEL_UPDATE = "model_update"


class ImprovementStatus(Enum):
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    REJECTED = "rejected"


@dataclass
class ImprovementRequest:
    id: str
    type: ImprovementType
    title: str
    description: str
    priority: int = 5
    source: str = ""
    context: Dict[str, Any] = field(default_factory=dict)
    files_affected: List[str] = field(default_factory=list)
    created_at: datetime = field(default_factory=datetime.now)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "type": self.type.value,
            "title": self.title,
            "description": self.description,
            "priority": self.priority,
            "source": self.source,
            "context": self.context,
            "files_affected": self.files_affected,
            "created_at": self.created_at.isoformat()
        }


@dataclass
class ImprovementResult:
    request_id: str
    status: ImprovementStatus
    agent: AgentType
    changes_made: List[str] = field(default_factory=list)
    files_modified: List[str] = field(default_factory=list)
    commit_hash: Optional[str] = None
    pr_url: Optional[str] = None
    error_message: Optional[str] = None
    completed_at: Optional[datetime] = None
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "request_id": self.request_id,
            "status": self.status.value,
            "agent": self.agent.value,
            "changes_made": self.changes_made,
            "files_modified": self.files_modified,
            "commit_hash": self.commit_hash,
            "pr_url": self.pr_url,
            "error_message": self.error_message,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None
        }


@dataclass
class AgentConfig:
    agent_type: AgentType
    api_url: str
    api_key: Optional[str] = None
    session_id: Optional[str] = None
    capabilities: List[str] = field(default_factory=list)
    rate_limit: float = 1.0
    timeout: int = 300


class DevinIntegration:
    def __init__(self, config: AgentConfig):
        self.config = config
        self.session: Optional[aiohttp.ClientSession] = None
        self.pending_requests: List[ImprovementRequest] = []
        self.completed_results: List[ImprovementResult] = []
        
    async def initialize(self):
        headers = {}
        if self.config.api_key:
            headers["Authorization"] = f"Bearer {self.config.api_key}"
        headers["Content-Type"] = "application/json"
        
        self.session = aiohttp.ClientSession(headers=headers)
        logger.info(f"Devin integration initialized with session: {self.config.session_id}")
    
    async def close(self):
        if self.session:
            await self.session.close()
    
    async def submit_improvement(self, request: ImprovementRequest) -> str:
        if not self.session:
            await self.initialize()
        
        payload = {
            "session_id": self.config.session_id,
            "task": {
                "type": request.type.value,
                "title": request.title,
                "description": request.description,
                "context": request.context,
                "files": request.files_affected
            }
        }
        
        try:
            async with self.session.post(
                f"{self.config.api_url}/tasks",
                json=payload,
                timeout=self.config.timeout
            ) as response:
                if response.status == 200:
                    data = await response.json()
                    self.pending_requests.append(request)
                    logger.info(f"Submitted improvement request: {request.id}")
                    return data.get("task_id", request.id)
                else:
                    error = await response.text()
                    logger.error(f"Failed to submit improvement: {error}")
                    return ""
        except Exception as e:
            logger.error(f"Error submitting improvement: {e}")
            return ""
    
    async def check_status(self, task_id: str) -> Optional[ImprovementResult]:
        if not self.session:
            await self.initialize()
        
        try:
            async with self.session.get(
                f"{self.config.api_url}/tasks/{task_id}/status",
                timeout=30
            ) as response:
                if response.status == 200:
                    data = await response.json()
                    
                    status_map = {
                        "pending": ImprovementStatus.PENDING,
                        "in_progress": ImprovementStatus.IN_PROGRESS,
                        "completed": ImprovementStatus.COMPLETED,
                        "failed": ImprovementStatus.FAILED
                    }
                    
                    return ImprovementResult(
                        request_id=task_id,
                        status=status_map.get(data.get("status"), ImprovementStatus.PENDING),
                        agent=AgentType.DEVIN,
                        changes_made=data.get("changes", []),
                        files_modified=data.get("files_modified", []),
                        commit_hash=data.get("commit_hash"),
                        pr_url=data.get("pr_url"),
                        completed_at=datetime.fromisoformat(data["completed_at"]) if data.get("completed_at") else None
                    )
        except Exception as e:
            logger.error(f"Error checking status: {e}")
        
        return None
    
    async def send_message(self, message: str) -> Optional[str]:
        if not self.session:
            await self.initialize()
        
        payload = {
            "session_id": self.config.session_id,
            "message": message
        }
        
        try:
            async with self.session.post(
                f"{self.config.api_url}/messages",
                json=payload,
                timeout=60
            ) as response:
                if response.status == 200:
                    data = await response.json()
                    return data.get("response")
        except Exception as e:
            logger.error(f"Error sending message: {e}")
        
        return None


class ManusIntegration:
    def __init__(self, config: AgentConfig):
        self.config = config
        self.session: Optional[aiohttp.ClientSession] = None
        self.pending_requests: List[ImprovementRequest] = []
        self.completed_results: List[ImprovementResult] = []
        
    async def initialize(self):
        headers = {}
        if self.config.api_key:
            headers["Authorization"] = f"Bearer {self.config.api_key}"
        headers["Content-Type"] = "application/json"
        
        self.session = aiohttp.ClientSession(headers=headers)
        logger.info("Manus integration initialized")
    
    async def close(self):
        if self.session:
            await self.session.close()
    
    async def submit_improvement(self, request: ImprovementRequest) -> str:
        if not self.session:
            await self.initialize()
        
        payload = {
            "task": {
                "type": request.type.value,
                "title": request.title,
                "description": request.description,
                "context": request.context,
                "files": request.files_affected
            }
        }
        
        try:
            async with self.session.post(
                f"{self.config.api_url}/tasks",
                json=payload,
                timeout=self.config.timeout
            ) as response:
                if response.status == 200:
                    data = await response.json()
                    self.pending_requests.append(request)
                    logger.info(f"Submitted improvement request to Manus: {request.id}")
                    return data.get("task_id", request.id)
                else:
                    error = await response.text()
                    logger.error(f"Failed to submit improvement to Manus: {error}")
                    return ""
        except Exception as e:
            logger.error(f"Error submitting improvement to Manus: {e}")
            return ""
    
    async def check_status(self, task_id: str) -> Optional[ImprovementResult]:
        if not self.session:
            await self.initialize()
        
        try:
            async with self.session.get(
                f"{self.config.api_url}/tasks/{task_id}/status",
                timeout=30
            ) as response:
                if response.status == 200:
                    data = await response.json()
                    
                    status_map = {
                        "pending": ImprovementStatus.PENDING,
                        "in_progress": ImprovementStatus.IN_PROGRESS,
                        "completed": ImprovementStatus.COMPLETED,
                        "failed": ImprovementStatus.FAILED
                    }
                    
                    return ImprovementResult(
                        request_id=task_id,
                        status=status_map.get(data.get("status"), ImprovementStatus.PENDING),
                        agent=AgentType.MANUS,
                        changes_made=data.get("changes", []),
                        files_modified=data.get("files_modified", []),
                        commit_hash=data.get("commit_hash"),
                        pr_url=data.get("pr_url"),
                        completed_at=datetime.fromisoformat(data["completed_at"]) if data.get("completed_at") else None
                    )
        except Exception as e:
            logger.error(f"Error checking Manus status: {e}")
        
        return None
    
    async def send_message(self, message: str) -> Optional[str]:
        if not self.session:
            await self.initialize()
        
        payload = {"message": message}
        
        try:
            async with self.session.post(
                f"{self.config.api_url}/messages",
                json=payload,
                timeout=60
            ) as response:
                if response.status == 200:
                    data = await response.json()
                    return data.get("response")
        except Exception as e:
            logger.error(f"Error sending message to Manus: {e}")
        
        return None


class AutomatedImprovementCycle:
    def __init__(
        self,
        devin_config: Optional[AgentConfig] = None,
        manus_config: Optional[AgentConfig] = None
    ):
        self.devin: Optional[DevinIntegration] = None
        self.manus: Optional[ManusIntegration] = None
        
        if devin_config:
            self.devin = DevinIntegration(devin_config)
        
        if manus_config:
            self.manus = ManusIntegration(manus_config)
        
        self.improvement_queue: List[ImprovementRequest] = []
        self.results_history: List[ImprovementResult] = []
        self.callbacks: List[Callable[[ImprovementResult], None]] = []
        self.is_running = False
    
    async def initialize(self):
        if self.devin:
            await self.devin.initialize()
        if self.manus:
            await self.manus.initialize()
    
    async def close(self):
        if self.devin:
            await self.devin.close()
        if self.manus:
            await self.manus.close()
    
    def add_callback(self, callback: Callable[[ImprovementResult], None]):
        self.callbacks.append(callback)
    
    def queue_improvement(self, request: ImprovementRequest):
        self.improvement_queue.append(request)
        logger.info(f"Queued improvement: {request.title}")
    
    def create_improvement_from_insight(
        self,
        insight: str,
        improvement_type: ImprovementType = ImprovementType.FEATURE_ADD
    ) -> ImprovementRequest:
        import uuid
        
        request = ImprovementRequest(
            id=str(uuid.uuid4())[:8],
            type=improvement_type,
            title=f"Improvement from insight: {insight[:50]}...",
            description=insight,
            source="continuous_learning",
            context={"auto_generated": True}
        )
        
        return request
    
    async def process_queue(self):
        while self.improvement_queue:
            request = self.improvement_queue.pop(0)
            
            agent = self._select_agent(request)
            
            if agent == AgentType.DEVIN and self.devin:
                task_id = await self.devin.submit_improvement(request)
                if task_id:
                    result = await self._wait_for_completion(self.devin, task_id)
                    if result:
                        self.results_history.append(result)
                        self._notify_callbacks(result)
            
            elif agent == AgentType.MANUS and self.manus:
                task_id = await self.manus.submit_improvement(request)
                if task_id:
                    result = await self._wait_for_completion(self.manus, task_id)
                    if result:
                        self.results_history.append(result)
                        self._notify_callbacks(result)
            
            await asyncio.sleep(1)
    
    def _select_agent(self, request: ImprovementRequest) -> AgentType:
        if request.type in [ImprovementType.CODE_REFACTOR, ImprovementType.BUG_FIX, ImprovementType.FEATURE_ADD]:
            if self.devin:
                return AgentType.DEVIN
        
        if request.type in [ImprovementType.DOCUMENTATION, ImprovementType.MODEL_UPDATE]:
            if self.manus:
                return AgentType.MANUS
        
        if self.devin:
            return AgentType.DEVIN
        if self.manus:
            return AgentType.MANUS
        
        return AgentType.CUSTOM
    
    async def _wait_for_completion(
        self,
        agent: DevinIntegration | ManusIntegration,
        task_id: str,
        timeout: int = 3600
    ) -> Optional[ImprovementResult]:
        start_time = datetime.now()
        
        while (datetime.now() - start_time).total_seconds() < timeout:
            result = await agent.check_status(task_id)
            
            if result and result.status in [ImprovementStatus.COMPLETED, ImprovementStatus.FAILED]:
                return result
            
            await asyncio.sleep(30)
        
        return ImprovementResult(
            request_id=task_id,
            status=ImprovementStatus.FAILED,
            agent=AgentType.CUSTOM,
            error_message="Timeout waiting for completion"
        )
    
    def _notify_callbacks(self, result: ImprovementResult):
        for callback in self.callbacks:
            try:
                callback(result)
            except Exception as e:
                logger.error(f"Callback error: {e}")
    
    async def coordinate_agents(self, task_description: str):
        if self.devin:
            await self.devin.send_message(
                f"@Manus I'm working on: {task_description}. "
                "What aspects would you like to handle?"
            )
        
        if self.manus:
            await self.manus.send_message(
                f"@Devin I'm coordinating on: {task_description}. "
                "Let me know what you need from me."
            )
    
    async def request_improvements_from_learning(
        self,
        insights: List[str],
        patterns: Dict[str, int]
    ):
        for insight in insights[:5]:
            if "correlation" in insight.lower():
                request = self.create_improvement_from_insight(
                    insight,
                    ImprovementType.MODEL_UPDATE
                )
            elif "pattern" in insight.lower():
                request = self.create_improvement_from_insight(
                    insight,
                    ImprovementType.FEATURE_ADD
                )
            else:
                request = self.create_improvement_from_insight(insight)
            
            self.queue_improvement(request)
        
        if patterns:
            top_patterns = sorted(patterns.items(), key=lambda x: -x[1])[:3]
            pattern_insight = f"Top recurring patterns: {', '.join(p[0] for p in top_patterns)}"
            request = self.create_improvement_from_insight(
                pattern_insight,
                ImprovementType.DOCUMENTATION
            )
            self.queue_improvement(request)
    
    def get_stats(self) -> Dict[str, Any]:
        completed = [r for r in self.results_history if r.status == ImprovementStatus.COMPLETED]
        failed = [r for r in self.results_history if r.status == ImprovementStatus.FAILED]
        
        return {
            "queue_size": len(self.improvement_queue),
            "total_processed": len(self.results_history),
            "completed": len(completed),
            "failed": len(failed),
            "success_rate": len(completed) / len(self.results_history) if self.results_history else 0,
            "devin_available": self.devin is not None,
            "manus_available": self.manus is not None
        }


def create_devin_config(
    session_id: str,
    api_url: str = "https://api.devin.ai/v1",
    api_key: Optional[str] = None
) -> AgentConfig:
    return AgentConfig(
        agent_type=AgentType.DEVIN,
        api_url=api_url,
        api_key=api_key,
        session_id=session_id,
        capabilities=["code_refactor", "bug_fix", "feature_add", "test_coverage"]
    )


def create_manus_config(
    api_url: str = "https://api.manus.ai/v1",
    api_key: Optional[str] = None
) -> AgentConfig:
    return AgentConfig(
        agent_type=AgentType.MANUS,
        api_url=api_url,
        api_key=api_key,
        capabilities=["documentation", "research", "model_update", "data_analysis"]
    )


def create_improvement_cycle(
    devin_session_id: Optional[str] = None,
    manus_api_key: Optional[str] = None
) -> AutomatedImprovementCycle:
    devin_config = None
    manus_config = None
    
    if devin_session_id:
        devin_config = create_devin_config(devin_session_id)
    
    if manus_api_key:
        manus_config = create_manus_config(api_key=manus_api_key)
    
    return AutomatedImprovementCycle(
        devin_config=devin_config,
        manus_config=manus_config
    )
