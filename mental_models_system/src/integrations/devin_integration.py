"""
Devin integration for automated improvement cycles.

This allows the Mental Models System to integrate with Devin AI for:
- Automated code improvements based on mental models analysis
- Continuous improvement cycle
- Feedback loop between analysis and implementation
"""

from typing import Any, Dict, List, Optional
import logging
from dataclasses import dataclass, field
from datetime import datetime

logger = logging.getLogger(__name__)


@dataclass
class ImprovementRequest:
    """Request for Devin to make an improvement."""
    id: str
    description: str
    priority: str  # low, medium, high, critical
    category: str  # bug_fix, feature, refactor, documentation
    context: Dict[str, Any] = field(default_factory=dict)
    created_at: datetime = field(default_factory=datetime.utcnow)
    status: str = "pending"  # pending, in_progress, completed, failed


@dataclass
class ImprovementResult:
    """Result from a Devin improvement."""
    request_id: str
    success: bool
    pr_url: Optional[str] = None
    changes_made: List[str] = field(default_factory=list)
    error: Optional[str] = None
    completed_at: Optional[datetime] = None


class DevinIntegration:
    """
    Integration with Devin AI for automated improvements.
    
    This creates a feedback loop where:
    1. Mental Models System analyzes code/documents
    2. Identifies areas for improvement
    3. Creates improvement requests for Devin
    4. Devin implements improvements
    5. Results feed back into the system
    """
    
    def __init__(self, session_url: Optional[str] = None):
        self.session_url = session_url or "https://app.devin.ai/sessions"
        self._pending_requests: List[ImprovementRequest] = []
        self._completed_requests: List[ImprovementResult] = []
    
    def create_improvement_request(
        self,
        description: str,
        priority: str = "medium",
        category: str = "refactor",
        context: Optional[Dict[str, Any]] = None
    ) -> ImprovementRequest:
        """
        Create a new improvement request for Devin.
        
        Args:
            description: What improvement is needed
            priority: Priority level (low, medium, high, critical)
            category: Type of improvement
            context: Additional context for the improvement
        """
        request = ImprovementRequest(
            id=f"imp_{datetime.utcnow().strftime('%Y%m%d%H%M%S')}",
            description=description,
            priority=priority,
            category=category,
            context=context or {}
        )
        
        self._pending_requests.append(request)
        logger.info(f"Created improvement request: {request.id}")
        
        return request
    
    def get_pending_requests(self) -> List[ImprovementRequest]:
        """Get all pending improvement requests."""
        return [r for r in self._pending_requests if r.status == "pending"]
    
    def get_request_by_id(self, request_id: str) -> Optional[ImprovementRequest]:
        """Get a specific improvement request."""
        for request in self._pending_requests:
            if request.id == request_id:
                return request
        return None
    
    def mark_request_completed(
        self,
        request_id: str,
        success: bool,
        pr_url: Optional[str] = None,
        changes_made: Optional[List[str]] = None,
        error: Optional[str] = None
    ) -> Optional[ImprovementResult]:
        """Mark an improvement request as completed."""
        request = self.get_request_by_id(request_id)
        if not request:
            return None
        
        request.status = "completed" if success else "failed"
        
        result = ImprovementResult(
            request_id=request_id,
            success=success,
            pr_url=pr_url,
            changes_made=changes_made or [],
            error=error,
            completed_at=datetime.utcnow()
        )
        
        self._completed_requests.append(result)
        logger.info(f"Completed improvement request: {request_id} (success={success})")
        
        return result
    
    def generate_improvement_message(self, request: ImprovementRequest) -> str:
        """
        Generate a message to send to Devin for an improvement request.
        
        This formats the request in a way that Devin can understand and act on.
        """
        lines = [
            f"## Improvement Request: {request.id}",
            "",
            f"**Priority:** {request.priority.upper()}",
            f"**Category:** {request.category}",
            "",
            "### Description",
            request.description,
            "",
        ]
        
        if request.context:
            lines.append("### Context")
            for key, value in request.context.items():
                lines.append(f"- **{key}:** {value}")
            lines.append("")
        
        lines.extend([
            "### Instructions",
            "1. Analyze the issue described above",
            "2. Implement the necessary changes",
            "3. Create a PR with the improvements",
            "4. Report back with the PR URL and changes made",
        ])
        
        return "\n".join(lines)
    
    def analyze_for_improvements(self, analysis_results: Dict[str, Any]) -> List[ImprovementRequest]:
        """
        Analyze results from the Mental Models System and create improvement requests.
        
        This is the bridge between analysis and action.
        """
        requests = []
        
        # Check for failure modes that need addressing
        failure_modes = analysis_results.get("failure_modes", [])
        for fm in failure_modes:
            if fm.get("severity") in ["high", "critical"]:
                request = self.create_improvement_request(
                    description=f"Address failure mode: {fm.get('name')}\n\n{fm.get('description')}",
                    priority="high" if fm.get("severity") == "critical" else "medium",
                    category="bug_fix",
                    context={"failure_mode": fm}
                )
                requests.append(request)
        
        # Check for detected biases
        biases = analysis_results.get("detected_biases", [])
        if biases:
            request = self.create_improvement_request(
                description=f"Review and address potential biases: {', '.join(biases)}",
                priority="medium",
                category="refactor",
                context={"biases": biases}
            )
            requests.append(request)
        
        # Check for suggestions
        suggestions = analysis_results.get("suggestions", [])
        for suggestion in suggestions:
            request = self.create_improvement_request(
                description=suggestion,
                priority="low",
                category="refactor",
                context={"source": "mental_models_analysis"}
            )
            requests.append(request)
        
        return requests
    
    def get_improvement_summary(self) -> Dict[str, Any]:
        """Get a summary of all improvement requests and results."""
        pending = [r for r in self._pending_requests if r.status == "pending"]
        in_progress = [r for r in self._pending_requests if r.status == "in_progress"]
        completed = [r for r in self._completed_requests if r.success]
        failed = [r for r in self._completed_requests if not r.success]
        
        return {
            "total_requests": len(self._pending_requests),
            "pending": len(pending),
            "in_progress": len(in_progress),
            "completed": len(completed),
            "failed": len(failed),
            "success_rate": len(completed) / max(len(self._completed_requests), 1),
            "pending_requests": [
                {"id": r.id, "description": r.description[:100], "priority": r.priority}
                for r in pending
            ],
            "recent_completions": [
                {"id": r.request_id, "success": r.success, "pr_url": r.pr_url}
                for r in self._completed_requests[-5:]
            ]
        }


class ImprovementCycle:
    """
    Manages the continuous improvement cycle between analysis and implementation.
    
    The cycle:
    1. Analyze code/documents with Mental Models System
    2. Identify improvements needed
    3. Create requests for Devin
    4. Track implementation
    5. Verify improvements
    6. Feed results back into analysis
    """
    
    def __init__(self, devin_integration: DevinIntegration):
        self.devin = devin_integration
        self._cycle_count = 0
        self._improvements_made = 0
    
    async def run_cycle(self, analysis_results: Dict[str, Any]) -> Dict[str, Any]:
        """
        Run one improvement cycle.
        
        Args:
            analysis_results: Results from Mental Models System analysis
            
        Returns:
            Summary of the cycle results
        """
        self._cycle_count += 1
        
        # Generate improvement requests from analysis
        requests = self.devin.analyze_for_improvements(analysis_results)
        
        # Generate messages for each request
        messages = []
        for request in requests:
            message = self.devin.generate_improvement_message(request)
            messages.append({
                "request_id": request.id,
                "message": message,
                "priority": request.priority
            })
        
        return {
            "cycle_number": self._cycle_count,
            "requests_created": len(requests),
            "messages": messages,
            "summary": self.devin.get_improvement_summary()
        }
    
    def get_cycle_stats(self) -> Dict[str, Any]:
        """Get statistics about the improvement cycles."""
        return {
            "total_cycles": self._cycle_count,
            "total_improvements": self._improvements_made,
            "improvement_summary": self.devin.get_improvement_summary()
        }
